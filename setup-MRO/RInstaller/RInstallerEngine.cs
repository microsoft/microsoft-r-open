using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using System.Windows.Threading;
using System.Xml.Linq;
using System.Security.Principal;

using Microsoft.Deployment.WindowsInstaller;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;
using Microsoft.Win32;

using RInstaller.Controllers;
using RInstaller.Models;
using RInstaller.views;
using RInstaller.ViewModels;
using CommandLine;

namespace RInstaller
{
    public class RInstallerEngine : BootstrapperApplication
    {
        // $TEMP: this mapping should be in a global manifest
        private readonly Dictionary<string, string> skuNames = new Dictionary<string, string>()
        {
            { "RServer.msi", "R Server" },
            { "RClient.msi", "R Client" },
            { "ROpen.msi", "MRO" }
        };

        private readonly Dictionary<string, long> productSize = new Dictionary<string, long>()
        {
            { "MRO", 500000000 },
            { "R Client", 4500000000 },
            { "R Server", 4500000000 }
        };

        private readonly XNamespace _manifestNamespace = (XNamespace)"http://schemas.microsoft.com/wix/2010/BootstrapperApplicationData";

        /// 
        /// Fetch BootstrapperApplicationData.xml and parse into XDocument.
        /// 
        public XElement ApplicationData
        {
            get
            {
                var workingFolder = Path.GetDirectoryName(this.GetType().Assembly.Location);
                var bootstrapperDataFilePath = Path.Combine(workingFolder, "BootstrapperApplicationData.xml");

                using (var reader = new StreamReader(bootstrapperDataFilePath))
                {
                    var xml = reader.ReadToEnd();
                    var xDoc = XDocument.Parse(xml);
                    return xDoc.Element(_manifestNamespace + "BootstrapperApplicationData");
                }
            }
        }

        private IList<SqlInstance> GetSqlInstances()
        {
            var instances = new List<SqlInstance>();

            try
            {
                var sqlBindR = new SqlBindR();

                if (sqlBindR != null)
                {
                    instances = sqlBindR.List();
                }
                else
                {
                    Engine.Log(LogLevel.Standard, "Error creating SqlBindR wrapper");
                }
            }
            catch
            {
                Engine.Log(LogLevel.Standard, "Error retrieving SQL instances");
            }

            return instances;
        }

        private ActionResult BindSqlInstances(ActionResult applyResult)
        {
            var sqlBindR = new SqlBindR();

            Model.PackagePercentage = 0;
            int instanceIncrementAmount = 100;
            if (SqlInstances.Count > 0) instanceIncrementAmount = 100 / instanceIncrementAmount;

            ActionResult actionResult = ActionResult.Success;

            foreach (SqlInstance sqlInstance in this.SqlInstances)
            {
                if (sqlInstance.Checked)
                {
                    if (!sqlInstance.Bound || Command.Action == LaunchAction.Install)
                    {
                        var features = new List<string>();
                        int exitCode = 0;

                        foreach (string featureName in new string[] { "Models", "Python" })
                        {
                            var feature = GetPackageFeature(featureName);

                            if (feature != null && feature.ShowSelected)
                            {
                                features.Add(featureName);
                            }
                        }

                        string message = string.Format("Binding {0} with features \"{1}\"", sqlInstance.Id, string.Join(" ", features.ToArray()));
                        Engine.Log(LogLevel.Standard, message);
                        Model.CurrentPackage = message;
                        exitCode = sqlBindR.Bind(this.Engine, sqlInstance.Id, Path.GetTempPath(), features.ToArray());
                        
                        if (exitCode != 0)
                        {
                            Engine.Log(LogLevel.Error, string.Format("{0}: bind error {1}", sqlInstance.Id, exitCode));
                            actionResult = ActionResult.Failure;
                        }
                    }
                    else
                    {
                        string message = string.Format("Not performing any change to instance: {0}", sqlInstance.Id);
                        Engine.Log(LogLevel.Standard, message);
                    }
                }
                else
                {
                    if (sqlInstance.Bound)
                    {
                        string message = string.Format("Unbinding {0}", sqlInstance.Id);
                        Engine.Log(LogLevel.Standard, message);
                        Model.CurrentPackage = message;
                        int exitCode = sqlBindR.Unbind(this.Engine, sqlInstance.Id);

                        if (exitCode != 0)
                        {
                            Engine.Log(LogLevel.Error, string.Format("{0}: unbind error {0}", sqlInstance.Id, exitCode));
                            actionResult = ActionResult.Failure;
                        }
                    }
                    else
                    {
                        string message = string.Format("Not performing any change to instance: {0}", sqlInstance.Id);
                        Engine.Log(LogLevel.Standard, message);
                    }
                }

                Model.PackagePercentage = Model.PackagePercentage += instanceIncrementAmount;
            }

            Model.PackagePercentage = 100;

            return actionResult;
        }

        private void HandlePackageDetected(object sender, DetectPackageCompleteEventArgs e)
        {
            Engine.Log(LogLevel.Standard, string.Format("Package {0} Detected in State {1}", e.PackageId, e.State));

            try
            {
                BundlePackage package = Packages.First(pkg => pkg.Id == e.PackageId);
                Engine.Log(LogLevel.Standard, package.DisplayName);
                package.ShowSelected = true;
            }
            catch
            {
                Engine.Log(LogLevel.Standard, "Package Not Available");
            }
        }

        private void HandleDetectComplete(object sender, DetectCompleteEventArgs e)
        { /* release the main thread to continue with work */ }

        private IList<BundlePackage> GetPackages()
        {
            List<BundlePackage> bundlePackages = new List<BundlePackage>();

            //
            // parse the ApplicationData to find included packages and features
            //
            var bundleManifestData = this.ApplicationData;
            var bundleDisplayName = bundleManifestData
                                      .Element(_manifestNamespace + "WixBundleProperties" )
                                      .Attribute( "DisplayName")
                                      .Value;
                                      
            BundleId = bundleManifestData.Element(_manifestNamespace + "WixBundleProperties").Attribute("Id").Value;
            BundleDisplayName = bundleManifestData.Element(_manifestNamespace + "WixBundleProperties").Attribute("DisplayName").Value;

            var mbaPrereqs = bundleManifestData.Descendants(_manifestNamespace + "WixMbaPrereqInformation")
                                               .Select(x => new MBAPreReqPackage(x))
                                               .ToList();

            //
            //exclude the MBA prereq packages, such as the .Net 4 installer
            //
            var pkgs = bundleManifestData.Descendants(_manifestNamespace + "WixPackageProperties")
                                         .Select(x => new BundlePackage(x))
                                         .Where(pkg => !mbaPrereqs.Any(preReq => preReq.PackageId == pkg.Id));

            //
            // Add the packages to a collection of BundlePackages
            //
            bundlePackages.AddRange(pkgs);

            //
            // check for features and associate them with their parent packages
            //
            var featureNodes = bundleManifestData.Descendants(_manifestNamespace + "WixPackageFeatureInfo");
            
            foreach (var featureNode in featureNodes)
            {
                var feature = new PackageFeature(featureNode);
                var parentPkg = bundlePackages.First(pkg => pkg.Id == feature.PackageId);

                // $HACK: Windows Installer does not honor sub-feature ordering
                if (feature.Feature == "Core")
                {
                    parentPkg.AllFeatures.Insert(0, feature);
                }
                else
                {
                    parentPkg.AllFeatures.Add(feature);
                }

                feature.Package = parentPkg;
            }

            return bundlePackages;
        }

        public long GetRequiredSpace()
        {
            var masterBundle = GetMasterBundle();
            string productName = skuNames[masterBundle.Id];
            long size = productSize[productName];
            return size;
        }

        public RInstallerEngine()
        {
            ApplyResult = ActionResult.NotExecuted;
            Packages = GetPackages();
        }

        static public Dispatcher BootstrapperDispatcher {get; private set;}

        public RInstallerModel Model;
        
        public IList<BundlePackage> Packages { get; set; }

        public IList<SqlInstance> SqlInstances { get; set; }

        public string BundleId { get; set; }
        
        public string BundleDisplayName { get; set; }

        public bool BundleInstalled { get; set; }

        public ActionResult ApplyResult { get; set; }

        private bool _cancelled = false;

        public bool Cancelled
        {
            get
            {
                return _cancelled;
            }
            set
            {
                _cancelled = value;
            }
        }

        private bool _isOffline = false;
        public bool IsOffline
        {
            get
            {
                return GetOfflineFwlinks().Length > 0;
            }
        }

        public string[] GetOfflineFwlinks()
        {
            var offlineFwlinks = new List<string>();

            foreach (var feature in GetMasterBundle().AllFeatures)
            {
                if (feature.ShowSelected)
                {
                    // $TODO: make this configuration-driven
                    if (feature.Feature.Equals("RServer", StringComparison.OrdinalIgnoreCase))
                    {
                        GetOfflineFwlinksHelper(offlineFwlinks, "SRO", CabVersions.SroVersion);
                    }
                    else if (feature.Feature.Equals("Models", StringComparison.OrdinalIgnoreCase))
                    {
                        GetOfflineFwlinksHelper(offlineFwlinks, "MLM", CabVersions.MlmVersion);
                    }
                    else if (feature.Feature.Equals("Python", StringComparison.OrdinalIgnoreCase))
                    {
                        GetOfflineFwlinksHelper(offlineFwlinks, "SPO", CabVersions.SpoVersion);
                        GetOfflineFwlinksHelper(offlineFwlinks, "SPS", CabVersions.SpsVersion);
                    }
                }
            }

            return offlineFwlinks.ToArray();
        }

        private string GetOfflineFwlinksHelper(List<string> offlineFwlinks, string componentName, string componentVersion)
        {
            var rsetup = new RSetup(componentName, componentVersion, "1033");
            string fwlink = null;

            if (!rsetup.CheckCache(Engine.StringVariables["MediaFolder"], true))
            {
                int fwlinkId = rsetup.CheckUrl(_isOffline);

                if (fwlinkId > 10000)
                {
                    offlineFwlinks.Add(string.Format("https://go.microsoft.com/fwlink/?LinkId={0}&clcid=1033", fwlinkId));
                }
            }

            return fwlink;
        }

        private void LayoutCabs(string layoutDir)
        {
            foreach (var feature in GetMasterBundle().AllFeatures)
            {
                if (feature.ShowSelected)
                {
                    if (feature.Feature.Equals("RServer", StringComparison.OrdinalIgnoreCase))
                    {
                        var rsetup = new RSetup("SRO", CabVersions.SroVersion, "1033");
                        rsetup.CacheCab(layoutDir);
                    }
                    else if (feature.Feature.Equals("Models", StringComparison.OrdinalIgnoreCase))
                    {
                        var rsetup = new RSetup("MLM", CabVersions.MlmVersion, "1033");
                        rsetup.CacheCab(layoutDir);
                    }
                    else if (feature.Feature.Equals("Python", StringComparison.OrdinalIgnoreCase))
                    {
                        var rsetup = new RSetup("SPO", CabVersions.SpoVersion, "1033");
                        rsetup.CacheCab(layoutDir);

                        rsetup = new RSetup("SPS", CabVersions.SpsVersion, "1033");
                        rsetup.CacheCab(layoutDir);
                    }
                }
            }
        }

        readonly ManualResetEventSlim _mainThreadResetEvent = new ManualResetEventSlim(false);

        private void EvaluateCommandLine(CommandLineOptions options)
        {
#if DEBUG
            if (options.Debug)
            {
                Debugger.Launch();
            }
#endif

            if (options.Cleanup)
            {
                CleanupInstallation();
                Environment.Exit(0);
            }

            if (options.Offline)
            {
                _isOffline = true;
            }

            if (!string.IsNullOrEmpty(options.InstallDir))
            {
                Engine.StringVariables["InstallFolder"] = options.InstallDir;
            }

            if (!string.IsNullOrEmpty(options.CacheDir))
            {
                Engine.StringVariables["CacheFolder"] = options.CacheDir;
            }

            if (!string.IsNullOrEmpty(options.MediaDir))
            {
                Engine.StringVariables["MediaFolder"] = options.MediaDir;
            }

            if (options.Models)
            {
                var packageFeature = GetPackageFeature("Models");

                if (packageFeature != null)
                {
                    packageFeature.ShowSelected = true;
                }
            }

            if (options.Python)
            {
                var packageFeature = GetPackageFeature("Python");

                if (packageFeature != null)
                {
                    packageFeature.ShowSelected = true;
                }
            }
        }

        public ActionResult PostInstallTask()
        {
            Model.CurrentPackage = "Running Post Install Tasks";
            Model.CurrentMessage = "Post Install";
            ActionResult applyResult = BindSqlInstances(ApplyResult);

            return applyResult;
        }

        public string GetInstallPath()
        {
            string registryPath = string.Format(@"HKLM\Software\Microsoft\{0}", skuNames[GetMasterBundle().Id]);
            return Utilities.GetRegistryValue(RegistryView.Registry64, registryPath, "Path");
        }

        private PackageFeature GetPackageFeature(string featureName)
        {
            PackageFeature packageFeature = null;

            foreach (var package in Packages)
            {
                var feature = package.AllFeatures.SingleOrDefault(x => string.Equals(x.Feature, featureName, StringComparison.OrdinalIgnoreCase));

                if (feature != null)
                {
                    packageFeature = feature;
                    break;
                }
            }

            return packageFeature;
        }

        private BundlePackage GetMasterBundle()
        {
            BundlePackage masterBundle = null;

            foreach (var bundle in Packages)
            {
                if (string.Equals(bundle.Id, "ROpen.msi", StringComparison.OrdinalIgnoreCase))
                {
                    masterBundle = bundle;
                    break;
                }
                else
                {
                    foreach (var feature in bundle.AllFeatures)
                    {
                        if (string.Equals(feature.Feature, "RServer", StringComparison.OrdinalIgnoreCase))
                        {
                            masterBundle = bundle;
                            break;
                        }
                    }
                }
            }

            return masterBundle;
        }
        
        private void CleanupInstallation()
        {
            var masterBundle = GetMasterBundle();
            string skuName = skuNames[masterBundle.Id];
            string upgradeCode = masterBundle.UpgradeCode;
            string productCode = null;
            string installFolder = null;
            var packageIds = new List<string>();
            RegistryKey key;

            Engine.Log(LogLevel.Standard, "Cleaning up registry");

            key = Utilities.GetRegistryKey(RegistryView.Registry64, @"HKLM\Software\Microsoft", RegistryKeyPermissionCheck.ReadWriteSubTree);
            if (key != null)
            {
                foreach (string name in key.GetSubKeyNames())
                {
                    if (string.Equals(name, skuName, StringComparison.OrdinalIgnoreCase))
                    {
                        var subKey = key.OpenSubKey(name);

                        if (subKey != null)
                        {
                            productCode = subKey.GetValue("ProductCode") as string;
                            installFolder = subKey.GetValue("Path") as string;

                            subKey.Close();
                        }

                        try
                        {
                            Engine.Log(LogLevel.Standard, string.Format("Deleting registry key: {0}\\{1}", key.ToString(), name));
                            key.DeleteSubKeyTree(name);
                        }
                        catch (Exception ex)
                        {
                            Engine.Log(LogLevel.Error, string.Format("Error deleting registry key: {0}", ex.Message));
                        }

                        break;
                    }
                }
            }

            if (!string.IsNullOrEmpty(upgradeCode))
            {
                string obfuscatedUpgradeCode = Utilities.ConvertToRegistryFormat(new Guid(upgradeCode));
                string[] regPaths = new string[]
                {
                    @"HKLM\SOFTWARE\Classes\Installer\UpgradeCodes",
                    @"HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Installer\UpgradeCodes"
                };

                foreach (string regPath in regPaths)
                {
                    key = Utilities.GetRegistryKey(RegistryView.Registry64, regPath, RegistryKeyPermissionCheck.ReadWriteSubTree);

                    if (key != null)
                    {
                        Engine.Log(LogLevel.Standard, string.Format("Deleting registry key: {0}\\{1}", key.ToString(), obfuscatedUpgradeCode));
                        key.DeleteSubKeyTree(obfuscatedUpgradeCode, false);
                    }
                }
            }

            if (!string.IsNullOrEmpty(productCode))
            {
                string obfuscatedProductCode = Utilities.ConvertToRegistryFormat(new Guid(productCode));
                string[] regPaths = new string[]
                {
                    @"HKLM\SOFTWARE\Classes\Installer\Features",
                    @"HKLM\SOFTWARE\Classes\Installer\Products",
                    string.Format(@"HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Installer\UserData\{0}\Products", new SecurityIdentifier(WellKnownSidType.LocalSystemSid, null).Value)
                };

                foreach (string regPath in regPaths)
                {
                    key = Utilities.GetRegistryKey(RegistryView.Registry64, regPath, RegistryKeyPermissionCheck.ReadWriteSubTree);

                    if (key != null)
                    {
                        Engine.Log(LogLevel.Standard, string.Format("Deleting registry key: {0}\\{1}", key.ToString(), obfuscatedProductCode));
                        key.DeleteSubKeyTree(obfuscatedProductCode, false);
                    }
                }
            }

            if (!string.IsNullOrEmpty(BundleDisplayName))
            {
                foreach (var view in new RegistryView[] { RegistryView.Registry64, RegistryView.Registry32 })
                {
                    key = Utilities.GetRegistryKey(view, @"HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall", RegistryKeyPermissionCheck.ReadWriteSubTree);

                    if (key != null)
                    {
                        foreach (string name in key.GetSubKeyNames())
                        {
                            var subKey = key.OpenSubKey(name);
                            bool isMatch = false;

                            if (subKey != null)
                            {
                                isMatch = string.Equals(subKey.GetValue("DisplayName") as string, BundleDisplayName);
                                subKey.Close();
                            }

                            if (isMatch)
                            {
                                Engine.Log(LogLevel.Standard, string.Format("Deleting registry key: {0}\\{1}", key.ToString(), name));
                                key.DeleteSubKeyTree(name, false);

                                packageIds.Add(name);
                            }
                        }
                    }
                }
            }

            Engine.Log(LogLevel.Standard, "Cleaning up files");

            if (!string.IsNullOrEmpty(installFolder) && Directory.Exists(installFolder))
            {
                try
                {
                    Engine.Log(LogLevel.Standard, string.Format("Deleting folder: {0}", installFolder));
                    Directory.Delete(installFolder, true);
                }
                catch (Exception ex)
                {
                    Engine.Log(LogLevel.Error, string.Format("Error deleting folder: {0}", ex.Message));
                }
            }

            if (packageIds.Count > 0)
            {
                string packageCacheFolder = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData), "Package Cache");

                foreach (string packageId in packageIds)
                {
                    string packageFolder = Path.Combine(packageCacheFolder, packageId);

                    if (Directory.Exists(packageFolder))
                    {
                        try
                        {
                            Engine.Log(LogLevel.Standard, string.Format("Deleting folder: {0}", packageFolder));
                            Directory.Delete(packageFolder, true);
                        }
                        catch (Exception ex)
                        {
                            Engine.Log(LogLevel.Error, string.Format("Error deleting folder: {0}", ex.Message));
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Entry point that is called when the bootstrapper application is ready to run.
        /// </summary>
        protected override void Run()
        {
            Engine.Log(LogLevel.Standard, "Entered RInstaller.dll");

            // Relaunch as admin if not one
            if (IsAdministrator() == false)
            {
                var exeName = Process.GetCurrentProcess().MainModule.FileName;
                ProcessStartInfo startInfo = new ProcessStartInfo(exeName);

                string arguments = String.Join(" ", Command.GetCommandLineArgs());

                switch (Command.Action)
                {
                    case LaunchAction.Uninstall:
                        arguments += " /uninstall";
                        break;
                    case LaunchAction.Modify:
                        arguments += " /modify";
                        break;
                    case LaunchAction.Repair:
                        arguments += " /repair";
                        break;
                    case LaunchAction.Layout:
                        arguments += " /layout";
                        break;
                    default:
                        arguments += " /install";
                        break;
                }

                switch (Command.Display)
                {
                    case Display.Passive:
                        arguments += " /passive";
                        break;
                    case Display.None:
                        arguments += " /quiet";
                        break;
                    default:
                        arguments += " /full";
                        break;
                }

                startInfo.Arguments = arguments;

                startInfo.Verb = "runas";
                Process.Start(startInfo);

                Dispatcher.CurrentDispatcher.InvokeShutdown();
                Engine.Quit(0);
                return;
            }

            // initialize burn variables
            Engine.StringVariables["CacheFolder"] = Path.GetTempPath();
            Engine.StringVariables["MediaFolder"] = Path.GetTempPath();

            // Replacing this
            // ParseCommandLine(Command.GetCommandLineArgs());
            CommandLineOptions options = new CommandLineOptions();
            string[] args = Utilities.FormatArguments(Command.GetCommandLineArgs());
            Parser.Default.ParseArguments(args, options);
            EvaluateCommandLine(options);

            SqlInstances = GetSqlInstances();

            DetectPackageComplete += HandlePackageDetected;
            DetectComplete += HandleDetectComplete;

            BootstrapperDispatcher = Dispatcher.CurrentDispatcher;
            Model = new RInstallerModel(this);

            if (Command.Action == LaunchAction.Layout)
            {
                LayoutCabs(Engine.StringVariables["CacheFolder"]);
            }
            else if (Command.Display == Display.Passive || Command.Display == Display.None || Command.Display == Display.Embedded)
            {
                // Change any defaults here

                // Start install
                Engine.Log(LogLevel.Standard, "Starting install...");

                Thread thread = Model.Start();
                thread.Join();
                BootstrapperDispatcher.InvokeShutdown();
            }
            else // Show UI
            {
                IRInstallerMainViewModel mainWindow = new RInstallerMainViewModel(Model);
                MainWindow window = new MainWindow(mainWindow);
                window.Closed += (sender, e) => BootstrapperDispatcher.InvokeShutdown();
                window.Show();

                Dispatcher.Run();
            }
            
            Engine.Quit((int)ApplyResult);
        }

        private static bool IsAdministrator()
        {
            WindowsIdentity identity = WindowsIdentity.GetCurrent();
            WindowsPrincipal principal = new WindowsPrincipal(identity);
            return principal.IsInRole(WindowsBuiltInRole.Administrator);
        }
    }
}

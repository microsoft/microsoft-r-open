using Cake.Core.IO;
using Microsoft.ShipR.Build.Tasks.Test;

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

Setup(context =>
{
    if (IsRunningOnWindows())
    {
        CreateDirectory("c:\\temp");

        System.Environment.SetEnvironmentVariable("temp", "c:\\temp");
        System.Environment.SetEnvironmentVariable("tmp", "c:\\temp");
        System.Environment.SetEnvironmentVariable("NUGET_PACKAGES", "c:\\temp");
    }
});


public static int RunProcess(string command,
                            string arguments,
                            out string output,
                            out string error)
{
    ProcessStartInfo startInfo = new ProcessStartInfo();
    startInfo.CreateNoWindow = true;
    startInfo.RedirectStandardOutput = true;
    startInfo.RedirectStandardInput = true;
    startInfo.RedirectStandardError = true;
    startInfo.Arguments = arguments;
    startInfo.FileName = command;
    startInfo.UseShellExecute = false;

    Process p = new Process();
    p.StartInfo = startInfo;
    p.Start();
    p.WaitForExit();

    output = p.StandardOutput.ReadToEnd();
    error = p.StandardError.ReadToEnd();
    return p.ExitCode;
}

private void RunCommand (string command, string arguments)
{
    Information("running command " + command + " " + arguments);
    if (StartProcess(command, arguments) != 0)
        throw new Exception("error while running " + command + " " + arguments);
}


private void Templated (string inFile, string outDir, Dictionary<string,string> tokens)
{
    Information("writing template from " + inFile + " to " + outDir);

    var transform = TransformTextFile(inFile);
    foreach (KeyValuePair<string,string> kv in tokens) {
        transform = transform.WithToken(kv.Key, kv.Value);
    }
    string transformed = transform.ToString();

    // do not call .Save(outFile) because it adds a random byte at the
    // begining of the file
    System.IO.File.WriteAllText(outDir + "/" + System.IO.Path.GetFileName(inFile),
                                transformed);
}


private void ChMod (string mode, string path)
{
    string arguments = "";
    if (path.Last() == '/') {
        Information("recursively changing mode on " + path + " to " + mode);
        arguments = "-R " + mode + " '" + path + "'";
    }
    else {
        Information("changing mode on " + path + " to " + mode);
        arguments = mode + " '" + path + "'";
    }

    if (StartProcess("chmod", arguments) != 0)
        throw new Exception("failed chmod " + arguments);
}


private bool IsRunningInDocker()
{
    return IsRunningOnUnix() && FileExists("/.dockerenv");
}

private void ReplaceFileString(string source, string dest, string key, string valueString)
{
    string text = System.IO.File.ReadAllText(source);
    text = text.Replace(key, valueString);
    System.IO.File.WriteAllText(dest, text);
}

private string GetVersionFromInstallFiles(string[] installFiles)
{
    string regexMatchString = "";
    if(IsRunningOnMac())
    {
        regexMatchString = @"[0-9]+\.[0-9]+\.[0-9+]";
    }
    else if(IsRunningOnUnix())
    {
        regexMatchString = @"[0-9]+\.[0-9+]";
    }
    else if (IsRunningOnWindows())
    {
        regexMatchString = @"[0-9]+\.[0-9]+\.[0-9+]";
    }

    var match = Regex.Match(installFiles[0], regexMatchString);
    if (match.Captures.Count == 0)
    {
        throw new Exception("Unable to retrieve version number from install files");
    }

    var version = match.Captures[0].Value;
    return version;
}

private void runIOQ(string computeContextFileName)
{
    string testScriptsFolder = "testScripts";
    var details = new Microsoft.ShipR.Build.TestDetails { Item = System.IO.Path.Combine(testScriptsFolder, "initializeHadoop.R"), RunnerType = "RUnit", ParallelCount = 6 };
    details.Options.Add("timeout", "02:00:00");
    details.Options.Add("sourceFile", System.IO.Path.Combine(testScriptsFolder, computeContextFileName));

    var runner = new Microsoft.ShipR.Build.Tasks.Test.RUnitTestRunner();
    runner.Initialize(Context);
    runner.ExecuteAsync(details, System.Threading.CancellationToken.None).GetAwaiter().GetResult();

    details.Item = System.IO.Path.Combine(testScriptsFolder, "subTests", "HadoopIOQ_*.R");
    runner.ExecuteAsync(details, System.Threading.CancellationToken.None).GetAwaiter().GetResult();
}

private void CopyLogFiles(string logName, string destinationDir)
{
    string logPath = string.Format("{0}{1}_*.log", System.IO.Path.GetTempPath(), logName);

    CreateDirectory(destinationDir);
    CopyFiles(logPath, string.Format("./{0}", destinationDir));
}

private string[] GetInstallFiles(string productName)
{
    Information("Getting Install Files for " + productName);

    if (productName == "Microsoft_R_Open")
    {
        if (IsRunningOnMac())
        {
            return System.IO.Directory.GetFiles("target/MRO/Mac", "*.pkg");
        }
        else if (IsRunningOnLinux())
        {
            string targetDirectory = "target/MRO/Linux";
            return System.IO.Directory.GetFiles(targetDirectory, "microsoft-r-open-mro*", System.IO.SearchOption.TopDirectoryOnly);
        }
        else
        {
            return System.IO.Directory.GetFiles("target/Release", "ROpenSetup.exe", System.IO.SearchOption.TopDirectoryOnly);
        }
    }
    else
    {
        Information("Invalid product name");
        return null;
    }
}

private void GenerateCabs(Dictionary<string, string> cabList)
{
    string cabsDirectory = "target/Cabs/";

    CreateDirectory(cabsDirectory);

    foreach(var item in cabList)
    {
        string cabName = item.Key;
        Version cabVersion;

        if (RBuildEnvironment.BuildDetails.AlternateVersions.ContainsKey(cabName))
        {
            Information(cabName + " alternate version found");
            cabVersion = new Version(RBuildEnvironment.BuildDetails.AlternateVersions[cabName].ProductVersion);
        }
        else
        {
            Information(cabName + " version information does not exist in branch.json, using productVersion instead");
            cabVersion = new Version(RBuildEnvironment.BuildDetails.VersionInformation.ProductVersion);
        }

        if (cabVersion.Revision < 0)
        {
            cabVersion = new Version(cabVersion.Major, cabVersion.Minor, cabVersion.Build, 0);
        }

        string ARG_POSTFIX = "_" + cabVersion.ToString(4) + "_1033.cab"; // 1033 is English
        string ARG_PREFIX = "-NoProfile -ExecutionPolicy Bypass .\\dependencies\\CabGen\\cabgen.ps1 ";
        string cabDestination = System.IO.Path.GetFullPath(cabsDirectory + cabName + "/" + cabName + ARG_POSTFIX);

        CreateDirectory(cabsDirectory + cabName);

        if(!FileExists(cabDestination))
        {
            string srcPath = System.IO.Path.GetFullPath(item.Value);
            string args = ARG_PREFIX + "-cabName " + cabDestination + " -srcPath " + srcPath;

            Information(string.Format("RUNNING CAB WITH ARGS: {0}", args));
            if(StartProcess("powershell", new ProcessSettings { Arguments =  args }) != 0)
            {
                throw new Exception(string.Format("Creating the {0} cab failed!", cabName));
            }
        }
        else
        {
            Information(string.Format("Cab: {0}, already exists, skipping creation.", cabName));
            continue;
        }

        // The cab powershell script doesn't return an exit code, so check and see if the cab exists if
        // we didn't continue already
        if(!FileExists(cabDestination))
        {
            throw new Exception(string.Format("Failed to generate cab file at {0}", cabDestination));
        }
    }
}

private void RunWindowsInstall(string productName, string[] installFiles)
{
    Information("Launching Windows install...");
    var installFile = installFiles.First(file => file.EndsWith(".exe"));

    string output;
    string error;
    string exe = string.Format("{0}", installFile);
    string args = string.Format("/install /quiet /installdir=\"C:\\Program Files\\Microsoft\\{0}\"", productName);

    Information(string.Format("Running: {0} {1}", installFile, args));

    int exitCode = RunProcess(exe, args, out output, out error);
    CopyLogFiles(productName, "logs");

    if (exitCode != 0)
    {

        throw new Exception(string.Format("Unable to install, failed with exit code: {0}", exitCode));
    }
}

private void RunWindowsUninstall(string productName, string[] installFiles)
{
  Information("Uninstalling Windows Install...");
  var installFile = installFiles.First(file => file.EndsWith(".exe"));
  Information(string.Format("Running file: {0}", installFile));

  string output;
  string error;
  string exe = string.Format("{0}", installFile);
  string args = string.Format("/uninstall /quiet");

  int exitCode = RunProcess(exe, args, out output, out error);
  if (exitCode != 0)
  {
      CopyLogFiles(productName, "target/logs");
      throw new Exception(string.Format("Unable to install, failed with exit code: {0}", exitCode));
 }
}

private void RunLinuxInstall(string productName, string[] installFiles)
{
    Information("Launching Linux install...");
    var debPackage = installFiles.First(file => file.EndsWith(".deb"));
    var rpmPackage = installFiles.First(file => file.EndsWith(".rpm"));

    string debOutput;
    string rpmOutput;
    string error;

    string timeStamp = DateTime.Now.ToString("yyyyMMddHHmmss");

    int exitCode = StartProcess(string.Format("bash", "target/tarballStage/microsoft-r-open/install.sh -u -a > target/logs/{0}-{1}.log", productName, timeStamp));
    if (exitCode != 0)
    {
      CopyLogFiles(productName, "target/logs");
      throw new Exception(string.Format("Unable to install, failed with exit code: {0}", exitCode));
    }

    /*
    RunProcess("which", "apt-get", out debOutput, out error);
    RunProcess("which", "rpm", out rpmOutput, out error);

    if (debOutput.Length > 0)
    {
        LaunchDebianInstall(debPackage);
    }
    else if (rpmOutput.Length > 0)
    {
        LaunchRPMInstall(rpmPackage);
    }
    else
    {
        throw new Exception("Linux install failed due to unsupported packaging system");
    }
     */
}

private void RunLinuxUninstall(string productName, string[] installFiles)
{
    return;
}

private void RunMacInstall(string productName, string[] installFiles)
{
    string installFile = installFiles.First(file => file.EndsWith(".pkg"));
    Information(string.Format("Running install with file: {0}", installFile));

    int exitCode = StartProcess("installer", new ProcessSettings{ Arguments=string.Format("-pkg {0} -target /", installFile) });

    if (exitCode != 0)
    {
        throw new Exception(string.Format("Unable to install, failed with exit code: {0}", exitCode));
    }
}

private void RunMacUninstall(string productName, string[] installFiles)
{
    string applicationPath = "/Applications/Microsoft R Open.app";
    string libraryPath = "/Library/Frameworks/R.Framework";

    Information(string.Format("Removing application: {0}", applicationPath));
    if(DirectoryExists(applicationPath))
    {
        DeleteDirectory(applicationPath, true);
    }

    // Currently removing symlinks doesn't work on Mac, so removing the directory fails
    /*
    Information(string.Format("Removing library: {0}", libraryPath));
    if(DirectoryExists(libraryPath))
    {
        DeleteDirectory(libraryPath, true);
    }
     */
}

private bool IsAdministrator()
{
    if(IsRunningOnUnix())
    {
        return true;
    }

    System.Security.Principal.WindowsIdentity identity = System.Security.Principal.WindowsIdentity.GetCurrent();
    System.Security.Principal.WindowsPrincipal principal = new System.Security.Principal.WindowsPrincipal(identity);
    return principal.IsInRole(System.Security.Principal.WindowsBuiltInRole.Administrator);
}

private bool IsRunningOnLinux()
{
    return IsRunningOnUnix() && !IsRunningOnMac();
}

private string GetDependencyPlatformString()
{
    if(IsRunningOnWindows())
    {
        return "Windows";
    }
    else if(IsRunningOnLinux())
    {
        return "Linux";
    }
    else if(IsRunningOnMac())
    {
        return "Mac";
    }
    else
    {
        throw new Exception("Unsupported platform");
    }
}

Task("LayoutR").IsDependentOn("RestoreDependencies").IsDependentOn("RestorePackages").Does(() => {
    var platformString = GetDependencyPlatformString();

    Information("Starting LayoutR");
    if(IsRunningOnMac())
    {
        // Get MRO version name
        var installFiles = System.IO.Directory.GetFiles("dependencies/R/Mac", "*.tar.gz");
        string version = GetVersionFromInstallFiles(installFiles);

        string mroDirectory = string.Format("{0}-MRO", version);

        CreateDirectory("stage/mro_mac_install_stage");
        CreateDirectory("stage/mro_mac_install_stage/R.framework");
        CreateDirectory("stage/mro_mac_install_stage/R.framework/Versions");
        CreateDirectory(string.Format("stage/mro_mac_install_stage/R.framework/Versions/{0}", mroDirectory));
        CreateDirectory("stage/mro_mac_install_stage/gui");
        CreateDirectory("stage/mro_mac_install_stage/libs");

        StartProcess("tar", new ProcessSettings{ Arguments = string.Format("-xvf ../../../../../dependencies/R/Mac/{0}.tar.gz", mroDirectory), WorkingDirectory = string.Format("./stage/mro_mac_install_stage/R.framework/Versions/{0}", mroDirectory) });
        StartProcess("tar", new ProcessSettings{ Arguments = "-xvf ../../../dependencies/mroMacGui/rgui.tar.gz", WorkingDirectory = "./stage/mro_mac_install_stage/gui" });
        CopyDirectory("packageScripts/mro/mac", "stage/mro_mac_install_stage");

        StartProcess("ln", new ProcessSettings{ Arguments = string.Format("-s {0} Current", mroDirectory), WorkingDirectory = "./stage/mro_mac_install_stage/R.framework/Versions"});

        StartProcess("ln", new ProcessSettings{ Arguments = "-s Versions/Current/Resources/include Headers", WorkingDirectory = "./stage/mro_mac_install_stage/R.framework"});
        StartProcess("ln", new ProcessSettings{ Arguments = "-s Versions/Current/Resources/lib Libraries", WorkingDirectory = "./stage/mro_mac_install_stage/R.framework"});
        StartProcess("ln", new ProcessSettings{ Arguments = "-s Versions/Current/PrivateHeaders PrivateHeaders", WorkingDirectory = "./stage/mro_mac_install_stage/R.framework"});
        StartProcess("ln", new ProcessSettings{ Arguments = "-s Versions/Current/Resources Resources", WorkingDirectory = "./stage/mro_mac_install_stage/R.framework"});
        StartProcess("ln", new ProcessSettings{ Arguments = "-s Versions/Current/R R", WorkingDirectory = "./stage/mro_mac_install_stage/R.framework"});

		CopyFiles("packageFiles/etc/Rprofile.site", "stage/mro_mac_install_stage/R.framework/Resources/etc");
		CopyFile("packageFiles/etc/Makeconf_Mac", "stage/mro_mac_install_stage/R.framework/Resources/etc/Makeconf");
		CopyFile("packageFiles/etc/Reviron.site_Mac", "stage/mro_mac_install_stage/R.framework/Resources/etc/Renviron.site");

        // Update readme text with verion number
        string text = System.IO.File.ReadAllText(@"stage/mro_mac_install_stage/README.txt");
        text = text.Replace("{VERSION_NUMBER}", version);
        System.IO.File.WriteAllText("./stage/mro_mac_install_stage/README.txt", text);
    }
    else
    {

        string stageDirectory = string.Format("stage/mro_install_stage/{0}", platformString);

        Information(string.Format("Staging R to {0}", stageDirectory));
        if(DirectoryExists(stageDirectory))
        {
            DeleteDirectory(stageDirectory, true);
        }

        CreateDirectory(stageDirectory);
        CopyDirectory(string.Format("dependencies/R/{0}", platformString), stageDirectory);
    }
});


Task("LayoutMRO").Does(() => {

    string platformString = GetDependencyPlatformString();

    /*Microsoft R Open Layout*/
    //List<string> MROPackages = new List<string> {  };
    List<string> MROExternalPackages = new List<string> { "R6", "jsonlite", "curl", "png", "doParallel", "RevoUtils", "RevoMods", "RUnit", "RevoIOQ", "MicrosoftR", "checkpoint", "foreach", "iterators" };
    List<string> MROEtcFiles = new List<string> { "Rprofile.site", "Renviron.site" };

    string MROStageDirectory = "stage/mro_install_stage";
    string MROLibraryPostFix = "library";
    string MROEtcPostfix = "etc";
    string etcSource = "packageFiles/etc";

    /* Add platform specific items to lists here! */
    if(IsRunningOnWindows())
    {
        MROExternalPackages.Add("RODBC");
        MROStageDirectory = System.IO.Path.Combine(MROStageDirectory, "Windows");
    }

    if(IsRunningOnLinux())
    {
        MROEtcFiles.Add("Renviron");
        MROEtcFiles.Add("Makeconf");
        MROStageDirectory = System.IO.Path.Combine(MROStageDirectory, "Linux");
        MROLibraryPostFix = "lib64/R/library";
        MROEtcPostfix = "lib64/R/etc";
    }

    if(IsRunningOnMac())
    {
        MROStageDirectory = System.IO.Path.Combine("stage", "mro_mac_install_stage");
        MROLibraryPostFix = "R.framework/Resources/library";
        MROEtcPostfix = "R.framework/Resources/etc";
    }

    if(!IsRunningOnMac())
    {
        MROExternalPackages.Add("deployrRserve");
    }

    /* No more platform specific stuff! */

    string MROLibraryDirectory = System.IO.Path.Combine(MROStageDirectory, MROLibraryPostFix);
    string MROEtcDirectory = System.IO.Path.Combine(MROStageDirectory, MROEtcPostfix);

    Information(string.Format("MRO Stage Directory => {0}", MROStageDirectory));
    Information(string.Format("MRO Library Directory => {0}", MROLibraryDirectory));
    Information(string.Format("MRO Etc Directory => {0}", MROEtcDirectory));

     //Pull in OSS packages
    foreach(var package in MROExternalPackages)
    {
        string source = System.IO.Path.Combine("dependencies/MRSPackagesOSS", platformString, package);

        if(DirectoryExists(source))
        {
            Information(string.Format("Found package {0} at {1}", package, source));
            CopyDirectory(source, System.IO.Path.Combine(MROLibraryDirectory, package));
        }
        else
        {
            throw new Exception(string.Format("Could not find package {0}!", package));
        }
    }

    //Pull in any extra files
    foreach(var etcFile in MROEtcFiles)
    {
        string source = System.IO.Path.Combine(etcSource, etcFile);
        string destination = System.IO.Path.Combine(MROEtcDirectory, etcFile);
        if(FileExists(source))
        {
            Information(string.Format("Found etc file {0} at {1}", etcFile, source));
            CopyFile(source, destination);
        }
        else
        {
            throw new Exception(string.Format("Could not find etc file {0}!", source));
        }
    }
});


Task("LayoutMKL").Does(() => {
    if(IsRunningOnMac())
    {
        Information("No MKL Stage required on Mac");
        return;
    }

    string mklStage = string.Format("stage/mkl_install_stage/{0}/bin/x64", GetDependencyPlatformString());
    CreateDirectory(mklStage);

    string libStage = string.Format("stage/mkl_install_stage/{0}/library", GetDependencyPlatformString());
    CreateDirectory(libStage);

    string binStage = string.Format("stage/mrs_install_stage/{0}/bin/x64", GetDependencyPlatformString());
    CreateDirectory(binStage);

    if(IsRunningOnWindows())
    {
        CopyFiles("dependencies/Intel_MKL/resources/Win/64/*.dll", binStage);
        CopyFiles("dependencies/Intel_MKL/resources/Win/64/*.dll", mklStage);
        CopyDirectory("dependencies/RevoUtilsMath/Windows", libStage);
    }
    else if(IsRunningOnLinux())
    {
        CopyFiles("dependencies/Intel_MKL/resources/Linux/64/*.so", mklStage);
        CopyFile("packageFiles/installScript/MKL_EULA.txt", mklStage + "/MKL_EULA.txt");
    }
});


//Noop task for chaining the layout flow
Task("Layout").IsDependentOn("LayoutR")
              .IsDependentOn("LayoutMRO")
              .IsDependentOn("LayoutMKL")
              .Does(() => {});

/* ------------------------------------------------------------------ */


RBuildEnvironment.Override("RestoreVendor", () => {
  if (HasArgument("noRestore")) return;
  RunDefault();
});
RBuildEnvironment.Override("RestorePackages", () => {
  if (HasArgument("noRestore")) return;
  RunDefault();
});


RBuildEnvironment.Override("RestoreDependencies", () => {
    if (HasArgument("noRestore")) return;
    RunDefault();

    if(IsRunningOnLinux())
    {
        // Fix execution persmissions; NuGet uses zip which discards them
        ChMod("a+x", "dependencies/R/Linux/lib64/R/bin/");
        ChMod("a+x", "dependencies/R/Linux/lib64/R/bin/exec/");
    }
});

private void HelperPrintDirectories(string path)
{
    var dirs = System.IO.Directory.EnumerateDirectories(path);
    Information("CAKE INFO: enumerating directories under " + path );
    foreach(string dr in dirs)
    {
        Information("CAKE INFO: " + dr );
    }
    Information("CAKE INFO: enumeration done" );
}

private void HelperPrintFiles(string path)
{
    var files = System.IO.Directory.EnumerateFiles(path);
    Information("CAKE INFO: enumerating files under " + path );
    foreach(string fl in files)
    {
        Information("CAKE INFO: " + fl );
    }
    Information("CAKE INFO: enumeration done" );
}

private bool ExpandSROSRSCabs(Version productVersion, string rserverFolder)
{
    bool result = false;

    string rSetupExe = "dependencies/RSetup/RSetup.exe";
    string sroCabPath = System.IO.Path.GetFullPath("target/Cabs/SRO");
    // string srsCabPath = System.IO.Path.GetFullPath("target/Cabs/SRS");
    var sroCandidate = GetFiles("target/Cabs/SRO/SRO*.cab").FirstOrDefault();

    if(IsAdministrator())
    {
        if(sroCandidate == null)
        {
            Warning("CAKE WARN: missing SRO candidate, skipping building azure sandbox installer");
        }
        else
        {
            Information("CAKE INFO: expanding SRO SRS cabs: productVersion=" + productVersion + ", rserverFolder=" +  rserverFolder);
            Information("CAKE INFO: found SRO Candidate: " + sroCandidate.FullPath);
            var sroVersion = (sroCandidate.GetFilenameWithoutExtension().ToString()).Replace("SRO_", "").Replace("_1033", "");
            Information("CAKE INFO: Extracted version: " + sroVersion);

            var sroProc = StartProcess(rSetupExe, new ProcessSettings { Arguments = "/install /component SRO /language en-US /version " + sroVersion + " /destdir \"" + rserverFolder + "\" /mediadir " + sroCabPath});
            // var srsProc = StartProcess(rSetupExe, new ProcessSettings { Arguments = "/install /component SRS /language en-US /version " + productVersion + " /destdir \"" + rserverFolder + "\" /mediadir " + srsCabPath});

            if(sroProc != 0)
            {
                throw new Exception("Failed to expand SRO cab using RSetup");
            }

            // CopyFile("RServer/Resources/RServerLicense.txt", System.IO.Path.Combine(rserverFolder, "RServerLicense.txt"));

            result = true;
        }
    }
    else
    {
        Warning("CAKE WARN: no admin privilegs to expand SRO cab");
    }

    return result;
}

RBuildEnvironment.Override("Build", () => {

    var productVersion = new Version(
      RBuildEnvironment.BuildDetails.VersionInformation.ProductVersion);


    var productFullVersion = new Version(RBuildEnvironment.BuildDetails.VersionInformation.FullVersion);

    var mroFullVersion = new Version(RBuildEnvironment.BuildDetails.GetVersionByName("mro").FullVersion);

    Information("Setting environment variable CMAKE_MLSERVER_VERSION to: " + productFullVersion);
    Environment.SetEnvironmentVariable("CMAKE_MLSERVER_VERSION", productFullVersion.ToString(), EnvironmentVariableTarget.Process);

    Information("Setting environment variable CMAKE_MRO_VERSION to: " + mroFullVersion);
    Environment.SetEnvironmentVariable("CMAKE_MRO_VERSION", mroFullVersion.ToString(), EnvironmentVariableTarget.Process);

    if (IsRunningOnMac())
    {
        RunDefault();
        // Install packages
        StartProcess("/usr/sbin/installer", new ProcessSettings{ Arguments = "-pkg dependencies/macOS-packager/Packages.pkg -target /"});

        // Get MRO version name
        var installFiles = System.IO.Directory.GetFiles("dependencies/R/Mac", "*.tar.gz");
        string version = GetVersionFromInstallFiles(installFiles);

        CreateDirectory("target/MRO/Mac");

        StartProcess("packagesbuild", new ProcessSettings{ Arguments = "./stage/mro_mac_install_stage/MRO.pkgproj"});
        StartProcess("mv", new ProcessSettings{ Arguments =string.Format("MRO.pkg MRO-{0}-OSX.pkg", version), WorkingDirectory = "./stage/mro_mac_install_stage/build"});

        CopyFiles("stage/mro_mac_install_stage/build/MRO*.pkg", "target/MRO/Mac");
    }
    else if(IsRunningOnUnix())
    {
        // TODO pass productVersion down to CMake
        // Run CPack
        RunDefault();

        /* -- R Open -- */
        CreateDirectory("target/MRO/Linux");
        CreateDirectory("target/MRO/SLES");
        CreateDirectory("stage/tarballStage/microsoft-r-open/rpm");
        CreateDirectory("stage/tarballStage/microsoft-r-open/rpm/rhel");
        CreateDirectory("stage/tarballStage/microsoft-r-open/rpm/sles");
        CreateDirectory("stage/tarballStage/microsoft-r-open/deb");
        CopyFile("packageFiles/installScript/install.sh", "stage/tarballStage/microsoft-r-open/install.sh");
        ChMod("a+x", "stage/tarballStage/microsoft-r-open/install.sh");
        CopyFile("packageFiles/installScript/MRO_EULA.txt", "stage/tarballStage/microsoft-r-open/MRO_EULA.txt");
        CopyFile("packageFiles/installScript/MKL_EULA.txt", "stage/tarballStage/microsoft-r-open/MKL_EULA.txt");


    }
    else // Windows
    {
        RunDefault();

        if (DirectoryExists("target/Release"))
        {
            if (DirectoryExists("target/MRO/Windows")) { DeleteDirectory("target/MRO/Windows", recursive:true); }
            CreateDirectory("target/MRO/Windows");
            MoveFiles("target/Release/ROpen/en-us/ROpen*.msi", "target/MRO/Windows/");
			CopyFile("target/Release/ROpenSetup.exe", "target/MRO/Windows/ROpenSetup.exe");
			
			
		    if (DirectoryExists("target/RInstaller")) { DeleteDirectory("target/RInstaller", recursive:true); }
            CreateDirectory("target/RInstaller");
			CopyFiles("RInstaller/*", "target/RInstaller");
			CreateDirectory("target/RInstaller/Attributes");
			CopyFiles("RInstaller/Attributes/*", "target/RInstaller/Attributes");
            CreateDirectory("target/RInstaller/Controllers");
			CopyFiles("RInstaller/Controllers/*", "target/RInstaller/Controllers");
            CreateDirectory("target/RInstaller/Models");
			CopyFiles("RInstaller/Models/*", "target/RInstaller/Models");
            CreateDirectory("target/RInstaller/Properties");
			CopyFiles("RInstaller/Properties/*", "target/RInstaller/Properties");
            CreateDirectory("target/RInstaller/Resources");
			CopyFiles("RInstaller/Resources/*", "target/RInstaller/Resources");
            CreateDirectory("target/RInstaller/ViewModels");
			CopyFiles("RInstaller/ViewModels/*", "target/RInstaller/ViewModels");
            CreateDirectory("target/RInstaller/views");
			CopyFiles("RInstaller/views/*", "target/RInstaller/views");
            CreateDirectory("target/RInstaller/views/Images");
			CopyFiles("RInstaller/views/Images/*", "target/RInstaller/views/Images");
            CreateDirectory("target/RInstaller/views/Pages");
			CopyFiles("RInstaller/views/Pages/*", "target/RInstaller/views/Pages");
        }
    }

});

Task("ExtractWixEngine").Does(() => {
    if(IsRunningOnWindows())
    {
        CreateDirectory("target/MRO/Windows/signing");
        CopyFile("target/MRO/Windows/ROpenSetup.exe", "target/MRO/Windows/signing/ROpenSetup.exe");
        var wixArgs = "-ib target/MRO/Windows/signing/ROpenSetup.exe -o target/MRO/Windows/signing/wixEngine.exe";
        RunCommand("packages/Wix.3.10.1/tools/insignia.exe", wixArgs);
    }

});

Task("AssembleDeliverables").Does(() => {
    if(IsRunningOnUnix())
    {
        if (System.IO.Directory.Exists("cmake_build_mro")) 
        {
            CopyFiles("cmake_build_mro/microsoft-r-open*.deb", "stage/tarballStage/microsoft-r-open/deb");
            CopyFiles("cmake_build_mro/microsoft-r-open*.rpm", "stage/tarballStage/microsoft-r-open/rpm/rhel");

            var tarArgs = "-czvf target/MRO/Linux/microsoft-r-open.tar.gz -C stage/tarballStage microsoft-r-open/";
            RunCommand("tar", tarArgs);
            
            DeleteDirectory("cmake_build_mro", true);
        }
        else if(System.IO.Directory.Exists("cmake_build_mro_rpm_sles"))
        {
            CopyFiles("cmake_build_mro_rpm_sles/microsoft-r-open*.rpm", "stage/tarballStage/microsoft-r-open/rpm/sles");

            var tarArgs = "-czvf target/MRO/SLES/microsoft-r-open.tar.gz -C stage/tarballStage microsoft-r-open/";
            RunCommand("tar", tarArgs);  

            DeleteDirectory("cmake_build_mro_rpm_sles", true);
        }
        else
        {
            Information("Nothing to be done");
        }
    }
    else
    {
        if (System.IO.Directory.Exists("target/MRO/Windows/signing"))
        { 
            var wixArgs = "-ab target/MRO/Windows/signing/wixEngine.exe target/MRO/Windows/signing/ROpenSetup.exe -o target/MRO/Windows/signing/microsoft-r-open.exe";
            RunCommand("packages/Wix.3.10.1/tools/insignia.exe", wixArgs); 
        }
    }

});

Task("VersionReplacement").Does(() => {
    
    // Set up versions
    string productVersion = RBuildEnvironment.BuildDetails.VersionInformation.ProductVersion;
    string productShortVersion = Regex.Match(productVersion, @"\d.\d").Value;

    string fullMroVersion = RBuildEnvironment.BuildDetails.GetVersionByName("mro").ProductVersion;
    string [] mroVersionArray = fullMroVersion.Split('.');
    string mroVersion = string.Join(".", mroVersionArray.Take(3));
    string mroShortVersion = Regex.Match(mroVersion, @"\d.\d").Value;

    Information("Versioning files");
    Information(string.Format("Product version: {0} ({1})", productVersion, productShortVersion));
    Information(string.Format("MRO version: {0} ({1})", mroVersion, mroShortVersion));

    // Replace strings in files
    ReplaceFileString(@"packageFiles/etc/Renviron", @"packageFiles/etc/Renviron", "{MRO_SHORT_VERSION}", mroShortVersion);
 
});

Task("DockerCheck").Does(() => {
    if(!IsRunningInDocker())
    {
        throw new Exception("The requested task must be run from within a docker image.");
    }
});

Task("InstallMro").IsDependentOn("RestoreDependencies").IsDependentOn("DockerCheck")
                   .Does(() => {
    var tarBall = GetFiles("target/MRO/Linux_tarball/microsoft-r-open.tar.gz").First();
    StartProcess("tar", new ProcessSettings { Arguments = "xvf " + tarBall.ToString()});

    var mroInstaller = GetFiles("microsoft-r-open/rpm/microsoft-r-open-mro-*.rpm").First();
    var intelMklInstaller = GetFiles("microsoft-r-open/rpm/microsoft-r-open-mkl-*.rpm").First();
    var mroForeachInstaller = GetFiles("microsoft-r-open/rpm/microsoft-r-open-foreachiterators-*.rpm").First();
 
    StartProcess("yum", new ProcessSettings { Arguments = "install -y " + mroInstaller.ToString()});
    StartProcess("yum", new ProcessSettings { Arguments = "install -y " + intelMklInstaller.ToString()});
    StartProcess("yum", new ProcessSettings { Arguments = "install -y " + mroForeachInstaller.ToString()});
 
});

Task("GenerateCabs").Does(() => {
    if (IsRunningOnWindows())
    {
        // Create array of cab data
        var productVersion = new Version(RBuildEnvironment.BuildDetails.VersionInformation.ProductVersion);
        Dictionary<string, string> cabList = new Dictionary<string, string>(); // (cabName, srcPath)

        cabList.Add("MKL", "stage/mkl_install_stage/Windows");
        cabList.Add("SRO", "stage/mro_install_stage/Windows");
        GenerateCabs(cabList);
    }
});

RBuildEnvironment.Override("ExtendedTest", () => {

    List<string> products = new List<string>();

    if (IsRunningOnMac() || IsRunningOnLinux() || IsRunningOnWindows())
    {
        products.Add("Microsoft_R_Open");
    }

    if (IsRunningOnWindows())
    {
        // Uninstall previous MRC install included in VS2017 shipr VM's.
        // There is a problem with the installer when upgrading where the install path is ignored. 
        // This was causing a test failure. 
        string output;
        string error;
        string exe = "msiexec";
        //string args = @"/x {02EFEF35-C9D6-465D-BB0E-EB48B549B3AB} /qn /L*v logs\Uninstall_existing_R_Client.log";
        //I don't know why the log path above causes msiexec to throw error 1622. Would be nice if it worked.
        string args = @"/x {02EFEF35-C9D6-465D-BB0E-EB48B549B3AB} /qn";
        int exitCode = RunProcess(exe, args, out output, out error);

        string userName = EnvironmentVariable("USERNAME");
        string currentDirectory = System.IO.Directory.GetCurrentDirectory();
        Information("\n running as user '{0}' in directory '{1}'", userName, currentDirectory);
        
        if (exitCode != 0 && exitCode != 1605)
        {
            //1605: This action is only valid for products that are currently installed.
            throw new Exception(string.Format("Unable to uninstall a previous install of MRC, failed with exit code: {0}", exitCode));
        }
    }

    foreach (string product in products)
    {
        Information("\n-----------------------------");
        Information(string.Format("Testing {0} Installer", product));
        Information("-----------------------------");

        string[] installFiles = GetInstallFiles(product);
        string rPath = "";

        if (installFiles.Count() == 0)
        {
            throw new Exception("Unable to find installation files.");
        }

        if (IsRunningOnMac())
        {
            // We do an uninstall first, since we aren't using clean VMs
            RunMacUninstall(product, installFiles);

            RunMacInstall(product, installFiles);

            // Check to see if files installed successfully
            if (DirectoryExists("/Applications/Microsoft R Open.app"))
            {
                Information("Files installed successfully.");
            }
            else
            {
                throw new Exception("Files not installed successfully.");
            }
        }
        else if (IsRunningOnLinux())
        {
            RunLinuxInstall(product, installFiles);

            // Check to see if files installed successfully
        }
        else
        {
            // Install current product
            RunWindowsInstall(product, installFiles);

            // Check to see if files installed successfully
            if (DirectoryExists(string.Format("C:\\Program Files\\Microsoft\\{0}", product)))
            {
                Information("Files installed successfully.");
            }
            else
            {
                throw new Exception("Files not installed successfully.");
            }


            RunWindowsUninstall(product, installFiles);

            // Check to see if uninstall is successful
            if (!DirectoryExists(string.Format("C:\\Program Files\\Microsoft\\{0}", product)))
            {
                Information("Files uninstalled successfully.");
            }
            else
            {
                throw new Exception("Files removed successfully.");
            }

            // Test Upgrade Scenario
            /*
            WebClient webClient = new WebClient();
            webClient.DownloadFile("http://mysite.com/myfile.txt", @"c:\myfile.txt");

            // Install older product
            RunWindowsInstall(product, installFiles);

            // Install newer product
            RunWindowsInstall(product, installFiles);
            */
        }
    }
});

RBuildEnvironment.Override("Sign", () => {
    RunDefault();
    CreateDirectory("target/Sign");
    CopyFile("target/Release/SqlBindR.exe", "target/Sign/SqlBindR.exe");
    MSBuild("Setup.sln", new MSBuildSettings { Configuration = "Sign" });
});

RBuildEnvironment.GetTask("Sign").TaskBuilder.IsDependentOn("RestorePackages");
RBuildEnvironment.GetTask("Sign").TaskBuilder.IsDependentOn("RestoreDependencies");
RBuildEnvironment.GetTask("Build").TaskBuilder.IsDependentOn("VersionReplacement");
RBuildEnvironment.GetTask("Build").TaskBuilder.IsDependentOn("Layout");
RBuildEnvironment.GetTask("Build").TaskBuilder.IsDependentOn("GenerateCabs");

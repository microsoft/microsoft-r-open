using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Microsoft.Deployment.WindowsInstaller;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

using RInstaller.Models;

using InstallMessage = Microsoft.Tools.WindowsInstallerXml.Bootstrapper.InstallMessage;
using Microsoft.Win32;
using System.Windows;

namespace RInstaller.Controllers
{
    public sealed partial class RInstallerModel
    {
        /*
         *  DETECTION HANDLERS GO HERE
         */
        #region DetectionHandlers


        private void OnDetectComplete(object sender, DetectCompleteEventArgs detectCompleteEventArgs) => _engineResetEvent.Set();

        private void OnDetectRelatedBundle(object sender, DetectRelatedBundleEventArgs detectRelatedBundleEventArgs)
        {
            if (_engine.BundleId == detectRelatedBundleEventArgs.ProductCode)
            {
                _engine.BundleInstalled = true;
            }
        }

        private void OnDetectMsiFeature(object sender, DetectMsiFeatureEventArgs args)
        {
            PackageFeature feature = _engine.Packages.SingleOrDefault(x => x.Id == args.PackageId)?.AllFeatures.SingleOrDefault(x => x.Feature == args.FeatureId);

            if (feature != null)
            {
                feature.FeatureState = args.State;

                if (Engine.Command.Action == LaunchAction.Modify)
                {
                    if (args.State == FeatureState.Local && feature.Optional)
                    {
                        //Shim for RSetup model problems, remove after 9.1 release
                        if(feature.Feature == "Models")
                        {
                            feature.Optional = false;
                        }

                        feature.ShowSelected = true;
                    }
                    else if (args.State == FeatureState.Absent && feature.Optional)
                    {
                        feature.ShowSelected = false;
                    }
                }
                
            }
        }

        /// <summary>
        /// This is called when we detect a given package in the bundle
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="detectPackageCompleteEventArgs"></param>
        /// <param name="args"></param>
        private void OnDetectPackageComplete(object sender, DetectPackageCompleteEventArgs args)
        {
            BundlePackage package = _engine.Packages.SingleOrDefault(x => x.Id == args.PackageId);

            if (package != null)
            {
                package.State = args.State;
            }
        }
        #endregion //DetectionHandlers

        /*
         *  PLAN HANDLERS GO HERE
         */
        #region PlanHandlers

        private void OnPlanComplete(object sender, PlanCompleteEventArgs planCompleteEventArgs) => _engineResetEvent.Set();

        private void OnPlanMsiFeature(object sender, PlanMsiFeatureEventArgs args)
        {
            PackageFeature feature = _engine.Packages.SingleOrDefault(x => x.Id == args.PackageId)?.AllFeatures.SingleOrDefault(x => x.Feature == args.FeatureId);

            if (feature != null && feature.ShowSelected && feature.FeatureState != FeatureState.Local)
            {
                //Install case
                args.State = FeatureState.Local;
            }
            else if(feature !=null && Engine.Command.Action != LaunchAction.Uninstall &&  feature.ShowSelected && feature.FeatureState == FeatureState.Local)
            {
                if (feature.Feature == "Models")
                {
                    var rServer = feature.Package.AllFeatures.Single(x => x.Feature == "RServer");
                    if (rServer.FeatureState == FeatureState.Local && !rServer.ShowSelected)
                    {
                        args.State = FeatureState.Absent;
                    }
                    else
                    {
                        args.State = FeatureState.Unknown;
                    }
                }
                else
                {
                    //Modify case
                    args.State = FeatureState.Unknown;
                }
            }
            else
            {
                //Uninstall case
                args.State = FeatureState.Absent;
            }
        }

        private void OnPlanPackageBegin(object sender, PlanPackageBeginEventArgs args)
        {
            BundlePackage package = _engine.Packages.SingleOrDefault(x => x.Id == args.PackageId);

            if (package != null)
            {
                args.State = package.RequestState;
            }
        }

        #endregion //PlanHandlers

        /*
         * APPLY HANDLERS GO HERE
         */

        #region ApplyHandlers

        private void OnApplyComplete(object sender, ApplyCompleteEventArgs applyCompleteEventArgs)
        {
            if (_engine.ApplyResult == ActionResult.Success)
            {
                ApplyResult = _engine.PostInstallTask();
            }
            else
            {
                ApplyResult = _engine.ApplyResult;
            }

            _engineResetEvent.Set();
        }

        private void OnExecutePackageComplete(object sender, ExecutePackageCompleteEventArgs executePackageCompleteEventArgs)
        {
            if (executePackageCompleteEventArgs.Status == 0)
            {
                if (_engine.ApplyResult == ActionResult.Success || _engine.ApplyResult == ActionResult.NotExecuted)
                _engine.ApplyResult = ActionResult.Success;
            }
            else
            {
                _engine.ApplyResult = ActionResult.Failure;
                ResultMessage += executePackageCompleteEventArgs.PackageId + " ";
            }
        }

        private void OnExecutePackageBegin(object sender, ExecutePackageBeginEventArgs executePackageBeginEventArgs)
        {
            if(executePackageBeginEventArgs.PackageId == "RServer.msi")
            {
                try
                {
                    MigrateDeployR();
                }  
                catch(Exception)
                {
                    // Ignore
                }
            }
            //Update UI to indicate that a package is being installed
            CurrentPackage = Packages.Single(x => x.Id == executePackageBeginEventArgs.PackageId)?.DisplayName;
        }

        private void MigrateDeployR()
        {
            using (var hklm = RegistryKey.OpenBaseKey(RegistryHive.LocalMachine, RegistryView.Registry64))
            using (var rServerKey = hklm.OpenSubKey(@"SOFTWARE\Microsoft\R Server"))
            {
                if (rServerKey != null)
                {
                    var rServerPath = rServerKey.GetValue("Path");
                    if (rServerPath != null)
                    {
                        string path = rServerPath as string;
                        if (path != null)
                        {
                            CurrentPackage = "o16n Uninstall Hook";
                            System.Diagnostics.ProcessStartInfo runProc = new System.Diagnostics.ProcessStartInfo("dotnet.exe", $"Microsoft.DeployR.Utils.AdminUtil.dll -uninstall uninstall");
                            runProc.WorkingDirectory = $"{path}\\R_SERVER\\DeployR\\Microsoft.DeployR.Utils.AdminUtil";
                            runProc.CreateNoWindow = true;
                            runProc.UseShellExecute = false;
                            var procstart = System.Diagnostics.Process.Start(runProc);
                            procstart.WaitForExit();
                        }
                    }
                }
            }
        }

        private void OnExecuteFilesInUse(object sender, ExecuteFilesInUseEventArgs executeFilesInUseEventArgs)
        {
            //Update UI to indicate that files are in use
            MessageBox.Show("Some files that need to be modified may be in use, please close all related processes and try installing again.");
        }

        private void OnExecuteComplete(object sender, ExecuteCompleteEventArgs executeCompleteEventArgs)
        {
            //Update UI to indicate that this stage is complete
        }


        private void EngineOnExecuteProgress(object sender, ExecuteProgressEventArgs executeProgressEventArgs)
        {
            OverallPercentage = executeProgressEventArgs.OverallPercentage;
            PackagePercentage = executeProgressEventArgs.ProgressPercentage;

            if (_engine.Cancelled && _engine.ApplyResult != ActionResult.UserExit)
            {
                executeProgressEventArgs.Result = Result.Cancel;
                _engine.ApplyResult = ActionResult.UserExit;
            }
        }

        private void OnExecuteMsiMessage(object sender, ExecuteMsiMessageEventArgs args)
        {
            if (args.Message.Contains("Failed"))
            {
                _engine.ApplyResult = ActionResult.Failure;
            }

            LogLevel level;
            switch (args.MessageType)
            {
                case InstallMessage.Error:
                    level = LogLevel.Error;
                    _engine.ApplyResult = ActionResult.Failure;
                    break;
                case InstallMessage.FatalExit:
                    level = LogLevel.Error;
                    _engine.ApplyResult = ActionResult.Failure;
                    break;
                case InstallMessage.Warning:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.User:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.Info:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.FilesInUse:
                    level = LogLevel.Error;
                    _engine.ApplyResult = ActionResult.Failure;
                    break;
                case InstallMessage.ResolveSource:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.OutOfDiskSpace:
                    level = LogLevel.Error;
                    _engine.ApplyResult = ActionResult.Failure;
                    break;
                case InstallMessage.ActionStart:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.ActionData:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.Progress:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.CommonData:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.Initialize:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.Terminate:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.ShowDialog:
                    level = LogLevel.Standard;
                    break;
                case InstallMessage.RMFilesInUse:
                    level = LogLevel.Standard;
                    break;
                default:
                    level = LogLevel.Standard;
                    break;
            }

            if (args.Message.Contains("Failed"))
            {
                _engine.ApplyResult = ActionResult.Failure;
            }

            _engine.Engine.Log(level, args.Message);
        }
        #endregion
    }
}

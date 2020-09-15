using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Threading;

using Microsoft.Deployment.WindowsInstaller;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

using RInstaller.Annotations;
using RInstaller.Models;
using RInstaller.ViewModels;

using ErrorEventArgs = Microsoft.Tools.WindowsInstallerXml.Bootstrapper.ErrorEventArgs;
using InstallMessage = Microsoft.Tools.WindowsInstallerXml.Bootstrapper.InstallMessage;

namespace RInstaller.Controllers
{
    public sealed partial class RInstallerModel : INotifyPropertyChanged
    {
        private readonly Regex InstallMessage1Regex = new Regex(@"^Action\s+(\d\d?):(\d\d?):(\d\d?):\s+", RegexOptions.Compiled);
        private readonly Regex InstallMessage2Regex = new Regex(@"^\S+\.\s+", RegexOptions.Compiled);
        private const string InstallFolderName = "InstallFolder";
        private const string OfflineFolderName = "MediaFolder";

        private const string TempFolderName = "TempFolder";

        private readonly RInstallerEngine _engine;

        private readonly ManualResetEventSlim _engineResetEvent = new ManualResetEventSlim(false);

        private int _overallPercentage;

        public LaunchAction LaunchAction => _engine.Command.Action;

        public RInstallerEngine Engine { get { return _engine; } }

        public string OfflinePath
        {
            get
            {
                if (_engine.Engine.StringVariables.Contains(OfflineFolderName))
                {
                    return _engine.Engine.FormatString($"[{OfflineFolderName}]");
                }

                return Path.GetTempPath();
            }
            set { _engine.Engine.StringVariables[OfflineFolderName] = value; }
        }

        public string InstallPath
        {
            get
            {
                string previousInstallPath = Engine.GetInstallPath();

                if (!string.IsNullOrEmpty(previousInstallPath))
                {
                    return previousInstallPath;
                }

                if (_engine.Engine.StringVariables.Contains(InstallFolderName))
                {
                    return _engine.Engine.FormatString($"[{InstallFolderName}]");
                }

                return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "NoDefaultSet");
            }
            set { _engine.Engine.StringVariables[InstallFolderName] = value; }
        }

        public string TempPath
        {
            get
            {
                if (_engine.Engine.StringVariables.Contains(TempFolderName))
                {
                    return _engine.Engine.FormatString($"[{TempFolderName}]");
                }

                return Path.Combine(Path.GetTempPath());
            }
            set { _engine.Engine.StringVariables[TempFolderName] = value; }
        }

        private IEnumerable<ProductAgreement> _productAgreements;

        public IEnumerable<ProductAgreement> ProductAgreements
        {
            get
            {
                var featuresByKey = Packages.SelectMany(p => p.AllFeatures).ToDictionary(f => f.Feature);

                return _productAgreements.Where(x => !featuresByKey.ContainsKey(x.FeatureId) || featuresByKey[x.FeatureId].ShowSelected);
            }
        }

        public void SetPossibleProductAgreements(IEnumerable<ProductAgreement> agreements)
        {
            _productAgreements = agreements ?? Enumerable.Empty<ProductAgreement>();
        }

        private ActionResult _applyResult;

        public ActionResult ApplyResult
        {
            get { return _applyResult; }
            set
            {
                _applyResult = value;
                _engine.ApplyResult = value;
                OnPropertyChanged(nameof(ApplyResult));
            }
        }

        private string _resultMessage;

        public string ResultMessage
        {
            get
            {
                if (_resultMessage != null)
                {
                    return _resultMessage;
                }
                else
                {
                    return "";
                }
            }
            set
            {
                _resultMessage = value;
            }
        }

        public int OverallPercentage
        {
            get
            {
                return _overallPercentage;
            }
            set
            {
                _overallPercentage = value;
                OnPropertyChanged(nameof(OverallPercentage));
            }
        }

        private int _overallTaskPercentage;

        public int OverallTaskPercentage
        {
            get { return _overallTaskPercentage; }
            set
            {
                _overallTaskPercentage = value;
                OnPropertyChanged(nameof(OverallTaskPercentage));
            }
        }

        private int _overallPackagePercentage;

        public int OverallPackagePercentage
        {
            get { return _overallPackagePercentage; }
            set
            {
                _overallPackagePercentage = value;
                OnPropertyChanged(nameof(OverallPackagePercentage));
            }
        }

        private int _packagePercentage;

        public int PackagePercentage
        {
            get { return _packagePercentage; }
            set
            {
                _packagePercentage = value;
                OnPropertyChanged(nameof(PackagePercentage));
            }
        }

        private string _currentPackage;

        public string CurrentPackage
        {
            get
            {
                return _currentPackage ?? "";
            }
            set
            {
                string actionText;
                switch (_engine.Command.Action)
                {
                    case LaunchAction.Install:
                        actionText = "Installing:";
                        break;
                    case LaunchAction.Uninstall:
                        actionText = "Removing:";
                        break;
                    case LaunchAction.Repair:
                        actionText = "Repairing";
                        break;
                    default:
                        actionText = "";
                        break;
                }

                _currentPackage = $"{actionText} {value}";
                OnPropertyChanged(nameof(CurrentPackage));
            }
        }

        private string _currentMessage;

        public string CurrentMessage
        {
            get { return _currentMessage ?? ""; }
            set
            {
                _currentMessage = value;
                OnPropertyChanged(nameof(CurrentMessage));
            }
        }

        public IList<BundlePackage> Packages => _engine.Packages;

        public IList<SqlInstance> SqlInstances => _engine.SqlInstances;

        public RInstallerModel(RInstallerEngine engine)
        {
            OverallPercentage = 0;
            _engine = engine;
            ApplyResult = ActionResult.NotExecuted;
            Detect();
        }

        public void Cancel()
        {
            _engine.Cancelled = true;
        }

        public Thread Start()
        {
            Thread thread = new Thread(() =>
                                       {
                                           MarkPackages();
                                           Plan();
                                           Apply();
                                       });

            thread.Start();

            return thread;
        }

        private void MarkPackages()
        {
            foreach (var package in Packages)
            {
                if (_engine.Command.Action == LaunchAction.Install && package.ShowSelected.HasValue && package.ShowSelected.Value)
                {
                    package.RequestState = RequestState.Present;
                }
                else if (_engine.Command.Action == LaunchAction.Modify && package.ShowSelected.HasValue && package.ShowSelected.Value)
                {
                    package.RequestState = RequestState.Present;
                }
                else if (_engine.Command.Action == LaunchAction.Uninstall)
                {
                    package.RequestState = RequestState.Absent;
                }

            }
        }

        /*
         * There are three main stages for which we interact with the burn engine:
         * 
         * Detect: This detects the state of packages on the machine and gives us information on whether 
         *         things are already installed. Our handlers must use this information and update the package
         *         models so that we know what to present to the user as installed/not installed
         *         
         * Plan:   This sets future state on the models, ie. what we want Apply() to do. Our handlers should
         *         take information that is collected from the user and use that information to set the passed
         *         in arguments appropriately. THIS MUST BE CALLED AFTER WE ASK THE USER WHAT THEY WANT TO INSTALL
         *         
         * Apply:  This does the requested actions. Our handlers should report back progress on this model so that other things can subscribe
         * 
         * The WiX bootstrapper does not currently provide Async methods for interacting with the engine. I've wrapped them
         * in functions that use a ManualResetEvent to signal completion and return control to the caller.
         * 
         */

        private void Detect()
        {
            //Wire up the event handlers
            _engine.DetectRelatedBundle += OnDetectRelatedBundle;
            _engine.DetectPackageComplete += OnDetectPackageComplete;
            _engine.DetectMsiFeature += OnDetectMsiFeature;
            _engine.DetectComplete += OnDetectComplete;
            //Set up the signal handler for completion
            _engineResetEvent.Reset();

            //Do Work
            _engine.Engine.Detect();

            //Wait for work to finish
            _engineResetEvent.Wait();

            //Clean up
            _engine.DetectRelatedBundle -= OnDetectRelatedBundle;
            _engine.DetectPackageComplete -= OnDetectPackageComplete;
            _engine.DetectMsiFeature -= OnDetectMsiFeature;
            _engine.DetectComplete -= OnDetectComplete;
        }

        private void Plan()
        {
            //Wire up event handlers
            _engine.PlanPackageBegin += OnPlanPackageBegin;
            _engine.PlanMsiFeature += OnPlanMsiFeature;
            _engine.PlanComplete += OnPlanComplete;

            //Set up the signal handler for completion
            _engineResetEvent.Reset();

            //Do Work
            _engine.Engine.Plan(_engine.Command.Action);

            //Wait for Completion
            _engineResetEvent.Wait();

            //Clean up
            _engine.PlanPackageBegin -= OnPlanPackageBegin;
            _engine.PlanMsiFeature -= OnPlanMsiFeature;
            _engine.PlanComplete -= OnPlanComplete;
        }

        private void Apply()
        {
            //Wire up event handlers
            _engine.ExecuteMsiMessage += OnExecuteMsiMessage;
            _engine.Error += EngineOnError;
            //_engine.Progress += OnProgress;
            _engine.ExecuteComplete += OnExecuteComplete;
            _engine.ExecuteFilesInUse += OnExecuteFilesInUse;
            _engine.ExecutePackageBegin += OnExecutePackageBegin;
            _engine.ExecutePackageComplete += OnExecutePackageComplete;
            _engine.ExecuteProgress += EngineOnExecuteProgress;
            _engine.ExecuteMsiMessage += EngineOnExecuteMsiMessage;
            _engine.ApplyComplete += OnApplyComplete;

            //Set up the signal handler
            _engineResetEvent.Reset();

            //Do Work
            _engine.Engine.Apply(IntPtr.Zero);

            //Wait for completion
            _engineResetEvent.Wait();

            //Clean up
            _engine.ExecuteMsiMessage -= OnExecuteMsiMessage;
            //_engine.Progress -= OnProgress;
            _engine.ExecuteComplete -= OnExecuteComplete;
            _engine.ExecuteFilesInUse -= OnExecuteFilesInUse;
            _engine.ExecutePackageBegin -= OnExecutePackageBegin;
            _engine.ExecutePackageComplete -= OnExecutePackageComplete;
            _engine.ExecuteProgress -= EngineOnExecuteProgress;
            _engine.ApplyComplete -= OnApplyComplete;
        }

        private void EngineOnError(object sender, ErrorEventArgs errorEventArgs)
        {
            Console.WriteLine(errorEventArgs.ErrorMessage);
        }

        private void EngineOnExecuteMsiMessage(object sender, ExecuteMsiMessageEventArgs executeMsiMessageEventArgs)
        {
            if (executeMsiMessageEventArgs.MessageType == InstallMessage.ActionStart)
            {
                CurrentMessage = InstallMessage1Regex.Replace(executeMsiMessageEventArgs.Message, string.Empty);
                CurrentMessage = InstallMessage2Regex.Replace(CurrentMessage, string.Empty);
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotifyPropertyChangedInvocator]
        private void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Documents;
using System.Windows.Markup;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;

using Microsoft.Win32;
using Newtonsoft.Json;

using RInstaller.Annotations;
using RInstaller.Controllers;
using RInstaller.Models;
using RInstaller.views;
using RInstaller.views.Pages;
using Microsoft.Deployment.WindowsInstaller;

namespace RInstaller.ViewModels
{
    public delegate void CommandCompletedEventHandler(object sender, EventArgs e);

    internal sealed class RInstallerMainViewModel : IRInstallerMainViewModel
    {
        readonly string[] SqlVersionKeys =
        {
            @"HKLM\SOFTWARE\Microsoft\Microsoft SQL Server\130\SQLServer2016\CurrentVersion",
            @"HKLM\SOFTWARE\Microsoft\Microsoft SQL Server\140\SQL2017\CurrentVersion"
        };

        public RInstallerMainViewModel(RInstallerModel installer)
        {
            Installer = installer;
            Packages = new List<BundlePackage>(installer.Packages);
            SqlInstances = new List<SqlInstance>(installer.SqlInstances);
            StartCommand = new DelegateCommand(() => { Installer.Start(); });
            CurrentProductAgreementIndex = 0;

            Installer.PropertyChanged += (sender, args) =>
                                     {
                                         if (args.PropertyName == nameof(Installer.ApplyResult))
                                         {
                                             CommandCompleted?.Invoke(this, new EventArgs());
                                         }
                                     };

            Assembly assembly = Assembly.GetExecutingAssembly();

            string resourceName = Path.Combine(Path.GetDirectoryName(GetType().Assembly.Location), "productResource.json");

            StreamReader reader;

            try
            {
                FileStream stream = new FileStream(resourceName, FileMode.Open, FileAccess.Read);
                reader = new StreamReader(stream);
            }
            catch
            {
                resourceName = "RInstaller.Resources.productResource.default.json";
                Stream stream = assembly.GetManifestResourceStream(resourceName);
                reader = new StreamReader(stream);
            }

            ProductResource = ProductResourceFactory.Create(reader.ReadToEnd(), Installer.LaunchAction);

            Installer.SetPossibleProductAgreements(ProductResource.ProductAgreements);
            _installationPath = installer.InstallPath;
            
            reader.Dispose();
        }

        public event PropertyChangedEventHandler PropertyChanged;

        public RInstallerModel Installer { get; }

        private string _installationPath;

        public string InstallationPath
        {
            get { return _installationPath; }
            set
            {
                _installationPath = value;
                OnPropertyChanged(nameof(InstallationPath));
            }
        }

        public string ResultMessage
        {
            get
            {
                return Installer.ResultMessage;
            }
        }

        public ProductResource ProductResource { get; }

        public IEnumerable<ProductAgreement> ProductAgreements
        {
            get { return Installer.ProductAgreements; }
        }

        public int CurrentProductAgreementIndex { get; set; }

        public ProductAgreement CurrentProductAgreement
        {
            get
            {
                var selectFrom = Installer.ProductAgreements.ToArray();
                return (CurrentProductAgreementIndex >= selectFrom.Length) ? null : selectFrom[CurrentProductAgreementIndex];
            }
        }

        public FlowDocument AgreementBody
        {
            get
            {
                string fileName = Path.Combine(Path.GetDirectoryName(GetType().Assembly.Location), CurrentProductAgreement.File);
                using (FileStream fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read))
                {
                    FlowDocument document = new FlowDocument();
                    document.Foreground = PrimaryTextColor;

                    TextRange textRange = new TextRange(document.ContentStart, document.ContentEnd);
                    textRange.Load(fileStream, DataFormats.Rtf);

                    return document;
                }
            }
        }

        public string ProductId => ProductResource.ProductId;

        public Visibility BindingVisibility
        {
            get
            {
                bool sqlVersionFound = false;

                if (ProductId == "server")
                {
                    foreach (string sqlVersionKey in SqlVersionKeys)
                    {
                        if (!string.IsNullOrEmpty(Utilities.GetRegistryValue(RegistryView.Registry64, sqlVersionKey, "Version")))
                        {
                            sqlVersionFound = true;
                            break;
                        }
                    }
                }

                return sqlVersionFound ? Visibility.Visible : Visibility.Hidden;
            }
        }

        public Uri IconUri => new Uri("pack://application:,,,/RInstaller;component/views/Images/" + ProductResource.ProductStyles.logoAsset + ".ico");

        public ImageSource HeaderLogoImage
        {
            get { return new BitmapImage(new Uri("pack://application:,,,/RInstaller;component/views/Images/" + ProductResource.ProductStyles.logoAsset + ".png")); }
        }

        public Brush WindowBackground
        {
            get { return new SolidColorBrush((Color)ColorConverter.ConvertFromString(ProductResource.ProductStyles.windowBackgroundColor)); }
        }

        public string HeaderTitlePrimary => ProductResource.ProductStrings.MarketingPrefix;

        public string HeaderTitleSecondary => ProductResource.ProductStrings.MarketingPostfix;

        public string MarketingVersion => ProductResource.ProductStrings.MarketingVersion;

        public string MarketingDescriptionPrimary => ProductResource.ProductStrings.MarketingDescriptionPrimary;

        public string MarketingDescriptionSecondary => ProductResource.ProductStrings.MarketingDescriptionSecondary;

        public Brush ProductStyleColor
        {
            get { return new SolidColorBrush((Color)ColorConverter.ConvertFromString(ProductResource.ProductStyles.productStyleColor)); }
        }
        public Brush ProductStyleColorOpaque
        {
            get
            {
                Brush brush = ProductStyleColor;
                brush.Opacity = 0.3;
                return brush;
            }
        }

        public Brush PrimaryTextColor
        {
            get { return new SolidColorBrush((Color)ColorConverter.ConvertFromString(ProductResource.ProductStyles.primaryTextColor)); }
        }

        public Brush SecondaryTextColor
        {
            get { return new SolidColorBrush((Color)ColorConverter.ConvertFromString(ProductResource.ProductStyles.secondaryTextColor)); }
        }

        public Brush LinkTextColor
        {
            get { return new SolidColorBrush((Color)ColorConverter.ConvertFromString(ProductResource.ProductStyles.linkTextColor)); }
        }

        public Brush ButtonBackgroundColor
        {
            get { return new SolidColorBrush((Color)ColorConverter.ConvertFromString(ProductResource.ProductStyles.buttonBackgroundColor)); }
        }

        public List<BundlePackage> Packages { get; }

        public List<SqlInstance> SqlInstances { get;  }

        public DelegateCommand StartCommand { get; private set; }

        public void CancelCommand()
        {
            Installer.Cancel();
        }

        public event CommandCompletedEventHandler CommandCompleted;

        [NotifyPropertyChangedInvocator]
        private void OnPropertyChanged(string propertyName) => PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }
}
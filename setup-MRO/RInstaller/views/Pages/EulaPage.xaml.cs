using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

using RInstaller.Models;
using RInstaller.ViewModels;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

namespace RInstaller.views.Pages
{
    /// <summary>
    /// Interaction logic for Page1.xaml
    /// </summary>
    public partial class EulaPage : Page, IRInstallerPage
    {
        public EulaPage(object dataContext)
        {
            InitializeComponent();
            DataContext = dataContext;

            RightButtonText = "Continue";
            LeftButtonText = "Back";
            RightButtonVisibility = Visibility.Visible;
            LeftButtonVisiblity = Visibility.Visible;

            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            viewModel.CurrentProductAgreement.Accepted = false;

            // Handle all navigation events
            eulaTextBox.AddHandler(Hyperlink.RequestNavigateEvent, new RequestNavigateEventHandler(eulaTextBox_RequestNavigate));
        }

        public string RightButtonText { get; }

        public string LeftButtonText { get; }

        public Visibility RightButtonVisibility { get; }

        public Visibility LeftButtonVisiblity { get; }

        public IRInstallerPage NavigateNext()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            IRInstallerPage page;
             
            if (viewModel.CurrentProductAgreement.Accepted)
            {                
                if (viewModel.CurrentProductAgreementIndex < (viewModel.ProductAgreements.Count() - 1))
                {
                    AdvanceAgreement();                  
                    page = this;
                }
                else
                {
                    page = new ConfirmPage(DataContext);
                    NavigationService.Navigate(page);
                }

                return page;
            }
            else
            {
                MessageBox.Show("Please accept the agreement before continuing.");
                return this;
            }
        }

        public IRInstallerPage NavigatePrevious()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            IRInstallerPage page;

            if (viewModel.CurrentProductAgreementIndex > 0)
            {
                BackAgreement();
                return this;
            }
            else
            {
                page = new CustomizePage(DataContext);
                NavigationService.Navigate(page);
                return page;
            }
        }

        private void ToggleButton_Agree(object sender, RoutedEventArgs e)
        {
            CheckBox checkBox = sender as CheckBox;
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            viewModel.CurrentProductAgreement.Accepted = (bool)checkBox.IsChecked;
        }

        public void AdvanceAgreement()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            viewModel.CurrentProductAgreementIndex++;

            PackageFeature packageFeature = AssociatedPackageFeature(DataContext);

            if (packageFeature != null && packageFeature.ShowSelected == false)
            {
                NavigateNext();
            }
            else
            {
                UpdateAgreement();
            }
        }

        public void BackAgreement()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            viewModel.CurrentProductAgreementIndex--;

            PackageFeature packageFeature = AssociatedPackageFeature(DataContext);

            if (packageFeature != null && packageFeature.ShowSelected == false)
            {
                NavigatePrevious();
            }
            else
            {
                UpdateAgreement();
            }
        }

        public void UpdateAgreement()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);

            eulaTitle.Content = viewModel.CurrentProductAgreement.Title;
            eulaTextBox.Document = viewModel.AgreementBody;
            eulaCheckbox.Content = viewModel.CurrentProductAgreement.AgreeOption;
            eulaCheckbox.IsChecked = false;
            viewModel.CurrentProductAgreement.Accepted = false;
        }

        public void eulaTextBox_RequestNavigate(object sender, RequestNavigateEventArgs e)
        {
            // Launch in new browser window
            Process.Start(new ProcessStartInfo(e.Uri.AbsoluteUri));
            e.Handled = true;
        }

        public static PackageFeature AssociatedPackageFeature(object DataContext)
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);

            foreach (BundlePackage package in viewModel.Packages)
            {
                foreach (PackageFeature feature in package.AllFeatures)
                {
                    if (feature.Feature == viewModel.CurrentProductAgreement.FeatureId)
                    {
                        return feature;
                    }
                }
            }

            return null;
        }
    }
}

using System;
using System.Collections.Generic;
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

using RInstaller.ViewModels;
using System.Diagnostics;
using Microsoft.Deployment.WindowsInstaller;

namespace RInstaller.views.Pages
{
    /// <summary>
    /// Interaction logic for Page1.xaml
    /// </summary>
    public partial class FinishPage : Page, IRInstallerPage
    {
        public FinishPage(object dataContext)
        {
            InitializeComponent();
            DataContext = dataContext;

            RightButtonText = "Finish";
            RightButtonVisibility = Visibility.Visible;
            LeftButtonVisiblity = Visibility.Hidden;

            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);

            if (viewModel.Installer.ApplyResult == ActionResult.Success)
            {
                // Don't do anything
            }
            else if (viewModel.Installer.ApplyResult == ActionResult.Failure)
            {
                RightButtonText = "Close";
                title.Content = "Install Failed";
                completeLabel.Text = "Installation has failed. Failure to install:";
                linkButton.Visibility = Visibility.Collapsed;
                failureLabel1.Visibility = Visibility.Visible;
                failureLabel2.Visibility = Visibility.Visible;
                logsButton.Visibility = Visibility.Visible;
            }
            else if (viewModel.Installer.ApplyResult == ActionResult.UserExit)
            {
                RightButtonText = "Close";
                title.Content = "Install Cancelled";
                completeLabel.Text = "Installation cancelled by user.";
                linkButton.Visibility = Visibility.Collapsed;
            }
            else
            {
                title.Content = "Unknown Error";
                completeLabel.Text = "An unknown error has occured. Please reference the logs.";
                linkButton.Visibility = Visibility.Collapsed;
                logsButton.Visibility = Visibility.Visible;
            }
        }

        public string RightButtonText { get; }

        public string LeftButtonText { get; }

        public Visibility RightButtonVisibility { get; }

        public Visibility LeftButtonVisiblity { get; }

        public IRInstallerPage NavigateNext()
        {
            return null;
        }

        public IRInstallerPage NavigatePrevious()
        {
            return this;
        }

        private void linkButton_Click(object sender, RoutedEventArgs e)
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            string url = viewModel.ProductResource.ProductStrings.MarketingUrl;

            System.Diagnostics.Process.Start(url);
        }

        private void logButton_Click(object sender, RoutedEventArgs e)
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            string path = viewModel.Installer.TempPath;

            ProcessStartInfo startInformation = new ProcessStartInfo();
            startInformation.FileName = path;
            Process proccess = Process.Start(startInformation);
        }
    }
}

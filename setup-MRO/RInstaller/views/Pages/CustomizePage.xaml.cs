using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Forms;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

using Microsoft.Win32;
using Microsoft.WindowsAPICodePack.Dialogs;

using RInstaller.ViewModels;

namespace RInstaller.views.Pages
{
    /// <summary>
    /// Interaction logic for CustomizePage.xaml
    /// </summary>
    public partial class CustomizePage : Page, IRInstallerPage
    {
        public CustomizePage(object dataContext)
        {
            IRInstallerMainViewModel viewModel = (dataContext as IRInstallerMainViewModel);

            InitializeComponent();
            DataContext = dataContext;

            RightButtonText = "Continue";
            LeftButtonText = "Back";
            RightButtonVisibility = Visibility.Visible;
            LeftButtonVisiblity = Visibility.Visible;

            // Prevent users from changing install folder in upgrade scenario
            if (!string.IsNullOrEmpty(viewModel.Installer.Engine.GetInstallPath()))
            {
                this.textBox.IsEnabled = false;
                this.browseButton.IsEnabled = false;
            }
        }

        public string RightButtonText { get; }

        public string LeftButtonText { get; }

        public Visibility RightButtonVisibility { get; }

        public Visibility LeftButtonVisiblity { get; }

        public IRInstallerPage NavigateNext()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            IRInstallerPage page;

            if (viewModel.Installer.Engine.IsOffline)
            {
                page = new OfflinePage(DataContext);
                NavigationService.Navigate(page);
                return page;
            }

            if (viewModel.ProductAgreements != null)
            {
                page = new EulaPage(DataContext);
                NavigationService.Navigate(page);
                return page;
            }

            page = new ConfirmPage(DataContext);
            NavigationService.Navigate(page);
            return page;
        }

        public IRInstallerPage NavigatePrevious()
        {
            IRInstallerPage page = new StartPage(DataContext);
            NavigationService.Navigate(page);
            return page;
        }

        private void browseButton_Click(object sender, RoutedEventArgs e)
        {
            using (CommonOpenFileDialog dlg = new CommonOpenFileDialog { IsFolderPicker = true })
            {
                dlg.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);
                CommonFileDialogResult result = dlg.ShowDialog();
                if (result == CommonFileDialogResult.Ok)
                {
                    textBox.Text = dlg.FileNames.Single();

                    IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
                    viewModel.Installer.Engine.Engine.StringVariables["InstallFolder"] = viewModel.Installer.InstallPath = textBox.Text;
                }
            }
        }
    }
}

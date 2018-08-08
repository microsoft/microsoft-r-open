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

using Microsoft.WindowsAPICodePack.Dialogs;

using RInstaller.ViewModels;

namespace RInstaller.views.Pages
{
    /// <summary>
    /// Interaction logic for Page1.xaml
    /// </summary>
    public partial class OfflinePage : Page, IRInstallerPage
    {
        public OfflinePage(object dataContext)
        {
            InitializeComponent();
            DataContext = dataContext;

            RightButtonText = "Continue";
            LeftButtonText = "Back";
            RightButtonVisibility = Visibility.Visible;
            LeftButtonVisiblity = Visibility.Visible;
            
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            this.offlineFwlinksTextbox.Text = string.Join(Environment.NewLine, viewModel.Installer.Engine.GetOfflineFwlinks());
        }

        public string RightButtonText { get; }

        public string LeftButtonText { get; }

        public Visibility RightButtonVisibility { get; }

        public Visibility LeftButtonVisiblity { get; }

        public IRInstallerPage NavigateNext()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);

            var offlineFwlinks = viewModel.Installer.Engine.GetOfflineFwlinks();

            if (offlineFwlinks.Length > 0)
            {
                string missingLinks = string.Join(Environment.NewLine, offlineFwlinks);

                MessageBox.Show(string.Format("Please download the following links before continuing:\n\n" + missingLinks));
                this.offlineFwlinksTextbox.Text = missingLinks;

                return this;
            }
            else
            {
                IRInstallerPage page;

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
        }

        public IRInstallerPage NavigatePrevious()
        {
            IRInstallerPage page = new CustomizePage(DataContext);
            NavigationService.Navigate(page);
            return page;
        }

        private void browseButton_Click(object sender, RoutedEventArgs e)
        {
            using (CommonOpenFileDialog dlg = new CommonOpenFileDialog { IsFolderPicker = true })
            {
                // $TODO: get folder of current process
                dlg.InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);
                CommonFileDialogResult result = dlg.ShowDialog();
                if (result == CommonFileDialogResult.Ok)
                {
                    IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
                    textBox.Text = viewModel.Installer.Engine.Engine.StringVariables["MediaFolder"] = dlg.FileNames.Single();
                }
            }
        }
    }
}

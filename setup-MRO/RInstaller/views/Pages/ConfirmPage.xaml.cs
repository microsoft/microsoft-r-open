using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
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
using RInstaller.Models;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

using Path = System.IO.Path;

namespace RInstaller.views.Pages
{
    /// <summary>
    /// Interaction logic for Page1.xaml
    /// </summary>
    public partial class ConfirmPage : Page, IRInstallerPage
    {
        public ConfirmPage(object dataContext)
        {
            InitializeComponent();
            DataContext = dataContext;

            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);

            RightButtonText = viewModel?.ProductResource.ProductStrings.ConfirmRightButton ?? "Continue";
            LeftButtonText = "Customize";
            RightButtonVisibility = Visibility.Visible;
            LeftButtonVisiblity = Visibility.Visible;

            if (viewModel.Installer.LaunchAction == LaunchAction.Uninstall)
            {
                LeftButtonVisiblity = Visibility.Hidden;
            }
        }

        public string RightButtonText { get; }

        public string LeftButtonText { get; }

        public Visibility RightButtonVisibility { get; }

        public Visibility LeftButtonVisiblity { get; }

        public IRInstallerPage NavigateNext()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);

            long requiredSpace = viewModel.Installer.Engine.GetRequiredSpace();

            bool enoughSpace = false;
            string installDrive = Path.GetPathRoot(viewModel.InstallationPath);
            var drive = DriveInfo.GetDrives().First(x => x.Name == installDrive);
            if (drive.AvailableFreeSpace > requiredSpace)
            {
                enoughSpace = true;
            }

            if (!enoughSpace)
            {
                // Create dialog
                float formattedSize = (requiredSpace / 1024f) / 1024f;
                MessageBox.Show(string.Format("Not enough drive space. {0} mb needed.", formattedSize));
                return this;
            }
            else
            {
                IRInstallerPage page = new ProgressPage(DataContext);
                viewModel.StartCommand.Execute();
                NavigationService.Navigate(page);
                return page;
            }
        }

        public IRInstallerPage NavigatePrevious()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            IRInstallerPage page;

            viewModel.CurrentProductAgreementIndex = 0;

            page = new CustomizePage(DataContext);
            NavigationService.Navigate(page);
            return page;
        }
    }
}

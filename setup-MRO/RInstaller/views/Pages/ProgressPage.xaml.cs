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

using RInstaller.ViewModels;
using System.Windows.Forms;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;
using Microsoft.Deployment.WindowsInstaller;

namespace RInstaller.views.Pages
{
    /// <summary>
    /// Interaction logic for Page1.xaml
    /// </summary>
    public partial class ProgressPage : Page, IRInstallerPage
    {
        public ProgressPage(object dataContext)
        {
            InitializeComponent();
            DataContext = dataContext;

            LeftButtonText = "Cancel";
            RightButtonVisibility = Visibility.Hidden;
            LeftButtonVisiblity = Visibility.Visible;

            //Wait to wire up the next page until NavigationService is available
            //Loaded += (sender, args) => WireUpNext();
        }

        private void WireUpNext()
        {
            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);

            // Careful here. Dispatcher.Invoke must be used as this event will be fired from another thread.
            Debug.Assert(viewModel != null, "viewModel != null");
            viewModel.CommandCompleted += (sender, args) => Dispatcher.Invoke((Action)(() => { NavigationService.Navigate(new FinishPage(DataContext)); }));
        }

        public string RightButtonText { get; }

        public string LeftButtonText { get; }

        public Visibility RightButtonVisibility { get; }

        public Visibility LeftButtonVisiblity { get; }

        public IRInstallerPage NavigateNext()
        {
            IRInstallerPage page = new FinishPage(DataContext);
            NavigationService.Navigate(page);
            return page;
        }

        public IRInstallerPage NavigatePrevious()
        {
            DialogResult dialogResult = System.Windows.Forms.MessageBox.Show("Are you sure you wish to cancel Installation?", "Confirm", MessageBoxButtons.YesNo);
            if (dialogResult == DialogResult.Yes)
            {
                IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
                viewModel.CancelCommand();
            }

            return this;
        }
    }
}

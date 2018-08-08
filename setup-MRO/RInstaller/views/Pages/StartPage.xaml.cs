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

namespace RInstaller.views.Pages
{
    /// <summary>
    /// Interaction logic for Page1.xaml
    /// </summary>
    public partial class StartPage : Page, IRInstallerPage
    {
        public StartPage(object dataContext)
        {
            InitializeComponent();
            DataContext = dataContext;

            RightButtonText = "Continue";
            RightButtonVisibility = Visibility.Visible;
            LeftButtonVisiblity = Visibility.Hidden;
        }

        public string RightButtonText { get; }

        public string LeftButtonText { get; }

        public Visibility RightButtonVisibility { get; }

        public Visibility LeftButtonVisiblity { get; }

        public IRInstallerPage NavigateNext()
        {
            IRInstallerPage page = new CustomizePage(DataContext);
            NavigationService.Navigate(page);
            return page;
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
    }
}

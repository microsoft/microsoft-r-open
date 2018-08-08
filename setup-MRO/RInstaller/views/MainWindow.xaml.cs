using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Interop;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

using RInstaller.Models;
using RInstaller.views.Pages;
using RInstaller.ViewModels;

namespace RInstaller.views
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public IRInstallerPage CurrentPage { get; set; }

        public MainWindow(IRInstallerMainViewModel dataContext)
        {
            InitializeComponent();
            DataContext = dataContext;
            Stream IconStream = Application.GetResourceStream(dataContext.IconUri)?.Stream;

            if (IconStream != null)
            {
                Icon img = new Icon(IconStream);
                Bitmap bitmap = img.ToBitmap();
                IntPtr hBitmap = bitmap.GetHbitmap();

                Icon = Imaging.CreateBitmapSourceFromHBitmap(hBitmap, IntPtr.Zero, Int32Rect.Empty, BitmapSizeOptions.FromEmptyOptions());
            }

            IRInstallerMainViewModel viewModel = (DataContext as IRInstallerMainViewModel);
            viewModel.CommandCompleted += (sender, args) => Dispatcher.Invoke((Action)(() =>
                                                                                       {
                                                                                           CurrentPage = CurrentPage.NavigateNext();
                                                                                           ResetNavigationButtons();
                                                                                       }));
            IRInstallerPage startPage;
            switch (viewModel.Installer.LaunchAction)
            {
                case LaunchAction.Uninstall:
                    startPage = new ConfirmPage(dataContext);
                    break;
                default:
                    startPage = new StartPage(dataContext);
                    break;
            }

            Title = string.Format("{0} {1}", viewModel.ProductResource.ProductStrings.MarketingPrefix, viewModel.ProductResource.ProductStrings.MarketingPostfix);

            CurrentPage = startPage;
            pageFrame.NavigationService.Navigate(CurrentPage);
            ResetNavigationButtons();
        }

        private void CloseButton_Click(object sender, RoutedEventArgs e)
        {
            Close();
        }

        private void MinimizeButton_Click(object sender, RoutedEventArgs e)
        {
            WindowState = WindowState.Minimized;
        }

        private void Window_MouseDown(object sender, MouseButtonEventArgs e)
        {
            if (e.ChangedButton == MouseButton.Left)
            {
                DragMove();
            }
        }

        private void RightButton_Click(object sender, RoutedEventArgs e)
        {
            CurrentPage = CurrentPage.NavigateNext();

            if (CurrentPage == null)
            {
                Close();
            }

            ResetNavigationButtons();
        }

        private void LeftButton_Click(object sender, RoutedEventArgs e)
        {
            CurrentPage = CurrentPage.NavigatePrevious();

            if (CurrentPage == null)
            {
#if DEBUG
                MessageBox.Show("Designer. Don't ever close on a left button click.");
#endif
                Close();
            }

            ResetNavigationButtons();
        }

        public void ResetNavigationButtons()
        {
            if (CurrentPage != null)
            {
                RightButton.Visibility = CurrentPage.RightButtonVisibility;
                RightButton.Content = CurrentPage.RightButtonText;
                LeftButton.Visibility = CurrentPage.LeftButtonVisiblity;
                LeftButton.Content = CurrentPage.LeftButtonText;
            }
        }
    }
}

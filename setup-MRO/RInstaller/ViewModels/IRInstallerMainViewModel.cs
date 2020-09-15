using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Documents;
using System.Windows.Media;
using System.Windows.Navigation;

using RInstaller.Controllers;
using RInstaller.Models;

namespace RInstaller.ViewModels
{
    public interface IRInstallerMainViewModel : INotifyPropertyChanged
    {
        RInstallerModel Installer { get; }

        string InstallationPath { get; set; }

        List<BundlePackage> Packages { get; }

        IEnumerable<ProductAgreement> ProductAgreements { get; }

        int CurrentProductAgreementIndex { get; set; }

        ProductAgreement CurrentProductAgreement { get; }

        FlowDocument AgreementBody { get; }

        DelegateCommand StartCommand { get; }

        ProductResource ProductResource { get; }

        ImageSource HeaderLogoImage { get; }

        Uri IconUri { get; }

        event CommandCompletedEventHandler CommandCompleted;

        void CancelCommand();

        string ResultMessage { get; }
    }
}

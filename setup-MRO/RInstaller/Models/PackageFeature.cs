using System.ComponentModel;
using System.Windows;
using System.Xml.Linq;

using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

using RInstaller.Annotations;

namespace RInstaller.Models
{
    public class PackageFeature : INotifyPropertyChanged
    {
        public string PackageId { get; }

        public string Feature { get; }

        public string Title { get; }

        public BundlePackage Package { get; set; }

        public FeatureState FeatureState { get; set; }

        public bool Optional { get; set; }

        private int _level = 1;
        public int Level
        {
            get
            {
                return _level;
            }
            set
            {
                _level = value;
            }
        }

        private bool _showSelected = true;
        public bool ShowSelected
        {
            get
            {
                return _showSelected;
            }
            set
            {
                _showSelected = value;
                OnPropertyChanged(nameof(Package.ShowSelected));
            }
        }

        public PackageFeature(XElement node)
        {
            PackageId = node.Attribute("Package")?.Value;
            Feature = node.Attribute("Feature")?.Value;
            Title = node.Attribute("Title")?.Value;
            Optional = node.Attribute("Attributes")?.Value != "16"; // $TODO: use proper bit mask

            if (int.TryParse(node.Attribute("Level")?.Value, out _level))
            {
                // $TODO: don't hard-code the default install level
                _showSelected = _level <= 1;
            }

            FeatureState = FeatureState.Unknown;
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotifyPropertyChangedInvocator]
        protected virtual void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
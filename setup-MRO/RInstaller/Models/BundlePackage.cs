using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Xml.Linq;

using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

using RInstaller.Annotations;
using System.Diagnostics;

namespace RInstaller.Models
{
    public class BundlePackage : INotifyPropertyChanged
    {
        public string Id { get; }

        public string DisplayName { get; }

        public bool Vital { get; }

        public bool Optional => !Vital;

        public IList<PackageFeature> AllFeatures { get; }

        public PackageState State { get; set; }

        public RequestState RequestState { get; set; }

        public string ProductCode { get; }

        public string UpgradeCode { get; }

        private bool? _showSelected;

        public bool? ShowSelected
        {
            get
            {
                if (!_showSelected.HasValue)
                {
                    return Vital || RequestState == RequestState.Present;
                }

                return _showSelected.Value;
            }
            set
            {
                if (_showSelected != null && _showSelected.Value == value)
                {
                    return;
                }

                _showSelected = value;
                NotifyPropertyChanged(nameof(ShowSelected));

                if (value.HasValue)
                {
                    RequestState = value.Value ? RequestState.Present : RequestState.Absent;
                }

                NotifyPropertyChanged(nameof(RequestState));
            }
        }


        public BundlePackage(XElement node)
        {
            Id = node.Attribute("Package")?.Value;
            DisplayName = node.Attribute("DisplayName")?.Value;
            ProductCode = node.Attribute("ProductCode")?.Value;
            UpgradeCode = node.Attribute("UpgradeCode")?.Value;
            
            if (node.Attribute("Vital")?.Value == "no")
            {
                Vital = false;
            }
            else if (node.Attribute("Absent")?.Value == "allow")
            {
                Vital = false;
            }
            else
            {
                Vital = true;
            }

            AllFeatures = new List<PackageFeature>();
            State = PackageState.Unknown;
            RequestState = RequestState.None;
        }

        public event PropertyChangedEventHandler PropertyChanged;

        private void NotifyPropertyChanged(string propertyName)
        {
            if (PropertyChanged != null)
            {
                PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
            }
        }

    }
}
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;

using RInstaller.Annotations;

namespace RInstaller.Models
{
    public class ProductAgreement
    {
        private ProductAgreement()
        {
            Accepted = false;
        }

        public event PropertyChangedEventHandler PropertyChanged;

        public string Title { get; set; }

        public string FeatureId { get; set; }

        public string File { get; set; }

        public string AgreeOption { get; set; }

        public bool Accepted { get; set; }
    }
}

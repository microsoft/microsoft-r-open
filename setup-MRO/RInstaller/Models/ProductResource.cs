using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RInstaller.Models
{
    public class ProductResource
    {
        public string ProductId { get; set; }

        public ProductStrings ProductStrings { get; set; }

        public ProductStyles ProductStyles { get; set; }

        public ProductAgreement[] ProductAgreements { get; set; }
    }
}

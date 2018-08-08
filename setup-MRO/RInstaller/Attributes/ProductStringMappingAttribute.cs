using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

namespace RInstaller.Attributes
{
    [AttributeUsage(AttributeTargets.Property)]
    public class ProductStringMappingAttribute : Attribute
    {
        public LaunchAction Action { get; }

        public string BaseProperty { get; }

        public ProductStringMappingAttribute(LaunchAction action, string baseProperty)
        {
            Action = action;
            BaseProperty = baseProperty;
        }
    }
}

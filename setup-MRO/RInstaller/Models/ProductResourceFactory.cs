using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;

using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

using Newtonsoft.Json;

using RInstaller.Attributes;

namespace RInstaller.Models
{
    public static class ProductResourceFactory
    {
        public static ProductResource Create(string json, LaunchAction action)
        {
            ProductResource ret = JsonConvert.DeserializeObject<ProductResource>(json);

            ResolveProperties(ret.ProductStrings, action);

            return ret;
        }

        private static void ResolveProperties(ProductStrings strings, LaunchAction action)
        {
            IEnumerable<PropertyInfo> allProperties = strings.GetType().GetProperties();
            IEnumerable<PropertyInfo> attributeProperties = allProperties.Where(property => property.IsDefined(typeof(ProductStringMappingAttribute), false));

            foreach (var property in attributeProperties)
            {
                ProductStringMappingAttribute attr = (ProductStringMappingAttribute)(property.GetCustomAttributes(typeof(ProductStringMappingAttribute), false).FirstOrDefault());

                if (attr?.Action != action)
                {
                    continue;
                }

                PropertyInfo targetProperty = allProperties.Single(x => x.Name == attr.BaseProperty);

                targetProperty.SetValue(strings, property.GetValue(strings, null), null);
            }
        }
    }
}

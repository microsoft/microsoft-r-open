using System.Xml.Linq;

namespace RInstaller.Models
{
    public class MBAPreReqPackage
    {
        public string PackageId { get; }

        public MBAPreReqPackage(XElement node)
        {
            PackageId = node.Attribute("PackageId")?.Value;
        }
    }
}
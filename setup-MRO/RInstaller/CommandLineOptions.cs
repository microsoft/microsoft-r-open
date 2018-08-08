using System.Collections.Generic;
using CommandLine;
using System.Text;

namespace RInstaller
{
    class CommandLineOptions
    {
        // Install variables
        [Option("installdir", Required = false, HelpText = "Directory to install files")]
        public string InstallDir { get; set; }

        [Option("cachedir", Required = false, HelpText = "Directory to cache downloaded install files")]
        public string CacheDir { get; set; }

        [Option("mediadir", Required = false, HelpText = "Directory to search for cached install files")]
        public string MediaDir { get; set; }

        // Component options
        [Option("models", Required = false, DefaultValue = false, HelpText = "Install pretrained machine learning models")]
        public bool Models { get; set; }

        [Option("python", Required = false, DefaultValue = false, HelpText = "Install Python support")]
        public bool Python { get; set; }

        // Run options
        [Option('d', "debug", Required = false, DefaultValue = false)]
        public bool Debug { get; set; }

        [Option("cleanup", Required = false, DefaultValue = false)]
        public bool Cleanup { get; set; }

        [Option("offline", Required = false, DefaultValue = false)]
        public bool Offline { get; set; }
    }
}

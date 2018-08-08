using System.IO;
using System.Reflection;


namespace RInstaller
{
    public class RSetup
    {
        public RSetup(string component, string version, string language)
        {
            Component = component;
            Version = version;
            Language = language;

            ExePath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "RSetup.exe");
        }

        public string Component { get; }

        public string Version { get; }

        public string Language { get; }

        public string ExePath { get; }

        public string CabFile
        {
            get { return string.Format("{0}_{1}_{2}.cab", Component.ToUpper(), Version, Language); }
        }

        public bool CheckCache()
        {
            return CheckCache(null);
        }

        public bool CheckCache(string cacheDir, bool isOffline = false)
        {
            string command = ExePath;
            string arguments = GetArgumentsForAction("checkcache");
            int exitCode = 0;

            if (!string.IsNullOrEmpty(cacheDir))
            {
                arguments += string.Format(" /cachedir \"{0}\"", cacheDir);
            }

            if (isOffline)
            {
                arguments += " /offline";
            }

            exitCode = Utilities.RunCommmand(command, arguments, true);

            return exitCode == 0;
        }

        public int CheckUrl(bool isOffline = false)
        {
            string command = ExePath;
            string arguments = GetArgumentsForAction("checkurl");
            int exitCode = 0;

            if (isOffline)
            {
                arguments += " /offline";
            }

            exitCode = Utilities.RunCommmand(command, arguments, true);

            return exitCode;
        }

        public int CacheCab(string cacheDir)
        {
            string command = ExePath;
            string arguments = GetArgumentsForAction("download");
            int exitCode = 0;

            exitCode = Utilities.RunCommmand(command, arguments + " /cachedir " + cacheDir, true);

            return exitCode;
        }

        private string GetArgumentsForAction(string action)
        {
            return string.Format("/{0} /component {1} /version {2} /language {3}", action, Component, Version, Language);
        }
    }
}
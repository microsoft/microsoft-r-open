using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;

using Microsoft.Win32;

namespace RInstaller
{
    public static class Utilities
    {
        public const string ProductVersionRegistryKey = @"HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{0}";

        private const string UpgradeCodeRegistryKey = @"HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Installer\UpgradeCodes\{0}";

        private static readonly int[] GuidRegistryFormatPattern = { 8, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2 };

        public static string GenerateLogPath(string logName)
        {
            return Path.Combine(Path.GetTempPath(), string.Format("{0}_{1}.log", logName, DateTime.Now.ToString("yyyyMMddhhmmss")));
        }

        public static bool ExtractResourceToPath(string filename, string targetPath)
        {
            string targetDir = Path.GetDirectoryName(targetPath);
            bool success = true;

            if (!string.IsNullOrEmpty(targetDir) && !Directory.Exists(targetDir))
                try
                {
                    //Log.WriteLog(LogLevel.Standard, string.Format("Creating cache folder: {0}", targetDir));
                    Directory.CreateDirectory(targetDir);
                }
                catch (Exception ex)
                {
                    //Log.WriteLog(LogLevel.Warn, string.Format("Error creating cache folder: {0}", ex.Message));
                    success = false;
                }

            if (!string.IsNullOrEmpty(targetDir) && Directory.Exists(targetDir))
                try
                {

                    string resourceName = string.Format("RInstaller.{0}", filename);
                    Stream stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resourceName);
                    FileStream fileStream = File.Create(targetPath);

                    stream.Seek(0, SeekOrigin.Begin);
                    stream.CopyTo(fileStream);
                    fileStream.Close();
                }
                catch (Exception ex)
                {
                    //Log.WriteLog(LogLevel.Error, string.Format("Error extracting resource: {0}", ex.Message));
                    success = false;
                }
            else
                success = false;

            return success;
        }

        public static int RunCommmand(string command, string arguments, bool isHidden)
        {
            return RunCommmand(command, arguments, Path.GetDirectoryName(command), isHidden);
        }

        public static int RunCommmand(string command, string arguments, string workingDirectory, bool isHidden)
        {
            Process process = null;

            try
            {
                ProcessStartInfo processStartInfo = new ProcessStartInfo();

                processStartInfo.FileName = command;
                processStartInfo.Arguments = arguments;
                processStartInfo.CreateNoWindow = isHidden;
                processStartInfo.RedirectStandardOutput = true;
                processStartInfo.RedirectStandardError = true;
                processStartInfo.UseShellExecute = false;
                processStartInfo.WindowStyle = isHidden ? ProcessWindowStyle.Hidden : ProcessWindowStyle.Normal;
                processStartInfo.WorkingDirectory = workingDirectory;

                //Log.WriteLog(LogLevel.Info, string.Format("Running command: {0} {1}", processStartInfo.FileName, processStartInfo.Arguments));

                process = Process.Start(processStartInfo);
                process.ErrorDataReceived += StandardErrorHandler;
                process.OutputDataReceived += StandardOutputHandler;
                process.BeginOutputReadLine();
                process.BeginErrorReadLine();
                process.WaitForExit();
            }
            catch (Exception ex)
            {
                //Log.WriteLog(LogLevel.Error, string.Format("Error running command: {0}", ex.Message));
                process = null;
            }

            return process != null ? process.ExitCode : 1;
        }

        public static int RunCommmand(string command, string arguments, bool isHidden, out string output)
        {
            return RunCommmand(command, arguments, Path.GetDirectoryName(command), isHidden, out output);
        }

        public static int RunCommmand(string command, string arguments, string workingDirectory, bool isHidden, out string output)
        {
            Process process = null;

            try
            {
                ProcessStartInfo processStartInfo = new ProcessStartInfo();

                processStartInfo.FileName = command;
                processStartInfo.Arguments = arguments;
                processStartInfo.CreateNoWindow = isHidden;
                processStartInfo.RedirectStandardOutput = true;
                processStartInfo.RedirectStandardError = true;
                processStartInfo.UseShellExecute = false;
                processStartInfo.WindowStyle = isHidden ? ProcessWindowStyle.Hidden : ProcessWindowStyle.Normal;
                processStartInfo.WorkingDirectory = workingDirectory;

                //Log.WriteLog(LogLevel.Info, string.Format("Running command: {0} {1}", processStartInfo.FileName, processStartInfo.Arguments));

                process = Process.Start(processStartInfo);
                process.WaitForExit();

                output = process.StandardOutput.ReadToEnd();
            }
            catch (Exception ex)
            {
                //Log.WriteLog(LogLevel.Error, string.Format("Error running command: {0}", ex.Message));
                process = null;
                output = null;
            }

            return process != null ? process.ExitCode : 1;
        }

        public static RegistryHive GetRegistryHive(string hiveName)
        {
            RegistryHive registryHive = RegistryHive.LocalMachine;

            switch (hiveName.ToUpper())
            {
                case "HKEY_CLASSES_ROOT":
                case "HKCR":
                    registryHive = RegistryHive.ClassesRoot;
                    break;

                case "HKEY_CURRENT_CONFIG":
                case "HKCC":
                    registryHive = RegistryHive.CurrentConfig;
                    break;

                case "HKEY_CURRENT_USER":
                case "HKCU":
                    registryHive = RegistryHive.CurrentUser;
                    break;

                case "HKEY_LOCAL_MACHINE":
                case "HKLM":
                    registryHive = RegistryHive.LocalMachine;
                    break;

                case "HKEY_PERFORMANCE_DATA":
                case "HKPD":
                    registryHive = RegistryHive.PerformanceData;
                    break;

                case "HKEY_USERS":
                case "HKU":
                    registryHive = RegistryHive.Users;
                    break;
            }

            return registryHive;
        }

        public static RegistryKey GetRegistryKey(RegistryView view, string path, RegistryKeyPermissionCheck permission)
        {
            string[] pathParts = path.Split('\\');
            string hiveName = pathParts[0];
            string subkeyName = string.Join("\\", pathParts.Skip(1).ToArray<string>());
            RegistryKey key = null;

            //Log.WriteLog(LogLevel.Info, string.Format(@"Opening registry key {0}", path));

            try
            {
                RegistryKey baseKey = RegistryKey.OpenBaseKey(GetRegistryHive(hiveName), view);
                key = baseKey != null ? baseKey.OpenSubKey(subkeyName, permission) : null;
            }
            catch (Exception ex)
            {
                //Log.WriteLog(LogLevel.Warn, string.Format("Error reading registry: {0}", ex.Message));
            }

            return key;
        }

        public static string GetRegistryValue(string path, string name)
        {
            return GetRegistryValue(RegistryView.Registry64, path, name);
        }

        public static string GetRegistryValue(RegistryView view, string path, string name)
        {
            string value = null;

            //Log.WriteLog(LogLevel.Info, string.Format(@"Reading registry value {0}\{1}", path, name));

            try
            {
                RegistryKey key = GetRegistryKey(view, path, RegistryKeyPermissionCheck.ReadSubTree);

                value = key != null ? key.GetValue(name) as string : string.Empty;

                //Log.WriteLog(LogLevel.Info, string.Format("Registry value: {0}", value));
            }
            catch (Exception ex)
            {
                //Log.WriteLog(LogLevel.Warn, string.Format("Error reading registry value: {0}", ex.Message));
            }

            return value;
        }

        public static string GetProductCodeFromUpgradeCode(RegistryView view, string upgradeCode)
        {
            string keyName = string.Format(UpgradeCodeRegistryKey, ConvertToRegistryFormat(new Guid(upgradeCode)));
            RegistryKey key = GetRegistryKey(RegistryView.Registry64, keyName, RegistryKeyPermissionCheck.ReadSubTree);
            Version latestVersion = null;
            string latestProductCode = null;

            if (key != null && key.ValueCount > 0)
                foreach (string subKeyName in key.GetValueNames())
                {
                    string productCode = ConvertFromRegistryFormat(subKeyName).ToString("B").ToUpper();
                    Version version = GetVersionFromProductCode(view, productCode);

                    if (version != null && latestVersion == null || version > latestVersion)
                    {
                        latestVersion = version;
                        latestProductCode = productCode;
                    }
                }

            return latestProductCode;
        }

        public static Version GetVersionFromProductCode(RegistryView view, string productCode)
        {
            string keyName = string.Format(ProductVersionRegistryKey, productCode);
            string versionString = GetRegistryValue(view, keyName, "DisplayVersion");
            Version version = null;

            if (!string.IsNullOrEmpty(versionString))
                Version.TryParse(versionString, out version);

            return NormalizeVersion(version);
        }

        public static string GetCacheDir()
        {
            return Path.Combine(Path.GetTempPath(), "RServerSetup_Cache");
        }

        public static Version NormalizeVersion(Version version)
        {
            if (version != null)
                if (version.Revision < 0)
                    version = new Version(version.Major, version.Minor, version.Build, 0);

            return version;
        }

        public static string ConvertToRegistryFormat(Guid guid)
        {
            return Reverse(guid, GuidRegistryFormatPattern);
        }

        public static Guid ConvertFromRegistryFormat(string guidIn)
        {
            Guid guidOut = Guid.Empty;

            if (guidIn != null && guidIn.Length == 32)
                Guid.TryParse(Reverse(guidIn, GuidRegistryFormatPattern), out guidOut);

            return guidOut;
        }

        private static void StandardOutputHandler(object sendingProcess, DataReceivedEventArgs outLine)
        {
            if (!string.IsNullOrWhiteSpace(outLine.Data))
            {
                //Log.WriteLog(LogLevel.Info, outLine.Data);
            }
        }

        private static void StandardErrorHandler(object sendingProcess, DataReceivedEventArgs outLine)
        {
            if (!string.IsNullOrWhiteSpace(outLine.Data))
            {
                //Log.WriteLog(LogLevel.Warn, outLine.Data);
            }
        }

        private static string Reverse(object value, params int[] pattern)
        {
            string inputString = value.ToString().Replace("-", "");
            StringBuilder returnString = new StringBuilder();
            int index = 0;

            foreach (int length in pattern)
            {
                returnString.Append(inputString.Substring(index, length).Reverse().ToArray());
                index += length;
            }

            return returnString.ToString();
        }

        public static string[] FormatArguments(string[] args)
        {
            for (int i = 0; i < args.Length; i++)
            {
                if (args[i].StartsWith("/"))
                {
                    args[i] = "--" + args[i].Substring(1);
                }
            }

            return args;
        }
    }
}
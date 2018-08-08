using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;

using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;
using RInstaller.Models;

namespace RInstaller
{
    public class SqlBindR
    {
        public SqlBindR()
        {
            ExePath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "SqlBindR.exe");
        }
        
        public string ExePath { get; }

        public List<SqlInstance> List()
        {
            var sqlInstances = new List<SqlInstance>();
            string output;

            if (Utilities.RunCommmand(this.ExePath, "/list", true, out output) == 0)
            {
                foreach (string instanceId in output.Split(new string[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries))
                {
                    var sqlInstance = new SqlInstance();
                    int checkResult = Check(instanceId);

                    sqlInstance.Id = instanceId;
                    sqlInstance.Enabled = checkResult != 4; // SqlBindR.ExitCode.NotBindable
                    sqlInstance.Bound = checkResult == 5; // SqlBindR.ExitCode.AlreadyBound
                    sqlInstance.Checked = sqlInstance.Bound;

                    sqlInstances.Add(sqlInstance);
                }
            }

            return sqlInstances;
        }

        public int Check(string instanceId)
        {
            return Utilities.RunCommmand(this.ExePath, string.Format("/check {0}", instanceId), true);
        }

        public int Bind(Engine engine, string instanceId, string cacheDir, string[] features)
        {
            var args = new List<string>();
            string argsString = string.Empty;

            if (features != null && features.Length > 0)
            {
                foreach (string feature in features)
                {
                    args.Add(string.Format("/{0}", feature));
                }
            }
            args.Add(string.Format("/bind {0}", instanceId));
            args.Add(string.Format("/cachedir \"{0}\"", cacheDir));

            argsString = string.Join(" ", args);

            engine.Log(LogLevel.Standard, string.Format("Running command: {0} {1}", this.ExePath, argsString));
            
            return Utilities.RunCommmand(this.ExePath, argsString, true);
        }

        public int Unbind(Engine engine, string instanceId)
        {
            string argsString = string.Format("/unbind {0}", instanceId);

            engine.Log(LogLevel.Standard, string.Format("Running command: {0} {1}", this.ExePath, argsString));

            return Utilities.RunCommmand(this.ExePath, argsString, true);
        }
    }
}

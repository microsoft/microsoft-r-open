#addin "Cake.FileHelpers"

using Cake.Core.IO;

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Diagnostics;
using System.IO;

 RBuildEnvironment.Override("Build", () => {
     CreateDirectory("target");
     CleanDirectory("target");
     
     if(IsRunningOnUnix())
     {
         StartProcess("touch", new ProcessSettings { Arguments = "configure.ac aclocal.m4 configure Makefile.am Makefile.in", WorkingDirectory = "vendor/pcre-8.37/" } );
         RunDefault();

         if (IsRunningOnMac())
         {
           Information("macOS");
           StartProcess("bash", new ProcessSettings { Arguments = "build-mac.sh" } );
         }
         else
         {
           Information("Linux");
           StartProcess("bash", new ProcessSettings { Arguments = "build-linux.sh" } );
         }
     }
     else
     {
         var vendor = RBuildEnvironment.CodeRoot + "/vendor";
         var rtoolsPath = vendor + "/rtools-3.3/bin;" + vendor + "/rtools-3.3/mingw_64/bin;";
     
         var path = EnvironmentVariable("path");
         Environment.SetEnvironmentVariable("path", rtoolsPath + path, EnvironmentVariableTarget.Process);
         
         Information(EnvironmentVariable("path"));
         
         Environment.SetEnvironmentVariable("tmpdir", "c:\\temp", EnvironmentVariableTarget.Process);
         var targetMkRules = RBuildEnvironment.CodeRoot + "/source/src/gnuwin32/MkRules.local";

         Information("Windows");
         string mkRulesContent = TransformTextFile("templates/MkRules.local").WithToken("VENDOR_DIR", ((DirectoryPath)vendor).FullPath).ToString();
         System.IO.File.WriteAllText(targetMkRules, mkRulesContent);
         
         CopyDirectory("vendor/rtools-3.3/R64/Tcl", "source/Tcl");
         
         StartProcess(vendor + "/rtools-3.3/bin/make.exe", new ProcessSettings { Arguments = "all", WorkingDirectory = "source/src/gnuwin32" } );
         StartProcess(vendor + "/rtools-3.3/bin/make.exe", new ProcessSettings { Arguments = "-j cairodevices", WorkingDirectory = "source/src/gnuwin32" } );
         StartProcess(vendor + "/rtools-3.3/bin/make.exe", new ProcessSettings { Arguments = "-j recommended", WorkingDirectory = "source/src/gnuwin32" } );
         StartProcess(vendor + "/rtools-3.3/bin/make.exe", new ProcessSettings { Arguments = "-j vignettes", WorkingDirectory = "source/src/gnuwin32" } );
         StartProcess(vendor + "/rtools-3.3/bin/make.exe", new ProcessSettings { Arguments = "manuals", WorkingDirectory = "source/src/gnuwin32" } );

         StartProcess("powershell.exe", new ProcessSettings { Arguments = "-file own-files.ps1 source/" } );
                  
         StartProcess(vendor + "/rtools-3.3/bin/make.exe", new ProcessSettings { Arguments = "imagedir", WorkingDirectory = "source/src/gnuwin32/installer" } );
         
         CreateDirectory("target/R");
         
         
         var rDir = GetDirectories("source/src/gnuwin32/installer/R-*");
         CopyDirectory(rDir.Single(), "target/R/Windows");
         CopyDirectory("vendor/rtools-3.3/R64/Tcl", "target/R/Windows/Tcl");
     }
 });

 static bool IsRunningOnMac()
 {
   string UnixName = ReadProcessOutput("uname", null);
   if (UnixName.Contains("Darwin"))
   {
     return true;
   }
   else
   {
     return false;
   }
 }

private static string ReadProcessOutput(string name, string args)
{
    try
    {
        Process p = new Process();
        p.StartInfo.UseShellExecute = false;
        p.StartInfo.RedirectStandardOutput = true;
        if (args != null && args != "") p.StartInfo.Arguments = " " + args;
        p.StartInfo.FileName = name;
        p.Start();

        // Do not wait for the child process to exit before
        // reading to the end of its redirected stream.
        // p.WaitForExit();
        // Read the output stream first and then wait.

        string output = p.StandardOutput.ReadToEnd();
        p.WaitForExit();
        if (output == null) output = "";
        output = output.Trim();
        return output;
    }
    catch
    {
        return "";
    }
}

//bool StartProcessThrowIfFailed(string command, string arguments, string W

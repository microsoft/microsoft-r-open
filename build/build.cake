#addin "Cake.FileHelpers" 

using Cake.Core.IO;
 
 RBuildEnvironment.Override("Build", () => {
     CreateDirectory("target");
     CleanDirectory("target");
     
     if(IsRunningOnUnix())
     {
     }
     else
     {
         var targetMkRules = RBuildEnvironment.CodeRoot + "/source/src/gnuwin32/MkRules.local";
         var vendor = RBuildEnvironment.CodeRoot + "/vendor";
         Information("Windows!");
         string mkRulesContent = TransformTextFile("templates/MkRules.local").WithToken("VENDOR_DIR", ((DirectoryPath)vendor).FullPath).ToString();
         System.IO.File.WriteAllText(targetMkRules, mkRulesContent);
         
         CopyDirectory("vendor/rtools-3.2/R64/Tcl", "source/Tcl");
         
         StartProcess(vendor + "/rtools-3.2/bin/make.exe", new ProcessSettings { Arguments = "-j all", WorkingDirectory = "source/src/gnuwin32" } );
         StartProcess(vendor + "/rtools-3.2/bin/make.exe", new ProcessSettings { Arguments = "-j cairodevices", WorkingDirectory = "source/src/gnuwin32" } );
         StartProcess(vendor + "/rtools-3.2/bin/make.exe", new ProcessSettings { Arguments = "-j recommended", WorkingDirectory = "source/src/gnuwin32" } );
         StartProcess(vendor + "/rtools-3.2/bin/make.exe", new ProcessSettings { Arguments = "-j vignettes", WorkingDirectory = "source/src/gnuwin32" } );
         StartProcess(vendor + "/rtools-3.2/bin/make.exe", new ProcessSettings { Arguments = "manuals", WorkingDirectory = "source/src/gnuwin32" } );
                  
         StartProcess(vendor + "/rtools-3.2/bin/make.exe", new ProcessSettings { Arguments = "imagedir", WorkingDirectory = "source/src/gnuwin32/installer" } );
         
         CreateDirectory("target/R");
         
         
         var rDir = GetDirectories("source/src/gnuwin32/installer/R-*");
         CopyDirectory(rDir.Single(), "target/R/Windows");
     }
 });
 
//bool StartProcessThrowIfFailed(string command, string arguments, string W
# The R Build environment
## How the build works

### Building
Building under the R build environment is simple.  From the root of the code repository run the corresponding script to build.

* Windows Powershell
```powershell
.\build.ps1
```

* Nix shell
```bash
.\build.sh
```

#### Building specific targets
Running the build script without any parameters runs the default target.  You can select different targets by simply specifying
the `-target` (Powershell) or `--target` (Nix shell) with a target name to run a specific target.  For example the default runs 
an incremental build, perhaps you want to clean prior to the build and thus specify the `Rebuild` flag which will clean then build. 


## Customizing the build process

### Where to extend the build process
Best practice is to update the `build.cake` file in the `build` directory that is contained in the root directory 
of your repository.  The `build.cake` file in the root path of your repository is implemented to load this file. Inside
this file you can extend the default targets, make your own, or both.

### Cake
The R build environment is based on the Cake build DSL.  The best place to get help with Cake is from <http://cakebuild.net>.

### R build extensions
#### What does the target run?
The R build environment plugs in on top of the Cake build system.  You can see what the build system will do without executing code 
by performing a dry run.  This is accomplished by running the build command with a flag:

* Windows Powershell
```powershell
.\builds.ps1 -WhatIf
```

* Nix shell
```bash
.\build.sh --dryrun
```

In addition to this you can select a specific target just as if you were running.

#### Extending a built in Cake task
Let's say you'd like to override the behavior of the built in `RestoreVendor` task.  You can do so by calling `Override()` on the
`RBuildEnvironment` object:

```csharp
RBuildEnvironment.Override("RestoreVendor", () => {
    Information("Hello world!");
});
```

The above code results in Cake displaying `Hello world!` instead of running the reference implementation for `RestoreVendor` when
it is run during the build process.

The R build tools also allows you to call the reference implementation somewhere in your flow.  For example:

```csharp 
RBuildEnvironment.Override("RestoreVendor", () => {
    Information("Starting to restore the vendor directory...");
    
    RunDefault();
    
    Information("Finished restoring the vendor directory...");
});
```

The above code results in Cake displaying `Starting to restore the vendor directory...` then proceeding to restore all the packages
in directories containing a `vendor.config` file in the repo and finally finishing up with `Finished restoring the vendor directory...`.

### Getting the task builder from a built in task
When working with Cake directly you would define a task using the `Task()` method.  Underneath Cake is providing you with a 
`CakeTaskBuilder<ActionTask>` object on which you can then, for example, call `.DependsOn()` method.  The reference tasks can be 
treated as a `CakeTaskBuilder<ActionTask>` and are retrieved through the `BuildEnvironment.GetTask(string)` method.  This can then 
be manipulated as any other task can in a Cake script.  For example the following code will create a new task `MyTask` and then set
the `Build` task to be dependent on it.  This means that Cake will run `MyTask` prior to running the `Build` task.

```csharp
Task("MyTask").Does(() => {
    Information("Cake is pretty cool");
});

RBuildEnvironment.GetTask("Build").DependsOn("MyTask");
```



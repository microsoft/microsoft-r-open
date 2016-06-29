#addin "Microsoft.R.Build"

var buildEnvironment = Initialize(Task, "branch.json");

// Place build customation in this file
#load "build/build.cake"

RunTarget(buildEnvironment.Target);
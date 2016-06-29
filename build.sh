#!/bin/bash
###############################################################
# This is the Cake bootstrapper script that is responsible for
# downloading Cake and all specified tools from NuGet.
###############################################################

# Define directories.
SCRIPT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
export TOOLS_DIR=$SCRIPT_DIR/tools
export NUGET_EXE=$TOOLS_DIR/nuget.exe
export CAKE_EXE=$TOOLS_DIR/Cake/Cake.exe

# Define default arguments.
SCRIPT="${SCRIPT_DIR}/build.cake"
TARGET="Default"
CONFIGURATION="Release"
VERBOSITY="verbose"
DRYRUN=false
SHOW_VERSION=false

# Parse arguments.
for i in "$@"; do
    case $1 in
        -s|--script) SCRIPT="$2"; shift ;;
        -t|--target) TARGET="$2"; shift ;;
        -c|--configuration) CONFIGURATION="$2"; shift ;;
        -v|--verbosity) VERBOSITY="$2"; shift ;;
        -d|--dryrun) DRYRUN=true ;;
        --version) SHOW_VERSION=true ;;
        -l|--lockcommon) LOCK_COMMON=true ;;
    esac
    shift
done

# Download NuGet if it does not exist.
if [ ! -f $NUGET_EXE ]; then
    echo "Downloading NuGet..."
    curl -Lsfo $NUGET_EXE https://dist.nuget.org/win-x86-commandline/v3.3.0/nuget.exe
    if [ $? -ne 0 ]; then
        echo "An error occured while downloading nuget.exe."
        exit 1
    fi
fi

mono $NUGET_EXE update -self

# Restore tools from NuGet.
pushd $TOOLS_DIR >/dev/null
mono $NUGET_EXE install -ExcludeVersion -OutputDirectory .

if [[ ! -d "addins/Microsoft.R.Build"  || ! ${LOCK_COMMON} ]]; then
    rm -rf "addins/Microsoft.R.Build"
fi
popd >/dev/null

# Make sure that Cake has been installed.
if [[ ! -f $CAKE_EXE ]]; then
    echo "Could not find Cake.exe."
    exit 1
fi

if [ -f $TOOLS_DIR/Cake/Nuget.Core.dll ]; then
  mv $TOOLS_DIR/Cake/Nuget.Core.dll $TOOLS_DIR/Cake/NuGet.Core.dll
fi

# Start Cake
if $SHOW_VERSION; then
    mono $CAKE_EXE -version
elif $DRYRUN; then
    mono $CAKE_EXE $SCRIPT -verbosity=$VERBOSITY -configuration=$CONFIGURATION -target=$TARGET -dryrun -coderoot=${SCRIPT_DIR}
else
    mono $CAKE_EXE $SCRIPT -verbosity=$VERBOSITY -configuration=$CONFIGURATION -target=$TARGET -coderoot=${SCRIPT_DIR} -BuildNumberOverride=1
fi
exit $?

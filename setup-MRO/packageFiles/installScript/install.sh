#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

CURL_BIN=$(type curl 2>/dev/null)
GETOPT_BIN=$(type getopt 2>/dev/null)
LESS_BIN=$(type less 2>/dev/null)
PING_BIN=$(type ping 2>/dev/null)
MORE_BIN=$(type more 2>/dev/null)
WGET_BIN=$(type wget 2>/dev/null)

MKL_EULA_ACCEPTED=false
SILENT=false
UNATTENDED=false
NOGPGCHECK=false

USE_YUM=false
USE_ZYPPER=false
USE_DPKG=false
USE_RPM=false
DISTRO_NAME=""

INTERNET_CONNECTION=false

DEBIAN_DEPENDENCIES="libxt6 libsm6 libpango1.0-0 libgomp1 curl less"
RPM_DEPENDENCIES="curl"

function error {
  echo -e "$*" >&2
}


function print_help {
cat <<EOF
  Usage: install.sh [OPTIONS]

    -a, --accept-mkl-eula        accept the license for the Intel MKL components
    -s, --silent                 perform a silent, unattended install
    -u, --unattended             perform an unattended install
    -n, --nogpgcheck             ignore package signature verification
    -h, --help                   print this help text

EOF
}

function message {
  if [ ${SILENT} == true ]; then
    return 0
  fi
  printf "$1"
}

function parse_options {
  local TEMP=$(getopt -o -adnpsuh --long \
               accept-mkl-eula,silent,unattended,nogpgcheck,help \
               -n "${PROG_NAME}" -- "$@")

  if [ $? != 0 ]; then
    echo "Option string malformed" >&2
    exit 1
  fi  

  eval set -- "${TEMP}"

  while true; do
    case "$1" in
      -a|--accept-mkl-eula)         MKL_EULA_ACCEPTED=true    ; shift ;;
      -s|--silent)                  SILENT=true               ; shift ;;
      -u|--unattended)              UNATTENDED=true           ; shift ;;
      -n|--nogpgcheck)              NOGPGCHECK=true           ; shift ;;
      -h|--help)                    print_help                ; exit  ;;
      --) shift; break ;;
      *) echo "Internal error while parsing options $*" >&2   ; exit 1 ;;
    esac
  done

  if [[ ${SILENT} == true || ${UNATTENDED} == true ]]; then
    #A silent install is always unattended
    UNATTENDED=true
  fi
}


function check_root {
  if [[ "$(id -u)" != "0" ]]; then
      error  "This script must be run with sudo or as root."
      exit 1
  fi
}

function check_getopt {
  if [[ -z ${GETOPT_BIN} ]]; then
    echo ""
    echo "This script requires GNU getopt to run. You may either install this utility"
    echo "and rerun this script, or install the provided packages using your distribution"
    echo "package manager."
    echo ""
    exit 1
  fi
}

function detect_package_manager {
  if [ -f /etc/redhat-release ]; then
    USE_YUM=true
    DISTRO_NAME="rhel"
  elif [ -f /etc/debian_version ]; then
    USE_DPKG=true
  elif [ -f /etc/SuSE-brand ] || [ -f /etc/SuSE-release ] || [ -f /etc/os-release ]; then
    USE_ZYPPER=true
    DISTRO_NAME="sles"
  else
    echo "Unsupported OS"
    exit 1
  fi   
}

function display_gpl {
  local eulaPath=$1
  local licenseName=$2

  if [[ ${SILENT} == true ]]; then
    return 0
  fi

  if [[ ${UNATTENDED} == true ]]; then
    cat ${eulaPath}
    return 0
  fi
  
  echo ""
  read -p "Press [Enter] key to display the ${licenseName} license. When finished reading, press q to continue: "

  if [[ -n ${LESS_BIN} ]]; then
    less ${eulaPath}
  elif [[ -n ${MORE_BIN} ]]; then
    more ${eulaPath}
  else
    echo ""
    echo "We could not detect the less or more utilities to display the license."
    echo "Please read the file ${eulaPath} for more information about the GPL"
    echo ""
  fi

  printf "\n"
}

function display_mkl_eula {

  local eulaPath=$1
  local licenseName=$2
  local installMKL=false
  
  if [[ ${SILENT} == true ]]; then
    return 0
  elif [[ ${MKL_EULA_ACCEPTED} == true && ${UNATTENDED} == true ]]; then
    return 0
  elif [[ ${MKL_EULA_ACCEPTED} == false && ${UNATTENDED} == true ]]; then
    echo ""
    echo "Unattended install requested but the flag to accept the MKL license was not passed."
    echo "Installing without MKL components."
    echo ""
    return 0
  elif [[ ${MKL_EULA_ACCEPTED} == false && ${SILENT} == true ]]; then
    return 0
  fi

  echo "Do you wish to install the Intel MKL libraries?"
  REPLY=""
  while [[ $REPLY != y && $REPLY != n ]] && read -n1 -r -p "Choose [y]es|[n]o "; do
  case $REPLY in
      y) installMKL=true && printf "\n";;
      n) printf "\nMKL components will not be installed.\n";;
      *) printf "\nInvalid input.\n";;
    esac
  done
  
  if [[ ${installMKL} == false ]]; then
    return 0
  fi 

  echo ""
  read -p "Press [Enter] key to display the ${licenseName} license. When finished reading, press q to continue: "

  if [[ -n ${LESS_BIN} ]]; then
    less ${eulaPath}
  elif [[ -n ${MORE_BIN} ]]; then
    more ${eulaPath}
  else
    echo ""
    echo "We could not detect the less or more utilities to display the license."
    echo "Please read the license files in this directory and rerun this script"
    echo "with the --accept-mkl-license flag if you agree to the licenses."
    echo ""
  fi

  echo "Do you agree to the terms of the previously displayed license?"
  REPLY=""
  while [[ $REPLY != y && $REPLY != n ]] && read -n1 -r -p "Choose [y]es|[n]o "; do
  case $REPLY in
      y) MKL_EULA_ACCEPTED=true && printf "\n";;
      n) printf "\nMKL components will not be installed.\n";;
      *) printf "\nInvalid input.\n";;
    esac
  done
  printf "\n"
}

function exit_message {

  if [[ ${SILENT} == true ]]; then
    return 0
  fi

  echo ""
  echo "Thank you for installing Microsoft R Open."
  echo "You will find logs for this installation in"
  echo "${SCRIPT_DIR}/logs"
  echo ""
}

function exit_error {

  if [[ ${SILENT} == true ]]; then
    return 0
  fi

  echo ""
  echo "There was an error installing Microsoft R Open."
  echo "Please check the logs at:"
  echo "${SCRIPT_DIR}/logs"
  echo ""
  exit 1
}

function install_rpm_yum {
    local package=$1
    local log=$2

    message "Installing ${package}..."
    if [[ ${NOGPGCHECK} == true ]]; then
      yum install -y --nogpgcheck ${package} > ${log} 2>&1
    else
      yum install -y ${package} > ${log} 2>&1
    fi    
    
    if [[ $? -eq 0 ]]; then
      message "done\n"
    else
      message "ERROR!\n"
      exit_error
    fi
}

function install_rpm_zypper {
    local package=$1
    local log=$2

    message "Installing ${package}..."
    if [[ ${NOGPGCHECK} == true ]]; then
      zypper --no-gpg-checks install -y ${package} > ${log} 2>&1
    else
      zypper install -y ${package} > ${log} 2>&1
    fi    
    
    if [[ $? -eq 0 ]]; then
      message "done\n"
    else
      message "ERROR!\n"
      exit_error
    fi
}

function install_deb_dpkg {
    local package=$1
    local log=$2

    message "Installing ${package}..."
    if [[ ${NOGPGCHECK} == true ]]; then
      dpkg --no-debsig --install ${package} > ${log} 2>&1
    else
      dpkg --install ${package} > ${log} 2>&1
    fi    
    
    if [[ $? -eq 0 ]]; then
      message "done\n"
    else
      message "ERROR!\n"
      exit_error
    fi
}


function install {
  if [[ ${USE_YUM} == true ]]; then
    mkdir -p ${SCRIPT_DIR}/logs
    install_rpm_yum ${SCRIPT_DIR}/rpm/${DISTRO_NAME}/microsoft-r-open-mro*.rpm ${SCRIPT_DIR}/logs/mro.txt
    if [[ ${MKL_EULA_ACCEPTED} == true ]]; then
      install_rpm_yum ${SCRIPT_DIR}/rpm/${DISTRO_NAME}/microsoft-r-open-mkl*.rpm ${SCRIPT_DIR}/logs/mkl.txt
    fi
  elif [[ ${USE_ZYPPER} == true ]]; then
    mkdir -p ${SCRIPT_DIR}/logs
    install_rpm_zypper ${SCRIPT_DIR}/rpm/${DISTRO_NAME}/microsoft-r-open-mro*.rpm ${SCRIPT_DIR}/logs/mro.txt
    if [[ ${MKL_EULA_ACCEPTED} == true ]]; then
      install_rpm_zypper ${SCRIPT_DIR}/rpm/${DISTRO_NAME}/microsoft-r-open-mkl*.rpm ${SCRIPT_DIR}/logs/mkl.txt
    fi
  elif [[ ${USE_DPKG} == true ]]; then
    mkdir -p ${SCRIPT_DIR}/logs
   
    message "Updating apt package repositories..."
    apt-get update > ${SCRIPT_DIR}/logs/update.txt 2>&1
    message "done\n"
    
    message "Installing apt package dependencies ${DEBIAN_DEPENDENCIES}..."
    apt-get -y install ${DEBIAN_DEPENDENCIES} > ${SCRIPT_DIR}/logs/dependencies.txt 2>&1
    message "done\n"

    install_deb_dpkg ${SCRIPT_DIR}/deb/microsoft-r-open-mro*.deb ${SCRIPT_DIR}/logs/mro.txt
    if [[ ${MKL_EULA_ACCEPTED} == true ]]; then
      install_deb_dpkg ${SCRIPT_DIR}/deb/microsoft-r-open-mkl*.deb ${SCRIPT_DIR}/logs/mkl.txt
    fi
  fi
}



check_getopt
parse_options "$@"
check_root
detect_package_manager
display_gpl "${SCRIPT_DIR}/MRO_EULA.txt" "Microsoft R Open"
display_mkl_eula "${SCRIPT_DIR}/MKL_EULA.txt" "Intel MKL"
install
exit_message

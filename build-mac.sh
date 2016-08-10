#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root" 1>&2
   exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

#if [ $? -ne 0 ]; then
#  echo "Failed to make dependencies!"
#  exit 1
#fi

export LDFLAGS=-L${SCRIPT_DIR}/vendor/build/lib
export CFLAGS="-I${SCRIPT_DIR}/vendor/build/include -DU_STATIC_IMPLEMENTATION -Wall -mtune=core2 -g -O2 -I/opt/X11/include"
export CXXFLAGS="-I${SCRIPT_DIR}/vendor/build/include -DU_STATIC_IMPLEMENTATIN -Wall -mtune=core2 -g -O2 -I/opt/X11/include"
export CPPFLAGS="-I${SCRIPT_DIR}/vendor/build/include -DU_STATIC_IMPLEMENTATION -Wall -mtune=core2 -g -O2 -I/opt/X11/include"
export OBJCFLAGS="-I${SCRIPT_DIR}/vendor/build/include -DU_STATIC_IMPLEMENTATION -Wall -mtune=core2 -g -O2 -I/opt/X11/include"
export PKG_CONFIG_PATH=${SCRIPT_DIR}/vendor/build/lib/pkgconfig

if [ ! -d ${SCRIPT_DIR}/R_build ] ; then
  mkdir ${SCRIPT_DIR}/R_build
fi
if [ ! -d ${SCRIPT_DIR}/target/R/mac ] ; then
  mkdir -p ${SCRIPT_DIR}/target/R/Mac
fi

pushd ${SCRIPT_DIR}/R_build

#--with-libpng --with-ICU --with-jpeglib --disable-rpath --with-tcltk --with-tcl-config=${SCRIPT_DIR}/vendor/build/lib/tclConfig.sh --with-tk-config=${SCRIPT_DIR}/vendor/build/lib/tkConfig.sh
#${SCRIPT_DIR}/patched_source/configure --verbose --with-x=yes --prefix=${SCRIPT_DIR}/target/R/Mac 'CC=clang' 'CXX=clang++' 'OBJC=clang' F77='gfortran -arch x86_64' FC='gfortran -arch x86_64' --with-blas="-framework Accelerate" '--with-lapack' '--enable-memory-profiling' --enable-R-framework=/Library/Frameworks FW_VERSION=3.3-MRO
${SCRIPT_DIR}/source/configure 'CC=clang' 'CXX=clang++' 'OBJC=clang' F77='gfortran -arch x86_64' FC='gfortran -arch x86_64' --with-jpeglib --with-libtiff --with-libpng --with-blas="-framework Accelerate" '--with-lapack' '--enable-memory-profiling' --enable-R-framework=/Library/Frameworks FW_VERSION=3.3-MRO
make -j32
make install
cp -r /Library/Frameworks/R.framework/* ${SCRIPT_DIR}/target/R/Mac

#cp /usr/lib64/libgfortran.so.1.0.0 ${SCRIPT_DIR}/target/R/Mac/lib64/R/lib/libgfortran.so.1
#popd

#cp -r ${SCRIPT_DIR}/vendor/build/lib/tcl8.6 ${SCRIPT_DIR}/target/R/Mac/lib64/R/share/
#cp -r ${SCRIPT_DIR}/vendor/build/lib/tk8.6 ${SCRIPT_DIR}/target/R/Mac/lib64/R/share/
#cp -r ${SCRIPT_DIR}/vendor/tcl8.6.5/library/msgcat ${SCRIPT_DIR}/target/R/Mac/lib64/R/share/tcl8.6
#sed -i 's/export R_SHARE_DIR/export R_SHARE_DIR\nexport TCL_LIBRARY=${R_SHARE_DIR}\/tcl8.6\//' ${SCRIPT_DIR}/target/R/Mac/lib64/R/bin/R
#cp ${SCRIPT_DIR}/files/microsoft-r-cacert.pem ${SCRIPT_DIR}/target/R/Mac/lib64/R/bin

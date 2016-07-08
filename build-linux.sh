#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

#if [ ! -d ${SCRIPT_DIR}/cmake_build ] ; then
#  mkdir ${SCRIPT_DIR}/cmake_build
#fi

#pushd ${SCRIPT_DIR}/cmake_build

#cmake ../vendor
#make -j32

#popd

export LDFLAGS=-L${SCRIPT_DIR}/vendor/build/lib
export LIBS='-licui18n -licuuc -licudata -lstdc++'
export CFLAGS="-I${SCRIPT_DIR}/vendor/build/include -DU_STATIC_IMPLEMENTATION"
export CXXFLAGS="-I${SCRIPT_DIR}/vendor/build/include -DU_STATIC_IMPLEMENTATIN"
export CPPFLAGS="-I${SCRIPT_DIR}/vendor/build/include -DU_STATIC_IMPLEMENTATION"
export PKG_CONFIG_PATH=${SCRIPT_DIR}/vendor/build/lib/pkgconfig
export C_INCLUDE_PATH="${SCRIPT_DIR}/vendor/build/include"
export CPLUS_INCLUDE_PATH="${SCRIPT_DIR}/vendor/build/include"
export CURL_LIBS="-lcurl -ldl -lssl -lcrypto -lssl -lcrypto -lz -lrt"
export TCLTK_CPPFLAGS="-pthread"

if [ ! -d ${SCRIPT_DIR}/R_build ] ; then
  mkdir ${SCRIPT_DIR}/R_build
fi
if [ ! -d ${SCRIPT_DIR}/target/Linux ] ; then
  mkdir -p ${SCRIPT_DIR}/target/Linux
fi

pushd ${SCRIPT_DIR}/R_build

${SCRIPT_DIR}/source/configure --verbose --with-x=yes --prefix=${SCRIPT_DIR}/target/Linux --enable-R-shlib --enable-BLAS-shlib --enable-memory-profiling --with-libpng --with-ICU --with-jpeglib --disable-rpath --with-tcltk --with-tcl-config=${SCRIPT_DIR}/vendor/build/lib/tclConfig.sh --with-tk-config=${SCRIPT_DIR}/vendor/build/lib/tkConfig.sh
make -j32
make install
cp /usr/lib64/libgfortran.so.1.0.0 ${SCRIPT_DIR}/target/Linux/lib64/R/lib/libgfortran.so.1
popd

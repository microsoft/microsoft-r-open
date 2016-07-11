#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cp ${SCRIPT_DIR}/files/microsoft-r-cacert.pem /etc

#if [ ! -d ${SCRIPT_DIR}/cmake_build ] ; then
#  mkdir ${SCRIPT_DIR}/cmake_build
#fi

#pushd ${SCRIPT_DIR}/cmake_build

#cmake ../vendor
#make -j32

#if [ $? -ne 0 ]; then
#  echo "Failed to make dependencies!"
#  exit 1
#fi

#popd

cp -r ${SCRIPT_DIR}/source ${SCRIPT_DIR}/patched_source

pushd ${SCRIPT_DIR}/patched_source

patch -p1 < ../patch/relocatable-r.patch

if [ $? -ne 0 ]; then
  echo "Patching code to be relocatable failed!"
  exit 1
fi

popd

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
export TCLTK_LIBS="-pthread -lz -lX11 -lXft -ltcl8.6 -ltk8.6 -lz"

if [ ! -d ${SCRIPT_DIR}/R_build ] ; then
  mkdir ${SCRIPT_DIR}/R_build
fi
if [ ! -d ${SCRIPT_DIR}/target/R/Linux ] ; then
  mkdir -p ${SCRIPT_DIR}/target/R/Linux
fi

pushd ${SCRIPT_DIR}/R_build

${SCRIPT_DIR}/patched_source/configure --verbose --with-x=yes --prefix=${SCRIPT_DIR}/target/R/Linux --enable-R-shlib --enable-BLAS-shlib --enable-memory-profiling --with-libpng --with-ICU --with-jpeglib --disable-rpath --with-tcltk --with-tcl-config=${SCRIPT_DIR}/vendor/build/lib/tclConfig.sh --with-tk-config=${SCRIPT_DIR}/vendor/build/lib/tkConfig.sh
make -j32
make install
cp /usr/lib64/libgfortran.so.1.0.0 ${SCRIPT_DIR}/target/R/Linux/lib64/R/lib/libgfortran.so.1
popd

cp -r ${SCRIPT_DIR}/vendor/build/lib/tcl8.6 ${SCRIPT_DIR}/target/R/Linux/lib64/R/share/
cp -r ${SCRIPT_DIR}/vendor/build/lib/tk8.6 ${SCRIPT_DIR}/target/R/Linux/lib64/R/share/
cp -r ${SCRIPT_DIR}/vendor/tcl8.6.5/library/msgcat ${SCRIPT_DIR}/target/R/Linux/lib64/R/share/tcl8.6
sed -i 's/export R_SHARE_DIR/export R_SHARE_DIR\nexport TCL_LIBRARY=${R_SHARE_DIR}\/tcl8.6\//' ${SCRIPT_DIR}/target/R/Linux/lib64/R/bin/R


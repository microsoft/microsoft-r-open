export LDFLAGS=-L${1}/build/lib
export CFLAGS="-I${1}/build/include -fPIC -I/opt/X11/include"
export CPPFLAGS="-I${1}/build/include -fPIC"
export PKG_CONFIG_PATH=${1}/build/lib/pkgconfig

pushd ${1}/${2}
sh autogen.sh
popd

${1}/${2}/configure --prefix=${1}/build --enable-static --disable-shared

export LDFLAGS=-L${1}/build/lib
export CFLAGS="-I${1}/build/include -fPIC"
export CPPFLAGS="-I${1}/build/include -fPIC"
export PKG_CONFIG_PATH=${1}/build/lib/pkgconfig

# If we are building for Mac we need to override the conf dir
if [ "${OSTYPE//[0-9.]/}" == "darwin" ]
then
    ${1}/${2}/configure --prefix=${1}/build --enable-static --disable-shared --sysconfdir=/opt/X11/lib/X11/
else
    ${1}/${2}/configure --prefix=${1}/build --enable-static --disable-shared
fi

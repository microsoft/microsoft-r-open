export LDFLAGS=-L${1}/build/lib

# If we are building for Mac we need to set Core Framworks
if [ "${OSTYPE//[0-9.]/}" == "darwin" ]
then
    export CFLAGS="-I${1}/build/include -fPIC -framework CoreFoundation -framework CoreGraphics -framework CoreText"
    export CPPFLAGS="-I${1}/build/include -fPIC -framework CoreFoundation -framework CoreGraphics -framework CoreText"
else
    export CFLAGS="-I${1}/build/include -fPIC"
    export CPPFLAGS="-I${1}/build/include -fPIC"
fi

export PKG_CONFIG_PATH=${1}/build/lib/pkgconfig

${1}/${2}/configure --prefix=${1}/build --enable-static --disable-shared

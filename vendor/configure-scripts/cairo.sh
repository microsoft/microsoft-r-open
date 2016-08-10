export LDFLAGS=-L${1}/build/lib
export CFLAGS="-I${1}/build/include -fPIC -framework CoreFoundation -framework CoreGraphics -framework CoreText"
export CPPFLAGS="-I${1}/build/include -fPIC -framework CoreFoundation -framework CoreGraphics -framework CoreText"
export PKG_CONFIG_PATH=${1}/build/lib/pkgconfig


${1}/${2}/configure --prefix=${1}/build --enable-static --disable-shared

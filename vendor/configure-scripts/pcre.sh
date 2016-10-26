export LDFLAGS=-L${1}/build/lib
export CFLAGS="-I${1}/build/include -fPIC"
export CPPFLAGS="-I${1}/build/include -fPIC"

${1}/${2}/configure --prefix=${1}/build --enable-static --disable-shared --enable-utf --enable-unicode-properties

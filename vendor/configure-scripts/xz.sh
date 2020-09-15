export LDFLAGS=-L${1}/build/lib
export CFLAGS="-I${1}/build/include -fPIC"
export CPPFLAGS="-I${1}/build/include -fPIC"

touch ${1}/${2}/aclocal.m4
touch ${1}/${2}/configure 

${1}/${2}/configure --prefix=${1}/build --enable-static --disable-shared

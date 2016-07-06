export LDFLAGS=-L${1}/build/lib
export CFLAGS="-I${1}/build/include -fPIC"
export CPPFLAGS="-I${1}/build/include -fPIC"

${1}/${2}/Configure no-shared --prefix=${1}/build linux-x86_64 

export LDFLAGS=-L${1}/build/lib
export CFLAGS="-I${1}/build/include -fPIC"
export CPPFLAGS="-I${1}/build/include -fPIC"

if [[ "$OSTYPE" == "darwin"* ]]; then
    ${1}/${2}/Configure darwin64-x86_64-cc no-shared -fPIC --prefix=${1}/build
else
    ${1}/${2}/config no-shared -fPIC --prefix=${1}/build
fi

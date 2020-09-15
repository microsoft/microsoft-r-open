#!/bin/bash

export CFLAGS=-fPIC

${1}/${2}/configure --prefix=${1}/build --enable-static --disable-shared

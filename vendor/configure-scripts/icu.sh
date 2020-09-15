#!/bin/bash

export CFLAGS=-fPIC
export CXXFLAGS=-fPIC

${1}/${2}/configure --prefix=${1}/build --enable-static --disable-shared

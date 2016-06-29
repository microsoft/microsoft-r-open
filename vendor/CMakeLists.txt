cmake_minimum_required (VERSION 3.5.0)
project(R-vendor)

include(ExternalProject)


ExternalProject_Add(
  zlib
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/zlib-1.2.8
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/zlib.sh ${CMAKE_SOURCE_DIR} zlib-1.2.8
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
  libpng
  DEPENDS zlib
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/libpng-1.6.23
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/png.sh ${CMAKE_SOURCE_DIR} libpng-1.6.23
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
  libjpg
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/jpeg-9b
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/jpeg.sh ${CMAKE_SOURCE_DIR} jpeg-9b
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
  libicu
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/icu-57.1/source
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/icu.sh ${CMAKE_SOURCE_DIR} icu-57.1/source
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
  tcl
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/tcl8.6.5/unix
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/tcl.sh ${CMAKE_SOURCE_DIR} tcl8.6.5/unix
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
  tk
  DEPENDS tcl
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/tk8.6.5/unix
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/tcl.sh ${CMAKE_SOURCE_DIR} tk8.6.5/unix
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
  readline
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/readline-6.3
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/readline.sh ${CMAKE_SOURCE_DIR} readline-6.3
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
  ncurses
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/ncurses-5.9
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/ncurses.sh ${CMAKE_SOURCE_DIR} ncurses-5.9
  BUILD_IN_SOURCE 1
)

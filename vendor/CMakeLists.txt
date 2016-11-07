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
  libtiff
  DEPENDS zlib libpng libjpg
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/tiff-4.0.6
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/png.sh ${CMAKE_SOURCE_DIR} tiff-4.0.6
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
  xz
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/xz-5.2.2
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/xz.sh ${CMAKE_SOURCE_DIR} xz-5.2.2
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
   openssl
   DEPENDS zlib
   SOURCE_DIR ${CMAKE_SOURCE_DIR}/openssl-1.0.2h
   CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/openssl.sh ${CMAKE_SOURCE_DIR} openssl-1.0.2h
   BUILD_COMMAND make depend && make
   BUILD_IN_SOURCE 1
)

if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")

   message("macOS")

   ExternalProject_Add(
     pixman
     SOURCE_DIR ${CMAKE_SOURCE_DIR}/pixman-0.34.0
     CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/pixman.sh ${CMAKE_SOURCE_DIR} pixman-0.34.0
     BUILD_IN_SOURCE 1
   )

   ExternalProject_Add(
       freetype
       DEPENDS libpng
       SOURCE_DIR ${CMAKE_SOURCE_DIR}/freetype-2.5.5
       CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/freetype.sh ${CMAKE_SOURCE_DIR} freetype-2.5.5
       BUILD_IN_SOURCE 1
   )
   

   ExternalProject_Add(
     fontconfig
     DEPENDS libpng freetype
     SOURCE_DIR ${CMAKE_SOURCE_DIR}/fontconfig-2.12.1
     CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/fontconfig.sh ${CMAKE_SOURCE_DIR} fontconfig-2.12.1
     BUILD_IN_SOURCE 1
   )

   ExternalProject_Add(
     cairo
     DEPENDS pixman fontconfig
     SOURCE_DIR ${CMAKE_SOURCE_DIR}/cairo-1.14.6
     CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/cairo.sh ${CMAKE_SOURCE_DIR} cairo-1.14.6
     BUILD_IN_SOURCE 1
   )


else()

     ExternalProject_Add(
       ncurses
       SOURCE_DIR ${CMAKE_SOURCE_DIR}/ncurses-5.9
       CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/ncurses.sh ${CMAKE_SOURCE_DIR} ncurses-5.9
       BUILD_IN_SOURCE 1
     )

     ExternalProject_Add(
       curl
       DEPENDS openssl
       SOURCE_DIR ${CMAKE_SOURCE_DIR}/curl-7.49.1
       CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/curl.sh ${CMAKE_SOURCE_DIR} curl-7.49.1
       BUILD_IN_SOURCE 1
     )

endif()

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
  pcre
  DEPENDS libpng
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/pcre-8.37
  CONFIGURE_COMMAND ${CMAKE_SOURCE_DIR}/configure-scripts/pcre.sh ${CMAKE_SOURCE_DIR} pcre-8.37
  BUILD_IN_SOURCE 1
)

ExternalProject_Add(
  bzip2
  DEPENDS zlib
  SOURCE_DIR ${CMAKE_SOURCE_DIR}/bzip2-1.0.6
  CONFIGURE_COMMAND ""
  BUILD_COMMAND make
  INSTALL_COMMAND make install PREFIX=${CMAKE_SOURCE_DIR}/build
  BUILD_IN_SOURCE 1
)

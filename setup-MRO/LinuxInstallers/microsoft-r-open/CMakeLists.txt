cmake_minimum_required(VERSION 3.5)

project(RInstallers)

set(REPO_ROOT "${CMAKE_SOURCE_DIR}/../..")

set(DependencyRoot "${REPO_ROOT}/dependencies")
MESSAGE( STATUS "DependencyRoot set to ${DependencyRoot}" )

set(TargetDirectory "${REPO_ROOT}/stage/mro_mac_install_stage")

set(MROStageDirectory "${REPO_ROOT}/stage/mro_install_stage")

set(MKLStageDirectory "${REPO_ROOT}/stage/mkl_install_stage/Linux")

set( MRO_PKGS_APACHE
     "foreach"
     "iterators"
)

set( MRO_PKGS_MKL
     "RevoUtilsMath"
)


# --- working path definitions ---
set(MROLibraryStageDirectory "${MROStageDirectory}/Linux/lib64/R/library/")
set(MROEtcStageDirectory "${MROStageDirectory}/Linux/lib64/R/etc/")

# --- package meta-data ---
set(CPACK_GENERATOR "RPM;DEB")
set(CPACK_RPM_COMPONENT_INSTALL ON)
set(CPACK_DEB_COMPONENT_INSTALL ON)
set(CPACK_PACKAGE_NAME "microsoft-r-open")
set(CPACK_PACKAGE_VENDOR "Microsoft")
set(CPACK_PACKAGE_CONTACT "revobuil@microsoft.com")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Microsoft R Open")

# the default version indicates that Cake did not pass productVersion
# in command line; see branch.json
# itemsToBuild.item=CmakeLists.txt.options.argumentCustomization
if(DEFINED ENV{CMAKE_MRO_VERSION})
    set(CPACK_PACKAGE_VERSION $ENV{CMAKE_MRO_VERSION})
    MESSAGE(STATUS ${CPACK_PACKAGE_VERSION})
else()
    set(CPACK_PACKAGE_VERSION "99.9.9.9" CACHE STRING "package version")
endif()
string(REPLACE "." ";" VERSION_LIST ${CPACK_PACKAGE_VERSION})
list(GET VERSION_LIST 0 CPACK_PACKAGE_VERSION_MAJOR)
list(GET VERSION_LIST 2 CPACK_PACKAGE_VERSION_PATCH)
list(GET VERSION_LIST 1 CPACK_PACKAGE_VERSION_MINOR)
list(GET VERSION_LIST 3 CPACK_PACKAGE_VERSION_TWEAK)

set(CPACK_PACKAGE_INSTALL_DIRECTORY "Microsoft R Server")
set(PACKAGE_POSTFIX "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}")
set(COMPONENT_NAME_MRO "mro-${PACKAGE_POSTFIX}")
set(COMPONENT_NAME_INTEL-MKL "mkl-${PACKAGE_POSTFIX}")
set(COMPONENT_NAME_MRO-APACHE "foreachiterators-${PACKAGE_POSTFIX}")

# TODO provide a description for each package
set(CPACK_PACKAGE_DESCRIPTION_FILE "${REPO_ROOT}/packageScripts/package_description_mro.txt")

# --- RPM meta-data ---
# do not strip executable files inside RPM packages
set(CPACK_RPM_SPEC_MORE_DEFINE "%global __os_install_post %{nil}")

set(CPACK_RPM_PACKAGE_AUTOREQPROV "no")
set(CPACK_RPM_PACKAGE_ARCHITECTURE "x86_64")

# --- Debian meta-data ---
set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")

# --- list of components ---
set(CPACK_COMPONENTS_ALL ${COMPONENT_NAME_MRO} ${COMPONENT_NAME_INTEL-MKL} ${COMPONENT_NAME_MRO-APACHE})


set(CPACK_PACKAGING_INSTALL_PREFIX "/opt/microsoft")
set(CPACK_RPM_EXCLUDE_FROM_AUTO_FILELIST_ADDITION
    "/opt/microsoft"
    "/opt/microsoft/ropen"
    "/opt/microsoft/ropen/${PACKAGE_POSTFIX}")

set(CPACK_RPM_${COMPONENT_NAME_MRO}_USER_FILELIST "/opt/microsoft/ropen")


# --- defining staging path for each package ---
install(DIRECTORY "${REPO_ROOT}/stage/mro_install_stage/Linux/share"
        DESTINATION "ropen/${PACKAGE_POSTFIX}"
        COMPONENT ${COMPONENT_NAME_MRO}
        USE_SOURCE_PERMISSIONS)
install(DIRECTORY "${REPO_ROOT}/stage/mro_install_stage/Linux/lib64"
        DESTINATION "ropen/${PACKAGE_POSTFIX}/"
        COMPONENT ${COMPONENT_NAME_MRO}
        USE_SOURCE_PERMISSIONS)
install(DIRECTORY "${REPO_ROOT}/stage/mkl_install_stage/Linux"
        DESTINATION "ropen/${PACKAGE_POSTFIX}/stage"
        COMPONENT ${COMPONENT_NAME_INTEL-MKL}
        USE_SOURCE_PERMISSIONS)

foreach( file_i ${MRO_PKGS_MKL} )
install(DIRECTORY "${REPO_ROOT}/dependencies/${file_i}/Linux/${file_i}"
        DESTINATION "ropen/${PACKAGE_POSTFIX}/lib64/R/library"
        COMPONENT ${COMPONENT_NAME_INTEL-MKL}
        USE_SOURCE_PERMISSIONS)
endforeach(file_i)

foreach( file_i ${MRO_PKGS_APACHE} )
install(DIRECTORY "${REPO_ROOT}/dependencies/${file_i}/Linux/${file_i}"
        DESTINATION "ropen/${PACKAGE_POSTFIX}/lib64/R/library"
        COMPONENT ${COMPONENT_NAME_MRO-APACHE}
        USE_SOURCE_PERMISSIONS)
endforeach(file_i)

# --- Debian package installation scripts ---
set(CPACK_DEBIAN_MRO-${PACKAGE_POSTFIX}_PACKAGE_CONTROL_EXTRA
	"${REPO_ROOT}/packageScripts/mro/debian/preinst;"
    "${REPO_ROOT}/packageScripts/mro/debian/postinst;"
    "${REPO_ROOT}/packageScripts/mro/debian/prerm;"
    "${REPO_ROOT}/packageScripts/mro/debian/postrm" )
set(CPACK_DEBIAN_MKL-${PACKAGE_POSTFIX}_PACKAGE_CONTROL_EXTRA
    "${REPO_ROOT}/packageScripts/intel-mkl/debian/postinst;"
    "${REPO_ROOT}/packageScripts/intel-mkl/debian/prerm" )

# --- Debian package dependencies ---

#TODO: Find a way to get rid of ${PACKAGE_POSTFIX}
set(CPACK_DEBIAN_MRO-${PACKAGE_POSTFIX}_PACKAGE_DEPENDS "libxt6,libsm6,libpango1.0-0,libgomp1,curl,less,bash")
set(CPACK_DEBIAN_MKL-${PACKAGE_POSTFIX}_PACKAGE_DEPENDS ${CPACK_PACKAGE_NAME}-${COMPONENT_NAME_MRO})
set(CPACK_DEBIAN_FOREACHITERATORS-${PACKAGE_POSTFIX}_PACKAGE_DEPENDS ${CPACK_PACKAGE_NAME}-${COMPONENT_NAME_MRO})

# --- RPM package installation scripts ---
set(CPACK_RPM_${COMPONENT_NAME_MRO}_POST_INSTALL_SCRIPT_FILE "${REPO_ROOT}/packageScripts/mro/rpm/postinst")
set(CPACK_RPM_${COMPONENT_NAME_MRO}_PRE_UNINSTALL_SCRIPT_FILE "${REPO_ROOT}/packageScripts/mro/rpm/prerm")
set(CPACK_RPM_${COMPONENT_NAME_MRO}_POST_UNINSTALL_SCRIPT_FILE "${REPO_ROOT}/packageScripts/mro/rpm/postrm")

set(CPACK_RPM_${COMPONENT_NAME_INTEL-MKL}_POST_INSTALL_SCRIPT_FILE "${REPO_ROOT}/packageScripts/intel-mkl/rpm/postinst")
set(CPACK_RPM_${COMPONENT_NAME_INTEL-MKL}_PRE_UNINSTALL_SCRIPT_FILE "${REPO_ROOT}/packageScripts/intel-mkl/rpm/prerm")
# --- RPM package dependencies ---
set(CPACK_RPM_${COMPONENT_NAME_MRO}_PACKAGE_REQUIRES "libXt.so.6()(64bit),libSM.so.6()(64bit),libpangocairo-1.0.so.0()(64bit),libpng12.so.0()(64bit),libgomp.so.1()(64bit),curl,less")
set(CPACK_RPM_${COMPONENT_NAME_INTEL-MKL}_PACKAGE_REQUIRES "${CPACK_PACKAGE_NAME}-${COMPONENT_NAME_MRO} = ${CPACK_PACKAGE_VERSION}")
set(CPACK_RPM_${COMPONENT_NAME_MRO-APACHE}_PACKAGE_REQUIRES "${CPACK_PACKAGE_NAME}-${COMPONENT_NAME_MRO} = ${CPACK_PACKAGE_VERSION}")
include(CPack)

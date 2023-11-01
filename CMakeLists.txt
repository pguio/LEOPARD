
# https://github.com/coderefinery/lessons/blob/master/cmake-basics.mkd

cmake_minimum_required(VERSION 3.19)
cmake_policy(VERSION 3.1)

#   Version and release date

set(leopard_MAJOR 1)
set(leopard_MINOR 0)
set(leopard_PATCH 0)
set(leopard_VERSION ${leopard_MAJOR}.${leopard_MINOR}.${leopard_PATCH})


#   Set the project

project (LEOPARD VERSION ${leopard_VERSION} LANGUAGES Fortran C)

#   Packaging

set(CPACK_GENERATOR "TBZ2")
set(CPACK_PACKAGE_VERSION_MAJOR ${leopard_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${leopard_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${leopard_PATCH})
set(CPACK_SOURCE_GENERATOR "TBZ2")
set(CPACK_SOURCE_PACKAGE_FILE_NAME leopard-${leopard_VERSION})
set(CPACK_SOURCE_IGNORE_FILES ".*.swp$" ".*.swo$" "~$" ".bz2$" ".gz$")

include(CPack)
include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

enable_language(Fortran)


if(NOT CMAKE_BUILD_TYPE)
#  set(CMAKE_BUILD_TYPE Release)
  set(CMAKE_BUILD_TYPE Debug)
endif()
set(CMAKE_Fortran_FLAGS "-O3")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -g")
set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g")

set(CMAKE_C_FLAGS "-O3")
set(CMAKE_C_FLAGS_RELEASE "-O3 -g")
set(CMAKE_C_FLAGS_DEBUG "-O3 -g")

file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/mpfun2015)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/mpfun2020)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/mpfunmpfr)

set(mpfun2015 "${CMAKE_BINARY_DIR}/mpfun2015")
set(mpfun2020 "${CMAKE_BINARY_DIR}/mpfun2020")
set(mpfunmpfr "${CMAKE_BINARY_DIR}/mpfunmpfr")

set(mpfun_src mpfun2015/mpmodule.f90 mpfun2015/mpfuna.f90 
mpfun2015/mpfunbq.f90 mpfun2015/mpfunc.f90 mpfun2015/mpfund.f90
mpfun2015/mpfune.f90 mpfun2015/mpfunf.f90 mpfun2015/mpfungq2.f90
mpfun2015/second.f90)

set(mpfun20_src mpfun2020/mpmodule.f90 mpfun2020/mpfuna.f90
mpfun2020/mpfunb.f90 mpfun2020/mpfunc.f90 mpfun2020/mpfund.f90
mpfun2020/mpfune.f90 mpfun2020/mpfunf.f90 mpfun2020/mpfung2.f90
mpfun2020/mpfunh2.f90 mpfun2020/mpmask13.f90 mpfun2020/second.f90)

set(mpfunmpfr_src mpfunmpfr/mpmodule.f90 mpfunmpfr/mpfuna.f90
mpfunmpfr/mpfunf.f90 mpfunmpfr/mpfungq2.f90
mpfunmpfr/mpfunhq2.f90 mpfunmpfr/second.f90 mpfunmpfr/mpinterface.c)

set(leopard_src param_mod.f90 disp_det.f90 Z_func.f90 dZ_func.f90
integrator.f90 muller.f90 polyfit.f90 read_data.f90 read_distr.f90
spline_interpol.f90 exp_Bessel_In.f90 exp_dBessel_In.f90 cerror.f90
cont_frac.f90 Bessel_int.f90 int_para.f90 F12.f90
F12_mpfun.f90 F23.f90 F23_mpfun.f90 gamma_func.f90 acc_F.f90 acc_Kvpa.f90
get_splinecoeff.f90 exp_Bessel_In_mpfun.f90 fort_Bes.f90)

# add floating point exception flags
set(fpe_flags "-ffpe-trap=invalid -ffpe-trap=zero -ffpe-trap=overflow")
set_source_files_properties(main.f90 int_para_mpfun.f90 ${leopard_src} PROPERTIES
COMPILE_OPTIONS ${fpe_flags})

# add real type flags
set(real_flags "-fdefault-real-8")
set_source_files_properties(main.f90 int_para_mpfun.f90 ${leopard_src} PROPERTIES
COMPILE_OPTIONS ${real_flags})

# add no inderscoring for Fortran and C mixing
set(no__flags "-fno-underscoring")
set_source_files_properties(mpfunmpfr/mpmodule.f90 mpfunmpfr/mpfuna.f90
mpfunmpfr/mpfunf.f90 mpfunmpfr/mpfungq2.f90 mpfunmpfr/mpfunhq2.f90
mpfunmpfr/second.f90 PROPERTIES COMPILE_OPTIONS ${no__flags})

find_library(mpfr NAMES mpfr HINTS /usr/lib64)

#   Libraries
add_library(mpfun STATIC ${mpfun_src})
set_target_properties(mpfun PROPERTIES Fortran_MODULE_DIRECTORY ${mpfun2015})

add_library(mpfun20 STATIC ${mpfun20_src})
set_target_properties(mpfun20 PROPERTIES Fortran_MODULE_DIRECTORY ${mpfun2020})

add_library(mpfunmpfr STATIC ${mpfunmpfr_src})
set_target_properties(mpfunmpfr PROPERTIES Fortran_MODULE_DIRECTORY ${mpfunmpfr})

add_library(leopard STATIC ${leopard_src})
set_target_properties(leopard PROPERTIES Fortran_MODULE_DIRECTORY ${mpfun2015})
add_dependencies(leopard mpfun)

add_library(leopard20 STATIC ${leopard_src})
set_target_properties(leopard20 PROPERTIES Fortran_MODULE_DIRECTORY ${mpfun2020})
add_dependencies(leopard20 mpfun20)

add_library(leopardmpfr STATIC ${leopard_src})
set_target_properties(leopardmpfr PROPERTIES Fortran_MODULE_DIRECTORY ${mpfunmpfr})
target_compile_options(leopardmpfr PUBLIC "-fno-underscoring")
add_dependencies(leopardmpfr mpfunmpfr)

#   Executables
add_executable(dsolve main.f90 int_para_mpfun.f90)
target_include_directories(dsolve PUBLIC ${mpfun2015})
set_target_properties(dsolve PROPERTIES Fortran_MODULE_DIRECTORY ${mpfun2015})
target_link_libraries (dsolve mpfun leopard)
add_dependencies(dsolve mpfun leopard)

add_executable(dsolve20 main.f90 int_para_mpfun.f90)
target_include_directories(dsolve20 PUBLIC ${mpfun2020})
set_target_properties(dsolve20 PROPERTIES Fortran_MODULE_DIRECTORY ${mpfun2020})
target_link_libraries (dsolve20 mpfun20 leopard20)
add_dependencies(dsolve20 mpfun20 leopard20)

add_executable(dsolvempfr main.f90 int_para_mpfun.f90)
target_include_directories(dsolvempfr PUBLIC ${mpfunmpfr})
set_target_properties(dsolvempfr PROPERTIES Fortran_MODULE_DIRECTORY ${mpfunmpfr})
target_link_libraries (dsolvempfr mpfunmpfr leopardmpfr mpfr)
target_compile_options(dsolvempfr PUBLIC "-fno-underscoring")
add_dependencies(dsolvempfr mpfunmpfr leopardmpfr)

# test various functionalities of mpfun
#add_executable(test test.f90)
#target_link_libraries (test mpfun mpfr)

#   Installation
set(SOURCES main.f90 int_para_mpfun.f90 ${leopard_src} ${mpfun_src}  ${mpfun20_src}) 
install(FILES ${SOURCES} DESTINATION .)
install(FILES CMakeLists.txt DESTINATION .)
install(FILES Readme.txt DESTINATION .)



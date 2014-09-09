


cmake_minimum_required(VERSION 2.6)
project(ehixs)
set (ehixstests_VERSION_MAJOR 0)
set (ehixstests_VERSION_MINOR 9)

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
#
#   If you have a local LHAPDF installation 
#   please edit below to set the absolute path for lhapdf 
#   (don't forget to remove the '#' before 'SET(..)'
#
#SET(LHAPDF_DIR my_lhapdf_directory)
#
#
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# configure a header file to pass some of the CMake settings
# to the source code
configure_file (
  "${PROJECT_SOURCE_DIR}/ehixs_config.h.in"
  "${PROJECT_BINARY_DIR}/ehixs_config.h"
  )
 
# add the binary tree to the search path for include files
# so that we will find TutorialConfig.h
include_directories("${PROJECT_BINARY_DIR}")
 
enable_language(Fortran)

if (Fortran_COMPILER_NAME STREQUAL "gfortran")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m64")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
elseif (Fortran_COMPILER_NAME STREQUAL "ifort")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
elseif (Fortran_COMPILER_NAME STREQUAL "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m64")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m64")
else (Fortran_COMPILER_NAME STREQUAL "gfortran")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O3...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m64")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME STREQUAL "gfortran")

set (CMAKE_CXX_FLAGS "-std=c++11  -DGTEST_HAS_PTHREAD=0 -pthread")

SET(EHIXS_SRC_DIR ${PROJECT_SOURCE_DIR})

SET(CHAPLIN_SOURCES ${EHIXS_SRC_DIR}/chaplin)

include_directories(${EHIXS_SRC_DIR}
                    ${CHAPLIN_SOURCES}
                    ${LHAPDF_DIR}/include
                    ${CUBA_DIR}
                    ${EHIXS_SRC_DIR}/new_b_bbar
                    ${EHIXS_SRC_DIR}/gluon_fusion
                    ${EHIXS_SRC_DIR}/gamma_star_gamma_star
                    ${EHIXS_SRC_DIR}/gluon_fusion_inclusive
                    ${EHIXS_SRC_DIR}/decay_z_gamma
                    ${EHIXS_SRC_DIR}/decay_h_to_gamma_gamma
                    ${EHIXS_SRC_DIR}/decay_wwzz
                    ${EHIXS_SRC_DIR}/decay_bb
                    ${EHIXS_SRC_DIR}/tools
                    ${EHIXS_SRC_DIR}/models
                    ${EHIXS_SRC_DIR}/core
                    ${EHIXS_SRC_DIR}/pdf
                    ${EHIXS_SRC_DIR}/chaplin
                    ${EHIXS_SRC_DIR}/ihixs
                    ${EHIXS_SRC_DIR}/../user_src
                    ${EHIXS_SRC_DIR}/../user_src/ggf_production
                    ${EHIXS_SRC_DIR}/../user_src/WWZZ_decay
                    ${EHIXS_SRC_DIR}/../user_src/decay_z_gamma
                    ${EHIXS_SRC_DIR}/../user_src/decay_gamma_gamma
                    ${EHIXS_SRC_DIR}/../user_src/
                    ${EHIXS_SRC_DIR}/mincuba
                    )

link_directories(
                ${LHAPDF_DIR}/lib/
               )


#-------------------------------------------------------------------------------
#
#   core and peripheral libs
#
add_subdirectory(mincuba)
add_subdirectory(chaplin)
add_subdirectory(tools)
add_subdirectory(models)
add_subdirectory(core)
add_subdirectory(pdf)
add_subdirectory(avh_olo)
#
#   testing with gtests
#
#add_subdirectory(gtests)
#
#   production modes
#
#add_subdirectory(gluon_fusion)
#add_subdirectory(gluon_fusion_inclusive)
#add_subdirectory(bottom_fusion)
add_subdirectory(new_b_bbar)
#add_subdirectory(gamma_star_gamma_star)
add_subdirectory(ihixs)
#
#   decay modes
#
#add_subdirectory(decay_bb)
add_subdirectory(decay_z_gamma)
add_subdirectory(decay_h_to_gamma_gamma)
add_subdirectory(decay_wwzz)
#-------------------------------------------------------------------------------

#find_library(LHAPDF ssl $ENV{LHAPDF_PATH})
#This will check the environment for a variable named LHAPDF_PATH. 
#If it exists, you can then use
#target_link_libraries(ehixs ${LHAPDF})
#If it does not, it can be set through the CMake GUI.

SET(GGF_LIBS gluon_fusion
            fortran_ggf 
            fortran_ggf_RR_gg2gg
            fortran_ggf_RR_gg2qqbar
            fortran_ggf_RR_qg2qg
            fortran_ggf_RR_gq2qg
            fortran_ggf_RR_qqbar
            fortran_ggf_RR_qq)


SET(LIBRARIES_FOR_GLOBAL_TESTS 
            #test_utils 
            core  
            decay_zzww 
            decay_z_gamma 
            higgs_zerfal 
            decay_h_to_gamma_gamma
            models
            tools
            #
            #gamma_star_gamma_star 
            #
            #GGF_LIBS
            #fortran_bbh
            bbh
            #
            chaplin
            pdf
            avh_olo
            mincuba
            #gtest 
            LHAPDF
            ${LHAPDF})

add_executable(ehixs  
            ${EHIXS_SRC_DIR}/ehixs.cpp)
target_link_libraries(ehixs 
            ${LIBRARIES_FOR_GLOBAL_TESTS})

set(CMAKE_BUILD_TYPE Release)










cmake_minimum_required(VERSION 2.6)
project(ehixstests)

# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
#
#   Please edit below to set the absolute paths for gtest, cuba and lhapdf
#
SET(GTEST_DIR /Users/lazopolis/other_codes/gtest-1.6.0)
SET(CUBA_DIR /Users/lazopolis/my_code/ehixs/Cuba-2.1)
SET(LHAPDF_DIR /Users/lazopolis/Downloads/lhapdf-5.8.6)
#
#
# . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .




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



set (CMAKE_CXX_FLAGS "-Wno-return-type-c-linkage -Wno-deprecated-writable-strings -Wno-delete-non-virtual-dtor -DGTEST_HAS_PTHREAD=0 -pthread")


#debug
#set (CMAKE_CXX_FLAGS "-Wno-return-type-c-linkage -Wno-deprecated-writable-strings -g -O0")

SET(EHIXS_SRC_DIR ${PROJECT_SOURCE_DIR})

SET(CHAPLIN_SOURCES ${EHIXS_SRC_DIR}/chaplin)

include_directories(${EHIXS_SRC_DIR}
                    ${CHAPLIN_SOURCES}
                    ${GTEST_DIR}/include
                    ${LHAPDF_DIR}/include
                    ${CUBA_DIR}
                    ${EHIXS_SRC_DIR}/bottom_fusion
                    ${EHIXS_SRC_DIR}/gluon_fusion
                    ${EHIXS_SRC_DIR}/gluon_fusion_inclusive
                    ${EHIXS_SRC_DIR}/decay_z_gamma
                    ${EHIXS_SRC_DIR}/decay_h_to_gamma_gamma
                    ${EHIXS_SRC_DIR}/decay_wwzz
                    ${EHIXS_SRC_DIR}/gtests
                    ${EHIXS_SRC_DIR}/decay_bb
                    ${EHIXS_SRC_DIR}/tools
                    ${EHIXS_SRC_DIR}/models
                    ${EHIXS_SRC_DIR}/core
                    ${EHIXS_SRC_DIR}/pdf
                    ${EHIXS_SRC_DIR}/chaplin
                    ${EHIXS_SRC_DIR}/../user_src/ggf_production
                    ${EHIXS_SRC_DIR}/../user_src/WWZZ_decay
                    ${EHIXS_SRC_DIR}/../user_src/decay_z_gamma
                    ${EHIXS_SRC_DIR}/../user_src/decay_gamma_gamma
                    ${EHIXS_SRC_DIR}/../user_src/
                    )

link_directories(
                ${CUBA_DIR}/
                ${GTEST_DIR}/build/
                ${LHAPDF_DIR}/lib/
                ${LHAPDFDIR}
               )


#-------------------------------------------------------------------------------

add_subdirectory(gluon_fusion)
add_subdirectory(gluon_fusion_inclusive)
add_subdirectory(bottom_fusion)
#add_subdirectory(gamma_star_gamma_star)
add_subdirectory(chaplin)
#add_subdirectory(decay_bb)
add_subdirectory(decay_z_gamma)
add_subdirectory(decay_h_to_gamma_gamma)
add_subdirectory(decay_wwzz)

add_subdirectory(tools)
add_subdirectory(models)
add_subdirectory(core)
add_subdirectory(pdf)

add_subdirectory(avh_olo)
add_subdirectory(gtests)


#find_library(LHAPDF ssl $ENV{LHAPDF_PATH})
#This will check the environment for a variable named LHAPDF_PATH. 
#If it exists, you can then use
#target_link_libraries(mytarget ${LHAPDF})
#If it does not, it can be set through the CMake GUI.

SET(LIBRARIES_FOR_GLOBAL_TESTS core  decay_zzww decay_z_gamma higgs_zerfal 
            decay_h_to_gamma_gamma
            models
            gluon_fusion tools
            fortran_ggf fortran_ggf_RR
            fortran_bbh
            chaplin
            pdf
            avh_olo
            cuba
            gtest test_utils
            LHAPDF
            ${LHAPDF})

SET(TEST_DIR ${EHIXS_SRC_DIR}/gtests)

#add_executable(tests  ${TEST_DIR}/tests.cpp)
#target_link_libraries(tests LIBRARIES_FOR_GLOBAL_TESTS)

#add_executable(pdf_test  ${TEST_DIR}/pdf_test.cpp)
#target_link_libraries(pdf_test  
#                      cuba gtest gfortran  tools pdf avh_olo LHAPDF ${LHAPDF})

#add_executable(integration_tests  ${TEST_DIR}/integration_tests.cpp)
#target_link_libraries(integration_tests  
#               cuba gtest gfortran chaplin tools  decay_bb
#               core decay_zzww fortran_bbh fortran_ggf fortran_ggf_RR pdf avh_olo LHAPDF ${LHAPDF})

add_executable(nlo_poles_test  ${TEST_DIR}/ggf_pole_cancelation_nlo_test.cpp)
target_link_libraries(nlo_poles_test ${LIBRARIES_FOR_GLOBAL_TESTS})  


add_executable(nlo_gg_xs_test ${TEST_DIR}/ggf_nlo_gg_channel_xsection_test.cpp)
target_link_libraries(nlo_gg_xs_test ${LIBRARIES_FOR_GLOBAL_TESTS})
  

add_executable(nnlo_q1q2_pole_test 
            ${TEST_DIR}/ggf_NNLO_q1q2_pole_cancelation_test.cpp)
target_link_libraries(nnlo_q1q2_pole_test
			${LIBRARIES_FOR_GLOBAL_TESTS})
  
add_executable(nnlo_qq_test 
            ${TEST_DIR}/ggf_nnlo_qq_test.cpp)
target_link_libraries(nnlo_qq_test
			${LIBRARIES_FOR_GLOBAL_TESTS})  

add_executable(nnlo_qqbar_test 
            ${TEST_DIR}/ggf_nnlo_qqbar_test.cpp)
target_link_libraries(nnlo_qqbar_test
			${LIBRARIES_FOR_GLOBAL_TESTS})
 

add_executable(nnlo_qg_test 
            ${TEST_DIR}/quark_gluon_nnlo_test.cpp)
target_link_libraries(nnlo_qg_test
			${LIBRARIES_FOR_GLOBAL_TESTS})  

add_executable(nnlo_gg_test 
            ${TEST_DIR}/gluon_gluon_nnlo_test.cpp)
target_link_libraries(nnlo_gg_test
			${LIBRARIES_FOR_GLOBAL_TESTS})  

add_executable(nnlo_gq_test 
            ${TEST_DIR}/gluon_quark_nnlo_test.cpp)
target_link_libraries(nnlo_gq_test
			${LIBRARIES_FOR_GLOBAL_TESTS}) 

add_executable(nlo_exact_test 
            ${TEST_DIR}/nlo_exact_test.cpp)
target_link_libraries(nlo_exact_test 
            ${LIBRARIES_FOR_GLOBAL_TESTS})
			

add_executable(one_d_interpolator_test 
            ${TEST_DIR}/one_d_interpolator_test.cpp)
target_link_libraries(one_d_interpolator_test 
            tools pdf gtest avh_olo LHAPDF ${LHAPDF})


#add_executable(iggf_test  ${EHIXS_SRC_DIR}/iggf.cpp)
#target_link_libraries(iggf_test  
#                   ggf_inclusive tools chaplin gluon_fusion  gfortran gtest)


add_executable(UI_test 
            ${TEST_DIR}/test_UI.cpp)
target_link_libraries(UI_test 
                        tools gtest)

add_executable(nlo_exact_virtual 
            ${TEST_DIR}/exact_virtual_test.cpp)
target_link_libraries(nlo_exact_virtual
                    fortran_ggf fortran_ggf_RR gluon_fusion
                    models avh_olo chaplin tools
                    cuba gtest)


add_executable(off_line_histograming 
            ${TEST_DIR}/off_line_histograming.cpp)
target_link_libraries(off_line_histograming
            tools gtest)


add_executable(iggf 
            ${TEST_DIR}/iggf.cpp)
target_link_libraries(iggf
            gtest ggf_inclusive
			chaplin tools models cuba pdf LHAPDF ${LHAPDF}
			)


add_executable(ihixs++ 
            ${EHIXS_SRC_DIR}/ihixs.cpp)
target_link_libraries(ihixs++
			ggf_inclusive chaplin tools models cuba pdf LHAPDF ${LHAPDF}
			)

add_executable(ehixs  
            ${EHIXS_SRC_DIR}/ehixs.cpp)
target_link_libraries(ehixs 
            ${LIBRARIES_FOR_GLOBAL_TESTS})

set(CMAKE_BUILD_TYPE Release)







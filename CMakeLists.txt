


cmake_minimum_required(VERSION 2.6)
project(ehixstests)

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
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME STREQUAL "gfortran")



set (CMAKE_CXX_FLAGS "-Wno-return-type-c-linkage -Wno-deprecated-writable-strings -Wno-delete-non-virtual-dtor ")

SET(EHIXS_SRC_DIR ${PROJECT_SOURCE_DIR}/src)

SET(CHAPLIN_SOURCES ${EHIXS_SRC_DIR}/chaplin)



include_directories(${EHIXS_SRC_DIR}
                    ${CHAPLIN_SOURCES}
                    /Users/lazopolis/other_codes/gtest-1.6.0/include
                    ${EHIXS_SRC_DIR}/bottom_fusion
                    ${EHIXS_SRC_DIR}/gluon_fusion
                    ${EHIXS_SRC_DIR}/gtests
                    ${EHIXS_SRC_DIR}/decay_bb
                    )

link_directories(
               #/Users/lazopolis/my_code/ehixs/Cuba-2.1/  
               /Users/lazopolis/other_codes/gtest-1.6.0/build/
               #${PROJECT_SOURCE_DIR}/../chaplin/cmakebuild/
               ${LHAPDFDIR}
               )

add_library(chaplin STATIC    ${CHAPLIN_SOURCES}/HPL1.f         
                              ${CHAPLIN_SOURCES}/HPL2ar1.f     
                              ${CHAPLIN_SOURCES}/HPL2atm1.f     
                              ${CHAPLIN_SOURCES}/HPL3ar0.f      
                              ${CHAPLIN_SOURCES}/HPL3at1.f      
                              ${CHAPLIN_SOURCES}/HPL4.f         
                              ${CHAPLIN_SOURCES}/HPL4arm1.f     
                              ${CHAPLIN_SOURCES}/HPL4else.f     
                              ${CHAPLIN_SOURCES}/basisexp.f
                              ${CHAPLIN_SOURCES}/HPL2.f         
                              ${CHAPLIN_SOURCES}/HPL2arm1.f     
                              ${CHAPLIN_SOURCES}/HPL2else.f     
                              ${CHAPLIN_SOURCES}/HPL3ar1.f      
                              ${CHAPLIN_SOURCES}/HPL3atm1.f     
                              ${CHAPLIN_SOURCES}/HPL4ar0.f      
                              ${CHAPLIN_SOURCES}/HPL4at1.f      
                              ${CHAPLIN_SOURCES}/aux.f          
                              ${CHAPLIN_SOURCES}/basisfct.f
                              ${CHAPLIN_SOURCES}/HPL2ar0.f      
                              ${CHAPLIN_SOURCES}/HPL2at1.f      
                              ${CHAPLIN_SOURCES}/HPL3.f         
                              ${CHAPLIN_SOURCES}/HPL3arm1.f     
                              ${CHAPLIN_SOURCES}/HPL3else.f     
                              ${CHAPLIN_SOURCES}/HPL4ar1.f      
                              ${CHAPLIN_SOURCES}/HPL4atm1.f     
                              ${CHAPLIN_SOURCES}/basis.f        
                              ${CHAPLIN_SOURCES}/coefficients.f
                              )



add_library(decay_bb 
                    ${EHIXS_SRC_DIR}/decay_bb/Decay_bb.cpp
                    )

add_library(test_utils ${EHIXS_SRC_DIR}/gtests/test_utils.cpp)

add_library(tools  
                    ${EHIXS_SRC_DIR}/hub.cpp
                    ${EHIXS_SRC_DIR}/vegas_adaptor.cpp
                    ${EHIXS_SRC_DIR}/histogram.cpp
                    ${EHIXS_SRC_DIR}/bin.cpp
                    ${EHIXS_SRC_DIR}/convolutions.cpp
                    ${EHIXS_SRC_DIR}/user_interface.cpp
                    )

add_library(models 
                    ${EHIXS_SRC_DIR}/model.cpp
                    ${EHIXS_SRC_DIR}/particle.cpp
            )

add_library(gluon_fusion 
                        # ${EHIXS_SRC_DIR}/gluon_fusion_inclusive/GluonFusionInclusive.cpp
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion.cpp
     
                        )

add_library(main
                    ${EHIXS_SRC_DIR}/production.cpp
                    ${EHIXS_SRC_DIR}/process.cpp
                    ${EHIXS_SRC_DIR}/decay.cpp
                )

add_library(pdf ${EHIXS_SRC_DIR}/luminosity.cpp
                ${EHIXS_SRC_DIR}/pdf.cpp
                ${EHIXS_SRC_DIR}/interpolator.cpp 
                ${EHIXS_SRC_DIR}/cached_interpolator.cpp
                ${EHIXS_SRC_DIR}/splitting_kernels.cpp
                ${EHIXS_SRC_DIR}/one_d_interpolator.cpp
               )




add_library(fortran_bbh 
                 #   ${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/kappa.f 
                    ${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbggH/bbggh1n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbggH/bbggh2n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbggH/bbggh3n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbggH/bbggh4n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbggH/bbggh5n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbggH/bbggh6n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbggH/bbggh7n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbggH/bbggh8n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbggH/bbggh9n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarbbarH/bbarbbarH1n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarbbarH/bbarbbarH2n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarbbarH/bbarbbarH3n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarbbarH/bbarbbarH4n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarbbarH/bbarbbarH5n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarbbarH/bbarbbarH6n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarbbarH/bbarbbarH7n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarbbarH/bbarbbarH8n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarbbarH/bbarbbarH9n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarqqbarH/bbarqqbarH1n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarqqbarH/bbarqqbarH2n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbarqqbarH/bbarqqbarH3n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbbbH/bbbbH1n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbbbH/bbbbH2n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbbbH/bbbbH3n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbbbH/bbbbH4n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbbbH/bbbbH5n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbbbH/bbbbH6n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbbbH/bbbbH7n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bbbbH/bbbbH8n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/bggbH1n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/bggbH2n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/bggbH3n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/bggbH4n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/bggbH5n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/bggbH6n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/bggbH7n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/bggbH8n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/bggbH9n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/gbgbH1n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/gbgbH2n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/gbgbH3n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/gbgbH4n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/gbgbH5n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/gbgbH6n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/gbgbH7n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/gbgbH8n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/gbgbH9n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bqqbH/bqqbH1n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bqqbH/bqqbH2n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bqqbH/bqqbH3n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bqqbH/bqqbH4n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bqqbH/bqqbH5n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bqqbH/bqqbH6n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/qbqbH/qbqbH1n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/qbqbH/qbqbH2n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/qbqbH/qbqbH3n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/qbqbH/qbqbH4n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/qbqbH/qbqbH5n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/qbqbH/qbqbH6n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/ggbbH/ggbbH1n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/ggbbH/ggbbH2n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/ggbbH/ggbbH3n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/ggbbH/ggbbH4n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/ggbbH/ggbbH5n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/qqbarbbarH/qqbarbbarH1n.f
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/RVbbargH/RVbbargH1n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/RVbbargH/RVbbargH2n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/RVbbargH/RVbbargH3n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/RVbbargH/RVbbargH4n.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/RVbbargH/polylog.f 
				${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/RVbbargH/functions.f 
                ${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/gbgbH/sigma_gbbH_RV.f 
                ${EHIXS_SRC_DIR}/bottom_fusion/nnlo_double_real/bggbH/RVbgbH1n.f 
                ${EHIXS_SRC_DIR}/bottom_fusion/RVbbargHren0n.f )


add_library(nlo_exact_matrix_elements  
                ${EHIXS_SRC_DIR}/gluon_fusion/nlo_exact_matrix_elements.cpp)

add_library(fortran_ggf ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rgg2ght1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rqg2qht1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rgq2qht1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rqqbar2ght1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrq1q22q1q2Ht1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrq1q22q1q2Ht2.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqq2qqHt1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqq2qqHt2.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqq2qqHt3.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2qqbarht1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2qqbarht2.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2qqbarht3.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2qqbarht4.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2qqbarht5.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2qqbarht6.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2gght1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2gght2.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2gght3.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2gght4.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2gght5.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqqbar2gght6.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rvqqbar2ght1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrq1q1bar2q2q2barht1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrq1q1bar2q2q2barht2.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght2.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght3.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght4.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght5.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght6.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght7.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght8.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght9.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght10.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght11.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght12.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght13.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght14.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgq2qght15.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rvgq2qht1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght2.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght3.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght4.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght5.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght6.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght7.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght8.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght9.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght10.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght11.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght12.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght13.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght14.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrqg2qght15.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rvqg2qht1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght2.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght3.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght4.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght5.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght6.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght7.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght8.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght9.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght10.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght11.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght12.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght13.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght14.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght15.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2gght16.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht1.f                         
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht2.f                         
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht3.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht4.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht5.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht6.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht7.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht8.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht9.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht10.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht11.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht12.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rrgg2qqbarht13.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rvgg2ght1.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rvgg2ght2.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/rvgg2ght4.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/gluon_fusion_matrix_elements/kappa.f
                         ${EHIXS_SRC_DIR}/gluon_fusion/exact_nlo_matrix_elements/massive_ampls.F
                         ${EHIXS_SRC_DIR}/gluon_fusion/exact_nlo_matrix_elements/masters.F
          )


add_library(avh_olo      ${EHIXS_SRC_DIR}/avh_olo/avh_olo_3div.f
                         ${EHIXS_SRC_DIR}/avh_olo/avh_olo_3fin.f
                         ${EHIXS_SRC_DIR}/avh_olo/avh_olo_4div.f
                         ${EHIXS_SRC_DIR}/avh_olo/avh_olo_4fin.f
                         ${EHIXS_SRC_DIR}/avh_olo/avh_olo_b11.f
                         ${EHIXS_SRC_DIR}/avh_olo/avh_olo_cd0.f
                         ${EHIXS_SRC_DIR}/avh_olo/avh_olo_cmplx.f
                         ${EHIXS_SRC_DIR}/avh_olo/avh_olo_func.f
                         ${EHIXS_SRC_DIR}/avh_olo/avh_olo_hello.f
                         ${EHIXS_SRC_DIR}/avh_olo/avh_olo_real.f
          )


find_library(LHAPDF ssl $ENV{LHAPDF_PATH})
#This will check the environment for a variable named LHAPDF_PATH. If it exists, you can then use
#target_link_libraries(mytarget ${LHAPDF})
#If it does not, it can be set through the CMake GUI.



add_executable(tests  ${EHIXS_SRC_DIR}/gtests/tests.cpp)
target_link_libraries(tests  
                                        main cuba gtest gfortran chaplin tools   decay_bb  fortran_bbh fortran_ggf  pdf avh_olo LHAPDF ${LHAPDF})

add_executable(channel_tests  ${EHIXS_SRC_DIR}/gtests/channel_tests.cpp)
target_link_libraries(channel_tests   
                                        gtest   tools)

#add_executable(pdf_test  ${EHIXS_SRC_DIR}/gtests/pdf_test.cpp)
#target_link_libraries(pdf_test  
#                                        cuba gtest gfortran  tools pdf avh_olo LHAPDF ${LHAPDF})

#add_executable(integration_tests  ${EHIXS_SRC_DIR}/gtests/integration_tests.cpp)
#target_link_libraries(integration_tests  
#                                        cuba gtest gfortran chaplin tools  decay_bb  main fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})

add_executable(nlo_poles_test  ${EHIXS_SRC_DIR}/gtests/ggf_pole_cancelation_nlo_test.cpp)
target_link_libraries(nlo_poles_test  
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})


add_executable(nlo_gg_xs_test  ${EHIXS_SRC_DIR}/gtests/ggf_nlo_gg_channel_xsection_test.cpp)
target_link_libraries(nlo_gg_xs_test  
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})

add_executable(nnlo_q1q2_pole_test ${EHIXS_SRC_DIR}/gtests/ggf_NNLO_q1q2_pole_cancelation_test.cpp)
target_link_libraries(nnlo_q1q2_pole_test  
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})

add_executable(nnlo_qq_test ${EHIXS_SRC_DIR}/gtests/ggf_nnlo_qq_test.cpp)
target_link_libraries(nnlo_qq_test  
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})

add_executable(nnlo_qqbar_test ${EHIXS_SRC_DIR}/gtests/ggf_nnlo_qqbar_test.cpp)
target_link_libraries(nnlo_qqbar_test 
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})

add_executable(nnlo_qg_test ${EHIXS_SRC_DIR}/gtests/quark_gluon_nnlo_test.cpp)
target_link_libraries(nnlo_qg_test  
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements  fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})

add_executable(nnlo_gg_test ${EHIXS_SRC_DIR}/gtests/gluon_gluon_nnlo_test.cpp)
target_link_libraries(nnlo_gg_test  
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})

add_executable(nnlo_gq_test ${EHIXS_SRC_DIR}/gtests/gluon_quark_nnlo_test.cpp)
target_link_libraries(nnlo_gq_test 
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})

add_executable(nlo_exact_test ${EHIXS_SRC_DIR}/gtests/nlo_exact_test.cpp)
target_link_libraries(nlo_exact_test 
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF})

add_executable(one_d_interpolator_test ${EHIXS_SRC_DIR}/gtests/one_d_interpolator_test.cpp)
target_link_libraries(one_d_interpolator_test 
                                        tools pdf gtest avh_olo LHAPDF ${LHAPDF})


#add_executable(iggf_test  ${EHIXS_SRC_DIR}/iggf.cpp)
#target_link_libraries(iggf_test  
#                                        ggf_inclusive tools chaplin gluon_fusion nlo_exact_matrix_elements gfortran gtest)


add_executable(UI_test ${EHIXS_SRC_DIR}/gtests/test_UI.cpp)
target_link_libraries(UI_test 
                                        tools gtest)

add_executable(nlo_exact_virtual ${EHIXS_SRC_DIR}/gtests/exact_virtual_test.cpp)
target_link_libraries(nlo_exact_virtual 
                                        nlo_exact_matrix_elements 
                                        fortran_ggf
                                        models
                                        avh_olo
                                        chaplin
                                        tools
                                        cuba
                                        gtest)


add_executable(off_line_histograming ${EHIXS_SRC_DIR}/gtests/off_line_histograming.cpp)
target_link_libraries(off_line_histograming
                                        tools gtest)

#add_executable(model_test ${EHIXS_SRC_DIR}/gtests/iggf.cpp)
#target_link_libraries(model_test
#         		gtest
#			models
#			)

add_library(ggf_inclusive      ${EHIXS_SRC_DIR}/exact_lo_inclusive.cpp
          )

add_executable(iggf ${EHIXS_SRC_DIR}/gtests/iggf.cpp)
target_link_libraries(iggf
         		gtest
			ggf_inclusive
			chaplin
			tools
			models
			cuba
			pdf
			LHAPDF ${LHAPDF} 
			)


add_executable(ihixs++ ${EHIXS_SRC_DIR}/ihixs.cpp)
target_link_libraries(ihixs++
			ggf_inclusive
			chaplin
			tools
			models
			cuba
			pdf
			LHAPDF ${LHAPDF} 
			)

add_executable(ehixs  ${EHIXS_SRC_DIR}/ehixs.cpp)
target_link_libraries(ehixs    
                                        models test_utils cuba gtest gfortran chaplin tools  decay_bb  main gluon_fusion nlo_exact_matrix_elements fortran_bbh fortran_ggf pdf avh_olo LHAPDF ${LHAPDF}  )

set(CMAKE_BUILD_TYPE Release)







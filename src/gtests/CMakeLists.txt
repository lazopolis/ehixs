
#SET(GTEST_DIR /Users/lazopolis/other_codes/gtest-1.7.0)
#
#include_directories(
#                ${GTEST_DIR}/include
#                    )
#
#link_directories(
#                ${GTEST_DIR}/mybuild/
#               )

#add_library(test_utils test_utils.cpp)

#add_executable(UI_test
#            test_UI.cpp)
#target_link_libraries(UI_test
#                        tools gtest)


#add_executable(nlo_poles_test  ggf_pole_cancelation_nlo_test.cpp)
#target_link_libraries(nlo_poles_test ${LIBRARIES_FOR_GLOBAL_TESTS})


#add_executable(nlo_gg_xs_test ggf_nlo_gg_channel_xsection_test.cpp)
#target_link_libraries(nlo_gg_xs_test ${LIBRARIES_FOR_GLOBAL_TESTS})
  

#add_executable(nnlo_q1q2_pole_test
#            ggf_NNLO_q1q2_pole_cancelation_test.cpp)
#target_link_libraries(nnlo_q1q2_pole_test
#			${LIBRARIES_FOR_GLOBAL_TESTS})
  
#add_executable(nnlo_qq_test
#            ggf_nnlo_qq_test.cpp)
#target_link_libraries(nnlo_qq_test
#			${LIBRARIES_FOR_GLOBAL_TESTS})

#add_executable(nnlo_qqbar_test
#            ggf_nnlo_qqbar_test.cpp)
#target_link_libraries(nnlo_qqbar_test
#			${LIBRARIES_FOR_GLOBAL_TESTS})
 

#add_executable(nnlo_qg_test
#            quark_gluon_nnlo_test.cpp)
#target_link_libraries(nnlo_qg_test
#			${LIBRARIES_FOR_GLOBAL_TESTS})

#add_executable(nnlo_gg_test
#            gluon_gluon_nnlo_test.cpp)
#target_link_libraries(nnlo_gg_test
#			${LIBRARIES_FOR_GLOBAL_TESTS})

#add_executable(nnlo_gq_test
#            gluon_quark_nnlo_test.cpp)
#target_link_libraries(nnlo_gq_test
#			${LIBRARIES_FOR_GLOBAL_TESTS})

#add_executable(nlo_exact_test
#            nlo_exact_test.cpp)
#target_link_libraries(nlo_exact_test
#            ${LIBRARIES_FOR_GLOBAL_TESTS})
			

#add_executable(one_d_interpolator_test
#            one_d_interpolator_test.cpp)
#target_link_libraries(one_d_interpolator_test
#            tools pdf gtest avh_olo LHAPDF ${LHAPDF})
            


#add_executable(live_convolution_test
#            live_convolution_test.cpp)
#target_link_libraries(live_convolution_test
#                    pdf tools mincuba LHAPDF ${LHAPDF}
#                    gtest)

#add_executable(test_ihixs
#            iggf.cpp)
#target_link_libraries(test_ihixs
#                    ihixs_lib
#                    pdf tools mincuba LHAPDF ${LHAPDF}
#                    gtest)

#add_executable(off_line_histograming
#            off_line_histograming.cpp)
#target_link_libraries(off_line_histograming
#            tools gtest)


#add_executable(iggf
#            iggf.cpp)
#target_link_libraries(iggf
#            gtest ggf_inclusive
#			chaplin tools models mincuba pdf LHAPDF ${LHAPDF}
#			)
            
#add_executable(gstar2_nlo_test ${EHIXS_SRC_DIR}/gamma_star_gamma_star/gtests.cpp)
#target_link_libraries(gstar2_nlo_test
#                    gamma_star_gamma_star
#                    tools pdf LHAPDF chaplin
#                    gtest)

add_executable(
            counterforge_test
            counterforge_test.cpp
)
target_link_libraries(
                    counterforge_test
                    chaplin
                    bbh
                    tools
)

add_executable(
            boxmaster_test
            boxmaster_test.cpp
)
target_link_libraries(
                    boxmaster_test
                    bbh
                    chaplin
                    tools
                    ${CLN_LIBRARIES}
)

add_executable(
            fourvector_test
            fourvector_test.cpp
            )
target_link_libraries(
                    fourvector_test
                    gtest
                    kinematics
                    )
add_executable(
            expansion_test
            expansion_test.cpp
            )
target_link_libraries(
                    expansion_test
                    gtest
                    tools
                    )



add_executable(ihixs_nlo_exact_test
            ihixs_nlo_exact_test.cpp)
target_link_libraries(ihixs_nlo_exact_test
                        exact_gluon_fusion
                        models
                        chaplin
                        #tools
                        gtest)




add_executable(
            ihixs_tests
            ihixs_tests.cpp
            )
target_link_libraries(
                    ihixs_tests
                    ihixs_lib
                    exact_gluon_fusion
                    electroweak_coeffs
                    avh_olo
                    chaplin
                    pdf
                    tools
                    models
                    LHAPDF
                    mincuba
                    gtest
                    )
                    
add_executable(
            ihixs_as_series
            ihixs_as_series.cpp
            )
target_link_libraries(
                    ihixs_as_series
                    ihixs_lib
                    mincuba
                    tools
                    gtest
                    )
                    
add_executable(
            ihixs_eft
            ihixs_eft.cpp
            )
target_link_libraries(
            ihixs_eft
            ihixs_lib
            mincuba
            chaplin
            models
            exact_gluon_fusion
            tools
            gtest
)

if (CLN_FOUND)
add_executable(
            mypow_test
            mypow_test.cpp
)
target_link_libraries(
            mypow_test
            ${CLN_LIBRARIES}
)
endif (CLN_FOUND)

if (CLN_FOUND)
add_executable(
            qq2yy_test
            qq2yy_test.cpp
)
target_link_libraries(
            qq2yy_test
            gammagamma
            kinematics
            tools
            chaplin
            bbh
            ${CLN_LIBRARIES}
)
endif (CLN_FOUND)

add_executable(
            pgenerator_test
            pgenerator_test.cpp
)
target_link_libraries(
            pgenerator_test
            kinematics
            tools
)

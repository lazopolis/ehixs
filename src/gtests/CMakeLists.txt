

SET(GTEST_DIR /Users/lazopolis/other_codes/gtest-1.6.0)


include_directories(
                ${GTEST_DIR}/include
                    )

link_directories(
                ${GTEST_DIR}/build/
               )

add_library(test_utils test_utils.cpp)

add_executable(UI_test 
            test_UI.cpp)
target_link_libraries(UI_test 
                        tools gtest)


add_executable(nlo_poles_test  ggf_pole_cancelation_nlo_test.cpp)
target_link_libraries(nlo_poles_test ${LIBRARIES_FOR_GLOBAL_TESTS})  


add_executable(nlo_gg_xs_test ggf_nlo_gg_channel_xsection_test.cpp)
target_link_libraries(nlo_gg_xs_test ${LIBRARIES_FOR_GLOBAL_TESTS})
  

add_executable(nnlo_q1q2_pole_test 
            ggf_NNLO_q1q2_pole_cancelation_test.cpp)
target_link_libraries(nnlo_q1q2_pole_test
			${LIBRARIES_FOR_GLOBAL_TESTS})
  
add_executable(nnlo_qq_test 
            ggf_nnlo_qq_test.cpp)
target_link_libraries(nnlo_qq_test
			${LIBRARIES_FOR_GLOBAL_TESTS})  

add_executable(nnlo_qqbar_test 
            ggf_nnlo_qqbar_test.cpp)
target_link_libraries(nnlo_qqbar_test
			${LIBRARIES_FOR_GLOBAL_TESTS})
 

add_executable(nnlo_qg_test 
            quark_gluon_nnlo_test.cpp)
target_link_libraries(nnlo_qg_test
			${LIBRARIES_FOR_GLOBAL_TESTS})  

add_executable(nnlo_gg_test 
            gluon_gluon_nnlo_test.cpp)
target_link_libraries(nnlo_gg_test
			${LIBRARIES_FOR_GLOBAL_TESTS})  

add_executable(nnlo_gq_test 
            gluon_quark_nnlo_test.cpp)
target_link_libraries(nnlo_gq_test
			${LIBRARIES_FOR_GLOBAL_TESTS}) 

add_executable(nlo_exact_test 
            nlo_exact_test.cpp)
target_link_libraries(nlo_exact_test 
            ${LIBRARIES_FOR_GLOBAL_TESTS})
			

add_executable(one_d_interpolator_test 
            one_d_interpolator_test.cpp)
target_link_libraries(one_d_interpolator_test 
            tools pdf gtest avh_olo LHAPDF ${LHAPDF})
            

add_executable(nlo_exact_virtual 
            exact_virtual_test.cpp)
target_link_libraries(nlo_exact_virtual
                    gluon_fusion fortran_ggf fortran_ggf_RR 
                    models avh_olo chaplin tools
                    mincuba gtest)




add_executable(off_line_histograming 
            off_line_histograming.cpp)
target_link_libraries(off_line_histograming
            tools gtest)


add_executable(iggf 
            iggf.cpp)
target_link_libraries(iggf
            gtest ggf_inclusive
			chaplin tools models mincuba pdf LHAPDF ${LHAPDF}
			)



            
add_executable(gstar2_nlo_test ${EHIXS_SRC_DIR}/gamma_star_gamma_star/gtests.cpp)
target_link_libraries(gstar2_nlo_test
                    gamma_star_gamma_star
                    tools pdf LHAPDF chaplin
                    gtest)
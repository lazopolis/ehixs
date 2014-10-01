

#add_subdirectory(RRroutinesv2)


add_library(gluon_fusion 
                         gluon_fusion.cpp
                         gluon_fusion_me.cpp
#                         nlo_exact_matrix_elements.cpp
#                         simple_sector.cpp
#                         matrix_element.cpp
#                         gluon_fusion_sector_box.cpp
#                         gluon_fusion_exact_coefficients.cpp
#                         gluon_fusion_ew_coefficients.cpp
#                         gluon_fusion_parametrization.cpp
#                         gluon_fusion_event_reconstructor.cpp
                        )


#add_library(fortran_ggf gluon_fusion_matrix_elements/rgg2ght1.f
#                         gluon_fusion_matrix_elements/rqg2qht1.f
#                         gluon_fusion_matrix_elements/rgq2qht1.f
#                         gluon_fusion_matrix_elements/rqqbar2ght1.f
#
#                         gluon_fusion_matrix_elements/rvqqbar2ght1.f
#
#                         gluon_fusion_matrix_elements/rvgq2qht1.f
#
#                         gluon_fusion_matrix_elements/rvqg2qht1.f
#
#                         gluon_fusion_matrix_elements/rvgg2ght1.f
#                         gluon_fusion_matrix_elements/rvgg2ght2.f
#                         gluon_fusion_matrix_elements/rvgg2ght4.f
#
#                         gluon_fusion_matrix_elements/rrqqbar2qqbarht1.f
#                         gluon_fusion_matrix_elements/rrqqbar2qqbarht2.f
#                         gluon_fusion_matrix_elements/rrqqbar2qqbarht3.f
#                         gluon_fusion_matrix_elements/rrqqbar2qqbarht4.f
#                         gluon_fusion_matrix_elements/rrqqbar2qqbarht5.f
#                         gluon_fusion_matrix_elements/rrqqbar2qqbarht6.f
#
#                         gluon_fusion_matrix_elements/kappa.f
#                         exact_nlo_matrix_elements/massive_ampls.F
#                         exact_nlo_matrix_elements/masters.F
#
#                         electroweak_h_plus_jet.f
#          )




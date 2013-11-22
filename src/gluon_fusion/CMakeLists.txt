

add_subdirectory(RRroutinesv2)


add_library(gluon_fusion 
                        # gluon_fusion_inclusive/GluonFusionInclusive.cpp
                         gluon_fusion.cpp
                         nlo_exact_matrix_elements.cpp
                        )


add_library(fortran_ggf gluon_fusion_matrix_elements/rgg2ght1.f
                         gluon_fusion_matrix_elements/rqg2qht1.f
                         gluon_fusion_matrix_elements/rgq2qht1.f
                         gluon_fusion_matrix_elements/rqqbar2ght1.f
                         
                         gluon_fusion_matrix_elements/rvqqbar2ght1.f
                         
                         gluon_fusion_matrix_elements/rvgq2qht1.f
                         
                         gluon_fusion_matrix_elements/rvqg2qht1.f
                         
                         gluon_fusion_matrix_elements/rvgg2ght1.f
                         gluon_fusion_matrix_elements/rvgg2ght2.f
                         gluon_fusion_matrix_elements/rvgg2ght4.f
                         
                         gluon_fusion_matrix_elements/rrqqbar2qqbarht1.f
                         gluon_fusion_matrix_elements/rrqqbar2qqbarht2.f
                         gluon_fusion_matrix_elements/rrqqbar2qqbarht3.f
                         gluon_fusion_matrix_elements/rrqqbar2qqbarht4.f
                         gluon_fusion_matrix_elements/rrqqbar2qqbarht5.f
                         gluon_fusion_matrix_elements/rrqqbar2qqbarht6.f

                         gluon_fusion_matrix_elements/kappa.f
                         exact_nlo_matrix_elements/massive_ampls.F
                         exact_nlo_matrix_elements/masters.F
                         
                         electroweak_h_plus_jet.f
          )




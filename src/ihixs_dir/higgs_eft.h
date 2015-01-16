#ifndef HIGGS_EFT_H
#define HIGGS_EFT_H

#include "constants.h"
#include<vector>
#include "as_series.h"
using namespace std;


namespace HEFT {
    
    double one_minus_z(const double& z, const double& L);
    double one(const double& z, const double& L);
    
    AsSeries n_delta_at_mh();
    AsSeries n_D0_at_mh();
    AsSeries n_D1_at_mh();
    AsSeries n_D2_at_mh();
    AsSeries n_D3_at_mh();
    AsSeries n_D4_at_mh();
    AsSeries n_D5_at_mh();
    
    AsSeries n_delta_log_muf(const double& L);
    AsSeries n_D0_log_muf(const double& L);
    AsSeries n_D1_log_muf(const double& L);
    AsSeries n_D2_log_muf(const double& L);
    AsSeries n_D3_log_muf(const double& L);
    AsSeries n_D4_log_muf(const double& L);

    double nlo_r_lz0(const double& z, const double& L);
    double nlo_r_lz1(const double& z, const double& L);
    double nnlo_r_lz0(const double& z, const double& L);
    double nnlo_r_lz1(const double& z, const double& L);
    double nnlo_r_lz2(const double& z, const double& L);
    double nnlo_r_lz3(const double& z, const double& L);
    double nnlo_r_lz0_const(const double& z, const double& L);
    double nnlo_r_lz0_logz(const double& z, const double& L);
    double nnlo_r_lz0_logz_sq(const double& z, const double& L);
    double nnlo_r_lz0_logz_cube(const double& z, const double& L);
    
    double gg_n3lo_r_lz0(const double& z, const double& L);
    double gg_n3lo_r_lz1(const double& z, const double& L);
    double gg_n3lo_r_lz2(const double& z, const double& L);
    double gg_n3lo_r_lz3(const double& z, const double& L);
    double gg_n3lo_r_lz4(const double& z, const double& L);
    double gg_n3lo_r_lz5(const double& z, const double& L);

    double gg_n3lo_r_lz0_NS(const double& z, const double& L);
    double gg_n3lo_r_lz1_NS(const double& z, const double& L);
    double gg_n3lo_r_lz2_NS(const double& z, const double& L);
    double gg_n3lo_r_lz3_NS(const double& z, const double& L);
    double gg_n3lo_r_lz4_NS(const double& z, const double& L);
    double gg_n3lo_r_lz5_NS(const double& z, const double& L);
    
    double nFull_minus_nS(const double& z, const double& L);
    double qg_nFull_minus_nS(const double& z, const double& L);

#ifdef __cplusplus
    extern "C" {
#endif
        double leggnnnloreg_(double* z, double* L);
        double leqgnnnloreg_(double* z, double* L);
        double leqqbnnnlo_(double* z, double* L);
        double leqqnnnlo_(double* z, double* L);
        double leq1q2nnnlo_(double* z, double* L);

#ifdef __cplusplus
    }
#endif
    

    double LEggNNNLOregSte(const double& z, const double& L);
    double LEggN3LOregFalko(const double& z, const double& L);

    double LEqgNNNLOregSte(const double& z, const double& L);
    double LEqgN3LOregFalko(const double& z, const double& L);

    double LEqqbNNNLOregSte(const double& z, const double& L);
    double LEqqbN3LOregFalko(const double& z, const double& L);
    
    double LEqqNNNLOregSte(const double& z, const double& L);
    double LEqqN3LOregFalko(const double& z, const double& L);
    
    double LEq1q2NNNLOregSte(const double& z, const double& L);
    double LEq1q2N3LOregFalko(const double& z, const double& L);
    
    double qg_nnlo_r_lz0(const double& z, const double& L);
    double qg_nnlo_r_lz1(const double& z, const double& L);
    double qg_nnlo_r_lz2(const double& z, const double& L);
    double qg_nnlo_r_lz3(const double& z, const double& L);
    double qg_nnlo_r_lz0_const(const double& z, const double& L);
    double qg_nnlo_r_lz0_logz(const double& z, const double& L);
    double qg_nnlo_r_lz0_logz_sq(const double& z, const double& L);
    double qg_nnlo_r_lz0_logz_cube(const double& z, const double& L);
    
    double qg_n3lo_r_lz0(const double& z, const double& L);
    double qg_n3lo_r_lz1(const double& z, const double& L);
    double qg_n3lo_r_lz2(const double& z, const double& L);
    double qg_n3lo_r_lz3(const double& z, const double& L);
    double qg_n3lo_r_lz4(const double& z, const double& L);
    double qg_n3lo_r_lz5(const double& z, const double& L);

    double qg_n3lo_r_lz0_NS(const double& z, const double& L);
    double qg_n3lo_r_lz1_NS(const double& z, const double& L);
    double qg_n3lo_r_lz2_NS(const double& z, const double& L);
    double qg_n3lo_r_lz3_NS(const double& z, const double& L);
    double qg_n3lo_r_lz4_NS(const double& z, const double& L);
    double qg_n3lo_r_lz5_NS(const double& z, const double& L);
    
    
    double qg_nlo_r_lz0(const double& z, const double& L);
    double qg_nlo_r_lz1(const double& z, const double& L);
    
    double qqb_nlo_r_lz0(const double& z, const double& L);
    
    double qqb_nnlo_r_lz0(const double& z, const double& L);
    double qqb_nnlo_r_lz1(const double& z, const double& L);
    double qqb_nnlo_r_lz2(const double& z, const double& L);

    double qqb_nnlo_r_lz0_const(const double& z, const double& L);
    double qqb_nnlo_r_lz0_logz(const double& z, const double& L);
    double qqb_nnlo_r_lz0_logz_sq(const double& z, const double& L);
    double qqb_nnlo_r_lz0_logz_cube(const double& z, const double& L);
    
    double qq_nnlo_r_lz0(const double& z, const double& L);
    double qq_nnlo_r_lz1(const double& z, const double& L);
    double qq_nnlo_r_lz2(const double& z, const double& L);
    
    double qq_nnlo_r_lz0_const(const double& z, const double& L);
    double qq_nnlo_r_lz0_logz(const double& z, const double& L);
    double qq_nnlo_r_lz0_logz_sq(const double& z, const double& L);
    double qq_nnlo_r_lz0_logz_cube(const double& z, const double& L);
    
    double q1q2_nnlo_r_lz0(const double& z, const double& L);
    double q1q2_nnlo_r_lz1(const double& z, const double& L);
    double q1q2_nnlo_r_lz2(const double& z, const double& L);
    
    double q1q2_nnlo_r_lz0_const(const double& z, const double& L);
    double q1q2_nnlo_r_lz0_logz(const double& z, const double& L);
    double q1q2_nnlo_r_lz0_logz_sq(const double& z, const double& L);
    double q1q2_nnlo_r_lz0_logz_cube(const double& z, const double& L);
    
    double n_LO_delta();
    
    double n_NLO_delta();
    
    double n_NNLO_delta();
    double n_NNLO_delta_L();
    double n_NNLO_delta_L2();
    
    double n_N3LO_delta();
    double n_N3LO_delta_L();
    double n_N3LO_delta_L2();
    double n_N3LO_delta_L3();

    double n_NLO_D0_L();
    
    double n_NLO_D1();
    
    double n_NNLO_D0();
    double n_NNLO_D0_L();
    double n_NNLO_D0_L2();
    
    double n_NNLO_D1();
    double n_NNLO_D1_L();
    double n_NNLO_D1_L2();
    
    double n_NNLO_D2();
    double n_NNLO_D2_L();
    
    double n_NNLO_D3();
    
    
    double n_N3LO_D0();
    double n_N3LO_D0_L();
    double n_N3LO_D0_L2();
    double n_N3LO_D0_L3();
    
    double n_N3LO_D1();
    double n_N3LO_D1_L();
    double n_N3LO_D1_L2();
    double n_N3LO_D1_L3();
    
    double n_N3LO_D2();
    double n_N3LO_D2_L();
    double n_N3LO_D2_L2();
    
    double n_N3LO_D3();
    double n_N3LO_D3_L();
    double n_N3LO_D3_L2();
    double n_N3LO_D2_L3();
    
    double n_N3LO_D4();
    double n_N3LO_D4_L();
    
    double n_N3LO_D5();
}



#endif
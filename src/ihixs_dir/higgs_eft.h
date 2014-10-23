#ifndef HIGGS_EFT_H
#define HIGGS_EFT_H

#include "constants.h"
#include<vector>
#include "as_series.h"
using namespace std;


namespace HEFT {
    AsSeries n_delta_at_muf(const double& L);
    AsSeries n_D0_at_muf(const double& L);
    AsSeries n_D1_at_muf(const double& L);
    AsSeries n_D2_at_muf(const double& L);
    AsSeries n_D3_at_muf(const double& L);
    AsSeries n_D4_at_muf(const double& L);
    AsSeries n_D5_at_muf(const double& L);
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

class HiggsEFT
{
public:
    
    
    //---------- plus terms
    
    
    
    
    
};

#endif
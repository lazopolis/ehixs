#ifndef CONVOLUTIONS_TOTAL_H
#define CONVOLUTIONS_TOTAL_H 

#include "ExclusiveClass.h"
#include "BottomFusion_NLO_ME.h"


// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"

class convolution_total:public ExclusiveClass
{
private:
    //b_b_to_H_b_b(){};
    BottomFusion_NLO_ME NLO_ME;
public: 
    convolution_total(const UserInterface& UI);
    ~convolution_total(){};
    void LO(){};
    void NLO();
    void NNLO();
    void prepare_normalization();
    void sigma_gb_LO();
    void sigma_bg_LO();
    void sigma_bb_LO_pole_1_NLO();
    void sigma_bb_LO_pole_1();
    void sigma_bb_LO_pole_2();
    void sigma_bb_NLO();
    void NNLO_conv_bb2gH_soft();
    void NNLO_conv_bb2gH_hard();
    void mur_over_muf_terms_bb_nlo_only();
    void mur_over_muf_terms_bb();
    void mur_over_muf_terms_bg();
    void mur_over_muf_terms_gb();
    
   
    double pref_sbb,x1LO,x2LO,measLO,zLO,x1,x2,z,lambda,meas,Log_1mz,lh,curs,cursLO;
    double Nij,special_normalization_factor_for_double_real;
    vector<double> cur_lumiLO;
    vector<double> cur_lumi;
    void (convolution_total::*pointer_to_function_for_sector)();
};

#endif

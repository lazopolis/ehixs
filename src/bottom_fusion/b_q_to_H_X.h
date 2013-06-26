#ifndef BQ2HX_H
#define BQ2HX_H 

#include "ExclusiveClass.h"
#include "BottomFusion_NLO_ME.h"


// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"

class b_q_to_H_X:public ExclusiveClass
{
private:
    //b_q_to_H_X(){};
    BottomFusion_NLO_ME NLO_ME;
public: 
    b_q_to_H_X(const UserInterface& UI);
    ~b_q_to_H_X(){};
    void LO(){};
    void NLO(){};
    void NNLO();
    void prepare_normalization();
    void NNLO_RR_Romain();
    //: franz below
    void NNLO_RR_Franz();
    void NNLO_gb_conv();
    void NNLO_bg_conv();
    void NNLO_bb_double_conv_e1();
    void NNLO_bb_double_conv_e2();
    void (*FRTopologies[100])(const int&, const int&, const double&,const double&,const double&,
                              const double&, const double&, const double&, const double&, 
                              const double&, const double&, const double&, const double&);
    int sector_init[100];
    int sector_fin[100];
    void set_topologies();
    //: romain's
    std::vector<pSubF> RTopologies;
    double pref_sbb,x1LO,x2LO,measLO,zLO,x1,x2,z,lambda,meas,Log_1mz,lh,curs,cursLO;
    double Nij,special_normalization_factor_for_double_real;
    vector<double> cur_lumiLO;
    vector<double> cur_lumi;
    void (b_q_to_H_X::*pointer_to_function_for_sector)();
};

#endif

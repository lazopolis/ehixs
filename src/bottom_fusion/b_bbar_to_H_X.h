#ifndef BBBAR2HX_H
#define BBBAR2HX_H 

#include "ExclusiveClass.h"
#include "BottomFusion_NLO_ME.h"
// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"

class b_bbar_to_H_X:public ExclusiveClass
{
    private:
        BottomFusion_NLO_ME NLO_ME;
    public: 
        b_bbar_to_H_X(const UserInterface& UI);
        ~b_bbar_to_H_X(){};
        void LO();
        void NLO();
        void NNLO();

        void NLO_conv();
        void bbdeltaLO();
        //:
        void bbrealNLO(); //obscolete
        void bbvirtualNLO();//obscolete
  
        void NLO_HARD();
        void NLO_SOFT();
        //:
        void bbvirtualNLO_times_conv();//obscolete
        void bbrealNLO_times_conv();//obscolete
        void NNLO_conv_bb2gH_hard();
        void NNLO_conv_bb2gH_soft();
        //:
        void NNLO_conv_RRa();
        void NNLO_conv_RRb();
        void NNLO_conv_RRc();
        void NNLO_conv_RRd();
        void NNLO_conv_RRe();
  
        void NNLO_Vsquare();
        void NNLO_VV();
        void NNLO_soft();  
        void prepare_normalization();
        void NNLO_RR_Romain();
        //: franz below
        void NNLO_RR_Franz();
        void NNLO_RV_Franz();
        void NNLO_RV_Franz_A1();
        void NNLO_RV_Franz_A2();
        void NNLO_RV_Franz_A3();
        void NNLO_RV_Franz_A4();
        void bb_renormalization();
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
        void (b_bbar_to_H_X::*pointer_to_function_for_sector)();

};

#endif

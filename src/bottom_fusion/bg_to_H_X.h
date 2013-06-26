#ifndef BG_2_HX
#define BG_2_HX 

#include "ExclusiveClass.h"
#include "BottomFusion_NLO_ME.h"
// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"

class b_g_to_H_X:public ExclusiveClass
{
    private:
        // b_g_to_H_X(){};
        BottomFusion_NLO_ME NLO_ME;
    public: 
        b_g_to_H_X(const UserInterface& UI);
        ~b_g_to_H_X(){};
        void LO(){};
        void NLO();
        void NNLO();
    
        void NNLO_RR_Franz();
        void NNLO_conv_bb_nlo_soft();
        void NNLO_conv_bb_nlo_hard();
        void NNLO_conv_bb_LO_e2();
        void NNLO_conv_bb_LO_e1();
        void NNLO_conv_bg();
        void NNLO_conv_gb();
        
        void (*FRTopologies[100])(const int&, const int&, const double&,const double&,const double&,
                                  const double&, const double&, const double&, const double&, 
                                  const double&, const double&, const double&, const double&);
        void  prepare_normalization();
        int sector_init[100];
        int sector_fin[100];
        void set_topologies();
        //: romain's
        std::vector<pSubF> RTopologies;
        void initialize_pdfs();
        double pref_sbb,x1LO,x2LO,measLO,zLO,x1,x2,z,lambda,meas,Log_1mz,lh,curs,cursLO,Nij,special_normalization_factor_for_double_real;
        vector<double> cur_lumiLO;
        vector<double> cur_lumi;
	
        void NLO_HARD_1();
        void NLO_HARD_2();
        void NLO_conv_1();
        void NLO_conv_2();
    
        void NNLO_RV_gb();
        void NNLO_RV_bg();
     void NNLO_renorm_gb();
     void NNLO_renorm_bg();

};

#endif

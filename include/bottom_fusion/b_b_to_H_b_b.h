#ifndef BB2BBH_H
#define BB2BBH_H 

#include "ExclusiveClass.h"
#include "BottomFusion_NLO_ME.h"


// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"

class b_b_to_H_b_b:public ExclusiveClass
{
    private:
        //b_b_to_H_b_b(){};
        BottomFusion_NLO_ME NLO_ME;
    public: 
        b_b_to_H_b_b(const UserInterface& UI);
        ~b_b_to_H_b_b(){};
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
        void (b_b_to_H_b_b::*pointer_to_function_for_sector)();
	};

#endif

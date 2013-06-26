#ifndef GG2BBBARH_H
#define GG2BBBARH_H 

#include "ExclusiveClass.h"
#include "BottomFusion_NLO_ME.h"
// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"

class g_g_to_b_bbar_H:public ExclusiveClass
{
private:
   // g_g_to_b_bbar_H(){};
   BottomFusion_NLO_ME NLO_ME;
public: 
  g_g_to_b_bbar_H(const UserInterface& UI);
  ~g_g_to_b_bbar_H(){};
  vector<double> calculate_luminosity_for_bbbar(const double & x1,const double & x2);
  void LO(){};
  void NLO(){};
  void NNLO();
  void NNLO_gg_2_bb_convolution();
  void NNLO_convolution_gg_2_bb_bb();
  void NNLO_convolution_gg_2_bb_gb();
  void NNLO_convolution_gg_2_bb_bg();
  void NNLO_gb_conv();
  void NNLO_bg_conv();
  void NNLO_RR_gg_2_bbH_Romain();
  //: franz below
  void NNLO_RR_gg_2_bbH_Franz();
  void NNLO_RR_epsilon_expansion(
		void (*my_func)(const int&, const int&, const double&,const double&,const double&,const double&, const double&, const double&, const double&, const double&, const double&, const double&, const double&),
		int sector,double z,double lh,double wd,double L,
		double lambda1,double lambda2,double lambda3,double lambda4, double,double,double,const string&);

  void (*FRTopologies[100])(const int&, const int&, const double&,const double&,const double&,
		const double&, const double&, const double&, const double&, 
		const double&, const double&, const double&, const double&);
  int sector_init[100];
  int sector_fin[100];
  void set_topologies();
  void prepare_normalization();
  //: romain's
  std::vector<pSubF> RTopologies;
        double pref_sbb,x1LO,x2LO,measLO,zLO,x1,x2,z,lambda,meas,lh,curs,cursLO;
        double Nij,special_normalization_factor_for_double_real,Log_1mz;
        vector<double> cur_lumiLO;
        vector<double> cur_lumi;
    void (g_g_to_b_bbar_H::*pointer_to_function_for_sector)();

};

#endif


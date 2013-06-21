#ifndef QQBAR2BBBARH_H
#define QQBAR2BBBARH_H 

#include "ExclusiveClass.h"

// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"

class q_qbar_to_b_bbar_H:public ExclusiveClass
{
private:
   // q_qbar_to_b_bbar_H(){};
public: 
  q_qbar_to_b_bbar_H(const UserInterface& UI);
  ~q_qbar_to_b_bbar_H(){};
  void LO(){};
  void NLO(){};
  void NNLO();
  void NNLO_gg_2_bb_convolution();
  void NNLO_convolution_gg_2_bb_bb();
  void NNLO_convolution_gg_2_bb_gb();
  void NNLO_convolution_gg_2_bb_bg();
  void NNLO_RR_Romain();
  //: franz below
  void NNLO_RR_Franz();
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
  //: romain's
  //std::vector<Subtraction*> RTopologies;
        vector<double> cur_lumiLO;
        vector<double> cur_lumi;

};

#endif

#ifndef LUMINOSITY_NEW_H
#define LUMINOSITY_NEW_H

#include "CPDF.h"
#include "CConstants.h"
#include <utility>
#include <string>
using namespace std;

class parton_lumi
{
public:
     parton_lumi(const string & iname_,int nas_,int neps_,const string & jname_)
     :iname(iname_),jname(jname_),nas(nas_),neps(neps_){};
     ~parton_lumi(){};
private:
     string iname,jname;
     int naps,neps;
};

class LuminosityNew
{
public:
     LuminosityNew(int pole_,double NF_, double muf_, double mur_, int pert_order_, const string& provider_, bool pdf_error_)
          :pole(pole_),
          nf(NF_),
          mu_f(muf_),
          mu_r(mur_),
          perturbative_order(pert_order_),
          provider(provider_),
          pdf_error(pdf_error_)
          {};
     
     ~LuminosityNew(){};
     
          
     void add_pair(const pdf_desc & , const pdf_desc &);
     void set_cur_lumi(const double &,const double &, vector<double> &);
          
     double nf;
     double mu_f,mu_r;
     int perturbative_order;
     string provider;
     bool pdf_error;
     unsigned pdf_size() const;
     //void set_alpha_s();
     void alpha_s_at_mz(vector<double> &);
     void  evolve_alpha_s_from_mz_to_mur(vector<double> &);
	void  evolve_mb_from_mb_ref_to_mur(double &,vector<double>&);
	double  SSC(double ,double ,int );//: a_s running
	double  yukawa_b(double ,int ,double ,double  ,
                      double ); //: function for Yukawa running
     vector<double> alpha_s_at_mz_vector,alpha_s_vector,yukawa_b_vector;
     
private:
     vector< pair<parton_lumi,parton_lumi> >     pairs;
     int pole;
     //: functions
     void expand_pairs();
     
     
};


#endif






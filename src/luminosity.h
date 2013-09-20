#ifndef LUMINOSITY_H
#define LUMINOSITY_H

#include "pdf.h"
#include "constants.h"
#include <utility>
#include <string>
#include "one_d_interpolator.h"
#include "user_interface.h"
using namespace std;

class Luminosity;




class Luminosity
{
    public://data
        
        
        struct pdf_desc
        {
                pdf_desc(int i_,int j_,int n_as_, int n_eps_):i(i_),j(j_),n_as(n_as_),n_eps(n_eps_){};
                
                bool operator==(const pdf_desc& o)
                { return i==o.i && j==o.j && n_as==o.n_as && n_eps==o.n_eps; }

                int i,j,n_as,n_eps;
                
        };
    
        
        static pdf_desc F_b_00 , F_bbar_00,F_g_00,F_b_from_g_11,F_bbar_from_g_11,F_d_00,
        F_u_00,F_s_00,F_c_00,F_dbar_00,F_ubar_00,F_sbar_00,F_cbar_00,F_b_from_b_11,F_bbar_from_bbar_11,
        F_b_from_b_21,F_bbar_from_bbar_21,F_b_from_b_22,F_bbar_from_bbar_22,F_g_from_b_11,F_g_from_bbar_11,
        F_b_from_bbar_22,F_b_from_bbar_21,F_bbar_from_b_22,F_bbar_from_b_21,F_b_from_g_22,F_bbar_from_g_22,
        F_b_from_g_21,F_bbar_from_g_21,
        // f_b_from_q_21
        F_b_from_d_21,
        F_b_from_u_21,
        F_b_from_s_21,
        F_b_from_c_21,
        F_b_from_dbar_21,
        F_b_from_ubar_21,
        F_b_from_sbar_21,
        F_b_from_cbar_21,
        // f_bbar_from_q_21
        F_bbar_from_d_21,
        F_bbar_from_u_21,
        F_bbar_from_s_21,
        F_bbar_from_c_21,
        F_bbar_from_dbar_21,
        F_bbar_from_ubar_21,
        F_bbar_from_sbar_21,
        F_bbar_from_cbar_21,
        // f_b_from_q_22
        F_b_from_d_22,
        F_b_from_u_22,
        F_b_from_s_22,
        F_b_from_c_22,
        F_b_from_dbar_22,
        F_b_from_ubar_22,
        F_b_from_sbar_22,
        F_b_from_cbar_22,
        // f_b_from_q_22
        F_bbar_from_d_22,
        F_bbar_from_u_22,
        F_bbar_from_s_22,
        F_bbar_from_c_22,
        F_bbar_from_dbar_22,
        F_bbar_from_ubar_22,
        F_bbar_from_sbar_22,
        F_bbar_from_cbar_22,
        //
        F_g_from_g_11,
        F_g_from_d_11,
        F_g_from_u_11,
        F_g_from_s_11,
        F_g_from_c_11,
        F_g_from_ubar_11,
        F_g_from_dbar_11,
        F_g_from_sbar_11,
        F_g_from_cbar_11,
    F_b_11,F_b_21,F_b_22,
    F_bbar_11,F_bbar_21,F_bbar_22,
    F_g_11,F_g_21,F_g_22,
     //
     F_g_from_q_11,F_g_from_qbar_11,
     F_g_from_q_21,F_g_from_qbar_21,
     F_g_from_q_22,F_g_from_qbar_22
     ;
     
          
        double nf;
        double mu_f,mu_r;
        int perturbative_order;
        string provider;
        bool pdf_error;
    
        vector<double> alpha_s_at_mz_vector,yukawa_b_vector;
    
public://methods
    Luminosity(double NF_, double muf_, double mur_,
               int pert_order_, const string& provider_, bool pdf_error_)
                :nf(NF_),mu_f(muf_),mu_r(mur_),
                perturbative_order(pert_order_),
                provider(provider_),pdf_error(pdf_error_){};
    Luminosity(const UserInterface& UI);
    ~Luminosity(){};
    void add_pair(const pdf_desc & , const pdf_desc &);
    void set_cur_lumi(const double &,const double &, vector<double> &);
    void set_cur_lumi(const double &x1,const double &x2);
    double LL(unsigned i) {return _local_current_luminosity[i];}
    unsigned pdf_size() const;
    vector<double> give_a_s_at_mz();
    void alpha_s_at_mz(vector<double> &);
    void  evolve_alpha_s_from_mz_to_mur(vector<double> &);
	void  evolve_mb_from_mb_ref_to_mur(double &,vector<double>&);
	double  SSC(double ,double ,int );//: a_s running
	double  yukawa_b(double ,int ,double ,double  ,
                      double ); //: function for Yukawa running
    
     double  give_lumi(const double &x1,const double &x2);
     void show_necessary_pdfs(){cout<<"\n--- necessary pdfs: ";for (int i=0;i<pdfs.size();i++) cout<<"\n"<<i<<" : "<<pdfs[i].second.i
          <<"_"<<pdfs[i].second.j<<"_"<<pdfs[i].second.n_as<<"_"<<pdfs[i].second.n_eps;}
    vector<double> calculate_pdf_error(const vector<double> xx)
                        {cout<<"\nyuppieeeee "<<xx[0];return pdfs[0].first->calculate_pdf_error(xx);}
private://data
        vector< pair<CPDF*,pdf_desc> >  pdfs; 
        vector< pair<CPDF*,CPDF*> >     pairs;
    vector<double> _local_current_luminosity;
};

class InterpolatedLuminosity:public Luminosity{
public:
     InterpolatedLuminosity(double NF_, double muf_, double mur_, int pert_order_, const string& provider_, bool pdf_error_,const double & _tau)
     :Luminosity(NF_,muf_,mur_,pert_order_,provider_,pdf_error_) ,tau(_tau){};
     ~InterpolatedLuminosity(){};
     double integrate_out_x1(const double &z);
     double ff(const double& z){return integrate_out_x1(z);}

     
     double tau;
     
};

class LuminosityInterpolator: public InterpolatorBase
{
public:
     LuminosityInterpolator(InterpolatedLuminosity* _dd):InterpolatorBase(){dd=_dd;}
private:
     InterpolatedLuminosity *dd;
     double f_value(const double & x){return dd->ff(x);}
};

#endif

#ifndef PDFPAIRLIST_H
#define PDFPAIRLIST_H

typedef pair<Luminosity::pdf_desc,Luminosity::pdf_desc> pdf_pair;

class pdf_pair_list
{
public:
     pdf_pair_list(){};
     void add_pair(Luminosity::pdf_desc ll, Luminosity::pdf_desc rr){mylist.push_back(pdf_pair(ll,rr));}
     vector<pdf_pair> lumilist(){return mylist;}
     int size(){return mylist.size();}
     pdf_pair give_one_pair(int i){return mylist[i];}
private:
     vector<pdf_pair> mylist;
};
#endif







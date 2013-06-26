#ifndef GLUONFUSIONINCLUSIVE_H
#define GLUONFUSIONINCLUSIVE_H 

#include "Production.h"
//#include "GluonFusionInclusive_NLO_ME.h"
// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"
#include "Interface_to_amplitudes.h"

#include <complex>
#include "chaplin.h"


class GluonFusionInclusive;

typedef void (GluonFusionInclusive::*ptr_to_GluonFusionInclusive_function)();


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

#include "Convolutions.h" 

class InclusiveMatrixElement
{
public:
     
     InclusiveMatrixElement(const string & _pi,const string & _pj,const string& _pord,
                   const string & _name, const string & _kin,
                   const string& _str_param,ptr_to_GluonFusionInclusive_function _the_ggf_func);
     
     string name;
     int dimension;
     string parton_i;
     string parton_j;
     int alpha_power;
     ptr_to_GluonFusionInclusive_function parametrization;
     ptr_to_GluonFusionInclusive_function the_ggf_func;

     
     friend ostream& operator<<(ostream&, const InclusiveMatrixElement&);
private:
     
};

class InclusiveSector
{
public:
     InclusiveSector(const vector<ExpansionTerm*>& _factors,InclusiveMatrixElement* _ME):factors(_factors),ME(_ME){};
     InclusiveMatrixElement* ME;
     vector<ExpansionTerm*> factors;
     //FFF F1,F2;
     int alpha_power;
     friend ostream& operator<<(ostream&, const InclusiveSector&);
     string name;
     
     
//     void add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi);
//     void single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
//     void double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
     int give_pid(const string & name);
     pdf_pair_list give_list_of_pdf_pairs();
};

/*
class InclusiveSector
{
public:
     InclusiveSector(const pdf_pair_list &pdfs,
                     int in_as,int in_aew,
                     const string& plus_t,const string & calc_t,
                     int in_dim,
                     const ptr_to_GluonFusionInclusive_function ptr, 
                     const string& nm
                     )
               {
                    myptr=ptr;
                    mypdfs=pdfs;
                    myname=nm;
                    a_s_power=in_as;
                    a_ew_power=in_aew;
                    plus_type=plus_t;
                    dimension=in_dim;
                    calculation_type=calc_t;
               }
     ~InclusiveSector(){};
     int pdfsize(){return mypdfs.size();}
     ptr_to_GluonFusionInclusive_function ptr(){return myptr;}
     pdf_pair give_pdf(int i){return mypdfs.give_one_pair(i);}
     
     int dim(){return dimension;}
     string name(){return myname;}
     
private:
     ptr_to_GluonFusionInclusive_function myptr;     
     pdf_pair_list  mypdfs;
     string myname;
     int a_s_power; //: relative to tree level production that is a_s^2
     int a_ew_power;
     string plus_type;//: delta, plus or regular
     string calculation_type;//: effective vs exact
     int dimension;
};
*/


class GluonFusionInclusive:public InclusiveProduction
{
private:
//     void gg_delta_LO_effective();
     
//     void book_production_event(const double &);


//	//void set_up_X_for_quarks();	// used inside the loop, it sets Xq for every quark
//     // Xq will be used in matrix elements
//     // and it depends on the virtuality of the higgs
//     // Currently it justs depends on the nominal higgs mass
//	//:
//	
//     double pref_sgg,meas,x1LO,x2LO,measLO,zLO,x1,x2,z,lambda;
//     double tau,lh;
     
//     vector<double> cur_lumiLO;
//     vector<double> cur_lumi;
//     int sector_control;
     
//     double WC_zero_LO;
//     complex<double> sum_of_quark_triangles();
//     complex<double> higgs_born(const complex<double> &);
     
//     void  prepare_phase_space_dependent_quantities();
//     void calculate_derived_variables();
//     void calculate_wilson_coefficients();
//     void (GluonFusionInclusive::*pointer_to_function_for_sector)();
public: 
     GluonFusionInclusive();
     ~GluonFusionInclusive(){};
     //: init is necessary (pure virtual of Production from which we inherit here)
     void init(const UserInterface&,TheHatch* the_hatch);
     vector<InclusiveSector*> sectors;
     //: evaluate_sector is necessary (pure virtual of Production)
     void evaluate_sector();
     
private:
     void set_up_matrix_elements();
     void set_up_sectors();
     vector<InclusiveSector*> available_sectors;
     vector<InclusiveMatrixElement* > available_me;
     void convolute_me(InclusiveMatrixElement* cur_ME);
     
     double LL;
     
     vector<ExpansionTerm*> WCET_vector;
     
     vector<ExpansionTerm*> A_square_LOG_vector;
     vector<ExpansionTerm*> A_cube_LOG_vector;
     vector<ExpansionTerm*> A_fourth_LOG_vector;
     
     WilsonCoefficients WC;
     BetaConstants beta;
     
     void me_gg_delta_LO_effective();
     void me_gg_delta_NLO_effective();
     void me_gg_plus_NLO_effective();
     void me_gg_reg_NLO_effective();
     void me_gg_delta_NNLO_effective();
     void me_gg_plus_NNLO_effective();
     void me_gg_reg_NNLO_effective();

     void me_gq_reg_NLO_effective();
     void me_gq_reg_NNLO_effective();

     void me_qqbar_reg_NLO_effective();
     void me_qqbar_reg_NNLO_effective();

     void me_qq_reg_NNLO_effective();

     void me_q1q2_reg_NNLO_effective();

     
};










#endif

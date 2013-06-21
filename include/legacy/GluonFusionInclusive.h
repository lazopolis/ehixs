
#ifndef GLUONFUSIONINCLUSIVE_H
#define GLUONFUSIONINCLUSIVE_H 

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


class InclusiveSector
{
public:
     InclusiveSector(const pdf_pair_list &pdfs,
                     int in_as,int in_aew,
                     const string& plus_t,const string & calc_t,
                     int in_dim,
                     const SingleIntegral * ptr_to_integral, 
                     const string& nm
                     )
               {
                    current_integral=ptr_to_integral;
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
     SingleIntegral * current_integral;
     pdf_pair_list  mypdfs;
     string myname;
     int a_s_power; //: relative to tree level production that is a_s^2
     int a_ew_power;
     string plus_type;//: delta, plus or regular
     string calculation_type;//: effective vs exact
     int dimension;
};









class GluonFusionInclusive:public InclusiveProduction
{
private:
     void gg_delta_LO_effective();
     
     void book_production_event(const double &);


//	//void set_up_X_for_quarks();	// used inside the loop, it sets Xq for every quark
//     // Xq will be used in matrix elements
//     // and it depends on the virtuality of the higgs
//     // Currently it justs depends on the nominal higgs mass
//	//:
//	
     double pref_sgg,meas,x1LO,x2LO,measLO,zLO,x1,x2,z,lambda;
     double tau,lh;
     
     vector<double> cur_lumi_soft_with_LO_pdfs;
     vector<double> cur_lumi_with_LO_pdfs;
     vector<double> cur_lumi_soft_with_NLO_pdfs;
     vector<double> cur_lumi_with_NLO_pdfs;
     vector<double> cur_lumi_soft_with_NNLO_pdfs;
     vector<double> cur_lumi_with_NNLO_pdfs;
     
     int sector_control;
     
     double WC_zero_LO;
     complex<double> sum_of_quark_triangles();
     complex<double> higgs_born(const complex<double> &);
     
     void  prepare_phase_space_dependent_quantities();
     void calculate_derived_variables();
     void calculate_wilson_coefficients();
     void (GluonFusionInclusive::*pointer_to_function_for_sector)();
public: 
     GluonFusionInclusive();
     ~GluonFusionInclusive(){};
     //: init is necessary (pure virtual of Production from which we inherit here)
     void init(const UserInterface&,TheHatch* the_hatch);
     vector<InclusiveSector*> sectors;
     //: evaluate_sector is necessary (pure virtual of Production)
     void evaluate_all_components();
     
  
     
};

#endif

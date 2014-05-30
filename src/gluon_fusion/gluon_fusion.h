#ifndef GLUON_FUSION_H
#define GLUON_FUSION_H 

#include "production.h"
//#include "interface_to_amplitudes.h"

#include "chaplin.h"
#include "convolutions.h"
#include <fstream>
#include "simple_sector.h"
#include "matrix_element.h"
#include "gluon_fusion_sector_box.h"
#include "gluon_fusion_exact_coefficients.h"
#include "gluon_fusion_ew_coefficients.h"
#include "gluon_fusion_parametrization.h"
#include "gluon_fusion_event_reconstructor.h"
#include <map>
using namespace std;


class GluonFusion;

typedef void (GluonFusion::*ptr_to_GluonFusion_function)();

class GluonFusion:public Production
{
public://methods
    GluonFusion(const UserInterface & UI);
    ~GluonFusion();
    int  set_sector_control_by_name(const string & user_sector_name);
    vector<string> give_sector_names(const string & pleft,
                                     const string & pright,
                                     const string & myorder,
                                     const int & ep_power,
                                     const string & me_approx){return all_sectors->give_sector_names(pleft, pright, myorder, ep_power,me_approx);}
    

    void evaluate_sector();
    
    void SetNumberOfParticles() {event_box.SetNumberOfParticles(5);}
    void SetDecayParticleIdInEventBox(){event_box.SetDecayParticleId(5);}
    
    
    //: public to integrate with fortran Fjet
    void book_production_event(); //: public to integrate with fortran Fjet
	void book_production_event(const double &,const double &,
                               const double &,const double &,
                               const double &,const double &,
                               const double &,const double &,
                               const double &);
    //: all the functions below are public so that pointers to them can be
    //: accessed by GluonFusionSectorBox
    void LO();
    void gg_NLO_SOFT();
    void gg_NLO_HARD();
    void gg_NNLO_SOFT();
    void nlo_me();
    void NNLO_hard_no_subtraction();
    void NNLO_hard_with_subtraction();
    void NNLO_rv_with_subtraction();
    void NNLO_subtraction(const double& lambda1,const double& lambda2,
                          const double& lambda3,const double& lambda4);
    void NNLO_RR_brute_force_wrap();
    void NNLO_RR_brute_force(const double& lambda1,const double& lambda2,const double& lambda3,const double& lambda4);
    
    void LO_exact();
    void NLO_soft_exact();
    void gg_NLO_hard_exact();
    void qg_NLO_hard_exact();
    void gq_NLO_hard_exact();
    void qqbar_NLO_hard_exact();
    
//    void NLO_ewk_soft();
//    void NLO_ewk_soft_exact();
    void NLO_ewk_uubar_h_plus_jet();
    void NLO_ewk_ddbar_h_plus_jet();
    void NLO_ewk_ug_h_plus_jet();
    void NLO_ewk_dg_h_plus_jet();
    void NLO_ewk_gu_h_plus_jet();
    void NLO_ewk_gd_h_plus_jet();


    string sector_name(){return the_sector->name;}
    int number_of_necessary_sectors(){return number_of_necessary_sectors_;}
private://data
    double smax[7],smin[7];
    GluonFusionSectorBox* all_sectors;
    SimpleSector* the_sector;
    double tau,pref_sgg;
    GluonFusionParametrization ISP;
    WilsonCoefficients WC;
    BetaConstants beta;
    GluonFusionExactCoefficients * exact_coefficients;
    GluonFusionEWCoefficients* electroweak_coefficients;
    int number_of_necessary_sectors_;
    //int vegas_point_counter;
    vector<FourMomentum*> momenta_pointers_;
    
    EventReconstructor event_reconstructor;
    
    map<string,ptr_to_GluonFusion_function > ggf_func_map_;
    ptr_to_GluonFusion_function the_func_;
    ptr_to_GluonFusion_function the_parametrization_;
private://methods
    void initialize_ggf_func_map();
    void determine_ggf_func();
    void determine_parametrization();
    
    //: public because of weird structure
    void LO_parametrization_only();
    void NLO_parametrization();
    
    void check_which_sectors_can_be_run_together(const vector<SimpleSector*>&);
    bool sectors_are_compatible(SimpleSector* s1,SimpleSector* s2);
    void set_up_wilson_coefficients();
    void readjust_wilson_coefficient_for_enhanced_effective();
    void readjust_wilson_coefficient_for_electroweak_effective();
    void readjust_wilson_coefficient_for_exact();
    
    void set_up_beta_constants();
    void prepare_phase_space_dependent_quantities();
    
    void parametrization_for_LO_kinematics();
    void parametrization_for_NLO_kinematics();
    
     
    double generate_x1(double & jac_from_rap_param);
    
    void Jnlo(const double & res, const double & x1,
               const double & x2,const double &z, const double & lambda);
    void JLO(const double &);
    void find_topology(const UserInterface & );
    void allocate_luminosity();
    
    bool vars_too_close_to_edges(const double&z,
             const double&lambda1
              ,const double&lambda2
              ,const double&lambda3
              ,const double&lambda4);
    
    void push_back_event(const double & sigma);
    
    void nlo_partonic_xsections(pointer_to_Franz_gluon_fusion the_franz_function,const double &,int i);
    void rgg2ghEXACT(int pole,double s,double x1,double x2,double z,
                     double lh,double  weight,double nf, double lambda);
    void gg2gh_exact_Q_m1(const double& w,const double& z, const double& x1,
                                 const double&x2);
    void gg2gh_exact_Q_fin(const double& weight,const double& z,
                           const double& x1,const double&x2, const double& lambda);

    
    void qg2qh_exact_Q_m1(const double& weight,const double& z,
                                       const double& x1,const double&x2);
    void qg2qh_exact_Q_fin(const double& weight,const double& z,
                                        const double& x1,const double&x2,
                                        const double& lambda);
    
    void gq2qh_exact_Q_m1(const double& weight,const double& z,
                          const double& x1,const double&x2);
    void gq2qh_exact_Q_fin(const double& weight,const double& z,
                           const double& x1,const double&x2,
                           const double& lambda);
    double EwkUUbar(const double& z, const double& lambda);
    double EwkDDbar(const double& z, const double& lambda);
    double EwkUG(const double& z, const double& lambda);
    double EwkDG(const double& z, const double& lambda);
};



#endif

#ifndef GLUON_FUSION_H
#define GLUON_FUSION_H 

#include "production.h"
//#include "interface_to_amplitudes.h"
#include "fortran_interface_for_ggf_amplitudes.h"
#include "chaplin.h"
#include "convolutions.h"
#include <fstream>
using namespace std;

typedef void (*pointer_to_Franz_gluon_fusion)(  const int&,
                                                const int&,
                                                const double&,
                                                const double&,
                                                const double&,
                                                const double&,
                                                const double&,
                                                const double&,
                                                const double&,
                                                const double&,
                                                const double&,
                                                const double&,
                                                const double&,
                                                const double&);

class GluonFusion;

typedef void (GluonFusion::*ptr_to_GluonFusion_function)();


class MeExternalInfo
{
public:
    MeExternalInfo(const string & _pi,const string & _pj,const string& _pord,
                   const string & _name, int _epower,
                   const string & _me_approximation,int alpha_ew_pow=0);
public:
    string name;
    string parton_i;
    string parton_j;
    int alpha_power;
    int alpha_ew_power;
    int epsilon_power;
    string me_approximation;
};

class FranzBinder
{
public:
    FranzBinder(){_ptr = NULL; _num_sectors = 0; _is_franz = false;}
    FranzBinder(pointer_to_Franz_gluon_fusion ptr,int num_sec)
                                        :_ptr(ptr),_num_sectors(num_sec)
                                        {_is_franz = true;}
    int number_of_sectors(){return _num_sectors;}
    pointer_to_Franz_gluon_fusion func(){return _ptr;}
    bool is_franz(){return _is_franz;}
private:
    pointer_to_Franz_gluon_fusion _ptr;
    int _num_sectors;
    bool _is_franz;
};

class MatrixElement
{
public:
    MatrixElement(MeExternalInfo* info,
                  const string & _kin,
                  const string& _str_param,
                  ptr_to_GluonFusion_function _the_ggf_func,
                  FranzBinder* fr,
                  double _e_exp_in_subtr);
     
public:
    int number_of_sectors_in_this_topology(){return _franz->number_of_sectors();}
    pointer_to_Franz_gluon_fusion franz_func(){return _franz->func();}
    bool is_franz_topology(){return _franz->is_franz();}
    friend ostream& operator<<(ostream&, const MatrixElement&);
    string give_name(){ostringstream  stream;
        stream<<*this;
        return (stream.str());}
    int alpha_power()const {return _info->alpha_power;}
    int alpha_ew_power()const {return _info->alpha_ew_power;}
    int epsilon_power()const {return _info->epsilon_power;}
    string parton_i()const {return _info->parton_i;}
    string parton_j()const {return _info->parton_j;}
    string name()const {return _info->name;}
    string me_approximation(){return _info->me_approximation;}
public://data
    int dimension;
    ptr_to_GluonFusion_function parametrization;
    ptr_to_GluonFusion_function the_ggf_func;
    double epsilon_exponent_in_z_subtraction;
private:
    MeExternalInfo* _info;
    FranzBinder* _franz;


};



class SimpleSector
{
public:
    SimpleSector(const FFF& _f1,const FFF& _f2,const vector<ExpansionTerm*>& _factors,MatrixElement* _ME);
    MatrixElement* ME;
    vector<ExpansionTerm*> factors;
    FFF F1,F2;
    int alpha_power,epsilon_power;
    friend ostream& operator<<(ostream&, const SimpleSector&);
    string name;
     
    void add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi);
    void single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
    void double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
    int give_pid(const string & name);
    pdf_pair_list give_list_of_pdf_pairs();
    
    void setUpPrefactor(const double & a_s_over_pi);
    double sector_specific_prefactors_from_a_e_expansion(){return _prefactor;}
private:
    double _prefactor;
private://methods
    void uubar(pdf_pair_list& curlumi);
    void ddbar(pdf_pair_list& curlumi);

};

struct ISparams{
     double x1LO,x2LO,zLO,measLO,cursLO;
     double x1,x2,z,meas,curs,Log_1mz;
     double lambda,phi;
};


class GluonFusionMatrixElementBox
{
public:
    GluonFusionMatrixElementBox();
    int size(){return available_matrix_elements.size();}
    MatrixElement* give_me(int k){return available_matrix_elements[k];}
private://data
    vector<MatrixElement*> available_matrix_elements;
    
private://methods
    void add_gg_sectors();
    void add_qg_sectors();
    void add_gq_sectors();
    void add_qqbar_sectors();
    void add_q1q2_sectors();
    void add_qq_sectors();
    void push_me(const string & _pi,
                 const string & _pj,
                 const string& _pord,
                 const string & _name,
                 const string & _kin,
                 const string& _str_param,
                 ptr_to_GluonFusion_function _the_ggf_func,
                 int from_k,int to_k,
                 FranzBinder*,
                 const string& me_approx,
                 double eps_exp);
};


class GluonFusionSectorBox
{
public:
    GluonFusionSectorBox(const WilsonCoefficients&, const BetaConstants&,const double& log_mur_sq_over_muf_sq);
    vector<string> give_sector_names(const string & pleft,const string & pright,const string & myorder,const int & requested_epsilon_power, const string& me_approx);
    vector<SimpleSector*> give_necessary_sectors(const UserInterface & UI);
    int size(){return available_sectors.size();}
    SimpleSector* give(int i){return available_sectors[i];}
private://data
    GluonFusionMatrixElementBox* available_matrix_elements;
    vector<SimpleSector*> available_sectors;
    vector<string> _av_partons;
    WilsonCoefficients _WC;
    BetaConstants _beta;
    double _log_mur_sq_over_muf_sq;
private://methods
    void build_sectors(const string& p_left,const string & p_right);
    void build_sectors_with_fixed_a_order(int,int,int,const string& p_left,
                                          const string & p_right);
    void build_sectors_with_fixed_a_order_and_pdfs(const FFF & F1,
                                                   const FFF & F2,int Sorder);
    void build_sectors_with_fixed_a_order_e_order_and_pdfs(const FFF & F1,
                                                           const FFF & F2,
                                                           int Sorder,
                                                           int Eorder,
                                                           const vector<MatrixElement*> & matching_mes);
    vector<FFF> give_possible_F(const string & parton,int f1order);

};


class GluonFusionExactCoefficients
{
public://methods
    GluonFusionExactCoefficients(const CModel&);
    double LO_epsilon(int m){return LO_exact_coefficient[m];}
    double NLO_epsilon(int m){return NLO_soft_exact_coefficient[m];}

private://data
    vector<double> LO_exact_coefficient;
    vector<double> NLO_soft_exact_coefficient;
    CModel Model;
private://emthods
    double NLO_soft_exact_e0();
    complex<double> born_e2(complex<double> x);
    complex<double> born_e(complex<double> x);
    complex<double> born(complex<double> x);
    double LO_exact_e2();
    double LO_exact_e1();
    double LO_exact_e0();
};

struct EwData{
    EwData(const double&m,const double& w){mass=m;deltaew=w;}
    double mass;
    double deltaew;
};

class GluonFusionEWCoefficients
{
public:
    GluonFusionEWCoefficients(const CModel&);
    complex<double> LO(){return NLO_ew_coeff_;}
private:
    complex<double> NLO_ew_coeff_;
    vector<EwData*> ew_data;
private://methods
    vector<double> givecoeff(double x[3],double y[3]);

};



class GluonFusionEvent : public Event
{
public:
    GluonFusionEvent(const double& weight,FourMomentum* p1,FourMomentum* p2,
                     FourMomentum* p3,FourMomentum* p4, FourMomentum* pH)
    :Event(weight){p1_=*p1;p2_=*p2;p3_=*p3;p4_=*p4;pH_=*pH;}
    //~GluonFusionEvent(){cout<<"\nevent destroyed";}
    FourMomentum* p1(){return &p1_;}
    FourMomentum* p2(){return &p2_;}
    FourMomentum* p3(){return &p3_;}
    FourMomentum* p4(){return &p4_;}
    FourMomentum* pH(){return &pH_;}
    FourMomentum* DecayParticleFourMomentum(){return &pH_;}
private:
    FourMomentum p1_;
    FourMomentum p2_;
    FourMomentum p3_;
    FourMomentum p4_;
    FourMomentum pH_;
    
};



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
    
    //: public because of weird structure
    void LO_parametrization_only();
    void NLO_parametrization();
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
    ISparams ISP;
    WilsonCoefficients WC;
    BetaConstants beta;
    GluonFusionExactCoefficients * exact_coefficients;
    GluonFusionEWCoefficients* electroweak_coefficients;
    int number_of_necessary_sectors_;
    //int vegas_point_counter;
    vector<FourMomentum*> momenta_pointers_;
private://methods
    void update_smaxmin(int,double);
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
    void  NNLO_event_kinematics(const double& sigma,
							const double & x1,
							const double & x2,
							const double & z,
							const double & s13,
							const double & s23,
							const double & s14,
							const double & s24,
							const double & s34);
    void LO_event_kinematics(const double& sigma,const double & x1,const double & x2);
    void NLO_event_kinematics(const double& sigma,const double & x1,
                              const double & x2,
                              const double & z,
                              const double & s13,
                              const double & s23);
    void Jnlo(const double & res, const double & x1,
               const double & x2,const double &z, const double & lambda);
    void JLO(const double &);
    void find_topology(const UserInterface & );
    void allocate_luminosity();
    
    bool vars_too_close_to_edges(const double&z,const double&lambda1
                                              ,const double&lambda2
                                              ,const double&lambda3
                                              ,const double&lambda4);
    
    void push_back_event(const double & sigma);
    void writeEventToFile(const double &,const double &,
                          const double &,const double &);
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

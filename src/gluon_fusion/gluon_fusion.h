#ifndef GLUON_FUSION_H
#define GLUON_FUSION_H 

#include "Production.h"
#include "Interface_to_amplitudes.h"
#include "fortran_interface_for_ggf_amplitudes.h"
#include "chaplin.h"
#include "Convolutions.h"








typedef void (*pointer_to_Franz_gluon_fusion)(const int&, const int&, 
                                 const double&,const double&,const double&,const double&, 
                                 const double&,const double&,const double&,const double&, 
                                 const double&, const double&,const double&,const double&);

class GluonFusion;

typedef void (GluonFusion::*ptr_to_GluonFusion_function)();



class MatrixElement
{
public:

     MatrixElement(const string & _pi,const string & _pj,const string& _pord,
                   const string & _name, const string & _kin,int _epower,
                   const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,
                   pointer_to_Franz_gluon_fusion  ptr_to_fr, int num_topologies,double _e_exp_in_subtr,const string & _me_approximation);

     string name;
     int dimension;
     string parton_i;
     string parton_j;
     int alpha_power;
     int epsilon_power;
     ptr_to_GluonFusion_function parametrization;
     ptr_to_GluonFusion_function the_ggf_func;
     bool is_franz_topology;
     pointer_to_Franz_gluon_fusion  franz_func;
     int  number_of_sectors_in_this_topology;
     double epsilon_exponent_in_z_subtraction;
     string me_approximation;
     friend ostream& operator<<(ostream&, const MatrixElement&);
     string give_name(){ostringstream  stream;
          stream<<*this;
          return (stream.str());}
private:
     
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
};

struct ISparams{
     double x1LO,x2LO,zLO,measLO,cursLO;
     double x1,x2,z,meas,curs,Log_1mz;
     double lambda,phi;
};





struct SectorData{
     
};

class ConsolidatedSector
{
public:
     ConsolidatedSector(SimpleSector* first_sector){sectors.push_back(first_sector);}
     bool has_same_me_as(SimpleSector* new_sector);
     vector<SimpleSector*> sectors;
     friend ostream& operator<<(ostream&, const ConsolidatedSector&);
};



class GluonFusion:public Production
{
private:
     void set_up_sectors(const UserInterface& UI);
     void consolidate_sectors();

     void add_gg_sectors();
     
     void add_qg_sectors();
     void add_gq_sectors();
     
     void add_qqbar_sectors();
     
     void add_q1q2_sectors();
     
     void add_qq_sectors();
     

     void calculate_derived_variables(const UserInterface& UI);
     
     
     
    // bool bjorken_x_out_of_range(const double & x1,const double & x2);
     double generate_x1(double & jac_from_rap_param);
     
     
     void  set_up_event_kinematics(
							const double & x1,
							const double & x2,
							const double & z,
							const double & s13,
							const double & s23,
							const double & s14,
							const double & s24,
							const double & s34);
     void LO_event_kinematics(const double & x1,const double & x2);
     void NLO_event_kinematics(const double & x1,
                              const double & x2,
                              const double & z,
                              const double & s13,
                              const double & s23);
     void Jnlo(const double & res, const double & x1, const double & x2,const double &z, const double & lambda);
     void JLO(const double &);
     
     void find_topology(const UserInterface & );
     void allocate_luminosity();
     //:data
     double smax[7],smin[7];
     void update_smaxmin(int,double);
     vector<MatrixElement*> available_matrix_elements;

     SimpleSector* the_sector;
     
     vector<SimpleSector*> available_sectors;
     
     vector<ConsolidatedSector*> all_cons_sectors;
     
     double tau,pref_sgg,lh,sector_specific_prefactors_from_a_e_expansion;
     ISparams ISP;
     double Nij_bbbar,Nij_bg,Nij_bq;
     

     WilsonCoefficients WC;
     BetaConstants beta;
     
     void build_sectors(const string& p_left,const string & p_right);
     void build_sectors_with_fixed_a_order(int,int,int,const string& p_left,const string & p_right);
     void build_sectors_with_fixed_a_order_and_pdfs(const FFF & F1,const FFF & F2,int Sorder);
     void build_sectors_with_fixed_a_order_e_order_and_pdfs(const FFF & F1,const FFF & F2,int Sorder,int Eorder,const vector<MatrixElement*> & matching_mes);

     
     vector<string> av_partons;
     vector<FFF> give_possible_F(const string & parton,int f1order);
     
     //Cluster the_cluster;

public: 
     GluonFusion();
     ~GluonFusion();
     
     void init(const UserInterface&,TheHatch* the_hatch);
     int  set_sector_control_by_name(const string & user_sector_name);
     vector<string> give_sector_names(const string & pleft,const string & pright,const string & myorder,const int &);

     vector<string> give_sector_names(const string & pleft,const string & pright,const string & myorder,const int & requested_epsilon_power,const string & me_approx);
     void check_which_sectors_can_be_run_together(const vector<SimpleSector*>&);
     vector<SimpleSector*> give_necessary_sectors(const UserInterface & UI);
     bool sectors_are_compatible(SimpleSector* s1,SimpleSector* s2);

     void evaluate_sector();

     void set_up_wilson_coefficients();
     void set_up_beta_constants();
     

     void clear_previously_allocated_events_and_free_memory();
     void prepare_phase_space_dependent_quantities();
     
     void parametrization_for_LO_kinematics();
     void parametrization_for_NLO_kinematics();
     void LO_parametrization_only();
     void NLO_parametrization();
     
     vector<double> cur_lumiLO;
     vector<double> cur_lumi;
     
     void (GluonFusion::*pointer_to_function_for_sector)();
     void (GluonFusion::*pointer_to_function_for_parametrization)();
     pointer_to_Franz_gluon_fusion myFR;
     vector<int> my_f_sectors;
     
     void book_production_event(); //: public to integrate with fortran Fjet
	void book_production_event(const double &,const double &,
                                const double &,const double &,
                                const double &,const double &,
                                const double &,const double &,
                                const double &);//: public to integrate with fortran Fjet
     void push_back_event(const double & sigma);

     
     void push_me(const string & _pi,const string & _pj,const string& _pord,
                               const string & _name, const string & _kin,
                               const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k,
                               pointer_to_Franz_gluon_fusion  ptr_to_fr[], int num_topologies,int num_sect[]);
     void push_me(const string & _pi,const string & _pj,const string& _pord,
                               const string & _name, const string & _kin,
                               const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k,
                               pointer_to_Franz_gluon_fusion  ptr_to_fr, int num_topologies);
     void push_me(const string & _pi,const string & _pj,const string& _pord,
                               const string & _name, const string & _kin,
                               const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k);
     void push_me(const string & _pi,const string & _pj,const string& _pord,
                               const string & _name, const string & _kin,
                               const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k,
                               pointer_to_Franz_gluon_fusion  ptr_to_fr[], int num_topologies,int num_sect[],double eps_exp);
     void push_me(const string & _pi,const string & _pj,const string& _pord,
                               const string & _name, const string & _kin,
                               const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k,const string & me_approx);
     void LO();
     void gg_NLO_SOFT();
     void gg_NLO_HARD();
     void gg_NNLO_SOFT();
     
     void nlo_partonic_xsections(pointer_to_Franz_gluon_fusion the_franz_function,const double &,int i);
     void nlo_me();

     void NNLO_hard_no_subtraction();
     void NNLO_hard_with_subtraction();
     void NNLO_rv_with_subtraction();
     void NNLO_subtraction(const double& lambda1,const double& lambda2,const double& lambda3,const double& lambda4);

     void LO_exact();
     void calculate_exact_born_me_LO();
     double LO_exact_e0();
     double LO_exact_e1();
     double LO_exact_e2();

     
     complex<double> born(complex<double> x);
     complex<double> born_e(complex<double> x);
     complex<double> born_e2(complex<double> x);
     vector<double> LO_exact_coefficient;
     
     void NLO_soft_exact();
     vector<double> NLO_soft_exact_coefficient;
     double NLO_soft_exact_e0();
     complex<double> virtual_0(complex<double> x);
     
     void gg_NLO_hard_exact();
     void rgg2ghEXACT(int pole,double s,double x1,double x2,double z,double lh,double  weight,double nf, double lambda);
     double abs_sq_of_sum_over_quarks_of(complex<double> (*f)(const double& z, const double& lambda,  const complex<double>& M,const double& QQQ),const double & z,const double & lambda);

     
};



#endif

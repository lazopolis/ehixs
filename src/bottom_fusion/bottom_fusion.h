#ifndef BOTTOM_FUSION_H
#define BOTTOM_FUSION_H 

#include "production.h"
//#include "BottomFusion_NLO_ME.h"
// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"
#include "interface_to_amplitudes.h"
#include "sector.h"


typedef void (*pointer_to_Franz)(const int&, const int&, 
                                 const double&,const double&,const double&,const double&, 
                                 const double&,const double&,const double&,const double&, 
                                 const double&, const double&,const double&);

class BottomFusion;

typedef void (BottomFusion::*ptr_to_BottomFusion_function)();




class Sector
{
public:
     Sector(const pdf_pair_list &pdfs,const ptr_to_BottomFusion_function ptr, int dim_int,string nm)
     {my_dim_int=dim_int;myptr=ptr;mypdfs=pdfs;myname=nm;}
     Sector(const pdf_pair_list &pdfs,const ptr_to_BottomFusion_function ptr, int dim_int,pointer_to_Franz FR,int si,int sf,string nm)
     {my_dim_int=dim_int;myptr=ptr;mypdfs=pdfs;myFR=FR;sec_in=si;sec_fi=sf;myname=nm;}
     ~Sector(){};
     int dim(){return my_dim_int;}
     int pdfsize(){return mypdfs.size();}
     ptr_to_BottomFusion_function ptr(){return myptr;}
     pdf_pair  give_pdf(int i){return mypdfs.give_one_pair(i);}
     int sec_init(){return sec_in;}
     int sec_fin(){return sec_fi;}
     pointer_to_Franz give_FR(){return myFR;}
     string name(){return myname;};
     
     
private:
     ptr_to_BottomFusion_function myptr;
     pointer_to_Franz myFR;
     
     int sec_in;
     int sec_fi;
     pdf_pair_list  mypdfs;
     int my_dim_int;
     string myname;
};



class BottomFusion:public Production
{
    private:
        // b_g_to_H_X(){};
        //BottomFusion_NLO_ME NLO_ME;
     void add_bq_sectors();
     void add_qb_RR();
     void add_bq_RR();
     void add_bq_conv_bg();
     void add_bq_conv_gb();
     void add_bq_conv_bb_e2();
     void add_bq_conv_bb_e1();
     void NNLO_RR_Franz_bq();
     //: b bbar
     void add_bbbar_sectors();
     void add_bbbar_LO_NLO_RV_RR();
     void add_bbbar_conv_fb0_fbb1();
     void add_bbbar_conv_fb1_fbb1();
     void add_bbbar_conv_fb22_fbb0();
     void add_bbbar_conv_fb21_fbb0();
     void add_bbbar_conv_fgb21_fbb0();
     void add_bbbar_conv_fb00_fgb11();
     //: bg
     void add_bg_sectors();
     void add_bg_NLO_RV_RR();
     void add_gb_NLO_RV_RR();
     void add_bg_conv_fg0_fb11();
     void add_bg_conv_fb11_fg0();
     void add_bg_conv_fb0_fbbarfromg11();
     void add_bg_conv_fbbarfromg11_fb0();
     void add_bg_conv_fb0_fbbarfromg11_for_nlo();
     void add_bg_conv_fb11_fbbarfromg11();
     void add_bg_conv_fb0_fbbarfromg21();
     void add_bg_conv_fb0_fbbarfromg22();
     
     void calculate_derived_variables();
     void Jnlo(const double & res, const double & x1, const double & x2,const double &z, const double & lambda);
	//void set_up_X_for_quarks();	// used inside the loop, it sets Xq for every quark
     // Xq will be used in matrix elements
     // and it depends on the virtuality of the higgs
     // Currently it justs depends on the nominal higgs mass
	//:
	
	void  set_up_event_kinematics(
							const double & x1,
							const double & x2,
							const double & z,
							const double & s13,
							const double & s23,
							const double & s14,
							const double & s24,
							const double & s34);
     //: 
     double generate_x1(double &);
     int pole;
     int sector_control;
     double tau;
    public: 
     BottomFusion();
     ~BottomFusion(){};

     void init(const UserInterface&,TheHatch* the_hatch);
     vector<Sector*> sectors;
     void evaluate_sector();
      
     void prepare_phase_space_dependent_quantities();
     
     void parametrization_for_LO_kinematics(double & x1, double & x2, double & z, double & meas);
     void parametrization_for_NLO_kinematics(double & x1, double & x2, double & z, double & meas);
     bool bjorken_x_out_of_range(const double & x1,const double & x2);
     
     vector<double> cur_lumiLO;
     vector<double> cur_lumi;
     
     void (BottomFusion::*pointer_to_function_for_sector)();
     
     void book_production_event(); //: public to integrate with fortran Fjet
	void book_production_event(const double &,const double &,
                                const double &,const double &,
                                const double &,const double &,
                                const double &,const double &,
                                const double &);//: public to integrate with fortran Fjet
     
     
     void bbdeltaLO();
     void NLO_SOFT();
     void NLO_HARD();
     void NLO_conv();

     void NNLO_SOFT();
     
     void NNLO_RV_Franz_A1();
     void NNLO_RV_Franz_A2();
     void NNLO_RV_Franz_A3();
     void NNLO_RV_Franz_A4();
     
     void NNLO_RR_Franz();
     
     void NNLO_conv_bb2gH_hard();
     void NNLO_conv_bb2gH_soft();
     //:
     void NNLO_conv_RRa();
     void NNLO_conv_RRb();
     void NNLO_conv_RRc();
     void NNLO_conv_RRd();
     void NNLO_conv_RRe();

     void NNLO_RR_Franz_bg();
     void NLO_HARD_1();
     void NLO_HARD_2();
     void NLO_conv_1();
     void NLO_conv_2();
     
     void NNLO_RV_gb();
     void NNLO_RV_bg();
     void NNLO_renorm_gb();
     void NNLO_renorm_bg();
     
     void NNLO_conv_bb_nlo_soft();
     void NNLO_conv_bb_nlo_hard();
     void NNLO_conv_bb_LO_e2();
     void NNLO_conv_bb_LO_e1();
     void NNLO_conv_bg();
     void NNLO_conv_gb();
     
     double  bb_soft_pole(const double & delta,const double &  plus);
     double  bb_soft_finite(const double &  delta,const double &  plus0,const double &  plus1);
     double  bb_soft_e(const double &  delta,const double &  plus0,const double &  plus1,const double &  plus2);
     
     double  bb_coll_pole();
     double  bb_coll_fin();
     double  bb_coll_e();
     double  bb_coll_soft_pole();
     double  bb_coll_soft_fin();
     double  bb_coll_soft_e();
     double  bb_hard_fin();
     double  bb_hard_e();
     
     double  bg_coll_pole();
     double  bg_coll_fin();
     double  bg_coll_e();
     double  bg_hard_fin();
     double  bg_hard_e();
     
     double  gb_coll_pole();
     double  gb_coll_fin();
     double  gb_coll_e();
     double  gb_hard_fin();
     double  gb_hard_e();

     
     
     
     
     
     
     void NNLO_RR_epsilon_expansion_nf(double,
                                       void (*my_func)(const int&, const int&, const double&,const double&,const double&,
                                                       const double&, const double&, const double&, const double&, const double&, const double&),
                                       int sector,double z,double lh,double wd,double L,
                                       double nf,double lambda1,double s,double x1,double x2,const string& number_of_II_gluons);
     void NNLO_RR_epsilon_expansion(
                                    pointer_to_Franz,
                                    int sector,double z,double lh,double wd,double L,
                                    double lambda1,double lambda2,double lambda3,double lambda4, double,double,double,const string&);
     void NNLO_RR_epsilon_expansion(double epsilon_coeff,
                                    pointer_to_Franz,
                                    int sector,double z,double lh,double wd,double L,
                                    double lambda1,double lambda2,double lambda3,double lambda4,double s,double x1,double x2,const string& number_of_II_gluons);
     
     double pref_sbb,x1LO,x2LO,measLO,zLO,x1,x2,z,lambda,meas,Log_1mz,lh,curs,cursLO,special_normalization_factor_for_double_real;
     double Nij_bbbar,Nij_bg,Nij_bq;


};

#endif

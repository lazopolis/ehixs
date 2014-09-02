


#ifndef PRODUCTION_H
#define PRODUCTION_H

#include <iostream>
using namespace std;


#include "model.h"
//#include "chaplin.h"
//#include "fvector.h"
//#include "CConstants.h"
#include "cut.h"
#include "interface_to_amplitudes.h"
#include "user_interface.h"
#include "luminosity.h"
#include "hub.hpp"
//#include "Decay.h"
//#include "VegasAdaptor.h"

#include "momenta.h"
#include "event.h"
#include "cross_section.h"

///@class Production
class Production
{
public://methods
    ~Production();
	void Configure(const UserInterface& UI);
    void perform();

    bool is_sector_defined() const
    {
        return sector_defined;
    }
    void set_up_the_hatch(TheHatch*);
    bool this_event_passes_cuts(int i)
    {return cuts_->passes_cuts(event_box.ptr_to_event(i));}
    void show_cut_info_and_exit(){cuts_->show_cut_info_and_exit();}
    
    //: pure virtual functions
    
    virtual void SetNumberOfParticles()=0;
    virtual void SetDecayParticleIdInEventBox()=0;
    //virtual int number_of_necessary_sectors() = 0;
    
    virtual void create_matrix_elements()=0;
    virtual void ConfigureCuts()=0;
    virtual void SetProcessSpecificParameters()=0;
    
    /*
    virtual void book_production_event(const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                                       const double &)=0;//: public to integrate with fortran Fjet
    */
    
    //double y_b(){return 0.0;}//this should be corrected!!
    /*
     virtual vector<string> give_sector_names(const string & pleft,
                                             const string & pright,
                                             const string & myorder,
                                             const int &,const string &)=0;
     */
    
    void evaluate_sector();
    int dimension_of_integration();
    double alpha_s_at_mz_from_lhapdfs();
    void info();
    void xml_info(const char* output_fname);

public:// data
	
    EventBox event_box;
    //: Model is public because Process needs to sync a_s
    //: between production and decay
    CModel Model;
protected:// data
    double* xx_vegas;
    CutBox* cuts_;
    bool sector_defined;
protected://data
    vector<CrossSection*> available_xs_;
    CrossSection* the_xs_;
protected://methods
    void find_the_xs(const UserInterface& UI);
    
    
    
    
    
    
    
//    //: kinematic variables
//    double Etot;
//    //: scales
//	double mu_f;
//	double mu_r;
//	//: logs
//	double log_muf_sq_over_mh_sq;
//	double log_mur_sq_over_mh_sq;
//	double log_muf_sq_over_mt_sq;
//	double log_mur_sq_over_muf_sq;
//    double log_one_minus_tau;
//    
//    int dim_of_integration;
//    string my_sector_name;//: similar to dim of integration
//	double ptbuf; //: set in constructor to almost zero, used in kinematics
};


/*
class ProductionMockUp:public Production
{
public: 
     void init(const UserInterface & UI,TheHatch*);
     void evaluate_sector();
     vector<string> give_sector_names(const string & pleft,const string & pright,const string & myorder);
     
private:
     
};





class InclusiveProduction
{
public:
	InclusiveProduction();
	~InclusiveProduction(){};
     virtual void init(const UserInterface & UI,TheHatch*)=0;
     
	void  perform();
     double* xx_vegas;
     int dimension_of_integration(){return dim_of_integration;}
     string sector_name(){return my_sector_name;}
     //: functions
     virtual void evaluate_sector()=0; // overloaded in daughter class, performs the event generation
     virtual void book_production_event(const double &,const double &,
                                        const double &,const double &,
                                        const double &,const double &,
                                        const double &,const double &,
                                        const double &)=0;//: public to integrate with fortran Fjet
     
     
     vector<double> alpha_s_vec(){return alpha_s_vector;}
     vector<double> y_b_vec(){return yukawa_b_vector;}
     virtual vector<string> give_sector_names(const string & pleft,const string & pright,const string & myorder,const int &,const string &)=0;
     bool is_sector_defined(){return sector_defined;}
     
     double weight;
     
protected:
     void init_base(const UserInterface &, TheHatch*);//called by daughter init(UI), sets mH, evolves couplings, sets mu_f,mu_r, calculates logs, sets Etot
     //: data
     CModel Model;
     Luminosity *lumi;
     vector<double> alpha_s_vector;
	vector<double> alpha_s_at_mz_vector;
	vector<double> yukawa_b_vector;
     
     bool sector_defined;
     
     //: kinematic variables
	
	double Etot;
	//double shat;
     //: scales
	double mu_f;
	double mu_r;
	//: logs, all set inside Evaluate_integral, because the virtuality of the Higgs is only
	//: known therein.
	double log_muf_sq_over_mh_sq;
	double log_mur_sq_over_mh_sq;
	double log_muf_sq_over_mt_sq;
	double log_mur_sq_over_muf_sq;
     double log_one_minus_tau;
     
     int dim_of_integration;
     string my_sector_name;//: similar to dim of integration
     
	double ptbuf; //: set in constructor to almost zero, used in kinematics
	
	
     
};







*/










#endif

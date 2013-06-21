
#ifndef PRODUCTION_H
#define PRODUCTION_H

#include <iostream>
using namespace std;


#include "Model.h"
//#include "chaplin.h"
//#include "fvector.h"
//#include "CConstants.h"
#include "CCut.h"
//#include "Interface_to_amplitudes.h"
#include "UserInterface.h"
#include "luminosity.h"
#include "hub.hpp"
//#include "Decay.h"
//#include "VegasAdaptor.h"

#include "Momenta.h"

class InclusiveProduction
{
public:
	InclusiveProduction();
	~InclusiveProduction(){};
    
    //: functions
    virtual void init(const UserInterface & UI,TheHatch*)=0;
    void  perform();
    int dimension_of_integration(){return dim_of_integration;}
    string sector_name(){return my_sector_name;}
    virtual void evaluate_all_components()=0; // overloaded in daughter class, performs the event generation
    vector<double> alpha_s_vec(){return alpha_s_vector;}
    vector<double> y_b_vec(){return yukawa_b_vector;}
    //: data
    double* xx_vegas;
    vector<Event*> production_events;
    
protected:
     //:functions
     void init_base(const UserInterface &, TheHatch*);//called by daughter init(UI), sets mH, evolves couplings, sets mu_f,mu_r, calculates logs, sets Etot
    
     //: data
     CModel Model;
     Luminosity *lumi_with_LO_pdfs;
     Luminosity *lumi_with_NLO_pdfs;
     Luminosity *lumi_with_NNLO_pdfs;
     vector<double> alpha_s_vector;
     vector<double> alpha_s_at_mz_vector;
	vector<double> yukawa_b_vector;
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
	double log_mur_sq_over_mt_sq;
	double log_mur_sq_over_muf_sq;
     double log_one_minus_tau;
     //: communication with daughter: dim_int is decided by sector that belongs to daughter class.
     //: dim_of_integration holds the value for base class, to be sent to Process through public function
     //: this construction sounds a bit stupid.
     int dim_of_integration;
     string my_sector_name;//: similar to dim of integration
	//: technical cut variables
	double ptbuf; //: set in constructor to almost zero, used in kinematics
	
	
	
	//int counter_of_wrong_events;
	//double trimmed(const double& );
	
	//time_t start_of_program,time_since_prev_iteration;
     //double total_intended_time_for_run,time_needed_for_last_iteration;
    
};
#endif







#ifndef PRODUCTION_H
#define PRODUCTION_H

#include <iostream>
using namespace std;


#include "Model.h"
//#include "chaplin.h"
//#include "fvector.h"
//#include "CConstants.h"
#include "CCut.h"
#include "Interface_to_amplitudes.h"
#include "UserInterface.h"
#include "luminosity.h"
#include "hub.hpp"
//#include "Decay.h"
//#include "VegasAdaptor.h"

#include "Momenta.h"

class Production
{
public:
	Production();
	~Production(){};
     virtual void init(const UserInterface & UI,TheHatch*)=0;
	Momenta all_momenta;
     
	void  perform();
     double* xx_vegas;
     //bool bjorken_x_out_of_range(const double & x1,const double & x2);
     vector<Event*> production_events;
     //vector<SmallEvent*> short_events;
     int dimension_of_integration(){return dim_of_integration;}
     string sector_name(){return my_sector_name;}
     //: functions
     virtual void evaluate_sector()=0; // overloaded in daughter class, performs the event generation
     virtual void book_production_event(const double &,const double &,
                                const double &,const double &,
                                const double &,const double &,
                                const double &,const double &,
                                const double &)=0;//: public to integrate with fortran Fjet
     

     //vector<double> alpha_s_vec(){return alpha_s_vector;}
     vector<double> y_b_vec(){return yukawa_b_vector;}
     virtual vector<string> give_sector_names(const string & pleft,const string & pright,const string & myorder,const int &,const string &)=0;
     bool is_sector_defined(){return sector_defined;}

     CModel Model;
protected:
      void init_base(const UserInterface &, TheHatch*);//called by daughter init(UI), sets mH, evolves couplings, sets mu_f,mu_r, calculates logs, sets Etot
     //: data 
     
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


















#endif

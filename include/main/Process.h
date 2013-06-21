#ifndef PROCESS_H
#define PROCESS_H


//#include "CConstants.h"

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <vector>
#include <math.h>
#include <cassert>
#include <complex>
#include <cstdlib>
#include <time.h>

using namespace std;

// ==================================================
// First includes
//#include "CPDF.h"
//#include "CMonteCarlo.h"
#include "Model.h"
#include "chaplin.h"
#include "fvector.h"
//#include "CConstants.h"
#include "CCut.h"

//#include "Interface_to_amplitudes.h"
#include "UserInterface.h"
//#include "luminosity.h"
#include "hub.hpp"
#include "Decay.h"
#include "Production.h"
#include "VegasAdaptor.h"
// ==================================================
// Forward declarations
class CHistogram;
class CHistogram2d;
class AverageObservable;

#include "Momenta.h"






class Process
{
public:
     Process(const UserInterface& UI);
     
     void  perform();
     
     void  Evaluate_integral(const double xx[]);//: public because it has to be accessed by Integrand
	void set_production(Production * theproduction);
     void set_decay(Decay * thedecay);
     VegasAdaptor Vegas;
     
     double total_xs(){return Vegas.vegas_integral_output[0];}
     double total_err(){return Vegas.vegas_error_output[0];}
     string sector_info();
     
     vector<CHistogram*> histogram_vector; //: public so that histograms from different sectors can be compared
     vector<string> give_sector_names(const string & pleft,const string & pright,const string & myorder,const int&,const string & );
protected:
     UserInterface my_UI;
     TheHatch the_hatch;
     void  book_histograms( Event *);
     void book_null_event();
     void book_event(Event *);
     void proceed_to_production_phase();
     void proceed_to_decay_phase(Event*);
     void perform_decay_alone();
     void book_decay_event(Event *);
     void print_output_intermediate();
     void  calculate_number_of_components();
     void  print_output();
     string input_filename,output_filename;
	//
	bool sectors_are_defined_in_production_and_decay();

	//
	
	//:
     int perturbative_order;
	int dimension_of_integration;
	void calculate_dimension_of_integration();
     //: histograms
     void  setup_histograms();
     void update_histograms_end_of_iteration(int NOP);
	void update_histograms_end_of_vegas_point();
     vector<CHistogram*> available_histograms;
     //vector<CHistogram2d*>  histogram2d_vector;
     //vector<AverageObservable*> average_observable_vector;
     
     //: cuts 
     void  setup_cuts();
	vector<CCut*> cuts;
     vector<CCut*> available_cuts;
     
     bool passes_cuts(Event* the_event);
     
     Decay *my_decay;
     Production* my_production;
     bool production_is_defined;
     bool decay_is_defined;
     //: event printing
     fstream my_event_stream;
     void open_event_filename();
     void close_event_filename();
     void print_event(Event*);
     //Cluster the_cluster;
     //
     vector<double>  vegas_variables_for_histograming_use;
};


class InclusiveProcess
{
public:
     InclusiveProcess(const UserInterface& UI);
     void set_production(InclusiveProduction * theproduction);
     void  perform();
     VegasAdaptor Vegas;
     void Evaluate_integral(const double xx[]);

private:
     TheHatch the_hatch;
     InclusiveProduction* my_production;
     bool production_is_defined;
     UserInterface my_UI;
     string input_filename,output_filename;

     
     bool sectors_are_defined_in_production();
     void  calculate_number_of_components();
     void print_output();
     void proceed_to_production_phase();
     void proceed_to_decay_phase(const double &w);
     void perform_decay_alone();
     void book_event(const double & w);

};





// ==================================================
// Last includes
#include "CBin.h"
#include "CHistogram.h"

#endif

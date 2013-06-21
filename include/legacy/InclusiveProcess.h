
#ifndef INCL_PROCESS_H
#define INCL_PROCESS_H

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


//#include "Interface_to_amplitudes.h"
#include "UserInterface.h"
//#include "luminosity.h"
#include "hub.hpp"
#include "VegasAdaptor.h"

class InclusiveProcess
{
public:
    InclusiveProcess(const UserInterface& UI);
    
    void  perform();
    
    void  Evaluate_integral(const double xx[]);//: public because it has to be accessed by Integrand
	void  set_production(InclusiveProduction * theproduction);
    VegasAdaptor Vegas;
    
    //double total_xs(){return Vegas.vegas_integral_output[0];}
    
    //string sector_info(){return my_production->sector_name()+" :: "+my_decay->sector_name();}
	
private:
    UserInterface my_UI;
    TheHatch the_hatch;
    //void  book_histograms( Event *);
    //void book_null_event();
    void book_event(Event *);
    void proceed_to_production_phase();
    void proceed_to_decay_phase(Event*);
    //void perform_decay_alone();
    //void book_decay_event(Event *);
    //void print_output_intermediate();
    void  calculate_number_of_components();
    void  print_output();
    string input_filename,output_filename;
	//
	
	//
	
	//:
    int perturbative_order;
	int dimension_of_integration;
	void calculate_dimension_of_integration();
    
    InclusiveProduction* my_production;
    bool production_is_defined;
    
};

#endif




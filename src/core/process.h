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
#include "timekeeper.h"

using namespace std;

// ==================================================
// First includes
//#include "CPDF.h"
//#include "CMonteCarlo.h"
#include "model.h"
#include "chaplin.h"
#include "fvector.h"
//#include "CConstants.h"
#include "cut.h"

//#include "Interface_to_amplitudes.h"
#include "user_interface.h"
//#include "luminosity.h"
#include "hub.hpp"
#include "decay.h"
#include "decay_wwzz.h"
#include "decay_h_to_gamma_gamma.h"
#include "decay_z_gamma.h"
#include "production.h"
#include "vegas_adaptor.h"
// ==================================================
// Forward declarations
class CHistogram;
class CHistogram2d;
class AverageObservable;

#include "momenta.h"

class HistogramBox
{
public://methods
    HistogramBox(const UserInterface & UI);
    void show_histogram_info_and_exit();
    void  book_histograms(const CombinedEvent&,const double & vegas_weight);
    void update_histograms_end_of_iteration(int NOP);
	void update_histograms_end_of_vegas_point();
    void print_histograms();
    string print_histograms_to_string();
    void write_to_histogram_file();
    CHistogram* ptr_to_histogram_with_id(unsigned m){return histogram_vector[m];}
    int size(){return histogram_vector.size();}

private://data
    vector<CHistogram*> histogram_vector;
    vector<CHistogram*> available_histograms;
    

private://methods
};



class Process
{
public://methods
    Process(const UserInterface& UI);
    void  perform();
    //: public because it has to be accessed by Integrand
    void  Evaluate_integral(const double xx[]);
	
    double total_xs(){return Vegas.vegas_integral_output[0];}
    double total_err(){return Vegas.vegas_error_output[0];}
    string sector_info();
    vector<string> give_sector_names(const string & pleft,
                                     const string & pright,
                                     const string & myorder,
                                     const int&,const string & );
    int number_of_active_histograms(){return _histograms->size();}
    CHistogram* ptr_to_histogram_with_id(unsigned m)
                    {return _histograms->ptr_to_histogram_with_id(m);}
    int number_of_necessary_sectors(){return my_production->number_of_necessary_sectors();}
    string sector_name(){return my_production->sector_name();}
public://data
    VegasAdaptor Vegas;
    //: public so that histograms from different sectors can be compared
    
private://data
    UserInterface my_UI;
    TheHatch the_hatch;
    HistogramBox* _histograms;
    
    Decay *my_decay;
    Production* my_production;
    ofstream my_event_stream;
    TimeKeeper myclock_;
    
    int decay_particle_id_;
    
    bool final_iteration_;
    
private://methods
    void set_production(Production * theproduction);
    void set_decay(Decay * thedecay);
    void book_null_event();
    void book_event(const CombinedEvent&);
    void proceed_to_production_phase();
    void proceed_to_decay_phase(Event*);
    void perform_decay_alone();
    void book_decay_event(Event *);
    void print_output_intermediate();
    void calculate_number_of_components();
    void print_output();
	bool sectors_are_defined_in_production_and_decay();
	void calculate_dimension_of_integration();
    bool production_is_defined;
    bool decay_is_defined;
    //: event printing
    void open_event_filename();
    void close_event_filename();
};


// ==================================================
// Last includes
#include "bin.h"
#include "histogram.h"

#endif

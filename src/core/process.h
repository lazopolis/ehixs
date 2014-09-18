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
//#include "fvector.h"
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

//#include "momenta.h"
#include "histogram.h"


class Process
{
public://methods
    Process(const UserInterface& UI);
    void  perform();
    //: public because it has to be accessed by Integrand
    void  Evaluate_integral(const double xx[]);
	
    double total_xs(){return Vegas.vegas_integral_output[0];}
    double total_err(){return Vegas.vegas_error_output[0];}
    vector<string> give_sector_names(const string & pleft,
                                     const string & pright,
                                     const string & myorder,
                                     const int&,const string & );
    int number_of_active_histograms(){return _histograms->size();}
    CHistogram* ptr_to_histogram_with_id(unsigned m)
                    {return _histograms->ptr_to_histogram_with_id(m);}
    
public://data
    VegasAdaptor Vegas;
    //: public so that histograms from different sectors can be compared
    
private://data
    UserInterface my_UI;
    TheHatch the_hatch;
    HistogramBox* _histograms;
    
    Decay *my_decay;
    Production* my_production;
    fstream my_event_stream;
    bool events_writing_;
    TimeKeeper myclock_;
    
    int decay_particle_id_;
    
    bool final_iteration_;
    bool no_grid_adaptation_;
    bool bin_by_bin_integration_;
    int current_bin_;
    CHistogram* current_histogram_;
    //ostringstream vegas_info_;
    
private://methods
    void choose_production(const UserInterface & UI);
    void choose_decay(const UserInterface & UI);
    void set_decay(Decay * thedecay);
    
    void perform_bin_by_bin_mode();
    void perform_no_adaptation_mode();
    void perform_default_mode();
    
    void book_null_event();
    void book_event(const CombinedEvent&);
    void proceed_to_production_phase();
    void proceed_to_decay_phase(Event*);
    void perform_decay_alone();
    void book_decay_event(Event *);
    void print_output_intermediate();
    void print_output();
	bool sectors_are_defined_in_production_and_decay();
	void calculate_dimension_of_integration();
    bool production_is_defined;
    bool decay_is_defined;
    //: event printing
    void open_event_filename();
    void close_event_filename();
    void write_event(const CombinedEvent& the_event);

};


// ==================================================
// Last includes
#include "bin.h"

#endif

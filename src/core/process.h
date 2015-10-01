#ifndef PROCESS_H
#define PROCESS_H


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
#include "model.h"
#include "cut.h"

#include "option.h"
#include "thehatch.h"
#include "decay.h"
#include "production.h"
#include "vegasadaptor.h"
// ==================================================
// Forward declarations
class CHistogram;
class CHistogram2d;
class AverageObservable;

#include "histogram.h"

class IProcess : protected OptionSet
{

    typedef vector<Decay*> DecayBox;

protected:

    /// \name Data Members
    /// @{

    bool _binbybin;
    bool _adaptive;
    Production* production;
    DecayBox*   decays;

    /// @}
    
public:
    
    /// \name Constructors and destructor
    /// @{
    
    IProcess()
    : OptionSet()
    {
        _opts().push_back(new Option<bool>("bin_by_bin",0,
                                           "switch on bin by bin integration (for every histogram separately)",
                                            Arg::Optional,_binbybin,false));
        _opts().push_back(new Option<bool>("adaptive",0,
                                            "switch for grid adaptation in Vegas",
                                            Arg::Optional,_adaptive,true));
        _opts().push_back(new Option<Production*>("production",0,
                                                  "production process",
                                                  Arg::Required,production,NULL));
        _opts().push_back(new Option<DecayBox*>("decay",0,
                                                "decay process",
                                                Arg::Required,decays,NULL));
    }
    
    virtual ~IProcess(){};
    
    /// @}
    
};

class Process : protected IProcess
{
public://methods
    Process();
    void  perform();
    //: public because it has to be accessed by Integrand
    void  Evaluate_integral(const double xx[]);
    
    double total_xs(){return Vegas.vegas_integral_output[0];}
    double total_err(){return Vegas.vegas_error_output[0];}
    vector<string> give_sector_names(const string& pleft,
                                     const string& pright,
                                     const string& myorder,
                                     const int&, const string&);
    int number_of_active_histograms(){return _histograms->size();}
    CHistogram* ptr_to_histogram_with_id(unsigned m)
    {return _histograms->ptr_to_histogram_with_id(m);}
    
public://data
    VegasAdaptor Vegas;
    //: public so that histograms from different sectors can be compared
    
private://data
    TheHatch the_hatch;
    HistogramBox* _histograms;
    
    fstream my_event_stream;
    bool events_writing_;
    TimeKeeper myclock_;
    
    CHistogram* current_histogram_;
    //ostringstream vegas_info_;
    bool final_iteration_;
    int current_bin_;

private://methods

    void perform_bin_by_bin_mode();
    
    void book_null_event();
    void book_event(const CombinedEvent&);
    void proceed_to_production_phase();
    void proceed_to_decay_phase(Event*);
    void perform_decay_alone();
    void book_decay_event(Event*);
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

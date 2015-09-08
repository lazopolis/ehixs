/**
 *
 * \file    production.h
 * \ingroup core
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef PRODUCTION_H
#define PRODUCTION_H

#include <iostream>
#include "model.h"
#include "cut.h"
#include "interface_to_amplitudes.h"
#include "option.h"
//#include "luminosity.h"
#include "thehatch.h"
#include "event.h"
#include "xsectionmaker.h"
using namespace std;

class IProduction : protected OptionSet
{
    
    IProduction(){
        /// \todo make this int?
        _opts().push_back(new Option<string>("sector_for_production",'s',"number of the production sector to run: attention: the id number depends on other user defined parameters, like the channel, the perturbative order, the pole etc.  ",Need::Required,sector_for_production,"none"));
    };
    
    string sector_for_production;
};

///\class Production
class Production : protected IProcess
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    Production() :
    event_box(), xx_vegas(), cuts_(), the_xs_(NULL), sectors()
    {}

    /// Destructor
    ~Production()
    {
        delete the_xs_;
    }

    /// @}

	void Configure();

    bool is_sector_defined() const
    {
        return the_xs_ != NULL;
    }

    void set_up_the_hatch(TheHatch& the_hatch)
    {
        the_hatch.request(xx_vegas, dimension_of_integration());
    }

    bool this_event_passes_cuts(const size_t i)
    {
        return cuts_.passes_cuts(&(event_box[i]));
    }

    void show_cut_info_and_exit()
    {
        cuts_.show_cut_info_and_exit();
    }
    
    //: pure virtual functions
    
    virtual void ConfigureCuts()=0;


    
    void evaluate_sector();
    size_t dimension_of_integration();
    const CModel& model();
    void info();
    void xml_info(const char* output_fname);

    EventBox event_box;

protected:

    CutBox cuts_;
    XSection* the_xs_;
    vector<BaseXSectionMaker*> sectors;
    vector<double> xx_vegas;

private:

    void find_the_xs();

};

#endif




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

//#include "momenta.h"
#include "event.h"
#include "cross_section.h"

///\class Production
class Production
{
public://methods
	void Configure(const UserInterface& UI);
    void perform();

    bool is_sector_defined() const
    {
        return the_xs_ != NULL;
    }
    void set_up_the_hatch(TheHatch*);
    bool this_event_passes_cuts(int i)
    {return cuts_->passes_cuts(event_box(i));}
    void show_cut_info_and_exit(){cuts_->show_cut_info_and_exit();}
    
    //: pure virtual functions
    
    //virtual void SetNumberOfParticles()=0;
    virtual void SetDecayParticleIdInEventBox()=0;
    //virtual int number_of_necessary_sectors() = 0;
    
    virtual void create_matrix_elements()=0;
    virtual void ConfigureCuts()=0;
    
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
    
    static void registerit(BaseSector* maker);
    void evaluate_sector();
    size_t dimension_of_integration();
    const CModel& model();
    void info();
    void xml_info(const char* output_fname);

public:// data
	
    EventBox event_box;

protected:// data
    double* xx_vegas;
    CutBox* cuts_;
    XSection* the_xs_;
protected://methods
    static vector<BaseSector*>& sectors();
    void find_the_xs(const UserInterface& UI);
    size_t _active;

};

#endif

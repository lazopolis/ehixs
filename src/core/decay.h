/**
 *
 * \file    decay.h
 * \ingroup core
 * \author  Achilleas Lazopoulos
 * \date    September 2015
 *
 */

/// \todo Decays are in very bad shape and need redesign.

#ifndef DECAY_H
#define DECAY_H

#include "option.h"
#include "constants.h"
#include "thehatch.h"
#include "cutbox.h"
#include <string>
#include <complex>
using namespace std;

class Decay
{

public:

    Decay(){
        my_sector_name = "If you see this, the specific decay hasn't declared it's sector_name yet.";
        sector_defined=false;
        cuts_ = new CutBox();
    }

    virtual int NumberOfParticles() = 0;
    virtual void do_decay(FourVector p)=0;
    virtual void do_decay()=0;//: default decay at rest frame
    EventBox event_box;
    int dimension_of_integration(){return dimension_of_integration_for_decay;}
    string sector_name(){return my_sector_name;}
    void set_alpha_s(double a_s_in){alpha_s=a_s_in;}
    void set_y_b(double yb_in){y_b=yb_in;}
    void SetModel(const CModel& model_input){Model = model_input;}
    CModel Model;
    bool is_sector_defined(){return sector_defined;}
    void set_up_the_hatch(TheHatch& the_hatch)
    {
        the_hatch.request(decay_xx_vegas, dimension_of_integration());
    }
    CutBox* cuts_;
    bool this_event_passes_cuts(int i){return !cuts_->isCut(event_box[i]);}
protected:
    string decay_mode;
    vector<double> decay_xx_vegas;

    string my_sector_name;

    int dimension_of_integration_for_decay;
    double alpha_s;
    double y_b;
    bool sector_defined;

};

#endif


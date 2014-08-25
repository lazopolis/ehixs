#include <iostream>
using namespace std;
#include "bottom_fusion.h"




//------------------------------------------------------------------------------


#include "bbh_cuts.h"





// defining all the cross secion objects that are available to the user
// all of them should inherit from CrossSection
void BottomFusion::create_matrix_elements()
{
    available_xs_.push_back(new BottomFusion_bb_LO);
    //available_xs_.push_back(new BottomFusionNLOSoft);
    //available_xs_.push_back(new BottomFusionNLOHard);
    //available_xs_.push_back(new BottomFusionNNLORV);
}


void BottomFusion::SetProcessSpecificParameters()
{
    // here we need to use setter functions of the derived class
    // (BottomFusionCrossSection) and I found no better way to do this than
    // explicit downcasting: I create a new pointer to the derived class,
    // I cast the the_xs_ pointer (of the base class) statically to the derived
    // and assign it to the new pointer. Then I use that pointer to call
    // the functions specific to bbH.
    BottomFusionCrossSection * the_bb_xs_ = static_cast<BottomFusionCrossSection*>(the_xs_);
    //: we need to make yukawa bottom an evolved coupling at Model first
    //the_xs_->SetYukawaBottom(Model.yukawa_bottom);
    the_bb_xs_->SetHiggsMass(Model.higgs.m());
    
}

void BottomFusion::ConfigureCuts()
{
    // The file includes the explicit declarations of cut objects.
    // The cuts declared there are available, not actually active.
    // They can be activated in runcard. The parsing of runcard for cuts
    // is done in Production, after the call to this function. 
    #include "bbh_cut_initialization.h"
    
}















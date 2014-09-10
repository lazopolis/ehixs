#include <iostream>
using namespace std;
#include "bottom_fusion.h"




//------------------------------------------------------------------------------


#include "bbh_cuts.h"





// defining all the cross secion objects that are available to the user
// all of them should inherit from CrossSection
void BottomFusion::create_matrix_elements()
{
    //available_xs_.push_back(BottomFusion_bb_LO::info_);
    available_xs_.push_back(new BottomFusion_bb_LO);
    available_xs_.push_back(new BottomFusion_bb_NLO_Soft);
    //available_xs_.push_back(new BottomFusionNLOHard);
    //available_xs_.push_back(new BottomFusionNNLORV);
}


void BottomFusion::ConfigureCuts()
{
    // The file includes the explicit declarations of cut objects.
    // The cuts declared there are available, not actually active.
    // They can be activated in runcard. The parsing of runcard for cuts
    // is done in Production, after the call to this function.
#include "bbh_cut_initialization.h"
    
}

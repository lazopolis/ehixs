#include <iostream>
using namespace std;
#include "gamma_star_gamma_star.h"
#include "gamma_star_gamma_star_me.h"



//------------------------------------------------------------------------------


#include "gstar_2_cuts.h"

GammaStarGammaStar::~GammaStarGammaStar()
{
    for (int i=0;i<available_xs_.size();i++)
        delete available_xs_[i];
}

void GammaStarGammaStar::SetNumberOfParticles()
{
    event_box.SetNumberOfParticles(6);
}

void GammaStarGammaStar::SetDecayParticleIdInEventBox()
{
    event_box.SetDecayParticleId(3);
}



void GammaStarGammaStar::create_matrix_elements()
{
    available_xs_.push_back(new Gstar2_qqbar_LO);
    available_xs_.push_back(new Gstar2_qqbar_NLO_soft);
//    available_xs_.push_back(new GstarGstarMeNLOHard);
//    available_xs_.push_back(new GstarGstarMeNLOConv(1));
//    available_xs_.push_back(new GstarGstarMeNLOConv(2));
//    available_xs_.push_back(new GstarGstarMENLOHardQuarkGluon);
//    available_xs_.push_back(new GstarGstarMeNLOConvQuarkGluon);
//    
    available_xs_.push_back(new Gstar2_qqbar_NNLO_soft);
//    available_xs_.push_back(new GstarGstarMeNNLOHard);
//    available_xs_.push_back(new GstarGstarMeNNLOConvLeft);
//    available_xs_.push_back(new GstarGstarMeNNLOConvRight);
//    available_xs_.push_back(new GstarGstarMeNNLO_R_remnant);
    
    //available_xs_.push_back(new GstarGstarMeNNLOMueller);
    //available_xs_.push_back(new GstarGstarMeNNLO_IL_Romain);
}


void GammaStarGammaStar::ConfigureCuts()
{
    // The file includes the explicit declarations of cut objects.
    // The cuts declared there are available, not actually active.
    // They can be activated in runcard. The parsing of runcard for cuts
    // is done in Production, after the call to this function.
#include "gstar_2_cut_initialization.h"
    
}


void GammaStarGammaStar::SetProcessSpecificParameters(const UserInterface& UI)
{
    // here we need to use setter functions of the derived class
    // (GammaStarGammaStar) and I found no better way to do this than
    // explicit downcasting: I create a new pointer to the derived class,
    // I cast the the_xs_ pointer (of the base class) statically to the derived
    // and assign it to the new pointer. Then I use that pointer to call
    // the functions specific to GammaStarGammaStar.
    Gstar2CrossSection * the_gs2_xs_ = static_cast<Gstar2CrossSection*>(the_xs_);

    the_gs2_xs_->SetPhotonMasses(UI.astar_m3,UI.astar_m4);
    the_xs_->Configure();
}











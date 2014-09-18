#include "event.h"
#include "decay_z_gamma.h"
#include "higgs_zerfal.h"
#include "decay_z_gamma_cuts.h"

Decay_H_to_Z_Gamma::Decay_H_to_Z_Gamma(const UserInterface & UI)
{
    dimension_of_integration_for_decay=5;
#include "decay_z_gamma_cut_initialization.h"
    cuts_->ParseCuts(UI);
    decay_mode_ = 6;
}

void Decay_H_to_Z_Gamma::do_decay()
{
    do_decay(FourVector(Model.higgs.m(), 0., 0., 0.));
}


void Decay_H_to_Z_Gamma::do_decay(FourVector PH)
{
    event_box.clear();
    
    int decaymode = 1;
    // decay mode 1:HZZeemm | 2: HZZllll | 3: HWWlnln | 4: HWWZZlnln
    //double pH[4]={PH->E(),PH->px(),PH->py(),PH->pz()};
    
    Momenta p;
    double decay_weight = 0.;
    //cout<<"\n[decay]: hello before"<<endl;
    /// \note move alpha_QED to constants!!
    double alpha_QED = 1.0/129.0;
    higgszerfall(
                 PH,
                 Model.W.m(),Model.Z.m(),Model.top.m(),
                 Model.W.width(),Model.Z.width(),consts::G_fermi,
                 alpha_QED,
                 decay_mode_,
                 decay_xx_vegas,// passing vegas variables
                 decay_weight,  // the weight of the decay will be set here
                 p
                 );  // the four-momenta of the final state particles
    // as set by HiggsZerfall
    
    
    
    decay_weight /= Model.higgs.width();
    
    event_box.push_back(Event(decay_weight,p));
}

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
    double  PH_rest[4] ={Model.higgs.m(),0.0,0.0,0.0};
    do_decay(PH_rest);

}


void Decay_H_to_Z_Gamma::do_decay(double* PH)
{
    event_box.CleanUp();
    
    int decaymode = 1;
    // decay mode 1:HZZeemm | 2: HZZllll | 3: HWWlnln | 4: HWWZZlnln
    //double pH[4]={PH->E(),PH->px(),PH->py(),PH->pz()};
    
    double p1[4];
    double p2[4];
    double p3[4];
    double p4[4];
    double decay_weight = 0.0;
    //cout<<"\n[decay]: hello before"<<endl;
    double alpha_QED = 1.0/129.0;
    higgszerfall(
                 PH,
                 Model.W.m(),Model.Z.m(),Model.top.m(),
                 Model.W.width(),Model.Z.width(),consts::G_fermi,
                 alpha_QED,
                 decay_mode_,
                 decay_xx_vegas,// passing vegas variables
                 decay_weight,  // the weight of the decay will be set here
                 p1,p2,p3,p4
                 );  // the four-momenta of the final state particles
    // as set by HiggsZerfall
    
    
    
    decay_weight = decay_weight / Model.higgs.width();
    
    event_box.AddNewEvent(decay_weight);
    event_box.SetP(1,p1[0],p1[1],p1[2],p1[3]);
    event_box.SetP(2,p2[0],p2[1],p2[2],p2[3]);
    event_box.SetP(3,p3[0],p3[1],p3[2],p3[3]);
    event_box.SetP(4,p4[0],p4[1],p4[2],p4[3]);
}
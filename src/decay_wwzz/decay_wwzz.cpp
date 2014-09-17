#include "fourvector.h"
#include "decay_wwzz.h"
#include "decay_WWZZ_cuts.h"
#include "higgs_zerfal.h"


//1 decaymode='HZZeemm'
//2 decaymode='HZZllll'
//21 decaymode='HZZllll'
//22 decaymode='HZZllll'
//3 decaymode='HWWlnln'
//4 decaymode='HWWZZlnln'
//41 decaymode='HWWZZlnln'
//42 decaymode='HWWZZlnln'


Decay_WWZZ::Decay_WWZZ(const UserInterface & UI)
{
    dimension_of_integration_for_decay=8;
#include "decay_WWZZ_cut_initialization.h"
    cuts_->ParseCuts(UI);
    if (UI.leptonic_decay_mode_in_wwzz=="none")
        {
        cout<<"\n[decay_WWZZ]: error, you have not specified which leptonic decay mode you want  - leptonic_decay_mode_in_wwzz was "
        << UI.leptonic_decay_mode_in_wwzz
        <<"  I must exit!"
        <<endl;
        exit(1);
        }
    if (UI.leptonic_decay_mode_in_wwzz == "eemumu") decay_mode_ = 1;
    if (UI.leptonic_decay_mode_in_wwzz == "llll") decay_mode_ = 2;
    if (UI.leptonic_decay_mode_in_wwzz == "lvlv") decay_mode_ = 3;
    if (UI.leptonic_decay_mode_in_wwzz == "lvlv_interference") decay_mode_ = 4;
    cout<<"\n** UI.decay_sector = "<<UI.decay_sector
        <<"\t decay_mode_ = "<<decay_mode_<<endl;
    if ((decay_mode_==2 or decay_mode_==3) and (UI.decay_sector==-1))
        {
        cout<<"\n[decay_WWZZ]: error, you have specified decay mode "
            << UI.leptonic_decay_mode_in_wwzz
            <<" but you haven't specified decay_sector number. There are two sectors here because of the treatment of the interference. I must exit!"
        <<endl;
        exit(1);
        }
    if (decay_mode_==2 and UI.decay_sector == 0) decay_mode_ = 21;
    if (decay_mode_==2 and UI.decay_sector == 1) decay_mode_ = 22;
    if (decay_mode_==3 and UI.decay_sector == 0) decay_mode_ = 41;
    if (decay_mode_==3 and UI.decay_sector == 1) decay_mode_ = 42;
    

}

void Decay_WWZZ::do_decay()
{
    do_decay(FourVector(Model.higgs.m(),0.0,0.0,0.0));
}


void Decay_WWZZ::do_decay(FourVector PH)
{
    event_box.clear();
    
    //int decaymode = 1;
    // decay mode 1:HZZeemm | 2: HZZllll | 3: HWWlnln | 4: HWWZZlnln
    //double pH[4]={PH->E(),PH->px(),PH->py(),PH->pz()};
    
    vector<FourVector> p;
    double decay_weight = 0.;
    //cout<<"\n[decay]: hello before"<<endl;
    double alpha_QED = 1.0/127.0;
    
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




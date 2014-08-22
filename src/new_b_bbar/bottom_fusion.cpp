#include <iostream>
using namespace std;
#include "bottom_fusion.h"




//------------------------------------------------------------------------------


#include "bbh_cuts.h"

~BottomFusion::BottomFusion()
{
    for (int i=0;i<available_xs_.size();i++)
        delete available_xs_[i];
}


int BottomFusion::dimension_of_integration()
{
    if (is_sector_defined()) return the_xs_->Dimension();
    else
    {
        cout<<"\nError: you asked for the dimension of integration, but the sector is not yet defined!"<<endl;
        exit(0);
    }
}


void BottomFusion::create_matrix_elements()
{
    available_xs_.push_back(new BottomFusion_bb_LO);
    //available_xs_.push_back(new BottomFusionNLOSoft);
    //available_xs_.push_back(new BottomFusionNLOHard);
    //available_xs_.push_back(new BottomFusionNNLORV);
}

void BottomFusion::SelectAndConfigureSector(const UserInterface& UI)
{
    find_the_xs(UI);
    if (is_sector_defined())
    {
        // dimension is already set in constructors
        dim_of_integration=the_xs_->Dimension();
        #include "gstar_2_cut_initialization.h"
        cuts_->ParseCuts(UI);
        cout <<"[ehixs] CrossSection name : "<<*the_xs_<<endl;
        the_xs_->SetEventBox(event_box);
        the_xs_->PassEColliderSq(pow(UI.Etot,2.0));
        the_xs_->AllocateLuminosity(UI);
        
        the_xs_->PassScales(UI.mur,UI.muf);
        the_xs_->Configure();
    }
    cout<<"\n[sector] end of construction phase"<<endl;
}

double BottomFusion::alpha_s_at_mz_from_lhapdfs()
{
    //set guard for the case the_xs_ is not assigned
    return the_xs_->alpha_s_at_mz_from_lhapdfs();
}


void BottomFusion::SetModelDependentParameters()
{
    the_xs_->PassAlphaStrong(Model.alpha_strong()/consts::Pi);
    //: we need to make yukawa bottom an evolved coupling at Modelfirst
    //the_xs_->SetYukawaBottom(Model.yukawa_bottom);
}

void BottomFusion::info()
{
    for (int i=0;i<available_xs_.size();i++)
    {
        cout<<"\n"<<i<<" : "<<*available_xs_[i];
    }
    cout<<endl<<endl;
}

void BottomFusion::xml_info(const char * output_fname)
{

}

void BottomFusion::evaluate_sector()
{
    event_box.CleanUp();
    the_xs_->Evaluate(xx_vegas);
    
}


void BottomFusion::find_the_xs(const UserInterface & UI)
{
    if (UI.sector_for_production=="none")
    {
        cout<<"\n[find_sector] Error: you haven't declared a sector_for_production"<<endl;
        throw "\n[find_sector] Can't proceed!\n";
    }
    else
    {
        int sector_id=atoi(UI.sector_for_production.c_str());
        if (sector_id>-1 and sector_id<available_xs_.size())
        {
            sector_defined=true;
            the_xs_=available_xs_[sector_id];
        }
        else
        {
            cout<<"\n[find_sector] The sector id number you asked for, "
            <<sector_id
            <<", was outside the bounds [0,"
            << available_xs_.size()<<"]";
            cout<<"\n[find_sector] Please run with UI.info=true"
            <<" or --info to get the list of sector names"<<endl;
            throw "\n[find_sector] Can't proceed!\n";
        }
    }
}











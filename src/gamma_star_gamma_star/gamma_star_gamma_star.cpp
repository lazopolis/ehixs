#include <iostream>
using namespace std;
#include "gamma_star_gamma_star.h"




//------------------------------------------------------------------------------


#include "gstar_2_cuts.h"


////: move everything to Production except the create_matrix_element function
GammaStarGammaStar::GammaStarGammaStar(const UserInterface & UI) : Production(UI)
{
    SetNumberOfParticles();
    create_matrix_elements();
    if (UI.info) info();
    else initialize_sector(UI);
}


void GammaStarGammaStar::initialize_sector(const UserInterface& UI)
{
    find_the_xs(UI);    
    if (is_sector_defined())
    {
        // dimension is already set in constructors
        dim_of_integration=the_xs_->Dimension();
        #include "gstar_2_cut_initialization.h"
        cuts_->ParseCuts(UI);
        cout <<"\nCrossSection name : "<<*the_xs_<<endl;
        the_xs_->SetEventBox(event_box);
        the_xs_->PassEColliderSq(pow(UI.Etot,2.0));
        the_xs_->AllocateLuminosity(lumi);
        the_xs_->PassAlphaStrong(Model.alpha_strong()/consts::Pi);
        the_xs_->PassScales(UI.mur,UI.muf);
        the_xs_->PassMasses(UI.astar_m3,UI.astar_m4);
        //the_xs_->SetPhotonMasses(UI.m3,UI.m4);
        the_xs_->Configure();        
    }
    cout<<"\n[sector] end of construction phase"<<endl;
}


GammaStarGammaStar::~GammaStarGammaStar()
{
for (int i=0;i<available_xs_.size();i++)
    delete available_xs_[i];
}

void GammaStarGammaStar::create_matrix_elements()
{
available_xs_.push_back(new GstarGstarMELO);   
available_xs_.push_back(new GstarGstarMeNLOSoft);
available_xs_.push_back(new GstarGstarMeNLOHard);
available_xs_.push_back(new GstarGstarMeNLOConv(1));
available_xs_.push_back(new GstarGstarMeNLOConv(2));
available_xs_.push_back(new GstarGstarMeNNLOSoft);
available_xs_.push_back(new GstarGstarMeNNLOHard);
available_xs_.push_back(new GstarGstarMeNNLOConvLeft);
available_xs_.push_back(new GstarGstarMeNNLOConvRight);
available_xs_.push_back(new GstarGstarMeNNLO_R_remnant);
available_xs_.push_back(new GstarGstarMeNNLOMueller);
available_xs_.push_back(new GstarGstarMeNNLO_IL_Romain);
available_xs_.push_back(new GstarGstarMENLOHardQuarkGluon);
available_xs_.push_back(new GstarGstarMeNLOConvQuarkGluon);

}



//: this can be moved in production
void GammaStarGammaStar::info()
{
    for (int i=0;i<available_xs_.size();i++)
        {
        cout<<"\n"<<i<<" : "<<*available_xs_[i];
        }
    cout<<endl<<endl;
    exit(0);
}

void GammaStarGammaStar::evaluate_sector()
{
    event_box.CleanUp();
    the_xs_->Evaluate(xx_vegas);

}


//: this can be moved in production
void GammaStarGammaStar::find_the_xs(const UserInterface & UI)
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
            cout<<"\n[find_sector] The sector id number you asked for, "<<sector_id
            <<", was outside the bounds [0,"
            << available_xs_.size()<<"]";
            cout<<"\n[find_sector] Please run with UI.info=true"
            <<" or --info to get the list of sector names"<<endl;
            throw "\n[find_sector] Can't proceed!\n";
            }
        }
}









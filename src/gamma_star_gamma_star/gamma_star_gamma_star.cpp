#include <iostream>
using namespace std;
#include "gamma_star_gamma_star.h"




//------------------------------------------------------------------------------




////: move everything to Production except the create_matrix_element function
GammaStarGammaStar::GammaStarGammaStar(const UserInterface & UI) : Production(UI)
{
    SetNumberOfParticles();
    // creating matrix elements
    create_matrix_elements();
    // passing parameters to matrix elements
    cout<<"\n[sector] passing parameters to matrix elements "<<UI.Etot<<endl;
    PassParametersToMatrixElements(pow(UI.Etot,2.0));
    // creating sectors
    all_sectors = new SectorBox(available_matrix_elements,UI);
        
    if (UI.info)
        {
        cout<<"\n Sectors that fit your selection criteria:\n";
        for (int i=0;i<all_sectors->size();i++)
            {
            cout<<"\n"<<i<<" : "<<*all_sectors->give(i);
            }
        cout<<"\n\n number of Sectors defined : "<<all_sectors->size()<<endl;

        exit(0);
        }
    
    if (UI.show_me_list)
        {
        cout<<"\n ME available:\n This should be re-implemented!!";
        for (int i=0;i<available_matrix_elements.size();i++)
            {
            cout<<*(available_matrix_elements[i])<<endl;
            }
        exit(0);
        }
    
    cout<<"\n[Gamma* Gamma*] : finding sector"<<endl;
    find_sector(UI);
    
    if (is_sector_defined())
        {
            
        //allocate_luminosity();
        cout <<"\n----------------------------------\n\tSECTOR \""
        <<*the_sector
        <<"\"\n----------------------------------\n"<<endl;
            
        the_sector->AllocateLuminosity(lumi);
        cout<<"\n[sector] setting up prefactor"<<endl;
        the_sector -> SetUpPrefactor(Model.alpha_strong()/consts::Pi);
        }
    cout<<"\n[sector] end of construction phase"<<endl;
//        cout<<"\na_s used = "<<Model.alpha_strong()
//        <<"\t^"<<the_sector->alpha_power
//        <<"(a/Pi)^"<<the_sector->alpha_power<<" = "<<pow(Model.alpha_strong()/consts::Pi,the_sector->alpha_power) ;
    
}

void GammaStarGammaStar::create_matrix_elements()
{
    available_matrix_elements.push_back(new GstarGstarMeLO(event_box));
}

void GammaStarGammaStar::PassParametersToMatrixElements(const double& s)
{
    for (int i=0;i<available_matrix_elements.size();i++)
        {
        available_matrix_elements[i]->set_S(s);
        cout<<"\n[gstar^2] we are gonna consolidate"<<endl;
        available_matrix_elements[i]->consolidate();

        }
}

//: this can be moved in production
void GammaStarGammaStar::evaluate_sector()
{
    event_box.CleanUp();
    the_sector->Evaluate(xx_vegas);

}


//: this can be moved in production
void GammaStarGammaStar::find_sector(const UserInterface & UI)
{
    if (UI.sector_for_production=="none")
        {
        cout<<"\n[find_sector] Error: you haven't declared a sector_for_production"<<endl;
        throw "\n[find_sector] Can't proceed!\n";
        }
    else
        {
        int sector_id=atoi(UI.sector_for_production.c_str());
        if (sector_id>-1 and sector_id<all_sectors->size())
            {
            sector_defined=true;
            the_sector=all_sectors->give(sector_id);
            dim_of_integration=the_sector->dimension();
            cout<<"[find_sector] dim_of_integration = "<<dim_of_integration;
            }
        else
            {
            cout<<"\n[find_sector] The sector id number you asked for, "<<sector_id
            <<", was outside the bounds [0,"
            << all_sectors->size()<<"]";
            cout<<"\n[find_sector] Please run with UI.info=true"
            <<" or --info to get the list of sector names"<<endl;
            throw "\n[find_sector] Can't proceed!\n";
            }
        }
}









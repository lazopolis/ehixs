#include <iostream>
using namespace std;
#include "gamma_star_gamma_star.h"




//------------------------------------------------------------------------------


#include "gstar_2_cuts.h"


int GammaStarGammaStar::dimension_of_integration()
{
    if (is_sector_defined()) return the_xs_->Dimension();
    else
    {
        cout<<"\nError: you asked for the dimension of integration, but the sector is not yet defined!"<<endl;
        exit(0);
    }
}


void GammaStarGammaStar::SelectAndConfigureSector(const UserInterface& UI)
{
    find_the_xs(UI);    
    if (is_sector_defined())
    {
        // dimension is already set in constructors
        #include "gstar_2_cut_initialization.h"
        cuts_->ParseCuts(UI);
        cout <<"[ehixs] CrossSection name : "<<*the_xs_<<endl;
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

void GammaStarGammaStar::find_the_xs(const UserInterface & UI)
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
available_xs_.push_back(new GstarGstarMENLOHardQuarkGluon);
available_xs_.push_back(new GstarGstarMeNLOConvQuarkGluon);

available_xs_.push_back(new GstarGstarMeNNLOSoft);
available_xs_.push_back(new GstarGstarMeNNLOHard);
available_xs_.push_back(new GstarGstarMeNNLOConvLeft);
available_xs_.push_back(new GstarGstarMeNNLOConvRight);
available_xs_.push_back(new GstarGstarMeNNLO_R_remnant);

//available_xs_.push_back(new GstarGstarMeNNLOMueller);
//available_xs_.push_back(new GstarGstarMeNNLO_IL_Romain);


}



//: this can be moved in production
void GammaStarGammaStar::info()
{
    for (int i=0;i<available_xs_.size();i++)
        {
        cout<<"\n"<<i<<" : "<<*available_xs_[i];
        }
    cout<<endl<<endl;
}

void GammaStarGammaStar::xml_info(const char * output_fname)
{

//            
//    fstream my_local_outfile(output_fname, fstream::out);
//    if(my_local_outfile.is_open())
//    {
//        my_local_outfile.precision(5);
//        my_local_outfile << "<ehixs_info " << endl;
//        my_local_outfile << "\nnumber_of_sectors=\""
//        <<available_xs_.size()<<"\"";
//            
//        my_local_outfile << "\n runcard_name=\""<<UI.input_filename
//            <<"\" >"<<endl;
//        for (int i=0;i<necessary_sectors.size();i++)
//        {
//            my_local_outfile<<"\n<sector id=\""<<i<<"\" name=\""<<necessary_sectors[i]->name<<"\" ></sector>";
//        }
//    
//        my_local_outfile << "</ehixs_info>" << endl;                    
//    }
//    else
//    {
//        cout<<"\nfailbit = "<<my_local_outfile.fail()<<endl;
//        cout << "Error opening file "<<UI.xml_info.c_str()<<endl;
//    }
//    my_local_outfile.close();
}

void GammaStarGammaStar::evaluate_sector()
{
    event_box.CleanUp();
    the_xs_->Evaluate(xx_vegas);

}











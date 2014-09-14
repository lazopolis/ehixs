#include "production.h"
#include <stdlib.h> //: for exit()


#ifndef ONCE_EC
#define ONCE_EC
//Production* EC; //: static global pointer to use as a handle for plugins (like the fortran or c++ NNLO double real pieces)
//Process* ptr_to_process;
#endif

const CModel& Production::model()
{
   return the_xs_->model;
}

void Production::Configure(const UserInterface& UI)
{
    if (sectors().empty()) cout << "sectors is empty!" << endl;
    cuts_ = new CutBox();
    //SetNumberOfParticles();
    create_matrix_elements();
    if (UI.info)
    {
        info();
        //        if (UI.xml_info)
        //            xml_info();
        exit(0);
    }
    else
    {
        delete the_xs_;
        find_the_xs(UI);
        if (is_sector_defined())
        {
            ConfigureCuts();
            cuts_->ParseCuts(UI);
            cout << "[ehixs] CrossSection name : " << sectors()[_active] << endl;
            the_xs_->SetEventBox(event_box);
            the_xs_->initialize(UI);
            the_xs_->Configure();
            
        }
        
    }
}



void Production::find_the_xs(const UserInterface & UI)
{
    if (UI.sector_for_production=="none")
    {
        cout<<"\n[find_sector] Error: you haven't declared a sector_for_production"<<endl;
        throw "\n[find_sector] Can't proceed!\n";
    }
    else
    {
        size_t sector_id=atoi(UI.sector_for_production.c_str());
        if (sector_id>-1 and sector_id<sectors().size())
        {
            the_xs_ = sectors()[sector_id]->create();
            _active = sector_id;
        }
        else
        {
            cout<<"\n[find_sector] The sector id number you asked for, "
            <<sector_id
            <<", was outside the bounds [0,"
            << sectors().size() << "]";
            cout<<"\n[find_sector] Please run with UI.info=true"
            <<" or --info to get the list of sector names"<<endl;
            throw "\n[find_sector] Can't proceed!\n";
        }
    }
}


// dimension_of_integration depends on the particular integral (e.g. the LO is one-dimensional, the nlo and nnlo are higher), so it's setting is delegated to the particular cross section object that is requested by the user
size_t Production::dimension_of_integration()
{
    if (is_sector_defined()) return sectors()[_active]->dim;
    else
    {
        cout<<"\nError: you asked for the dimension of integration, but the sector is not yet defined!"<<endl;
        exit(0);
    }
}


void Production::set_up_the_hatch(TheHatch* the_hatch)
{

    //: setting up the hatch array that will be used to store
    //:the vegas variables we need
    xx_vegas = the_hatch->RequestPtr();
    for (unsigned i=0;i<dimension_of_integration();i++)
        the_hatch->RequestVar("VEGAS");

     
}

void Production::registerit(BaseSector* maker)
{
    sectors().push_back(maker);
}

void Production::evaluate_sector()
{
    event_box.clear();
    the_xs_->Evaluate(xx_vegas);
    
}

vector<BaseSector*>& Production::sectors()
{
    static vector<BaseSector*> sectorList;
    return sectorList;
}

void Production::info()
{
    for (size_t i = 0; i < sectors().size(); ++i)
        cout << "\n" << i << " : " << sectors()[i];
    cout<<endl<<endl;
}

void Production::xml_info(const char * output_fname)
{
    
}

/*
void ProductionMockUp::init(const UserInterface& UI,TheHatch* the_hatch)
{
     dim_of_integration=7;
     lumi=new Luminosity(UI);
     lumi->add_pair(pdf_desc(5,5,0,0),pdf_desc(5,5,0,0));
     set_up_the_hatch(the_hatch);//: calling base init
}


void ProductionMockUp::evaluate_sector()
{

}


vector<string> ProductionMockUp::give_sector_names(const string & pleft,const string & pright,const string & myorder)
{
     return vector<string>();
}
*/



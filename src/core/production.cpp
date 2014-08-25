#include "production.h"
#include <stdlib.h> //: for exit()


#ifndef ONCE_EC
#define ONCE_EC
//Production* EC; //: static global pointer to use as a handle for plugins (like the fortran or c++ NNLO double real pieces)
//Process* ptr_to_process;
#endif


//: Franz's functions communicate with the rest of the code via this fjet_ . Note that there is a global pointer passed around!
/*
TO MOVE TO GLUON FUSION
 
double fjet_(double* x1,double *x2,double *s12,double *s13,double *s23,double *s14,double *s24,double *s34,double *w)
{
     double ns13= -(*s13);
     double ns14= -(*s14);
     double ns23= -(*s23);
     double ns24= -(*s24);
     double ns34=(*s34);
     double z=(*s12-ns13-ns14-ns23-ns24+ns34)/(*s12);
     
     if ((*w)!=(*w))
          {
          cout<<setprecision(16);
          cout<<"\n Nan found coming from franz: "<<(*w)
               <<"\t kinematics: "
               <<"x1="<<*x1<<" "
          <<"x2="<<*x2<<" "
          <<"z="<<z<<" "
          <<"s12="<<(*s12)<<" "
          <<"s13="<<ns13<<" "
          <<"s23="<<ns23<<" "
          <<"s14="<<ns14<<" "
          <<"s24="<<ns24<<" "
          <<"s34="<<ns34<<endl;
          cout<<setprecision(8);
          exit(1);
          }
     
     EC->book_production_event(*w,
                                       *x1,
                                       *x2,
                                       z,
                                       ns13,ns23,ns14,ns24,ns34);
     
     //cout<<"\n*w="<<*w;
     return 1;//: VERY IMPORTANT TOP RETURN 1 here 
}
*/
//-----------------------------------------------------------------------------

Production::~Production()
{
    for (int i=0;i<available_xs_.size();i++)
        delete available_xs_[i];
}


void Production::Configure(const UserInterface & UI)
{
    cuts_ = new CutBox();
    SetNumberOfParticles();
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
        sector_defined=false;
        find_the_xs(UI);
        if (is_sector_defined())
        {
            ConfigureCuts();
            cuts_->ParseCuts(UI);
            cout <<"[ehixs] CrossSection name : "<<*the_xs_<<endl;
            the_xs_->SetEventBox(event_box);
            the_xs_->SetEColliderSq(pow(UI.Etot,2.0));
            the_xs_->AllocateLuminosity(UI);
            
            the_xs_->SetScales(UI.mur,UI.muf);
            Model.Configure(alpha_s_at_mz_from_lhapdfs(),
                            UI.mur_over_mhiggs,
                            UI.perturbative_order,
                            UI.m_higgs);
            the_xs_->SetAlphaStrong(Model.alpha_strong()/consts::Pi);
            SetProcessSpecificParameters();
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


// dimension_of_integration depends on the particular integral (e.g. the LO is one-dimensional, the nlo and nnlo are higher), so it's setting is delegated to the particular cross section object that is requested by the user
int Production::dimension_of_integration()
{
    if (is_sector_defined()) return the_xs_->Dimension();
    else
    {
        cout<<"\nError: you asked for the dimension of integration, but the sector is not yet defined!"<<endl;
        exit(0);
    }
}


double Production::alpha_s_at_mz_from_lhapdfs()
{
    //set guard for the case the_xs_ is not assigned
    return the_xs_->alpha_s_at_mz_from_lhapdfs();
}

void Production::set_up_the_hatch(TheHatch* the_hatch)
{

    //: setting up the hatch array that will be used to store
    //:the vegas variables we need
    xx_vegas = the_hatch->RequestPtr();
    for (unsigned i=0;i<dimension_of_integration();i++)
        the_hatch->RequestVar("VEGAS");

     
}

void Production::evaluate_sector()
{
    event_box.CleanUp();
    the_xs_->Evaluate(xx_vegas);
    
}



void Production::info()
{
    for (int i=0;i<available_xs_.size();i++)
    {
        cout<<"\n"<<i<<" : "<<*available_xs_[i];
    }
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



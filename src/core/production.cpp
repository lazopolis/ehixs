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
        SelectAndConfigureSector(UI);
        Model.Configure(alpha_s_at_mz_from_lhapdfs(),
                        UI.mur_over_mhiggs,
                        UI.perturbative_order,
                        UI.m_higgs);
        SetModelDependentParameters();
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



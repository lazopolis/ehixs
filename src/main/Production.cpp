#include "Production.h"


#ifndef ONCE_EC
#define ONCE_EC
Production* EC; //: static global pointer to use as a handle for plugins (like the fortran or c++ NNLO double real pieces)
//Process* ptr_to_process;
#endif


//: Franz's functions communicate with the rest of the code via this fjet_ . Note that there is a global pointer passed around!
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
          cout<<"\n Nan found coming from franz: "<<(*w)
               <<"\t kinematics: "
               <<"x1="<<*x1<<" "
          <<"x1="<<*x2<<" "
          <<"z="<<z<<" "
          <<"s12="<<(*s12)<<"<-  "
          <<"s13="<<ns13<<" "
          <<"s23="<<ns23<<" "
          <<"s14="<<ns14<<" "
          <<"s24="<<ns24<<" "
          <<"s34="<<ns34<<endl;
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




Production::Production()
{
     ptbuf = 1e-10;
     my_sector_name = "If you see this, the specific decay hasn't declared it's sector_name yet.";
     sector_defined=false;
     //counter_of_wrong_events=0;
     //jetalgorithm=false;
     //: setting the start time
     //    time(&start_of_program);
     //    time(&time_since_prev_iteration);
     //    total_intended_time_for_run = 10.0;
     EC=this;
}

void Production::init_base(const UserInterface & UI, TheHatch* the_hatch)
{
     Model.higgs.set_m_at_ref_scale(UI.m_higgs);
     //the_hatch = hatch;
     
     
     if (UI.number_of_flavours!=5)
          {
          cout<<"\n\nerror in constructor of Production: during refactoring the nf became a global variable consts::nf and we don;t want to be changing it from 5. If you really feel like doing so, uncomment the next line in the code";exit(1);
          }
     //consts::nf = UI.number_of_flavours;
     
     lumi->evolve_alpha_s_from_mz_to_mur(alpha_s_vector); // necessarily before evolve_mb_from_mb_ref_to_mur
     for (int i=0;i<alpha_s_vector.size();i++)
          {
          cout<<"\nalpha_s["<<i<<"]="<<alpha_s_vector[i];
          }
     Model.evolve(lumi->alpha_s_at_mz_vector,mu_r,UI.perturbative_order);
    
     
     
     //: setting up the hatch array that will be used to store the vegas variables we need
     xx_vegas = the_hatch->RequestPtr();
     for (unsigned i=0;i<dim_of_integration;i++) the_hatch->RequestVar("VEGAS");
     
}







void ProductionMockUp::init(const UserInterface& UI,TheHatch* the_hatch)
{
     dim_of_integration=7;
     lumi=new Luminosity(UI.number_of_flavours,UI.muf_over_mhiggs * UI.m_higgs,UI.mur_over_mhiggs * UI.m_higgs,UI.perturbative_order,UI.pdf_provider,UI.pdf_error);
     lumi->add_pair(Luminosity::F_b_00,Luminosity::F_bbar_00);
     init_base(UI,the_hatch);//: calling base init
}


void ProductionMockUp::evaluate_sector()
{

}


vector<string> ProductionMockUp::give_sector_names(const string & pleft,const string & pright,const string & myorder)
{
     return vector<string>();
}




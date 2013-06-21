#include "InclusiveProduction.h"


#ifndef ONCE_IEC
#define ONCE_IEC
InclusiveProduction* EC; //: static global pointer to use as a handle for plugins (like the fortran or c++ NNLO double real pieces)
//Process* ptr_to_process;
#endif


InclusiveProduction::InclusiveProduction()
{
    ptbuf = 1e-10;
    my_sector_name = "If you see this, the specific decay hasn't declared it's sector_name yet.";
    //counter_of_wrong_events=0;
    //jetalgorithm=false;
    //: setting the start time
    //    time(&start_of_program);
    //    time(&time_since_prev_iteration);
    //    total_intended_time_for_run = 10.0;
    
    EC=this;
}

void InclusiveProduction::init_base(const UserInterface & UI, TheHatch* the_hatch)
{
    Model.higgs.m=UI.m_higgs;
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
    lumi->evolve_mb_from_mb_ref_to_mur(Model.bottom.m,yukawa_b_vector);
    
    mu_r=UI.mur_over_mhiggs * UI.m_higgs;
    mu_f=UI.muf_over_mhiggs * UI.m_higgs;
    
    Etot=UI.Etot;
    log_muf_sq_over_mh_sq = 2.0*log(mu_f/Model.higgs.m);
    log_mur_sq_over_mh_sq = 2.0*log(mu_r/Model.higgs.m);
    log_mur_sq_over_mt_sq = 2.0*log(mu_r/Model.top.m);
    log_mur_sq_over_muf_sq = 2.0*log(mu_r/mu_f);
    log_one_minus_tau = log(1.0 - pow(Model.higgs.m,2.0)/pow(Etot,2.0));
    
    
    //: setting up the hatch array that will be used to store the vegas variables we need
    xx_vegas = the_hatch->RequestPtr();
    for (unsigned i=0;i<dim_of_integration;i++) the_hatch->RequestVar("VEGAS");
    
}









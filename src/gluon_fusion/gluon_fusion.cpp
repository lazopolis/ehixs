#include "gluon_fusion.h"

#ifndef ONCE_PTR_TO_GGF
#define ONCE_PTR_TO_GGF
Production* ptr_to_GGF; //: static global pointer to use as a handle for plugins (like the fortran  NNLO double real pieces)
//Process* ptr_to_process;
#endif


//: external function definitions
#include "nlo_exact_matrix_elements.h"




#include "ggf_cuts.h"

GluonFusion::GluonFusion(const UserInterface & UI) : Production(UI)
{
    ptr_to_GGF = this;
    SetNumberOfParticles();
    SetDecayParticleIdInEventBox();
    for (int i=0;i<7;i++){smax[i]=0.0;smin[i]=1000.0;}
    
    set_up_wilson_coefficients();
    
    if (UI.matrix_element_approximation=="effective_enhanced")
        {
        readjust_wilson_coefficient_for_enhanced_effective();
        }
    // exact: the |WC|^2 terms must have no pure qcd component
    // so we adjust the WC.c*_qcd_only coefficients
    if (UI.matrix_element_approximation=="exact")
        {
        //by default in exact mode the qcd wilson coeffs of NNLO are "enhanced"
        readjust_wilson_coefficient_for_enhanced_effective();
        //arrange so that the LO and NLO effective
        //pure qcd contributions are removed
        readjust_wilson_coefficient_for_exact();
        }
    if (UI.ew_soft or UI.ew_h_plus_j)
        {
        electroweak_coefficients = new GluonFusionEWCoefficients(Model);
        if (UI.ew_soft)
            readjust_wilson_coefficient_for_electroweak_effective();
        }
        
    set_up_beta_constants();
    //cout<<"\n[GluonFusion] : setting up sectors"<<endl;
    all_sectors = new GluonFusionSectorBox(WC,beta,log_mur_sq_over_muf_sq,UI.rr_treatment);
    
    //cout<<"\n hello before Parse"<<endl;
    #include "ggf_cut_initialization.h"
    cuts_->ParseCuts(UI);
    
    if (UI.info) print_sector_info_and_exit(UI);
    if (UI.show_me_list) print_available_me_and_exit();

    
    //cout<<"\n[GluonFusion] : finding sector"<<endl;
    find_topology(UI); //: finding topology and setting all appropriate pointers to Channel, Convolution, PartonicMode, PartonicXS, Topology etc.
    if (UI.dummy_process == false)
        {
        if (is_sector_defined())
            {
            initialize_ggf_func_map();
            determine_ggf_func();
            determine_parametrization();
           
            allocate_luminosity();
            cout <<"\n[ehixs]\n[ehixs] Gluon Fusion: sector "<<endl
            <<"[ehixs] "<<the_sector->name
            <<endl;
            
            
            //:after init_base is called (where Etot is set)
            tau = pow(Model.higgs.m(),2.0)/pow(Etot,2.0);
            ISP.Configure(tau,Model.higgs.m());
            event_reconstructor.Configure(Etot,&event_box,Model.higgs.m());
             
            if (UI.matrix_element_approximation=="exact")
                {
                exact_coefficients = new GluonFusionExactCoefficients(Model);
                }
            
            
            //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
            //: Gf = 1.16637*10^{-5} * 0.389379*10^9
            pref_sgg = 35.0309;
            
            the_sector -> setUpPrefactor(Model.alpha_strong()/consts::Pi);
            
            }
        cout<<"[ehixs]"<<endl;
        
        }
}

void GluonFusion::print_sector_info_and_exit(const UserInterface& UI)
{
    vector<SimpleSector*> necessary_sectors=
    all_sectors->give_necessary_sectors(UI);
    cout<<"\n Sectors that fit your selection criteria:\n";
    for (int i=0;i<necessary_sectors.size();i++)
    {
        cout<<"\n"<<i<<" : "<<necessary_sectors[i]->name;
    }
    cout<<"\n\n number of Sectors defined : "<<necessary_sectors.size()<<endl;
    //check_which_sectors_can_be_run_together(necessary_sectors);
    if (UI.xml_info!="none")
    {
        const char * output_fname = UI.xml_info.c_str();
        fstream my_local_outfile(output_fname, fstream::out);
        if(my_local_outfile.is_open())
        {
            my_local_outfile.precision(5);
            my_local_outfile << "<ehixs_info " << endl;
            my_local_outfile << "\nnumber_of_sectors=\""
            <<necessary_sectors.size()<<"\"";
            
            my_local_outfile << "\n runcard_name=\""<<UI.input_filename
            <<"\" >"<<endl;
            for (int i=0;i<necessary_sectors.size();i++)
            {
                my_local_outfile<<"\n<sector id=\""<<i<<"\" name=\""<<necessary_sectors[i]->name<<"\" ></sector>";
            }
            
            my_local_outfile << "</ehixs_info>" << endl;                    
        }
        else
        {
            cout<<"\nfailbit = "<<my_local_outfile.fail()<<endl;
            cout << "Error opening file "<<UI.xml_info.c_str()<<endl;
        }
        my_local_outfile.close();
        
    }
    
    exit(0);
}

void GluonFusion::print_available_me_and_exit()
{
    cout<<"[ehixs] This facility is not working currently"<<endl;
    exit(0);
}


void GluonFusion::initialize_ggf_func_map()
{
    ggf_func_map_["LO"] = &GluonFusion::LO;
    ggf_func_map_["nlo_me"] = &GluonFusion::nlo_me;
    ggf_func_map_["gg_NLO_SOFT"] = &GluonFusion::gg_NLO_SOFT;
    ggf_func_map_["gg_NLO_HARD"] = &GluonFusion::gg_NLO_HARD;
    ggf_func_map_["gg_NNLO_SOFT"] = &GluonFusion::gg_NNLO_SOFT;
    ggf_func_map_["NNLO_rv_with_subtraction"] = &GluonFusion::NNLO_rv_with_subtraction;
    ggf_func_map_["NNLO_hard_with_subtraction"] = &GluonFusion::NNLO_hard_with_subtraction;
    ggf_func_map_["NNLO_hard_no_subtraction"] = &GluonFusion::NNLO_hard_no_subtraction;
    ggf_func_map_["NNLO_RR_brute_force_wrap"] = &GluonFusion::NNLO_RR_brute_force_wrap;
    ggf_func_map_["LO_exact"] = &GluonFusion::LO_exact;
    ggf_func_map_["NLO_soft_exact"] = &GluonFusion::NLO_soft_exact;
    ggf_func_map_["gg_NLO_hard_exact"] = &GluonFusion::gg_NLO_hard_exact;
    ggf_func_map_["qg_NLO_hard_exact"] = &GluonFusion::qg_NLO_hard_exact;
    ggf_func_map_["gq_NLO_hard_exact"] = &GluonFusion::gq_NLO_hard_exact;
    ggf_func_map_["qqbar_NLO_hard_exact"] = &GluonFusion::qqbar_NLO_hard_exact;
    ggf_func_map_["NLO_ewk_uubar_h_plus_jet"] = &GluonFusion::NLO_ewk_uubar_h_plus_jet;
    ggf_func_map_["NLO_ewk_ddbar_h_plus_jet"] = &GluonFusion::NLO_ewk_ddbar_h_plus_jet;
    ggf_func_map_["NLO_ewk_ug_h_plus_jet"] = &GluonFusion::NLO_ewk_ug_h_plus_jet;
    ggf_func_map_["NLO_ewk_dg_h_plus_jet"] = &GluonFusion::NLO_ewk_dg_h_plus_jet;
    ggf_func_map_["NLO_ewk_gu_h_plus_jet"] = &GluonFusion::NLO_ewk_gu_h_plus_jet;
    ggf_func_map_["NLO_ewk_gd_h_plus_jet"] = &GluonFusion::NLO_ewk_gd_h_plus_jet;
}

void GluonFusion::determine_ggf_func()
{
    if (ggf_func_map_.count(the_sector->ME->the_ggf_func)==1)
        the_func_=ggf_func_map_[the_sector->ME->the_ggf_func];
    else
        {
            cout<<"\nFatal error: unidentified ggf_func() pointer with key name: "<<the_sector->ME->the_ggf_func<<endl;
            exit(1);
        }
}

void GluonFusion::determine_parametrization()
{
    if (the_sector->ME->parametrization == "param:LO")  the_parametrization_=&GluonFusion::LO_parametrization_only;
    else if (the_sector->ME->parametrization=="param:NLO") the_parametrization_=&GluonFusion::NLO_parametrization;
    else 
    {
        cout<<"\nerror, param not equal to LO or NLO"<<endl;
        exit(1);
        }
}


void GluonFusion::check_which_sectors_can_be_run_together(const vector<SimpleSector*> &local_sectors)
{
    vector<vector<int> >all_sets;
    for (unsigned i=0;i<local_sectors.size();i++)
        {
        vector<int> cur_set;
        cur_set.push_back(i);
        for (unsigned j=i+1;j<local_sectors.size();j++)
            {
            if (sectors_are_compatible(local_sectors[i],local_sectors[j]))
                {
                cur_set.push_back(j);
                }
            }
        if (cur_set.size()>1)
            {
            //cout<<endl;
            //for (unsigned m=0;m<cur_set.size();m++) cout<<" "<<cur_set[m];
            all_sets.push_back(cur_set);
            }
        }
    if (all_sets.size()>0)
        {
        vector<vector<int> >final_sets;
        final_sets.push_back(all_sets[0]);
        for (int i=1;i<all_sets.size();i++)
            {
            bool found=false;
            vector<int> theset=all_sets[i];
            for (int j=0;j<i;j++)
                {
                vector<int> prev_set=all_sets[j];
                if (theset.size()<prev_set.size())
                    {
                    int size_diff=prev_set.size()-theset.size();
                    vector<int> compareto(prev_set.begin()+size_diff,prev_set.end());
                    //cout<<"\ncomparing [";for (int c=0;c<theset.size();c++)cout<<" "<<theset[c];
                    //cout<<"] with [";for (int c=0;c<compareto.size();c++)cout<<" "<<compareto[c];
                    //cout<<"]";
                    if (theset==compareto)
                        {
                        found=true;
                        break;
                        }
                    }
                }
            if (!found) final_sets.push_back(theset);
            }
        
        cout<<endl<<"You can run together the following sectors"<<endl;
        int subtract_counter=0;
        for (int c=0;c<final_sets.size();c++)
            {
            subtract_counter += final_sets[c].size();
            cout<<endl;
            for (unsigned m=0;m<final_sets[c].size();m++) cout<<" "<<final_sets[c][m];
            }
        cout<<endl;
        cout<<"\n resulting to a total of "<<local_sectors.size()<<"->"<<local_sectors.size()-subtract_counter+final_sets.size()<<endl;
        
        }
}

bool GluonFusion::sectors_are_compatible(SimpleSector* s1,SimpleSector* s2)
{
    if (s1->ME->give_name()==s2->ME->give_name())
        {
        return true;
        }
    return false;
}


void GluonFusion::find_topology(const UserInterface & UI)
{
     bool found=false;
     //: searching by sector_name (has priority)
     if (UI.sector_name!="none")
          {
          
          for (int i=0;i<all_sectors->size();i++)
               {
               if (all_sectors->give(i)->name==UI.sector_name)
                    {
                    found=true;
                    sector_defined=true;
                    the_sector=all_sectors->give(i);
                    dim_of_integration=the_sector->ME->dimension;
                    }
               }
          // if a sector name was provided and didn;t match we have an error
          if (not(found))
               {
               cout<<"\nI couldn't match the sector/topology you asked for.";
               cout<<"\n You asked for sector with the name "<<UI.sector_name;
               cout<<"\n which was not found in the list."
                    <<" Please run with UI.info=true"
                    <<" or --info to get the list of sector names"<<endl;
               throw "\n Can't proceed!\n";
               
               }
          }
     else if (UI.sector_for_production!="none")
          {
          int sector_id=atoi(UI.sector_for_production.c_str());
          vector<SimpleSector*> necessary_sectors =
                                    all_sectors->give_necessary_sectors(UI);
          // number_of_necessary_sectors is useful when we want to run
          // many sectors
          number_of_necessary_sectors_ = necessary_sectors.size();
          if (sector_id>-1 and sector_id<necessary_sectors.size())
               {
               found=true;
               sector_defined=true;
               the_sector=necessary_sectors[sector_id];
               dim_of_integration=the_sector->ME->dimension;
               //cout<<"[GGF] Dimension of integration for production = "<<dim_of_integration;
               }
          else
               {
               cout<<"\n The sector id number you asked for, "<<sector_id
                    <<", was outside the bounds [0,"
                    << necessary_sectors.size()<<"]";
               cout<<"\n Please run with UI.info=true"
                    <<" or --info to get the list of sector names"<<endl;
               throw "\n Can't proceed!\n";
               
               }
          }
     else if (UI.dummy_process == false)
          {
          cout<<"\n You didn't specify a sector name or a sector id number";
          cout<<"\n Please run with UI.info=true"
                <<" or --info to get the list of sector names"<<endl;
          throw "\n Can't proceed!\n";
          }
     else
         {
         cout<<"\n dummy process - no sector name provided"<<endl;
         }
     
}
 

void GluonFusion::allocate_luminosity()
{
     pdf_pair_list list_of_pdf_pairs=the_sector->give_list_of_pdf_pairs();
     
     for (unsigned i=0;i<list_of_pdf_pairs.size();i++)
          {
          //cout<<"\n pair #"<<i+1;
          pair<pdf_desc,pdf_desc> cur_pair=list_of_pdf_pairs.give_one_pair(i);
          lumi->add_pair(cur_pair.first,cur_pair.second);
          cout<<"\n[ehixs] luminosity : "
              <<cur_pair.first.name()<<" "<<cur_pair.second.name();
          }     
}





void GluonFusion::set_up_wilson_coefficients()
{
     //: QCD plain, in MSBar scheme :
     WC.c0 = complex<double>(1.0,0.0);
     WC.c1 = complex<double>(11.0/4.0,0.0);
     WC.c2 = complex<double>(2777.0/288.0
                              +(19.0/16.0)*log_muf_sq_over_mt_sq
                              +consts::nf*(
                                             (1.0/3.0)*log_muf_sq_over_mt_sq
                                           -67.0/96.0
                                           ),0.0);
    WC.c0_qcd_only = 0.0;
    WC.c1_qcd_only = 0.0;
    WC.c2_qcd_only = 0.0;
    WC.exact = false;
    WC.ew_soft = false;
}


void GluonFusion::readjust_wilson_coefficient_for_enhanced_effective()
{
    complex<double> LO_top_only_exact_coeff(0.0,0.0);
    for (int i=0;i<Model.quarks.size();i++)
        {
        //defeinition of heavy quark: m > 100 GeV
        if (Model.quarks[i]->m() > 100.0)
            {
            cout<<"\n quark contributing: "<<Model.quarks[i]->name()<<endl;
            LO_top_only_exact_coeff += Model.quarks[i]->Y() * born(Model.quarks[i]->X());
            }
        }
    
    WC.c0 = WC.c0 * LO_top_only_exact_coeff;
    WC.c1 = WC.c1 * LO_top_only_exact_coeff;
    WC.c2 = WC.c2 * LO_top_only_exact_coeff;
    cout<<"\n enhanced WC0! abs(WC0)^2 = "<<pow(abs(WC.c0),2.0)<<endl;
    
}

void GluonFusion::readjust_wilson_coefficient_for_electroweak_effective()
{
    
    WC.c0 = WC.c0 + electroweak_coefficients->LO() * 1.0;
    WC.c1 = WC.c1 + electroweak_coefficients->LO() * 7.0/6.0;
    WC.c2 = WC.c2 + electroweak_coefficients->LO() * 10.0;
    cout<<"\n[electroweak corrections to WCs] WC0 |WC0|^2 = "
        <<pow(abs(WC.c0),2.0)<<endl;
    
    WC.ew_soft = true;
}

void GluonFusion::readjust_wilson_coefficient_for_exact()
{
    WC.c0_qcd_only = WC.c0;
    WC.c1_qcd_only = WC.c1;
    WC.c2_qcd_only = WC.c2;
    WC.exact = true;
}


void GluonFusion::set_up_beta_constants()
{
     beta.zero = 11.0/4.0-consts::nf/6.0;
     beta.one =  51.0/8.0-19.0/24.0*consts::nf;
     beta.two = 2857.0/128.0-(5033.0/1152.0)*consts::nf+(325.0/3456.0)*pow(consts::nf,2.0);
     beta.three = 1.0/256.0*( 149753.0/6.0+3564.0*consts::z3
                             +(-1078361.0/162.0-6508.0/27.0*consts::z3)*consts::nf
                             +(50065.0/162.0+6472.0/81.0*consts::z3)*pow(consts::nf,2.0)
                             +(1093.0/729.0)*pow(consts::nf,3.0));
}

void GluonFusion::evaluate_sector()
{
    event_box.CleanUp();
    prepare_phase_space_dependent_quantities();
    (this->*the_func_)();
}


void GluonFusion:: prepare_phase_space_dependent_quantities()
{
     (this->*(the_parametrization_))();
     lumi->set_cur_lumiLO(ISP.x1LO,ISP.x2LO);
     lumi->set_cur_lumi(ISP.x1,ISP.x2);
}

void GluonFusion::LO_parametrization_only()
{
     //:setting x1LO,x2LO,zLO,measLO to LO kinematics
     ISP.parametrization_for_LO_kinematics(xx_vegas);
}

void GluonFusion::NLO_parametrization()
{
     //:setting x1LO,x2LO,zLO,measLO to LO kinematics
     ISP.parametrization_for_LO_kinematics(xx_vegas);
     //:setting x1,x2,z,meas to NLO kinematics
     ISP.parametrization_for_NLO_kinematics(xx_vegas);     
}

void GluonFusion::book_production_event()
{
}

void GluonFusion::book_production_event(
        const double & sigma_central,
         const double & x1,
         const double & x2,
         const double & z,
         const double & s13,
         const double & s23,
         const double & s14,
         const double & s24,
         const double & s34)
{
     if ((sigma_central)!=sigma_central)
          {
          cout<<"\n Nan found now in book_production_event: "<<sigma_central
          <<"\t kinematics: "
          <<x1<<" "<<x2<<" "<<z<<" "<<s13<<" "<<s23<<" "
          <<s14<<" "<<s24<<" "<<s34<<" \too==>"<<ISP.meas;
          }
     if (sigma_central!=0.0)
          {
          if (s13==0.0 and s23==0.0)
              {
              //: NLO kinematics: we rename particle 4 to be particle 3 and call NLO_event
              event_reconstructor.NLO_event_kinematics(sigma_central,x1,x2,z,s14,s24,ISP.phi);
              }
          else if (s14==0.0 and s24==0.0)
              {
              //: NLO kinematics: we  call NLO_event
              event_reconstructor.NLO_event_kinematics(sigma_central,x1,x2,z,s13,s23,ISP.phi);
              }
          else
              {
              event_reconstructor.NNLO_event_kinematics(sigma_central,x1,x2,z,s13,s23,s14,s24,s34,ISP.phi);
              }
          }
}


void GluonFusion::Jnlo(const double & sigma, const double & x1, const double & x2,const double &z, const double & lambda)
{
    const double  s12 = x1*x2*pow(Etot,2.0);
    const double  s13 = s12*(1.0-z)*lambda;
    const double  s23 = s12*(1.0-z)*(1.0-lambda);
    event_reconstructor.NLO_event_kinematics(sigma,x1,x2,z,s13,s23,ISP.phi);
}

void GluonFusion::JLO(const double & sigma)
{
    double x1=ISP.x1LO;
    double x2=ISP.x2LO;
    event_reconstructor.LO_event_kinematics(sigma,x1,x2);

}


//: ------- gluon fusion LO 
void GluonFusion::LO()
{
     //: the_sector->ME->epsilon_power doesn't matter here
     //: because LO has the structure 1+e+e^2
     if (the_sector->ME->epsilon_power()>=0)
          {
          double sigma_central =   pref_sgg
                              *ISP.measLO
                              *lumi->LL_LO(0)
                              *the_sector->sector_specific_prefactors_from_a_e_expansion();
          
          JLO(sigma_central);
          }
}


//: SOFT means virtual + soft real + renormalization
void GluonFusion::gg_NLO_SOFT()
{
    if (the_sector->ME->epsilon_power()==0)
    {
        double sigma_soft_nlo = pref_sgg
        *ISP.measLO
        *lumi->LL_LO(0)
        *the_sector->sector_specific_prefactors_from_a_e_expansion()
        * consts::pi_square;
        JLO(sigma_soft_nlo);
    }
    else if (the_sector->ME->epsilon_power()==1)
    {
        double sigma_soft_nlo = pref_sgg
        *ISP.measLO
        *lumi->LL_LO(0)
        *the_sector->sector_specific_prefactors_from_a_e_expansion()
        * (consts::pi_square - 3.0);
        JLO(sigma_soft_nlo);
    }
    else
    {
        book_production_event();
    }
}

//: DOUBLE SOFT
void GluonFusion::gg_NNLO_SOFT()
{
    double weight=pref_sgg
    *ISP.meas
    //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
    *lumi->LL(0)
    *the_sector->sector_specific_prefactors_from_a_e_expansion();
    double weightLO=pref_sgg
    *ISP.measLO
    //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
    *lumi->LL_LO(0)
    *the_sector->sector_specific_prefactors_from_a_e_expansion();
    double delta=weightLO;
    double DD0= (weight-weightLO)/(1.0-ISP.z)*0.0 ;
    double DD1= (weight-weightLO)/(1.0-ISP.z)*ISP.Log_1mz * 0.0;
    double DD2= (weight-weightLO)/(1.0-ISP.z)*pow(ISP.Log_1mz,2.0) * 0.0;
    double DD3= (weight-weightLO)/(1.0-ISP.z)*pow(ISP.Log_1mz,3.0) * 0.0;
    if (the_sector->ME->epsilon_power()==-2)
    {
        double sigma_m2=    -3.0*consts::pi_square*delta
        +(-33.0/4.0+1.0/2.0*consts::nf)*DD0
        +36.0*DD1;
        JLO(sigma_m2);
    }
    else if (the_sector->ME->epsilon_power()==-1)
    {
        double sigma_m1 =(-1.0/4.0*consts::pi_square
                          -315.0/4.0*consts::z3+27.0/4.0
                          -1.0/6.0*consts::nf*consts::pi_square
                          -11.0/12.0*consts::nf)
        *delta
        +(27.0/4.0*consts::pi_square-25.0+4.0/3.0*consts::nf)*DD0
        +(69.0-2.0*consts::nf)*DD1
        -108.0*DD2;
        JLO(sigma_m1);
    }
    else if (the_sector->ME->epsilon_power()==0)
    {
        double sigma_0 = 
        +(   -120.0*consts::z3
          +813.0/16.0
          -9.0/80.0*pow(consts::pi_square,2.0)
          +16.0/3.0*consts::pi_square
          -131.0/18.0*consts::nf
          -4.0/9.0*consts::nf*consts::pi_square
          +5.0/6.0*consts::nf*consts::z3)
        *delta
        +(   26.0/9.0*consts::nf
          +639.0/2.0*consts::z3
          -122.0/3.0
          -7.0/12.0*consts::nf*consts::pi_square
          +131.0/8.0*consts::pi_square)
        *DD0
        +(-57.0*consts::pi_square+136.0-16.0/3.0*consts::nf)*DD1
        +(-174.0+4.0*consts::nf)*DD3
        +168.0*DD3;
        JLO(sigma_0);
    }
    else
    {
        book_production_event();
    }
}



void GluonFusion::nlo_me()
{
     if (ISP.meas==0.0) {book_production_event();}//: note: no plus expansion here
     else
          {
          double shat =ISP.curs;
          double x1=ISP.x1;
          double x2 = ISP.x2;
          double z=ISP.z;
          double lambda=ISP.lambda;
          
          
          
          double weight = pref_sgg
          *ISP.meas
          *lumi->LL(0)
          *the_sector->sector_specific_prefactors_from_a_e_expansion()
          ;
          
          
          //: note that this is a finite sector, so no z-subtraction is needed.
          //: note that it also has one integral
          for (unsigned i=0;i<the_sector->ME->number_of_sectors_in_this_topology();i++)
               {
               double dummyres;
               pointer_to_Franz_gluon_fusion the_func= the_sector->ME->franz_func();
               (*the_func)(i+1,the_sector->ME->epsilon_power(),shat,x1,x2,  z,
                           -log_muf_sq_over_mh_sq, weight,
                           consts::nf,lambda,0.0,0.0,0.0,dummyres);
               }
          }
}




#include "fortran_interface_for_ggf_amplitudes.h"

//: HARD
void GluonFusion::gg_NLO_HARD()
{
     if (abs(the_sector->ME->epsilon_power())>2 or 1.0-ISP.z<1e-14)
          {
          book_production_event();
          }
     else
          {
          double dummyres;
          double weight = pref_sgg
                         *ISP.meas
                         *lumi->LL(0)
                         *1.0/(1.0-ISP.z)
                         *the_sector->sector_specific_prefactors_from_a_e_expansion()
                         ;
        
          double weightLO = pref_sgg
                         *ISP.measLO
                         *lumi->LL_LO(0)
                         *1.0/(1.0-ISP.z)
                         *the_sector->sector_specific_prefactors_from_a_e_expansion()
                         ;

          if (the_sector->ME->epsilon_power()== -1)
               {
               for (int ts=1;ts<3;ts++) //: ts=topology sector (there are 2 in rgg2ght1)
                    {
                    rgg2ght1(ts,-1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,
                             -log_muf_sq_over_mh_sq, weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,-weightLO,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    }
               }
          else if (the_sector->ME->epsilon_power()==0)
               {
               
               for (int ts=1;ts<3;ts++) //: ts=topology sector (there are 2 in rgg2ght1)
                    {
                    //: contribution from finite coefficient of rgg2ght1
                    rgg2ght1(ts,0,ISP.curs,ISP.x1,ISP.x2,  ISP.z,
                             -log_muf_sq_over_mh_sq, weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,0,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,-weightLO,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    
                    //: contribution from pole coefficient times logs of (1-z) and muf/mh
                    //: see documentation for explanation of extra_weight
                    double extra_weight = -2.0*ISP.Log_1mz ;
                    rgg2ght1(ts,-1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,
                             -log_muf_sq_over_mh_sq,
                             weight*extra_weight,
                             consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,
                             -weightLO*extra_weight,
                             consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    
                    }
               }
          else if (the_sector->ME->epsilon_power()==1)
               {
               for (int ts=1;ts<3;ts++) //: ts=topology sector (there are 2 in rgg2ght1)
                    {
                    
                    //e^1
                    rgg2ght1(ts,1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,
                             -log_muf_sq_over_mh_sq,
                             weight,
                             consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,
                             -weightLO,
                             consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                                        
                    double extra_weight = -2.0*ISP.Log_1mz;
                    rgg2ght1(ts,0,ISP.curs,ISP.x1,ISP.x2,  ISP.z,
                             -log_muf_sq_over_mh_sq,
                             weight*extra_weight,
                             consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,0,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,
                             -weightLO*extra_weight,
                             consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);

                    
                     double extra_weight2 = 2.0*pow(ISP.Log_1mz,2.0);
                    rgg2ght1(ts,-1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,
                             -log_muf_sq_over_mh_sq,
                             weight*extra_weight2,
                             consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,
                             -weightLO*extra_weight2,
                             consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    
                    }

               }
          }
          
}




void GluonFusion::NNLO_hard_no_subtraction()
{
     if (ISP.meas==0.0){book_production_event();}
     else
          {
          double dummyres;
          double shat =ISP.curs;
          double x1=ISP.x1;
          double x2 = ISP.x2;
          double z=ISP.z;
          double weight = pref_sgg
          *ISP.meas
          *lumi->LL(0)
          *the_sector->sector_specific_prefactors_from_a_e_expansion()
          //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
          //*pow(WC.c0,2.0)
          ;
          
          //z=0.1;x2=tau/z/x1;shat=pow(Model.higgs.m,2.0)/z; weight=1.0;//: franz's test point
          if (the_sector->ME->is_franz_topology())
               {
               for (int i=0;i<the_sector->ME->number_of_sectors_in_this_topology();i++)
                    {
                    pointer_to_Franz_gluon_fusion the_func = the_sector->ME->franz_func();
                    (* the_func)
                         (i+1,//:franz counts from one
                          the_sector->ME->epsilon_power(),
                          shat,x1,x2,  z,-log_muf_sq_over_mh_sq, weight,consts::nf,
                          xx_vegas[2],xx_vegas[3],xx_vegas[4],xx_vegas[5],dummyres);
                    
                    }
               
               }
          }
}


void GluonFusion::NNLO_hard_with_subtraction()
{
     //: this is double real kinematics in general, using maximum number of vegas variables
     NNLO_subtraction(xx_vegas[2],xx_vegas[3],xx_vegas[4],xx_vegas[5]);
}

void GluonFusion::NNLO_rv_with_subtraction()
{
     //: this is single real kinematics in general, using only lambda
     NNLO_subtraction(ISP.lambda,0.0,0.0,0.0);
}

void GluonFusion::NNLO_subtraction(const double& lambda1,const double& lambda2,const double& lambda3,const double& lambda4)
{
     if (the_sector->ME->epsilon_power()<-3
         or the_sector->ME->epsilon_power()>1
         or vars_too_close_to_edges(ISP.z,lambda1,lambda2,lambda3,lambda4))
         {
         //cout<<"\n cut::::::::::::::::::::::::::::::::::::::::::::::::::::::";
         book_production_event();
         }
     else
          {
          double dummyres;
          double shat =ISP.curs;
          double x1=ISP.x1;
          double x2 = ISP.x2;
          double z=ISP.z;
          double weight = pref_sgg
                         *ISP.meas
                         *lumi->LL(0)
                         *the_sector->sector_specific_prefactors_from_a_e_expansion()
                         //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                         *1.0/(1.0-ISP.z)
                         //*pow(WC.c0,2.0)
                         ;
          double weightLO = pref_sgg
                         *ISP.measLO
                         *lumi->LL(0)
                         *the_sector->sector_specific_prefactors_from_a_e_expansion()
                         //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                         *1.0/(1.0-ISP.z)
                         //*pow(WC.c0,2.0)
                         ;
//          if (weightLO==0.0 and weight==0.0)
//              {
//              //cout<<"\noooooooooooooooooooooooooooooooooooooooooooooooooooooo";
//              }
//          else
//              {
//              cout<<"\n"<<setprecision(16)<<setw(40)<<ISP.z
//                    <<setw(40)<<ISP.x1<<setw(40)<<ISP.x2
//                    <<setw(40)<<lambda1<<setw(40)<<lambda2
//                    <<setw(40)<<lambda3<<setw(40)<<lambda4
//                ;
//              }
          if (the_sector->ME->is_franz_topology())
               {
               for (int m=-3;m<the_sector->ME->epsilon_power()+1;m++)
                    {
                    double thelog;
                    int k=the_sector->ME->epsilon_power()-m;
                    if (k>0)
                         {
                         double factorials[3]={1.0,2.0,6.0};
                         thelog=pow(-the_sector->ME->epsilon_exponent_in_z_subtraction*log(1.0-ISP.z),k)/factorials[k-1];
                         }
                    else if (k==0)
                         {
                         thelog=1.0;
                         }
                    else
                         {
                         cout<<"\n error in NNLO_hard_with_subtraction: coefficient with negative exponent of log(1-z) just appeared. I exit!"<<endl;
                         exit(1);
                         }
                    for (int i=the_sector->ME->min_sec();i<the_sector->ME->max_sec()+1;i++)
                         {
                         dummyres=0.0;
                         pointer_to_Franz_gluon_fusion the_func = the_sector->ME->franz_func();
                         
                         
                         (* the_func)
                         (i,//:franz counts from one
                          m,
                          shat,x1,x2,  z,-log_muf_sq_over_mh_sq, weight*thelog,consts::nf,
                          lambda1,lambda2,lambda3,lambda4,dummyres);
                         
                         dummyres=0.0;
                         (*the_func)
                         (i,//:franz counts from one
                          m,
                          ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,-log_muf_sq_over_mh_sq, -weightLO*thelog,consts::nf,
                          lambda1,lambda2,lambda3,lambda4,dummyres);
                         }
                    }
               }
          }
}

void GluonFusion::NNLO_RR_brute_force_wrap()
{
    //: this is double real kinematics in general, using maximum number of vegas variables
    NNLO_RR_brute_force(xx_vegas[2],xx_vegas[3],xx_vegas[4],xx_vegas[5]);
}


void GluonFusion::NNLO_RR_brute_force(const double& lambda1,const double& lambda2,const double& lambda3,const double& lambda4)
{
    if (the_sector->ME->epsilon_power()<-3
        or the_sector->ME->epsilon_power()>1
        or vars_too_close_to_edges(ISP.z,lambda1,lambda2,lambda3,lambda4))
        {
        //cout<<"\n cut::::::::::::::::::::::::::::::::::::::::::::::::::::::";
        book_production_event();
        }
    else
        {
        double dummyres;
        double shat =ISP.curs;
        double x1=ISP.x1;
        double x2 = ISP.x2;
        double z=ISP.z;
        double weight = pref_sgg
        *ISP.meas
        *lumi->LL(0)
        *the_sector->sector_specific_prefactors_from_a_e_expansion()
        *1.0/(1.0-ISP.z)
        ;
        double weightLO = pref_sgg
        *ISP.measLO
        *lumi->LL(0)
        *the_sector->sector_specific_prefactors_from_a_e_expansion()
        *1.0/(1.0-ISP.z)
        ;
                   for (int m=-3;m<the_sector->ME->epsilon_power()+1;m++)
                {
                double thelog;
                int k=the_sector->ME->epsilon_power()-m;
                if (k>0)
                    {
                    double factorials[3]={1.0,2.0,6.0};
                    thelog=pow(-the_sector->ME->epsilon_exponent_in_z_subtraction*log(1.0-ISP.z),k)/factorials[k-1];
                    }
                else if (k==0)
                    {
                    thelog=1.0;
                    }
                else
                    {
                    cout<<"\n error in NNLO_hard_with_subtraction: coefficient with negative exponent of log(1-z) just appeared. I exit!"<<endl;
                    exit(1);
                    }
                //cout<<"\n"<<the_sector->ME->fr_.size()<<" sectors to run"<<endl;
                for (int RRsec=0;RRsec<the_sector->ME->fr_.size();RRsec++)
                    {
                    FranzBinder* franz_sec = the_sector->ME->fr_[RRsec];
                    pointer_to_Franz_gluon_fusion the_func = franz_sec->func();
                    for (int i=0;i<franz_sec->number_of_sectors();i++)
                        {
                        dummyres=0.0;
                        
                        
                        
                        (* the_func)
                        (i+1,//:franz counts from one
                         m,
                         shat,x1,x2,  z,-log_muf_sq_over_mh_sq, weight*thelog,consts::nf,
                         lambda1,lambda2,lambda3,lambda4,dummyres);
                        
                        dummyres=0.0;
                        (*the_func)
                        (i+1,//:franz counts from one
                         m,
                         ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,-log_muf_sq_over_mh_sq, -weightLO*thelog,consts::nf,
                         lambda1,lambda2,lambda3,lambda4,dummyres);
                        }
                    }
                }
            }
        
}




bool GluonFusion::vars_too_close_to_edges(const double&z,const double&lambda1
                                          ,const double&lambda2
                                          ,const double&lambda3
                                          ,const double&lambda4)
{
    const double tc = 1e-10;
    if (z<tc or 1.0-z<tc)
        {
        //cout<<"\n cut z="<<z;
        return true;
        }
    if (lambda1<tc or 1.0-lambda1<tc)
        {
        //cout<<"\n cut lambda1="<<lambda1;
        return true;
        }
    if (lambda2<tc or 1.0-lambda2<tc)
        {
        //cout<<"\n cut lambda2="<<lambda2;
        return true;
        }
    if (lambda3<tc or 1.0-lambda3<tc)
        {
        //cout<<"\n cut lambda3="<<lambda3;
        return true;
        }
    if (lambda4<tc or 1.0-lambda4<tc)
        {
        //cout<<"\n cut lambda4="<<lambda4;
        return true;
        }
    
    return false;

}


//: exact MEs below

void GluonFusion::LO_exact()
{
     if (the_sector->ME->epsilon_power()>=0)
          {
          double sigma_central =   pref_sgg
          *ISP.measLO
          *lumi->LL_LO(0)
          *the_sector->sector_specific_prefactors_from_a_e_expansion()
          *exact_coefficients -> LO_epsilon(the_sector->ME->epsilon_power());
          JLO(sigma_central);
          }
}

void GluonFusion::NLO_soft_exact()
{
     if (the_sector->ME->epsilon_power()==0) //: we do not have an implementation of the e-pieces
          {
          double sigma_central =   pref_sgg
          *ISP.measLO
          *lumi->LL_LO(0)
          *the_sector->sector_specific_prefactors_from_a_e_expansion()
          *exact_coefficients -> NLO_epsilon(the_sector->ME->epsilon_power());
          JLO(sigma_central);
          }
}




void GluonFusion::gg_NLO_hard_exact()
{
    if (abs(the_sector->ME->epsilon_power())>2)
        {
        book_production_event();
        }
    else
        {
        double common_weight_factors = pref_sgg * 1.0/(1.0-ISP.z)
                * the_sector->sector_specific_prefactors_from_a_e_expansion();
        double weight = common_weight_factors * ISP.meas * lumi->LL(0);
        double weightLO = common_weight_factors * ISP.measLO * lumi->LL_LO(0);
          
        if (the_sector->ME->epsilon_power()== -1)
            {
            gg2gh_exact_Q_m1(weight,ISP.z, ISP.x1, ISP.x2);
            gg2gh_exact_Q_m1(-weightLO,1.0, ISP.x1LO, ISP.x2LO);
            }
        else if (the_sector->ME->epsilon_power()==0)
            {
            double weight_log = -2.0*log(1.0-ISP.z);
            gg2gh_exact_Q_m1(weight * weight_log,   ISP.z,  ISP.x1,   ISP.x2);
            gg2gh_exact_Q_m1(-weightLO * weight_log,1.0,    ISP.x1LO, ISP.x2LO);
            gg2gh_exact_Q_fin(weight,
                              ISP.z,  ISP.x1,     ISP.x2,   ISP.lambda);
            gg2gh_exact_Q_fin(-weightLO,
                              1.0,    ISP.x1LO,   ISP.x2LO, ISP.lambda);
            }
        }
     
}


void GluonFusion::gg2gh_exact_Q_m1(const double& weight,const double& z,
                             const double& x1,const double&x2)
{
    //: here only the pole from the collinear term contributes
    double C = (-1.0)*3.0*pow(1.0-z+z*z,2.0)/z
                                    *exact_coefficients -> LO_epsilon(0);
    Jnlo(C * weight, x1, x2, z, 0.0);
    Jnlo(C * weight, x1, x2, z, 1.0);

}

void GluonFusion::gg2gh_exact_Q_fin(const double& weight,const double& z,
                              const double& x1,const double&x2,
                                    const double& lambda)
{
    double pgg = pow(1.0-z+z*z,2.0);
    double F = 3.0* pgg * exact_coefficients->LO_epsilon(0);
    double F_epsilon = 3.0* pgg * exact_coefficients->LO_epsilon(1);
    
    //cout<<"\n"<<exact_coefficients -> LO_epsilon(0);
    if (z!=1.0)
        {
        
        double H1 = 3.0/2.0*pow(z,4.0) *sum_of_abs_sq_of_Aqi(z,lambda,&Model);
        double H = H1/z/lambda/(1.0-lambda);
        Jnlo(H * weight,    x1, x2,z,lambda);
        
        double C0 = -F/z/lambda ;
        double C1 = -F/z/(1.0-lambda);
        double C_epsilon = -  F_epsilon / z;
        Jnlo(C0 * weight + C_epsilon * weight,   x1, x2,z,0.0);
        Jnlo(C1 * weight + C_epsilon * weight,   x1, x2,z,1.0);
        
        double Crem = (-1.0) * F/z * (log(z)+log_muf_sq_over_mh_sq);
        Jnlo(Crem * weight, x1, x2,z,0.0);
        Jnlo(Crem * weight, x1, x2,z,1.0);
        }
    else if (z==1.0)
        {
        double Crem =  - F *  (log_muf_sq_over_mh_sq);
        double C_epsilon = -  F_epsilon;

        JLO(2.0*Crem * weight + 2.0*C_epsilon*weight);
        }
}


void GluonFusion::qg_NLO_hard_exact()
{
    if (abs(the_sector->ME->epsilon_power())>2)
        {
        book_production_event();
        }
    else
        {
        double weight = pref_sgg
                * the_sector->sector_specific_prefactors_from_a_e_expansion()
                * ISP.meas
                * lumi->LL(0);
        
        if (the_sector->ME->epsilon_power()== -1)
            {
            qg2qh_exact_Q_m1(weight,ISP.z, ISP.x1, ISP.x2);
            }
        else if (the_sector->ME->epsilon_power()==0)
            {
            qg2qh_exact_Q_fin(weight,
                              ISP.z,  ISP.x1,     ISP.x2,   ISP.lambda);
            
            }
        }
    
}

void GluonFusion::qg2qh_exact_Q_m1(const double& weight,const double& z,
                                   const double& x1,const double&x2)
{
    //: here only the pole from the collinear term contributes
    double Pqg = (1.0+pow(1.0-z,2.0))/z;
    double C = -2.0/3.0 * Pqg * exact_coefficients -> LO_epsilon(0);
    Jnlo(C * weight, x1, x2, z, 0.0);
    
}

void GluonFusion::qg2qh_exact_Q_fin(const double& weight,const double& z,
                                    const double& x1,const double&x2,
                                    const double& lambda)
{
    double Pqg_lambda = (1.0+pow(1.0-z,2.0) * (1.0-lambda))/z;
    double Pqg = (1.0+pow(1.0-z,2.0))/z;
    
    
    double ysp = -(1.0-z)*lambda;
    double H = sum_of_abs_sq_of_Aqqgh(1.0/ysp,&Model) * Pqg_lambda / lambda;
    Jnlo(H * weight,    x1, x2,z,lambda);
    
    double C0 =  exact_coefficients -> LO_epsilon(0) * Pqg / lambda ;
    double Cpole = exact_coefficients -> LO_epsilon(0) * (-2.0/3.0);
    double Cpole_epsilon = exact_coefficients->LO_epsilon(1) * (-2.0/3.0);
    double Crem = Cpole *( -z
                          + Pqg * log(z/pow(1.0-z,2.0))
                          + Pqg * log_muf_sq_over_mh_sq)
                + Cpole_epsilon * Pqg;
    
    
    Jnlo( - C0 * weight +  Crem * weight ,   x1, x2,z,0.0);


}



void GluonFusion::gq_NLO_hard_exact()
{
    if (abs(the_sector->ME->epsilon_power())>2)
        {
        book_production_event();
        }
    else
        {
        double weight = pref_sgg
        * the_sector->sector_specific_prefactors_from_a_e_expansion()
        * ISP.meas
        * lumi->LL(0);
        
        if (the_sector->ME->epsilon_power()== -1)
            {
            gq2qh_exact_Q_m1(weight,ISP.z, ISP.x1, ISP.x2);
            }
        else if (the_sector->ME->epsilon_power()==0)
            {
            gq2qh_exact_Q_fin(weight,
                              ISP.z,  ISP.x1,     ISP.x2,   ISP.lambda);
            
            }
        }
    
}

void GluonFusion::gq2qh_exact_Q_m1(const double& weight,const double& z,
                                   const double& x1,const double&x2)
{
    //: here only the pole from the collinear term contributes
    double Pqg = (1.0+pow(1.0-z,2.0))/z;
    double C = -2.0/3.0 * Pqg * exact_coefficients -> LO_epsilon(0);
    Jnlo(C * weight, x1, x2, z, 1.0);
    
}

void GluonFusion::gq2qh_exact_Q_fin(const double& weight,const double& z,
                                    const double& x1,const double&x2,
                                    const double& lambda)
{
    double Pqg_lambda = (1.0+pow(1.0-z,2.0) * lambda)/z;
    double Pqg = (1.0+pow(1.0-z,2.0))/z;
    
    
    double ysp = -(1.0-z)*(1.0-lambda);
    double H = sum_of_abs_sq_of_Aqqgh(1.0/ysp,&Model) * Pqg_lambda / (1.0-lambda);
    Jnlo(H * weight,    x1, x2,z,lambda);
    
    double C0 =  exact_coefficients -> LO_epsilon(0) * Pqg / (1.0-lambda) ;
    double Cpole = exact_coefficients -> LO_epsilon(0) * (-2.0/3.0);
    double Cpole_epsilon = exact_coefficients->LO_epsilon(1) * (-2.0/3.0);
    double Crem = Cpole *( -z
                          + Pqg * log(z/pow(1.0-z,2.0))
                          + Pqg * log_muf_sq_over_mh_sq)
    + Cpole_epsilon * Pqg;
    Jnlo( - C0 * weight +  Crem * weight ,   x1, x2,z,1.0);
    
    
}


void GluonFusion::qqbar_NLO_hard_exact()
{
    if (the_sector->ME->epsilon_power()!=0)
        {
        book_production_event();
        }
    else
        {
        double weight = pref_sgg
        * the_sector->sector_specific_prefactors_from_a_e_expansion()
        * ISP.meas
        * lumi->LL(0);
        //old inclusive implementation
//        double sigma = 32.0 / 27.0 * pow(1.0-ISP.z,3.0) / ISP.z
//                        *sum_of_abs_sq_of_Aqqgh(ISP.z,&Model);
        double sigma = 32.0 / 27.0 * pow(1.0-ISP.z,3.0) / ISP.z
            *(pow(ISP.lambda,2.)+pow(1.-ISP.lambda,2.))*3./2.
            *sum_of_abs_sq_of_Aqqgh(ISP.z,&Model);
        Jnlo(sigma * weight, ISP.z,  ISP.x1,     ISP.x2,   ISP.lambda);
        }
    
}

//: ewk matrix elements below
//void GluonFusion::NLO_ewk_soft()
//{
//    if (the_sector->ME->epsilon_power()==0)
//        {
//        double sigma_central =   pref_sgg
//        *ISP.measLO
//        *lumi->LL_LO(0)
//        *the_sector->sector_specific_prefactors_from_a_e_expansion()
//        *electroweak_coefficients->LO();
//        JLO(sigma_central);
//        }
//}
//
//
//void GluonFusion::NLO_ewk_soft_exact()
//{
//    if (the_sector->ME->epsilon_power()==0)
//        {
//        double sigma_central =   pref_sgg
//        *ISP.measLO
//        *lumi->LL_LO(0)
//        *the_sector->sector_specific_prefactors_from_a_e_expansion()
//        *exact_coefficients -> LO_epsilon(0)
//        *electroweak_coefficients->LO();
//        JLO(sigma_central);
//        }
//}

//: ewk corrections to h+j (a_s^2 * m_t/v * a_w * m_v/v)
//
//
//      ------>---- ~~~~~~~~ - - - - -  - - - - - ---- ggggggggggg
//                |        }                      | /
//                |        {                      |/
//                |        }                      g
//                |        {                      g
//      gggggggggg------>-------------- ------->----------->------


void GluonFusion::NLO_ewk_uubar_h_plus_jet()
{
    NLO_ewk_h_plus_j_fork("EwkUUbar");
}

void GluonFusion::NLO_ewk_ddbar_h_plus_jet()
{
    NLO_ewk_h_plus_j_fork("EwkDDbar");

}


void GluonFusion::NLO_ewk_ug_h_plus_jet()
{
    NLO_ewk_h_plus_j_fork("EwkUG");
}

void GluonFusion::NLO_ewk_dg_h_plus_jet()
{
    NLO_ewk_h_plus_j_fork("EwkDG");
}


void GluonFusion::NLO_ewk_gu_h_plus_jet()
{
    NLO_ewk_h_plus_j_fork("EwkGU");
    
}

void GluonFusion::NLO_ewk_gd_h_plus_jet()
{
    NLO_ewk_h_plus_j_fork("EwkGD");
    
}

void GluonFusion::NLO_ewk_h_plus_j_fork(const string& channel_selector)
{
    double ew_me;
    if (channel_selector == "EwkUUbar") 
        ew_me = electroweak_coefficients->EwkUUbar(ISP.curs,ISP.z,ISP.lambda);
    else if (channel_selector == "EwkDDbar") 
        ew_me = electroweak_coefficients->EwkDDbar(ISP.curs,ISP.z,ISP.lambda);
    else if (channel_selector == "EwkUG" or channel_selector=="EwkGU") 
        ew_me = electroweak_coefficients->EwkUG(ISP.curs,ISP.z,ISP.lambda);
    else if (channel_selector == "EwkDG" or channel_selector=="EwkGD" ) 
        ew_me = electroweak_coefficients->EwkDG(ISP.curs,ISP.z,ISP.lambda);
    else {cout<<"\nFatal error: channel_selector in NLO_ewk_h_plus_j_fork not recognized : "<<channel_selector<<endl;exit(0);}
    double my_lambda = ISP.lambda;
    if (channel_selector=="EwkGU" or channel_selector=="EwkGD")
        my_lambda = 1.-ISP.lambda;
        
    if (the_sector->ME->epsilon_power()==0)
    {
        if (ISP.meas==0.0) Jnlo(0.0,ISP.z,  ISP.x1,     ISP.x2,   ISP.lambda);
        else
        {
            double sigma =   pref_sgg
            *ISP.meas
            *lumi->LL(0)
            *the_sector->sector_specific_prefactors_from_a_e_expansion()
            *ew_me;
            Jnlo(sigma , ISP.z,  ISP.x1,     ISP.x2,my_lambda);
//            cout<<"\n pref_sgg = "<<pref_sgg<<" "<<ISP.meas<<" "<<lumi->LL(0)<<" "<<the_sector->sector_specific_prefactors_from_a_e_expansion()
//            <<" "<<sigma;
        }
    }
}








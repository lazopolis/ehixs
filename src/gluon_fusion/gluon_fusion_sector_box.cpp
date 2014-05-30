#include "gluon_fusion_sector_box.h"




//------------------------------------------------------------------------------

GluonFusionSectorBox::GluonFusionSectorBox(const WilsonCoefficients& WC, const BetaConstants& beta,const double& log_mur_sq_over_muf_sq,const string& rr_treatment)
{
    _WC = WC;
    _beta = beta;
    _log_mur_sq_over_muf_sq = log_mur_sq_over_muf_sq;
    //cout<<"\n[GluonFusionSectorBox] : setting up MatrixElements"<<endl;
    
    available_matrix_elements = new GluonFusionMatrixElementBox(rr_treatment);
    //cout<<"\n[GluonFusionSectorBox] : there are "
    //    <<available_matrix_elements->size()<<" possible Matrix Elements"<<endl;
    
    _av_partons.push_back("gluon");
    build_sectors("gluon","gluon");
    // cout<<"\n[GluonFusionSectorBox] : after gg, "<<available_sectors.size()
    //    <<" sectors"<<endl;
    _av_partons.push_back("quark");
    _av_partons.push_back("antiquark");
    build_sectors("quark","gluon");
    build_sectors("gluon","quark");
    //cout<<"\n[GluonFusionSectorBox] : after qg, "<<available_sectors.size()
    //<<" sectors"<<endl;
    build_sectors("quark","antiquark");
    //cout<<"\n[GluonFusionSectorBox] : after q qbar, "<<available_sectors.size()
    //<<" sectors"<<endl;
    build_sectors("quark","quark");
    //cout<<"\n[GluonFusionSectorBox] : after qq, "<<available_sectors.size()
    //<<" sectors"<<endl;
    _av_partons.push_back("quark2");
    build_sectors("quark","quark2");
    //cout<<"\n[GluonFusionSectorBox] : after q1q2, "<<available_sectors.size()
    //<<" sectors"<<endl;
    _av_partons.push_back("up");
    _av_partons.push_back("upbar");
    build_sectors("up","upbar");
    build_sectors("up","gluon");
    build_sectors("gluon","up");
    _av_partons.push_back("down");
    _av_partons.push_back("downbar");
    build_sectors("down","downbar");
    build_sectors("down","gluon");
    build_sectors("gluon","down");
    
    
}

void GluonFusionSectorBox::build_sectors(const string &parton_left, const string &parton_right)
{
    //cout<<"\n building "<<parton_left<<" , "<<parton_right<<endl;
    //: LO : partitions of 0 in 3
    build_sectors_with_fixed_a_order(0,0,0,parton_left,parton_right);
    //: NLO : partitions of 1 in 3
    build_sectors_with_fixed_a_order(0,0,1,parton_left,parton_right);
    build_sectors_with_fixed_a_order(0,1,0,parton_left,parton_right);
    build_sectors_with_fixed_a_order(1,0,0,parton_left,parton_right);
    //: partitions of 2 in 3
    build_sectors_with_fixed_a_order(0,0,2,parton_left,parton_right);
    build_sectors_with_fixed_a_order(1,0,1,parton_left,parton_right);
    build_sectors_with_fixed_a_order(0,1,1,parton_left,parton_right);
    build_sectors_with_fixed_a_order(1,1,0,parton_left,parton_right);
    build_sectors_with_fixed_a_order(2,0,0,parton_left,parton_right);
    build_sectors_with_fixed_a_order(0,2,0,parton_left,parton_right);
}

void GluonFusionSectorBox::build_sectors_with_fixed_a_order(int f1order,int f2order,int Sorder,const string&pleft,const string &pright)
{
    //cout<<"\n building "<<pleft<<" , "<<pright<<"\twith alpha_powers "<<f1order<<" "<<f2order<<" "<<Sorder<<endl;
    vector<FFF> possible_f1=give_possible_F(pleft,f1order);
    vector<FFF> possible_f2=give_possible_F(pright,f2order);
    
    for (int i=0;i<possible_f1.size();i++)
    {
        //cout<<"\nchecking left";
        if (possible_f1[i].is_valid())
        {
            for (int j=0;j<possible_f2.size();j++)
            {
                // cout<<"\nchecking right";
                if (possible_f2[j].is_valid())
                {
                    
                    
                    build_sectors_with_fixed_a_order_and_pdfs(possible_f1[i],possible_f2[j],Sorder+2);
                }
            }
        }
    }
    
}

vector<FFF> GluonFusionSectorBox::give_possible_F(const string & parton,int f1order)
{
    vector<FFF> possible_f;
    for (unsigned i=0;i<_av_partons.size();i++)
    {
        possible_f.push_back(FFF(_av_partons[i],parton,f1order));
        if (f1order==2)
        {
            possible_f.push_back(FFF(_av_partons[i],parton,f1order,1));//: adding the 2_1 pdfs
        }
    }
    return possible_f;
}

void GluonFusionSectorBox::build_sectors_with_fixed_a_order_and_pdfs(const FFF & F1,const FFF & F2,int Sorder)
{
    vector<MatrixElement*> matching_mes;
    for (int ime=0;ime<available_matrix_elements->size();ime++)
    {
        //:checking whether the pdfs' partons match with the ME's
        string pleft = F1.parton_i;
        string pright= F2.parton_i;
        //:bending rules related to quark-antiquark symmetry etc
        if (pleft=="gluon" and pright=="antiquark") pright="quark";
        if (pleft=="antiquark" and pright=="gluon") pleft="quark";
        if (pleft=="gluon" and pright=="quark2") pright="quark";
        if (pleft=="quark2" and pright=="gluon") pleft="quark";
        if (pleft=="antiquark" and pright=="quark") {pleft="quark"; pright="antiquark";}
        if (pleft=="upbar" and pright=="up") {pleft="up"; pright="upbar";}
        //: the check
        if (pleft==available_matrix_elements->give_me(ime)->parton_i()
            and pright==available_matrix_elements->give_me(ime)->parton_j()
            )
        {
            // for electroweak matrix elements we do not want to have collinear
            // counterterms
            bool is_ewk = available_matrix_elements->give_me(ime)->alpha_ew_power()>0;
            bool the_pdfs_are_not_LO = F1.order + F2.order >0;
            bool no_counterterm_case = is_ewk and the_pdfs_are_not_LO;
            if (not(no_counterterm_case))
            {
                matching_mes.push_back(available_matrix_elements->give_me(ime));
            }
        }
    }
    if (matching_mes.size()>0)
    {
        for (int epsilon=-3;epsilon<3;epsilon++)
        {
            build_sectors_with_fixed_a_order_e_order_and_pdfs( F1, F2, Sorder,epsilon,matching_mes);
        }
    }
}

void GluonFusionSectorBox::build_sectors_with_fixed_a_order_e_order_and_pdfs(
         const FFF & F1,
         const FFF & F2,
         int Sorder,int Eorder,
         const vector<MatrixElement*> & matching_mes)
{
    
    
    // *** here starts the operating version
    vector<ExpansionTerm*> WCET_vector_original;
    vector<ExpansionTerm*> ZREN_vector;
    
    vector<ExpansionTerm*> AREN_vector;
    vector<ExpansionTerm*> AREN_vector_trivial;
    //: constructing the wilson coefficient factor [a*(c0+a*c1+a^2*c2)]^2
    WCET_vector_original.push_back(
            new ExpansionTerm("(c0^2 a^2)",
                    pow(abs(_WC.c0),2.0),
                    2,0));
    WCET_vector_original.push_back(
            new ExpansionTerm("(2*c0*c1* a^3)",
         2.0*real(_WC.c0*conj(_WC.c1)),
         3,0));
    WCET_vector_original.push_back(
            new ExpansionTerm("[(c1^2 + 2*c0*c2)*a^4]",
         pow(abs(_WC.c1),2.0) + 2.0*real(_WC.c0*conj(_WC.c2)),
         4,0));
    vector<ExpansionTerm*> WCET_vectorEW;
    WCET_vectorEW.push_back(
        new ExpansionTerm("(cm0^2 a^2)",
          pow(abs(_WC.c0),2.0)
          -pow(abs(_WC.c0_qcd_only),2.0),
          2,0));
    WCET_vectorEW.push_back(
        new ExpansionTerm("(2*cm0*cm1* a^3)",
          2.0*real(_WC.c0*conj(_WC.c1))
          -2.0*real(_WC.c0_qcd_only*conj(_WC.c1_qcd_only)),
          3,0));
    WCET_vectorEW.push_back(
        new ExpansionTerm("[(cm1^2 + 2*cm0*cm2)*a^4]",
          pow(abs(_WC.c1),2.0) + 2.0*real(_WC.c0*conj(_WC.c2))
          -pow(abs(_WC.c1_qcd_only),2.0) 
                + 2.0*real(_WC.c0_qcd_only*conj(_WC.c2_qcd_only)),
          4,0));
    
    
    
    //: a^2 * Z^2 = { a * (1+a*b0*L + a^2*b1*L + a^2*b0^2*L^2) * ( 1 - a*b0/e + a^2 * b0^2/e^2-a^2*b1/e) }^2
    ZREN_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
    ZREN_vector.push_back(new ExpansionTerm("(a)*(-2*b0/e)",-2.0*_beta.zero,1,-1));
    ZREN_vector.push_back(new ExpansionTerm("(a)*(2*b0*L)",2.0*_beta.zero*_log_mur_sq_over_muf_sq,1,0));
    
    ZREN_vector.push_back(new ExpansionTerm("(a^2)*(3*b0^2/e^2)",3.0*pow(_beta.zero,2.0),2,-2));
    ZREN_vector.push_back(new ExpansionTerm("(a^2)*(-2*b1/e)",-2.0*_beta.one,2,-1));
    ZREN_vector.push_back(new ExpansionTerm("(a^2)*(-4*b0^2*L/e)",-4.0*pow(_beta.zero,2.0)*_log_mur_sq_over_muf_sq,2,-1));
    ZREN_vector.push_back(new ExpansionTerm("(a^2)*(3*b0^2*L^2)",3.0*pow(_beta.zero,2.0)*pow(_log_mur_sq_over_muf_sq,2),2,0));
    ZREN_vector.push_back(new ExpansionTerm("(a^2)*(2*b1*L)",2.0*_beta.one*_log_mur_sq_over_muf_sq,2,0));
    
    AREN_vector_trivial.push_back(new ExpansionTerm("(1)",1.0,0,0));
    
    AREN_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
    AREN_vector.push_back(new ExpansionTerm("(a)*(-b0/e)",-_beta.zero,1,-1));
    AREN_vector.push_back(new ExpansionTerm("(a)*b0*L)",_beta.zero*_log_mur_sq_over_muf_sq,1,0));
    
    for (int ime=0;ime<matching_mes.size();ime++)
    {
        
        if (matching_mes[ime]->alpha_power()<=Sorder)
        {
            vector<ExpansionTerm*> WCET_vector = WCET_vector_original;
            MatrixElement* cur_me=matching_mes[ime];
            // if the me_approximation of the cur_me is "exact", trivialize WC
            if (cur_me->me_approximation() == "exact")
            {
                WCET_vector.clear();
                //exact matrix element: the WCs should be set to 1.0
                WCET_vector.push_back(new ExpansionTerm("(a^2)",1.0,2,0));
            }
            else
            {
                // effective matrix element:
                // if the WC.exact = true it means we are in
                // a run with exact matrix elements
                // so we need to do the LO and NLO effective sectors only if
                // if WC.ew_soft = true. Then there are ew soft
                // contributions
                // so we need to keep the non-pure QCD pieces
                
                if (_WC.exact and cur_me->alpha_power()<2)
                {
                    
                    // if there are no ew soft, we kill
                    // the matrix element alltogether
                    WCET_vector.clear();
                    // if there are ew soft contributions then we modify the
                    // WCET vector as follows
                    if (_WC.ew_soft)
                    {
                        WCET_vector = WCET_vectorEW;
                    }
                }
                
            }
            //: assigning trivial renormalization factor (1)
            vector<ExpansionTerm*> cur_aren=AREN_vector_trivial;
            if (cur_me->alpha_power()==1) cur_aren=AREN_vector;
            for (int iwc=0;iwc<WCET_vector.size();iwc++)
            {
                for (int izren=0;izren<ZREN_vector.size();izren++)
                {
                    for (int iaren=0;iaren<cur_aren.size();iaren++)
                    {
                        
                        int total_alpha_order = WCET_vector[iwc]->give_a_power()
                        +ZREN_vector[izren]->give_a_power()
                        +cur_aren[iaren]->give_a_power()
                        +cur_me->alpha_power();
                        int total_epsilon_order = WCET_vector[iwc]->give_e_power()
                        +ZREN_vector[izren]->give_e_power()
                        +cur_aren[iaren]->give_e_power()
                        +cur_me->epsilon_power();
                        
                        if (total_alpha_order==Sorder
                            and total_epsilon_order==Eorder)
                        {
                            // ew corrections to h+j shouldn't be multiplied
                            // by a*b0*L
                            if (cur_me->alpha_ew_power()>0
                                and (cur_aren[iaren]->give_a_power()
                                     +ZREN_vector[izren]->give_a_power()
                                     +WCET_vector[iwc]->give_a_power())>2)
                            {
                                continue;
                            }
                            else
                            {
                                vector<ExpansionTerm*> factors;
                                factors.push_back(WCET_vector[iwc]);
                                factors.push_back(ZREN_vector[izren]);
                                factors.push_back(cur_aren[iaren]);
                                
                                available_sectors.push_back(new SimpleSector(F1,F2,factors,cur_me));
                            }
                            //cout<<" : success";
                        }
                        else
                        {
                            //cout<<"failure because (a,e)= ("<<total_alpha_order<<","<<total_epsilon_order
                            //<<") != ("<<Sorder<<","<<Eorder<<")";
                        }
                    }
                }
            }
        }
        
    }
}


vector<string> GluonFusionSectorBox::give_sector_names(const string & pleft,const string & pright,const string & myorder,const int & requested_epsilon_power,
                                                       const string & me_approx)
{
    int requested_alpha_power=2;
    if (myorder=="NLO") requested_alpha_power=3;
    if (myorder=="NNLO") requested_alpha_power=4;
    vector<string> all_names;
    for (int i=0;i<available_sectors.size();i++)
    {
        if (pleft==available_sectors[i]->F1.parton_from and pright==available_sectors[i]->F2.parton_from and
            available_sectors[i]->alpha_power==requested_alpha_power
            and available_sectors[i]->epsilon_power==requested_epsilon_power
            and available_sectors[i]->ME->me_approximation()==me_approx
            )
        {
            cout<<"\n\t\t\t***** me_approx = "
            <<available_sectors[i]->ME->me_approximation()
            <<" for sector "<<available_sectors[i]->name<<endl;
            all_names.push_back(available_sectors[i]->name);
        }
        /* else if (pleft==available_sectors[i]->F1.parton_from and pright==available_sectors[i]->F2.parton_from)
         {
         cout<<"\n-- sector failed "<<available_sectors[i]->name;
         }
         */
    }
    return all_names;
}


vector<SimpleSector*> GluonFusionSectorBox::give_necessary_sectors(const UserInterface & UI)
{
    // this is where the runcard semantics are resolved
    // qcd_perturbative_order
    int min_a_power_requested,max_a_power_requested;
    // default  option: qcd_perturbative_order
    if (UI.qcd_perturbative_order == "LO")
    {
        min_a_power_requested = 2;
        max_a_power_requested = 2;
    }
    if (UI.qcd_perturbative_order == "NLO")
    {
        min_a_power_requested = 2;
        max_a_power_requested = 3;
    }
    if (UI.qcd_perturbative_order == "NNLO")
    {
        min_a_power_requested = 2;
        max_a_power_requested = 4;
    }
    // advanced  : alpha_s_power. If defined by user it fixes the a_s order
    if (UI.alpha_s_power != -1)
    {
        min_a_power_requested = UI.alpha_s_power;
        max_a_power_requested = UI.alpha_s_power;
    }
    // default  option ew_h_plus_j
    int min_aw_power_requested = 0;
    int max_aw_power_requested = 0;
    if (UI.ew_h_plus_j)
    {
        max_aw_power_requested = 1;
    }
    // advanced option ew_h_plus_j_only
    if (UI.only_ew_h_j)
    {
        min_aw_power_requested = 1;
        max_aw_power_requested = 1;
    }
    
    cout<<"\n[GluonFusionSectorBox] : looking for necessary sectors"<<endl;
    vector<SimpleSector*> necessary_sectors;
    
    for (int i=0;i<available_sectors.size();i++)
    {
        SimpleSector* the_sector = available_sectors[i];
        bool a_power_fits = the_sector->alpha_power>=min_a_power_requested
        and the_sector->alpha_power<=max_a_power_requested;
        if (not(a_power_fits)) continue;
        
        bool initial_state_partons_fit=(
                                        UI.Fleft==the_sector->F1.parton_from
                                        and UI.Fright==the_sector->F2.parton_from
                                        )
        or
        (UI.Fleft=="none" and UI.Fright=="none");
        if (not(initial_state_partons_fit)) continue;
        
        bool e_power_fits = the_sector->epsilon_power==UI.pole;
        if (not(e_power_fits)) continue;
        
        bool aw_fits = the_sector->ME->alpha_ew_power() >= min_aw_power_requested
        and
        the_sector->ME->alpha_ew_power() <= max_aw_power_requested;
        if (not(aw_fits)) continue;
        
        string real_life_ME_approx = UI.matrix_element_approximation;
        // electroweak h+j sector
        if (the_sector->ME->alpha_ew_power()==1)
        {
            real_life_ME_approx == "exact";
        }
        // qcd sector
        else
        {
            if (UI.matrix_element_approximation == "effective")
                real_life_ME_approx = "effective";
            if (UI.matrix_element_approximation == "effective_enhanced")
                real_life_ME_approx = "effective";
            if (UI.matrix_element_approximation == "exact")
            {
                if (the_sector->alpha_power == 4)
                    real_life_ME_approx = "effective";
                else
                {
                    if (UI.ew_soft) real_life_ME_approx = "any";
                    else real_life_ME_approx = "exact";
                }
            }
        }
        
        // trying to match matrix_element_approximation
        // nnlo MEs all have matrix_element_approximation = effective
        
        
        
        // in an exact run we might have effective LO and NLO MEs if there
        // are ew corrections
        bool me_approx_fits =
        the_sector->ME->me_approximation()==real_life_ME_approx
        or real_life_ME_approx == "any";
        if (not(me_approx_fits)) continue;
        necessary_sectors.push_back(the_sector);
    }
    
    
    cout<<"\n[GluonFusionSectorBox] : "<<necessary_sectors.size()
    <<" sectors matched"<<endl;
    return necessary_sectors;
}





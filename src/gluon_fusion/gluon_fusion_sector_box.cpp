#include "gluon_fusion_sector_box.h"




//------------------------------------------------------------------------------

GluonFusionSectorBox::GluonFusionSectorBox(const WilsonCoefficients& WC, const BetaConstants& beta,const double& log_mur_sq_over_muf_sq,const string& rr_treatment)
{
    _WC = WC;
    _beta = beta;
    _log_mur_sq_over_muf_sq = log_mur_sq_over_muf_sq;
    //cout<<"\n[GluonFusionSectorBox] : setting up MatrixElements"<<endl;
    
    available_matrix_elements = new GluonFusionMatrixElementBox(rr_treatment);
    
    
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
    build_ew_sectors();

//    _av_partons.push_back("up");
//    _av_partons.push_back("upbar");
//    build_sectors("up","upbar");
//    build_sectors("up","gluon");
//    build_sectors("gluon","up");
//    _av_partons.push_back("down");
//    _av_partons.push_back("downbar");
//    build_sectors("down","downbar");
//    build_sectors("down","gluon");
//    build_sectors("gluon","down");
    
    
}


void GluonFusionSectorBox::build_ew_sectors()
{
    
    
    FFF up("up","up",0);
    FFF upbar("upbar","upbar",0);
    FFF down("down","down",0);
    FFF downbar("downbar","downbar",0);
    FFF gluon("gluon","gluon",0);
    
    add_simple_sector(up,upbar,"NLO_ewk_uubar_h_plus_jet"); 
    add_simple_sector(upbar,up,"NLO_ewk_uubar_h_plus_jet");
    add_simple_sector(down,downbar,"NLO_ewk_ddbar_h_plus_jet");
    add_simple_sector(downbar,down,"NLO_ewk_ddbar_h_plus_jet");
    add_simple_sector(up,gluon,"NLO_ewk_ug_h_plus_jet");
    add_simple_sector(upbar,gluon,"NLO_ewk_ug_h_plus_jet");
    add_simple_sector(down,gluon,"NLO_ewk_dg_h_plus_jet");
    add_simple_sector(downbar,gluon,"NLO_ewk_dg_h_plus_jet");
    add_simple_sector(gluon,up,"NLO_ewk_gu_h_plus_jet");
    add_simple_sector(gluon,upbar,"NLO_ewk_gu_h_plus_jet");
    add_simple_sector(gluon,down,"NLO_ewk_gd_h_plus_jet");
    add_simple_sector(gluon,downbar,"NLO_ewk_gd_h_plus_jet");
// still missing the bg channels 
//case(12)	! b g massive mixed QCD/EWK NLO
//case(13)	! b bar massive mixed QCD/EWK NLO

}

void GluonFusionSectorBox::add_simple_sector(const FFF& f1,const FFF& f2, const string& matr_elem_name)
{
    vector<ExpansionTerm*> one;
    one.push_back(new ExpansionTerm("(1)",1.0,0,0));
    available_sectors.push_back(
        new SimpleSector(f1,f2,one,find_matrix_element(matr_elem_name)));
}

MatrixElement* GluonFusionSectorBox::find_matrix_element(const string& the_name)
{
    for (int i=0;i<available_matrix_elements->size();i++)
    {
        if (available_matrix_elements->give_me(i)->the_ggf_func==the_name)
            return available_matrix_elements->give_me(i);
    }

    cout<<"\n[ehixs]FATAL ERROR: matrix element "<<the_name<<" not found!"<<endl;
    exit(0);
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
                // if the WC.exact == true it means we are in
                // a run with exact matrix elements.
                // We still need the LO and NLO effective sectors 
                // to combine with N*LO order PDFS which leads to 
                // NNLO effective convolutions.
                
                // Now, to get the correct ew sectors in the exact run case, we also need to run LO and NLO wit ew_soft coefficients. The way to implement this still needs to be decided...
//                
//                // 
//                // so we need to do the LO and NLO effective sectors only if
//                // if WC.ew_soft = true. Then there are ew soft
//                // contributions
//                // so we need to keep the non-pure QCD pieces
//                
//                if (_WC.exact and cur_me->alpha_power()<2)
//                {
//                    
//                    // if there are no ew soft, we kill
//                    // the matrix element alltogether
//                    WCET_vector.clear();
//                    // if there are ew soft contributions then we modify the
//                    // WCET vector as follows
//                    if (_WC.ew_soft)
//                    {
//                        WCET_vector = WCET_vectorEW;
//                    }
//                }
                
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
    
    
    for (int i=0;i<available_sectors.size();i++)
    {
        SimpleSector* the_sector = available_sectors[i];
//        if (the_sector->ME->me_approximation()=="effective" 
//            and the_sector->ME->epsilon_power()>0)
//            cout<<"\nSECTOR CONSIDERED: "<<the_sector->name;
        if (the_sector->ME->alpha_ew_power()==0)
            select_qcd_sector(the_sector,UI);
        else
            select_ew_sector(the_sector,UI);
    }
    return necessary_sectors;
}

void GluonFusionSectorBox::select_qcd_sector(SimpleSector* the_sector,const UserInterface& UI)
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

    bool a_power_fits = the_sector->alpha_power>=min_a_power_requested
    and the_sector->alpha_power<=max_a_power_requested;
    
    bool initial_state_partons_fit=(
                                    UI.Fleft==the_sector->F1.parton_from
                                    and UI.Fright==the_sector->F2.parton_from
                                    )
    or
    (UI.Fleft=="none" and UI.Fright=="none");
    
    bool e_power_fits = the_sector->epsilon_power==UI.pole;
    
    //matrix element approximation
    bool me_approx_fits = false;
    if (the_sector->alpha_power==2 or the_sector->alpha_power==3)
    {
        me_approx_fits =
        the_sector->ME->me_approximation()==UI.matrix_element_approximation;
    }
    else
    {
        me_approx_fits =
        the_sector->ME->me_approximation()=="effective";
    }
    
    
    bool pure_qcd_is_not_excluded = UI.alpha_ew_power<1;
    
    if (a_power_fits and initial_state_partons_fit and e_power_fits and me_approx_fits and pure_qcd_is_not_excluded)
    {
        necessary_sectors.push_back(the_sector);
    }
    else
    {
//        if (the_sector->ME->me_approximation()=="effective" 
//            and the_sector->ME->epsilon_power()>0)
//            cout<<"\nSECTOR FAILED: "<<the_sector->name;
    }

    
}

void GluonFusionSectorBox::select_ew_sector(SimpleSector* the_sector, const UserInterface& UI)
{
    //note: we rely here on the fact that all ew matrix-elements declared are by default exact, so matrix_element_approximation doesn't affect the selection. 
    int min_aw_power_requested = 0;
    int max_aw_power_requested = 0;
    if (UI.ew_h_plus_j)
    {
        max_aw_power_requested = 1;
    }
    // advanced option ew_h_plus_j_only
    if (UI.alpha_ew_power!= -1)
    {
        min_aw_power_requested = UI.alpha_ew_power;
        max_aw_power_requested = UI.alpha_ew_power;
    }
    
    bool ew_sectors_are_on = max_aw_power_requested==1;
    
    
    bool initial_state_partons_fit=(
                                    UI.Fleft==the_sector->F1.parton_from
                                    and UI.Fright==the_sector->F2.parton_from
                                    )
    or
    (UI.Fleft=="none" and UI.Fright=="none");
    
    bool e_power_fits = the_sector->epsilon_power==UI.pole;

    if (ew_sectors_are_on and initial_state_partons_fit and e_power_fits)
        necessary_sectors.push_back(the_sector);
    
}










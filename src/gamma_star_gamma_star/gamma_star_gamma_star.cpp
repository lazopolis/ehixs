



//
////: ------- gluon fusion LO
//void GammaStarGammaStar::LO()
//{
//    //: the_sector->ME->epsilon_power doesn't matter here
//    //: because LO has the structure 1+e+e^2
//    if (the_sector->ME->epsilon_power()>=0)
//        {
//        double sigma_central =   pref_sgg
//        *ISP.measLO
//        *lumi->LL_LO(0)
//        *the_sector->sector_specific_prefactors_from_a_e_expansion();
//        
//        JLO(sigma_central);
//        }
//}
//




MeExternalInfo::MeExternalInfo(const string & _pi,const string & _pj,const string& _pord,
                               const string & _name, int _epower,
                               const string & _me_approximation)
{
    parton_i=_pi;
    parton_j=_pj;
    name=_name;
    epsilon_power=_epower;
    if (_pord=="LO") alpha_power=0;
    else if (_pord=="NLO") alpha_power=1;
    else if (_pord=="NNLO") alpha_power=2;
    else if (_pord=="N3LO") alpha_power=3;
    else {cout<<"\n unrecognized pord when constructing MatrixElement"<<endl;exit(1);}
    me_approximation=_me_approximation;
}

//------------------------------------------------------------------------------




ostream& operator<<(ostream& stream, const MatrixElement& ME)
{
    stream<<"S("<<ME.parton_i()<<","<<ME.parton_j()<<","<< ME.name()
    <<",a^"<<ME.alpha_power()<<",e^"<<ME.epsilon_power()
    <<" ,dim="<<ME.dimension<<")";
    
    return stream;
}


//------------------------------------------------------------------------------

NewSimpleSector::NewSimpleSector(const FFF& _f1,const FFF& _f2,
                           const vector<ExpansionTerm*>& _factors,
                           MatrixElement* _ME,Luminosity* lumi)
    :F1(_f1),F2(_f2),factors(_factors),ME(_ME), lumi_(lumi)
{
    
    alpha_power= F1.order+F2.order+ME->alpha_power();
    //: the minus below: FFF has an epsilon order defined positive (otherwise the pdf complain)
    epsilon_power = -F1.epsilon_order-F2.epsilon_order+ME->epsilon_power();
    for (int i=0;i<factors.size();i++)
        {
        alpha_power += factors[i]->give_a_power();
        epsilon_power += factors[i]->give_e_power();
        }
    stringstream stream;
    stream<<F1<<"(*)"<<F2<<"(*)";
    for (int i=0;i<factors.size();i++)
        {
        stream<<factors[i]->give_name()<<"(*)";
        }
    stream<<*ME;
    stream<<" : a^"<<alpha_power<<",e^"<<epsilon_power;
    name=stream.str();
}

void NewSimpleSector::Evaluate()
{
    SetInitialStateVars();
    lumi->set_cur_lumi(x1,x2);
    double factor_for_ME =   pref_sgg
                            *ISP.measLO
                            *lumi->LL(0)
                            *prefactor_;
    ME->Evaluate(factor_for_ME);
    
}

void NewSimpleSector::SetInitialStateVars()
{
    
}

void SimpleSector::add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    curlumi.add_pair(
                     pdf_desc(i,j,F1.order,F1.epsilon_order),
                     pdf_desc(k,m,F2.order,F2.epsilon_order)
                     );
}

void SimpleSector::single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    for (int s=-5;s<6;s++) {if (s!=0) add_pair(i*s,j*s,k*s,m*s,curlumi);}
}

void SimpleSector::double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    for (int s=-5;s<6;s++)
        {
        for (int r=-5;r<6;r++)
            {
            if (s!=0 and r!=0 and s!=r and s!=-r)
                {
                int ii,jj,kk,mm;
                if (abs(i)==1){ii=s*i;} else if (i==2){ii=r;} else {ii=0;}
                if (abs(j)==1){jj=s*j;} else if (j==2){jj=r;} else {jj=0;}
                if (abs(k)==1){kk=s*k;} else if (k==2){kk=r;} else {kk=0;}
                if (abs(m)==1){mm=s*m;} else if (m==2){mm=r;} else {mm=0;}
                
                add_pair(ii,jj,kk,mm,curlumi);
                }
            }
        }
}

int SimpleSector::give_pid(const string & name)
{
    if (name=="gluon") return 0;
    if (name=="quark") return 1;
    if (name=="antiquark") return -1;
    if (name=="quark2") return 2;
    cout<<"\nSimpleSector::give_pid doesn't recognize parton name: "<<name;
    exit(1);
    return 0;
}

pdf_pair_list SimpleSector::give_list_of_pdf_pairs()
{
    pdf_pair_list curlumi;
    //: mapping glion,quark,antiquark,quark2 to 0,1,-1,2
    int pid1=give_pid(F1.parton_i);
    int pid2=give_pid(F1.parton_from);
    int pid3=give_pid(F2.parton_i);
    int pid4=give_pid(F2.parton_from);
    //: case g_from_g g_from_g
    if (abs(pid1)==0 and abs(pid2)==0 and abs(pid3)==0 and abs(pid4)==0) add_pair(0,0,0,0,curlumi);
    //: case with no second quark flavor, so single sum over flavors
    else if (abs(pid1)<2 and abs(pid2)<2 and abs(pid3)<2 and abs(pid4)<2) single_quark(pid1,pid2,pid3,pid4,curlumi);
    //: case with two different quark flavors
    else double_quark(pid1, pid2, pid3, pid4, curlumi);
    
    return curlumi;
}

void SimpleSector::setUpPrefactor(const double & a_s_over_pi)
{
    _prefactor =1.0;
    for (unsigned i=0;i<factors.size();i++)
        {
        _prefactor = _prefactor * factors[i]->give_value();
        }
    _prefactor = _prefactor * pow(a_s_over_pi,alpha_power);
}


//------------------------------------------------------------------------------


GammaStarGammaStarMatrixElementBox::GammaStarGammaStarMatrixElementBox()
{
    //: linking the matrix elements
    available_matrix_elements.push_back(new GstarGstarMeLO); 
}


//------------------------------------------------------------------------------

GammaStarGammaStarSectorBox::GammaStarGammaStarSectorBox(const WilsonCoefficients& WC, const BetaConstants& beta,const double& log_mur_sq_over_muf_sq)
{
    _WC = WC;
    _beta = beta;
    _log_mur_sq_over_muf_sq = log_mur_sq_over_muf_sq;
    
    available_matrix_elements = new GluonFusionMatrixElementBox;
    
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
}

void GammaStarGammaStarSectorBox::build_sectors(const string &parton_left, const string &parton_right)
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

void GammaStarGammaStarSectorBox::build_sectors_with_fixed_a_order(int f1order,int f2order,int Sorder,const string&pleft,const string &pright)
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

vector<FFF> GammaStarGammaStarSectorBox::give_possible_F(const string & parton,int f1order)
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

void GammaStarGammaStarSectorBox::build_sectors_with_fixed_a_order_and_pdfs(const FFF & F1,const FFF & F2,int Sorder)
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
        //: the check
        if (pleft==available_matrix_elements->give_me(ime)->parton_i()
            and pright==available_matrix_elements->give_me(ime)->parton_j())
            {
            matching_mes.push_back(available_matrix_elements->give_me(ime));
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

void GammaStarGammaStarSectorBox::build_sectors_with_fixed_a_order_e_order_and_pdfs(const FFF & F1,const FFF & F2,int Sorder,int Eorder,const vector<MatrixElement*> & matching_mes)
{
    //cout<<"\n specified pdfs "<<F1<<" , "<<F2<<" and e-order = "<<Eorder<<endl;
    vector<ExpansionTerm*> WCET_vector;
    vector<ExpansionTerm*> ZREN_vector;
    
    vector<ExpansionTerm*> AREN_vector;
    vector<ExpansionTerm*> AREN_vector_trivial;
    //: constructing the wilson coefficient factor [a*(c0+a*c1+a^2*c2)]^2
    //vector<ExpansionTerm*> WCET_vector;
    WCET_vector.push_back(new ExpansionTerm("(c0^2 a^2)",pow(_WC.c0,2.0),2,0));
    WCET_vector.push_back(new ExpansionTerm("(2*c0*c1* a^3)",2.0*_WC.c0*_WC.c1,3,0));
    WCET_vector.push_back(new ExpansionTerm("[(c1^2 + 2*c0*c2)*a^4]",
                                            pow(_WC.c1,2.0) + 2.0*_WC.c0*_WC.c2,4,0));
    
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
            MatrixElement* cur_me=matching_mes[ime];
            // if the me_approximation of the cur_me is "exact", trivialize WC
            if (cur_me->me_approximation() == "exact")
                {
                WCET_vector.clear();
                WCET_vector.push_back(new ExpansionTerm("(a^2)",1.0,2,0));
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
                        if (total_alpha_order==Sorder)
                            {
                            for (int me_epsilon_power = cur_me->info_->epsilon_power_min;
                                 me_epsilon_power<cur_me->info_->epsilon_power_max+1; me_epsilon_power++)
                                {
                                if (total_epsilon_order+me_epsilon_power == Eorder)
                                    {
                                    vector<ExpansionTerm*> factors;
                                    factors.push_back(WCET_vector[iwc]);
                                    factors.push_back(ZREN_vector[izren]);
                                    factors.push_back(cur_aren[iaren]);
                                    
                                    available_sectors.push_back(new SimpleSector(F1,F2,factors,cur_me,me_epsilon_power));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        
        }
}


vector<string> GammaStarGammaStarSectorBox::give_sector_names(const string & pleft,const string & pright,const string & myorder,const int & requested_epsilon_power,
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


vector<SimpleSector*> GammaStarGammaStarSectorBox::give_necessary_sectors(const UserInterface & UI)
{
    cout<<"\n[GluonFusionSectorBox] : looking for necessary sectors"<<endl;
    vector<SimpleSector*> necessary_sectors;
    for (int i=0;i<available_sectors.size();i++)
        {
        bool initial_state_partons_fit=(UI.Fleft==available_sectors[i]->F1.parton_from
                                        and UI.Fright==available_sectors[i]->F2.parton_from
                                        )
        or
        (UI.Fleft=="none" and UI.Fright=="none");
        bool a_power_fits=available_sectors[i]->alpha_power==UI.alpha_s_power;
        bool e_power_fits = available_sectors[i]->epsilon_power==UI.pole;
        bool me_approx_fits = available_sectors[i]->ME->me_approximation()==UI.matrix_element_approximation;
        if (initial_state_partons_fit and a_power_fits and e_power_fits and me_approx_fits)
            {
            
            necessary_sectors.push_back(available_sectors[i]);
            }
        /* else if (pleft==available_sectors[i]->F1.parton_from and pright==available_sectors[i]->F2.parton_from)
         {
         cout<<"\n-- sector failed "<<available_sectors[i]->name;
         }
         */
        }
    cout<<"\n[GluonFusionSectorBox] : "<<necessary_sectors.size()
    <<" sectors matched"<<endl;
    return necessary_sectors;
}

//------------------------------------------------------------------------------





GammaStarGammaStar::GammaStarGammaStar(const UserInterface & UI) : Production(UI)
{
    ptr_to_GGF = this;
    SetNumberOfParticles();
    set_up_beta_constants();
    all_sectors = new GammaStarGammaStarSectorBox(WC,beta,log_mur_sq_over_muf_sq);
        
    if (UI.info)
        {
        
        vector<SimpleSector*> necessary_sectors=
        all_sectors->give_necessary_sectors(UI);
        cout<<"\n Sectors that fit your selection criteria:\n";
        for (int i=0;i<necessary_sectors.size();i++)
            {
            cout<<"\n"<<i<<" : "<<necessary_sectors[i]->name;
            }
        cout<<"\n\n number of Sectors defined : "<<necessary_sectors.size()<<endl;
        check_which_sectors_can_be_run_together(necessary_sectors);
        exit(0);
        }
    
    if (UI.show_me_list)
        {
        cout<<"\n ME available:\n This should be re-implemented!!";
        //        for (int i=0;i<available_matrix_elements->size();i++)
        //            {
        //            cout<<*available_matrix_elements->give_me(i)<<endl;
        //            }
        exit(0);
        }
    
    cout<<"\n[GluonFusion] : finding sector"<<endl;
    find_topology(UI); //: finding topology and setting all appropriate pointers to Channel, Convolution, PartonicMode, PartonicXS, Topology etc.
    if (UI.dummy_process == false)
        {
        if (is_sector_defined())
            {
            
            allocate_luminosity();
            cout <<"\n----------------------------------\n\tSECTOR \""
            <<the_sector->name
            <<"\"\n----------------------------------\n"<<endl;
            
            
            //:after init_base is called (where Etot is set)
            tau = pow(Model.higgs.m(),2.0)/pow(Etot,2.0);
            
            
                        
            //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
            //: Gf = 1.16637*10^{-5} * 0.389379*10^9
            pref_sgg = 35.0309;
            
            the_sector -> setUpPrefactor(Model.alpha_strong()/consts::Pi);
            
            }
        
        cout<<"\na_s used = "<<Model.alpha_strong()
        <<"\t^"<<the_sector->alpha_power
        <<"(a/Pi)^"<<the_sector->alpha_power<<" = "<<pow(Model.alpha_strong()/consts::Pi,the_sector->alpha_power) ;
        }
}


void GammaStarGammaStar::evaluate_sector()
{
    event_box.CleanUp();
    prepare_phase_space_dependent_quantities();
    (this->*(the_sector->ME->the_ggf_func))();
}

void GluonFusion:: prepare_phase_space_dependent_quantities()
{
    //cout<<"\n[GluonFusion::prepare_phase_space_dependent_quantities] "
    //    <<"calling the parametrization function"<<endl;
    
    //(this->*pointer_to_function_for_parametrization)();
    (this->*(the_sector->ME->parametrization))();
    //cout<<"\n[GluonFusion::prepare_phase_space_dependent_quantities] "
    //<<"setting LO lumi"<<endl;
    lumi->set_cur_lumiLO(ISP.x1LO,ISP.x2LO);
    //cout<<"\n[GluonFusion::prepare_phase_space_dependent_quantities] "
    //<<"setting normal lumi"<<endl;
    //:sets the cur_lumi_LO according to the luminosity initialized in the constructor for this sector
    lumi->set_cur_lumi(ISP.x1,ISP.x2);
    //:sets the cur_lumi according to the luminosity initialized in the constructor for this sector
}



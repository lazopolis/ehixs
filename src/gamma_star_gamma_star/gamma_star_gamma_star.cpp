#include <iostream>
using namespace std;
#include "gamma_star_gamma_star.h"


//------------------------------------------------------------------------------




ostream& operator<<(ostream& stream, const NewMatrixElement& ME)
{
    stream<<"S("<<ME.parton_i()<<","<<ME.parton_j()<<","<< ME.name()
    <<",a^"<<ME.alpha_power()<<",e^"<<ME.epsilon_power()
    <<" ,dim="<<ME.dimension()<<")";
    
    return stream;
}


//------------------------------------------------------------------------------

NewSimpleSector::NewSimpleSector(const FFF& _f1,const FFF& _f2,
                           const vector<ExpansionTerm*>& _factors,
                           NewMatrixElement* _ME,
                                 int ep_pow,
                                 Luminosity* lumi,
                                 double* xx_vegas)
    :F1(_f1),F2(_f2),factors(_factors),ME(_ME), lumi_(lumi), xx_vegas_(xx_vegas)
{
    
    alpha_power= F1.order+F2.order+ME->alpha_power();
    //: the minus below: FFF has an epsilon order defined positive (otherwise the pdf complain)
    ME->set_epsilon_power(ep_pow);
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
    initial_state_jacobian_ = 1.0;
}

void NewSimpleSector::Evaluate()
{
    SetInitialStateVars();
    lumi_->set_cur_lumi(x_[0],x_[1]);
    double factor_for_ME =   initial_state_jacobian_
                            *lumi_->LL(0)
                            *prefactor_;
    ME->Evaluate(factor_for_ME,x_,lambda_);
    
}

void NewSimpleSector::SetInitialStateVars()
{
    x_[0] = xx_vegas_[0];
    x_[1] = xx_vegas_[1];
    for (int i=0;i<ME->dimension()-2;i++)
        {
        lambda_[i] = xx_vegas_[i+2];
        }
}

void NewSimpleSector::add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    curlumi.add_pair(
                     pdf_desc(i,j,F1.order,F1.epsilon_order),
                     pdf_desc(k,m,F2.order,F2.epsilon_order)
                     );
}

void NewSimpleSector::single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
    for (int s=-5;s<6;s++) {if (s!=0) add_pair(i*s,j*s,k*s,m*s,curlumi);}
}

void NewSimpleSector::double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi)
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

int NewSimpleSector::give_pid(const string & name)
{
    if (name=="gluon") return 0;
    if (name=="quark") return 1;
    if (name=="antiquark") return -1;
    if (name=="quark2") return 2;
    cout<<"\nSimpleSector::give_pid doesn't recognize parton name: "<<name;
    exit(1);
    return 0;
}

pdf_pair_list NewSimpleSector::give_list_of_pdf_pairs()
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

void NewSimpleSector::setUpPrefactor(const double & a_s_over_pi)
{
    prefactor_ =1.0;
    for (unsigned i=0;i<factors.size();i++)
        {
        prefactor_ = prefactor_ * factors[i]->give_value();
        }
    prefactor_ = prefactor_ * pow(a_s_over_pi,alpha_power);
}


//------------------------------------------------------------------------------


GammaStarGammaStarMatrixElementBox::GammaStarGammaStarMatrixElementBox(EventBox& event_box)
{
    //: linking the matrix elements
    available_matrix_elements.push_back(new GstarGstarMeLO(event_box));
}


//------------------------------------------------------------------------------

GammaStarGammaStarSectorBox::GammaStarGammaStarSectorBox
        (EventBox& event_box,const double& log_mur_sq_over_muf_sq,
         double* xx_vegas,Luminosity* lumi)
{
    log_mur_sq_over_muf_sq_ = log_mur_sq_over_muf_sq;
    xx_vegas_ = xx_vegas;
    lumi_ = lumi;
    available_matrix_elements = new GammaStarGammaStarMatrixElementBox(event_box);
    
    _av_partons.push_back("quark");
    _av_partons.push_back("antiquark");
    build_sectors("quark","antiquark");

    build_sectors("quark","gluon");
    build_sectors("gluon","quark");
    
    _av_partons.push_back("gluon");
    build_sectors("gluon","gluon");
    
    build_sectors("quark","quark");
    
    _av_partons.push_back("quark2");
    build_sectors("quark","quark2");
    
    cout<<"\nSectors built: there are "<<available_sectors.size()<<" available"
        <<endl;
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
                    
                    
                    build_sectors_with_fixed_a_order_and_pdfs(possible_f1[i],possible_f2[j],Sorder);
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
    cout<<"\n*** Sorder = "<<Sorder<<endl;
    vector<NewMatrixElement*> matching_mes;
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

void GammaStarGammaStarSectorBox::build_sectors_with_fixed_a_order_e_order_and_pdfs
            (const FFF & F1,
             const FFF & F2,
             int Sorder,int Eorder,
             const vector<NewMatrixElement*> & matching_mes)
{
    cout<<"\n arrived at core sector construction site "<<endl;
    cout<<"with Sorder = "<<Sorder<<" and Eorder = "<<Eorder<<endl;
    cout<<"number of matching matrix elements "<<matching_mes.size()<<endl;
    
    vector<ExpansionTerm*> AREN_vector;
    vector<ExpansionTerm*> AREN_vector_trivial;
       
    AREN_vector_trivial.push_back(new ExpansionTerm("(1)",1.0,0,0));
    
    AREN_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
    AREN_vector.push_back(new ExpansionTerm("(a)*(-b0/e)",-consts::beta_zero,1,-1));
    AREN_vector.push_back(new ExpansionTerm("(a)*b0*L)",consts::beta_zero*log_mur_sq_over_muf_sq_,1,0));
    
    for (int ime=0;ime<matching_mes.size();ime++)
        {
        if (matching_mes[ime]->alpha_power()<=Sorder)
            {
            NewMatrixElement* cur_me=matching_mes[ime];
            
            //: assigning trivial renormalization factor (1)
            vector<ExpansionTerm*> cur_aren=AREN_vector_trivial;
            if (cur_me->alpha_power()==0) cur_aren=AREN_vector;
            for (int iaren=0;iaren<cur_aren.size();iaren++)
                {
                int total_alpha_order = cur_aren[iaren]->give_a_power()
                                    +cur_me->alpha_power();
                
                if (total_alpha_order==Sorder)
                    {
                    for (int me_epsilon_power = cur_me->epsilon_power_min();
                        me_epsilon_power<cur_me->epsilon_power_max()+1;
                         me_epsilon_power++)
                        {
                        int total_epsilon_order = cur_aren[iaren]->give_e_power()
                        +me_epsilon_power;
                        if (total_epsilon_order == Eorder)
                            {
                            vector<ExpansionTerm*> factors;
                            factors.push_back(cur_aren[iaren]);
                            available_sectors.push_back(new NewSimpleSector(F1,F2,factors,cur_me,me_epsilon_power,lumi_,xx_vegas_));
                            }
                        else
                            {
                            cout<<"total_epsilon_order  = "<<total_epsilon_order
                            <<" failed"<<endl;
                            }
                        }
                    }
                else
                    {
                    cout<<"total_alpha_order  = "<<total_alpha_order
                    <<" failed"<<endl;
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
            )
            {
            
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


vector<NewSimpleSector*> GammaStarGammaStarSectorBox::give_necessary_sectors(const UserInterface & UI)
{
    cout<<"\n[Gamma* Gamma* SectorBox] : looking for necessary sectors"<<endl;
    cout<<"There are "<<available_sectors.size()<<" sectors available"<<endl;
    vector<NewSimpleSector*> necessary_sectors;
    for (int i=0;i<available_sectors.size();i++)
        {
        bool initial_state_partons_fit=(UI.Fleft==available_sectors[i]->F1.parton_from
                                        and UI.Fright==available_sectors[i]->F2.parton_from
                                        )
        or
        (UI.Fleft=="none" and UI.Fright=="none");
        bool a_power_fits=available_sectors[i]->alpha_power==UI.alpha_s_power;
        bool e_power_fits = available_sectors[i]->epsilon_power==UI.pole;
        
        if (initial_state_partons_fit and a_power_fits and e_power_fits)
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
    SetNumberOfParticles();
    all_sectors = new GammaStarGammaStarSectorBox(event_box,
                                                  log_mur_sq_over_muf_sq,
                                                  xx_vegas,
                                                  lumi);
        
    if (UI.info)
        {
        
        vector<NewSimpleSector*> necessary_sectors=
        all_sectors->give_necessary_sectors(UI);
        cout<<"\n Sectors that fit your selection criteria:\n";
        for (int i=0;i<necessary_sectors.size();i++)
            {
            cout<<"\n"<<i<<" : "<<necessary_sectors[i]->name;
            }
        cout<<"\n\n number of Sectors defined : "<<necessary_sectors.size()<<endl;

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
    the_sector->Evaluate();

}



void GammaStarGammaStar::find_topology(const UserInterface & UI)
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
                dim_of_integration=the_sector->ME->dimension();
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
        vector<NewSimpleSector*> necessary_sectors =
                                        all_sectors->give_necessary_sectors(UI);
        number_of_necessary_sectors_ = necessary_sectors.size();
        if (sector_id>-1 and sector_id<necessary_sectors.size())
            {
            found=true;
            sector_defined=true;
            the_sector=necessary_sectors[sector_id];
            dim_of_integration=the_sector->ME->dimension();
            cout<<"===> dim_of_integration = "<<dim_of_integration;
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


void GammaStarGammaStar::allocate_luminosity()
{
    pdf_pair_list list_of_pdf_pairs=the_sector->give_list_of_pdf_pairs();
    
    for (unsigned i=0;i<list_of_pdf_pairs.size();i++)
        {
        //cout<<"\n pair #"<<i+1;
        pair<pdf_desc,pdf_desc> cur_pair=list_of_pdf_pairs.give_one_pair(i);
        lumi->add_pair(cur_pair.first,cur_pair.second);
        }
}






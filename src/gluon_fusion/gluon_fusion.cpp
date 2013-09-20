#include "gluon_fusion.h"

#ifndef ONCE_PTR_TO_GGF
#define ONCE_PTR_TO_GGF
Production* ptr_to_GGF; //: static global pointer to use as a handle for plugins (like the fortran or c++ NNLO double real pieces)
//Process* ptr_to_process;
#endif


//: external function definitions
#include "nlo_exact_matrix_elements.h"



GluonFusion::GluonFusion(const UserInterface & UI) : Production(UI)
{
     ptr_to_GGF = this;
     for (int i=0;i<7;i++){smax[i]=0.0;smin[i]=1000.0;}
    set_up_wilson_coefficients();
    set_up_beta_constants();
    //cout<<"\ntau="<<tau<<"\tEtot="<<Etot;
    //g_ew=sqrt(16.0*pow(Model.W.m,4.0)*pow(consts::G_fermi,2.0)/pow(consts::Pi,3.0));
    //: using the real mass
    
    
    set_up_sectors(UI);
    if (UI.info)
        {
        vector<SimpleSector*> necessary_sectors=give_necessary_sectors(UI);
       
        
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
        cout<<"\n ME available:\n";
        for (int i=0;i<available_matrix_elements.size();i++)
            {
            cout<<*available_matrix_elements[i]<<endl;
            }
        exit(0);
        }
    
    
    find_topology(UI); //: finding topology and setting all appropriate pointers to Channel, Convolution, PartonicMode, PartonicXS, Topology etc.
    if (UI.dummy_process == false)
        {
        if (is_sector_defined())
            {
            
            allocate_luminosity();
            cout <<"\n----------------------------------\n\tSECTOR "
            <<the_sector->name
            <<"\n----------------------------------\n"<<endl;
            //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
            cur_lumi = vector<double>(lumi->pdf_size(),0.0);
            cur_lumiLO = vector<double>(lumi->pdf_size(),0.0);
            
            //:after init_base is called (where Etot is set)
            tau = pow(Model.higgs.m(),2.0)/pow(Etot,2.0);
            
            
            all_momenta.init_fvector("p1");
            all_momenta.init_fvector("p2");
            all_momenta.init_fvector("h");
            all_momenta.init_fvector("pf3");
            all_momenta.init_fvector("pf4");
            
            if (UI.matrix_element_approximation=="exact")
                {
                calculate_exact_born_me_LO();
                }
            
            //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
            //: Gf = 1.16637*10^{-5} * 0.389379*10^9
            pref_sgg = 35.0309;
            lh= -log_muf_sq_over_mh_sq;//: note the '-' sign: Franz's convention for lh=log(m_h^2/mu_f^2)
            sector_specific_prefactors_from_a_e_expansion=1.0;
            for (unsigned i=0;i<the_sector->factors.size();i++)
                {
                sector_specific_prefactors_from_a_e_expansion = sector_specific_prefactors_from_a_e_expansion * the_sector->factors[i]->give_value();
                }
            sector_specific_prefactors_from_a_e_expansion = sector_specific_prefactors_from_a_e_expansion
            * pow(Model.alpha_strong[0]/consts::Pi,the_sector->alpha_power);
            }
        
        cout<<"\n***** \t\t a_s used = "<<Model.alpha_strong[0]
        <<"\t^"<<the_sector->alpha_power;
        }
}


vector<string> GluonFusion::give_sector_names(const string & pleft,const string & pright,const string & myorder,const int & requested_epsilon_power)
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
              and available_sectors[i]->ME->me_approximation()=="effective"
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
     if (/*s1->F1.name()==s2->F1.name() and s1->F2.name()==s2->F2.name() and*/ s1->ME->give_name()==s2->ME->give_name())
          {
          return true;
          }
     else
          {
//          cout<<endl<<"incompatible: ";
//          cout<<endl<<s1->F1.name()<<" vs "<<s2->F1.name()
//               <<endl<<s1->F2.name()<<" vs "<<s2->F2.name()
//               <<endl<<s1->ME->give_name()<<" vs "<<s2->ME->give_name();
          }
     return false;
}

vector<string> GluonFusion::give_sector_names(const string & pleft,const string & pright,const string & myorder,const int & requested_epsilon_power,const string & me_approx)
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


vector<SimpleSector*> GluonFusion::give_necessary_sectors(const UserInterface & UI)
{

     vector<SimpleSector*> necessary_sectors;
     for (int i=0;i<available_sectors.size();i++)
          {
          bool initial_state_partons_fit=(UI.Fleft==available_sectors[i]->F1.parton_from
                                                and UI.Fright==available_sectors[i]->F2.parton_from
                                                )
                                                  or
                                             (UI.Fleft=="none" and UI.Fright=="none");
          bool a_power_fits=available_sectors[i]->alpha_power==UI.perturbative_order+2;
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
     return necessary_sectors;
}


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

MatrixElement::MatrixElement( MeExternalInfo* info,const string & _kin,
                             const string& _str_param,
                             ptr_to_GluonFusion_function _the_ggf_func,
                             pointer_to_Franz_gluon_fusion  ptr_to_fr,
                             int num_topologies,
                             double _e_exp_in_subtr)
{
    _info=info;
    if (_kin=="kinematics:LO") dimension=2;
    else if (_kin=="kinematics:NLO") dimension=4;
    else if (_kin=="kinematics:NNLO") dimension=6;
    else {cout<<"\nUnrecognized kinematics when constructing MatrixElement"<<endl;exit(1);}
    if (_str_param == "param:LO")  parametrization=&GluonFusion::LO_parametrization_only;
    else if (_str_param=="param:NLO") parametrization=&GluonFusion::NLO_parametrization;
    else {cout<<"\nerror, param not equal to LO or NLO"<<endl;exit(1);}
    
    
    the_ggf_func = _the_ggf_func;
    is_franz_topology=true;
    franz_func=ptr_to_fr;
    number_of_sectors_in_this_topology=num_topologies;
    epsilon_exponent_in_z_subtraction = _e_exp_in_subtr;
}

MatrixElement::MatrixElement(const string & _pi,const string & _pj,const string& _pord,
              const string & _name, const string & _kin,int _epower,
              const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,
              pointer_to_Franz_gluon_fusion  ptr_to_fr, int num_topologies,double _e_exp_in_subtr,const string & _me_approx)
{
    _info= new MeExternalInfo(_pi,_pj,_pord,_name,_epower,_me_approx);
    
     if (_kin=="kinematics:LO") dimension=2;
     else if (_kin=="kinematics:NLO") dimension=4;
     else if (_kin=="kinematics:NNLO") dimension=6;
     else {cout<<"\nUnrecognized kinematics when constructing MatrixElement"<<endl;exit(1);}
     
     if (_str_param == "param:LO")  parametrization=&GluonFusion::LO_parametrization_only;
     else if (_str_param=="param:NLO") parametrization=&GluonFusion::NLO_parametrization;
     else {cout<<"\nerror, param not equal to LO or NLO"<<endl;exit(1);}
     
     the_ggf_func = _the_ggf_func;
     is_franz_topology=true;
     franz_func=ptr_to_fr;
     number_of_sectors_in_this_topology=num_topologies;
     epsilon_exponent_in_z_subtraction = _e_exp_in_subtr;
}


SimpleSector::SimpleSector(const FFF& _f1,const FFF& _f2,const vector<ExpansionTerm*>& _factors,MatrixElement* _ME):F1(_f1),F2(_f2),factors(_factors),ME(_ME)
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



void SimpleSector::add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi)
{
     curlumi.add_pair(
                      Luminosity::pdf_desc(i,j,F1.order,F1.epsilon_order),
                      Luminosity::pdf_desc(k,m,F2.order,F2.epsilon_order)
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



ostream& operator<<(ostream& stream, const MatrixElement& ME)
{
     stream<<"S("<<ME.parton_i()<<","<<ME.parton_j()<<","<< ME.name()
        <<",a^"<<ME.alpha_power()<<",e^"<<ME.epsilon_power()
        <<" ,dim="<<ME.dimension<<")";
     
     return stream;
}





void GluonFusion::push_me(const string & _pi,const string & _pj,const string& _pord,
                          const string & _name, const string & _kin,
                          const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k)
{
     pointer_to_Franz_gluon_fusion the_null_franz=NULL;
     int zero_sectors=0;
     for (int k=from_k;k<to_k+1;k++)
          {
          available_matrix_elements.push_back(new MatrixElement(_pi,_pj,_pord,_name,_kin,k,_str_param,_the_ggf_func,the_null_franz,zero_sectors,0.0,"effective"));
          }
}


void GluonFusion::push_me(const string & _pi,const string & _pj,const string& _pord,
                          const string & _name, const string & _kin,
                          const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k,const string & me_approx)
{
     pointer_to_Franz_gluon_fusion the_null_franz=NULL;
     int zero_sectors=0;
     for (int k=from_k;k<to_k+1;k++)
          {
          available_matrix_elements.push_back(new MatrixElement(_pi,_pj,_pord,_name,_kin,k,_str_param,_the_ggf_func,the_null_franz,zero_sectors,0.0,me_approx));
          }
}

void GluonFusion::push_me(const string & _pi,const string & _pj,const string& _pord,
                          const string & _name, const string & _kin,
                          const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k,
                          pointer_to_Franz_gluon_fusion  ptr_to_fr, int num_topologies)
{
     for (int k=from_k;k<to_k+1;k++)
          {
          available_matrix_elements.push_back(new MatrixElement(_pi,_pj,_pord,_name,_kin,k,_str_param,_the_ggf_func,ptr_to_fr,num_topologies,0.0,"effective"));
          }
}



void GluonFusion::push_me(const string & _pi,const string & _pj,const string& _pord,
                          const string & _name, const string & _kin,
                          const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k,
                          pointer_to_Franz_gluon_fusion  ptr_to_fr[], int num_topologies,int num_sect[],double eps_exp)
{
     for (int s=0;s<num_topologies;s++)
          {
          for (int k=from_k;k<to_k+1;k++)
               {
               stringstream name_str;
               name_str<<_name<<" t"<<s+1;
               available_matrix_elements.push_back(new MatrixElement(_pi,_pj,_pord,name_str.str(),_kin,k,_str_param,_the_ggf_func,ptr_to_fr[s],num_sect[s],eps_exp,"effective"));
               }
          }
}

void GluonFusion::push_me(const string & _pi,const string & _pj,const string& _pord,
                          const string & _name, const string & _kin,
                          const string& _str_param,ptr_to_GluonFusion_function _the_ggf_func,int from_k,int to_k,
                          pointer_to_Franz_gluon_fusion  ptr_to_fr[], int num_topologies,int num_sect[])
{
    for (int s=0;s<num_topologies;s++)
         {
        for (int k=from_k;k<to_k+1;k++)
             {
            stringstream name_str;
            name_str<<_name<<" t"<<s+1;
            available_matrix_elements.push_back(new MatrixElement(_pi,_pj,_pord,name_str.str(),_kin,k,_str_param,_the_ggf_func,ptr_to_fr[s],num_sect[s],0.0,"effective"));
             }
         }
}

void GluonFusion::add_qqbar_sectors()
{     

     const int num_topologies=14;
     pointer_to_Franz_gluon_fusion rr[num_topologies]={rrqqbar2qqbarht1,rrqqbar2qqbarht2,rrqqbar2qqbarht3,rrqqbar2qqbarht4,rrqqbar2qqbarht5,rrqqbar2qqbarht6,rrqqbar2gght1,rrqqbar2gght2,rrqqbar2gght3,rrqqbar2gght4,rrqqbar2gght5,rrqqbar2gght6,rrq1q1bar2q2q2barht1,rrq1q1bar2q2q2barht2};
     int num_sect[num_topologies]={1,2,1,4,1,1,1,2,1,1,4,1,1,1};//: number of sectors per topology

     push_me("quark","antiquark","NLO","R","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,0,1,rqqbar2ght1,1);
     push_me("quark","antiquark","NNLO","RV","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-2,0,rvqqbar2ght1,1);
     push_me("quark","antiquark","NNLO","RR","kinematics:NNLO","param:NLO",&GluonFusion::NNLO_hard_no_subtraction,-2,0,rr,num_topologies,num_sect);
     
}


void GluonFusion::add_q1q2_sectors()
{
     const int num_topologies=2;
     pointer_to_Franz_gluon_fusion rr[num_topologies]={rrq1q22q1q2ht1,rrq1q22q1q2ht2};
     int num_sect[num_topologies]={1,1};//: number of sectors per topology

     push_me("quark","quark2","NNLO","RR","kinematics:NNLO","param:NLO",&GluonFusion::NNLO_hard_no_subtraction,-2,0,rr,num_topologies,num_sect);
     
}

void GluonFusion::add_qq_sectors()
{
     const int num_topologies=3;
     pointer_to_Franz_gluon_fusion rr[num_topologies]={rrqq2qqht1,rrqq2qqht2,rrqq2qqht3};
     int num_sect[num_topologies]={1,1,8};//: number of sectors per topology
     push_me("quark","quark","NNLO","RR","kinematics:NNLO","param:NLO",&GluonFusion::NNLO_hard_no_subtraction,-2,0,rr,num_topologies,num_sect);

}

void GluonFusion::add_gg_sectors()
{
     
     

     const int num_topologies=29;
     pointer_to_Franz_gluon_fusion rr[num_topologies]={rrgg2gght1,rrgg2gght2,rrgg2gght3,rrgg2gght4,rrgg2gght5,rrgg2gght6,rrgg2gght7,rrgg2gght8,rrgg2gght9,rrgg2gght10,rrgg2gght11,rrgg2gght12,rrgg2gght13,rrgg2gght14,rrgg2gght15,rrgg2gght16,rrgg2qqbarht1,rrgg2qqbarht2,rrgg2qqbarht3,rrgg2qqbarht4,rrgg2qqbarht5,rrgg2qqbarht6,rrgg2qqbarht7,rrgg2qqbarht8,rrgg2qqbarht9,rrgg2qqbarht10,rrgg2qqbarht11,rrgg2qqbarht12,rrgg2qqbarht13};
     int num_sect[num_topologies]={2,2,1,1,8,2,2,4,2,1,1,2,1,1,1,1,2,2,1,1,2,2,4,2,1,1,1,2,1};//: number of sectors per topology

     push_me("gluon","gluon","LO","B","kinematics:LO","param:LO",&GluonFusion::LO,0,2);
     push_me("gluon","gluon","NLO","S","kinematics:LO","param:NLO",&GluonFusion::gg_NLO_SOFT,0,1);//: the soft is finite
     push_me("gluon","gluon","NLO","H","kinematics:NLO","param:NLO",&GluonFusion::gg_NLO_HARD,-1,1);
     push_me("gluon","gluon","NNLO","DOUBLE SOFT","kinematics:LO","param:NLO",&GluonFusion::gg_NNLO_SOFT,-2,0);
     
     
     
     const int rv_no_sub_numtop=1;
     pointer_to_Franz_gluon_fusion rv_no_sub[rv_no_sub_numtop]={rvgg2ght1};
     int rv_no_subnum_sect[rv_no_sub_numtop]={6};
     push_me("gluon","gluon","NNLO","RVt1","kinematics:NLO","param:NLO",
             &GluonFusion::nlo_me,-3,0,rv_no_sub,rv_no_sub_numtop,rv_no_subnum_sect);
     
     
     const int rv_2=1;
     pointer_to_Franz_gluon_fusion rv_2_pt[rv_2]={rvgg2ght2};
     int rv_2_sect[rv_2]={4};
     
     
     push_me("gluon","gluon","NNLO","RVt2","kinematics:NLO","param:NLO",
             &GluonFusion::NNLO_rv_with_subtraction,-3,0,rv_2_pt,rv_2,rv_2_sect,2.0);
     
     const int rv_4=1;
     pointer_to_Franz_gluon_fusion rv_4_pt[rv_4]={rvgg2ght4};
     int rv_4_sect[rv_4]={2};
     push_me("gluon","gluon","NNLO","RVt4","kinematics:NLO","param:NLO",
             &GluonFusion::NNLO_rv_with_subtraction,-3,0,rv_4_pt,rv_4,rv_4_sect,4.0);
     
     push_me("gluon","gluon","NNLO","RR","kinematics:NNLO","param:NLO",
             &GluonFusion::NNLO_hard_with_subtraction,-3,0,rr,num_topologies,num_sect,4.0);
     
     
     //: exact matrix elements
     push_me("gluon","gluon","LO","LO exact","kinematics:LO","param:LO",&GluonFusion::LO_exact,0,2,"exact");
     push_me("gluon","gluon","NLO","NLO soft exact","kinematics:LO","param:NLO",&GluonFusion::NLO_soft_exact,0,0,"exact");//: only the e^0 piece
     push_me("gluon","gluon","NLO","NLO hard exact","kinematics:NLO","param:NLO",&GluonFusion::gg_NLO_hard_exact,-2,0,"exact");
 }

void GluonFusion::add_qg_sectors()
{
     const int num_topologies=15;
     pointer_to_Franz_gluon_fusion rr[num_topologies]={rrqg2qght1,rrqg2qght2,rrqg2qght3,rrqg2qght4,rrqg2qght5,rrqg2qght6,rrqg2qght7,
          rrqg2qght8,rrqg2qght9,rrqg2qght10,rrqg2qght11,rrqg2qght12,rrqg2qght13,rrqg2qght14,rrqg2qght15};
     int num_sect[num_topologies]={1,2,1,1,8,2,2,4,2,1,1,1,1,1,1};//: number of sectors per topology

     const int rnumtop=1;
     pointer_to_Franz_gluon_fusion r_sectors[rnumtop]={rqg2qht1};
     int rnumsect[rnumtop]={1};
     push_me("quark","gluon","NLO","H","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-1,1,r_sectors,rnumtop,rnumsect);
     const int rvnumtop=1;
     pointer_to_Franz_gluon_fusion rv[rvnumtop]={rvgq2qht1};
     int rvnum_sect[rvnumtop]={3};
     push_me("quark","gluon","NNLO","RV","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-3,0,rv,rvnumtop,rvnum_sect);
     push_me("quark","gluon","NNLO","RR","kinematics:NNLO","param:NLO",&GluonFusion::NNLO_hard_no_subtraction,-3,0,rr,num_topologies,num_sect);
     
     
}

void GluonFusion::add_gq_sectors()
{
    
    const int num_topologies=15;
     pointer_to_Franz_gluon_fusion rr[num_topologies]={rrgq2qght1,rrgq2qght2,rrgq2qght3,rrgq2qght4,rrgq2qght5,rrgq2qght6,rrgq2qght7,rrgq2qght8,rrgq2qght9,rrgq2qght10,rrgq2qght11,rrgq2qght12,rrgq2qght13,rrgq2qght14,rrgq2qght15};
     int num_sect[num_topologies]={1,2,1,1,8,2,2,4,2,1,1,1,1,1,1};//: number of sectors per topology

     
     const int rnumtop=1;
     pointer_to_Franz_gluon_fusion r_sectors[rnumtop]={rgq2qht1};
     int rnumsect[rnumtop]={1};
     push_me("gluon","quark","NLO","H","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-1,1,r_sectors,rnumtop,rnumsect);
     const int rvnumtop=1;
     pointer_to_Franz_gluon_fusion rv[rvnumtop]={rvqg2qht1};
     int rvnum_sect[rvnumtop]={3};
     push_me("gluon","quark","NNLO","RV","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-3,0,rv,rvnumtop,rvnum_sect);
     push_me("gluon","quark","NNLO","RR","kinematics:NNLO","param:NLO",&GluonFusion::NNLO_hard_no_subtraction,-3,0,rr,num_topologies,num_sect);
     
 }


void GluonFusion::find_topology(const UserInterface & UI)
{
     bool found=false;
     //: searching by sector_name (has priority)
     if (UI.sector_name!="none")
          {
          
          for (int i=0;i<available_sectors.size();i++)
               {
               if (available_sectors[i]->name==UI.sector_name)
                    {
                    found=true;
                    sector_defined=true;
                    the_sector=available_sectors[i];
                    dim_of_integration=the_sector->ME->dimension;
                    }
               }
          // if a sector name was provided and didn;t match we have an error
          if (not(found))
               {
               cout<<"\nI couldn't match the sector/topology you asked for.";
               cout<<"\n You asked for sector with the name "<<UI.sector_name;
               cout<<"\n which was not found in the list. please run with UI.info=true or --info to get the list of sector names"<<endl;
               throw "\n Can't proceed!\n";
               
               }
          }
     else if (UI.sector_for_production!="none")
          {
          int sector_id=atoi(UI.sector_for_production.c_str());
          vector<SimpleSector*> necessary_sectors=give_necessary_sectors(UI);
          if (sector_id>-1 and sector_id<necessary_sectors.size())
               {
               found=true;
               sector_defined=true;
               the_sector=necessary_sectors[sector_id];
               dim_of_integration=the_sector->ME->dimension;
               cout<<"===> dim_of_integration = "<<dim_of_integration;
               }
          else
               {
               cout<<"\n The sector id number you asked for, "<<sector_id
                    <<", was outside the bounds [0,"<<necessary_sectors.size()<<"]";
               cout<<"\n Please run with UI.info=true or --info to get the list of sector names"<<endl;
               throw "\n Can't proceed!\n";
               
               }
          }
     else if (UI.dummy_process == false)
          {
          cout<<"\n You didn't specify a sector name or a sector id number";
          cout<<"\n Please run with UI.info=true or --info to get the list of sector names"<<endl;
          throw "\n Can't proceed!\n";
          }
     else
         {
         cout<<"\n dummy process - no sector name provided"<<endl;
         }
     
}
 
void GluonFusion::init(const UserInterface& UI,TheHatch* the_hatch)
{
     
}

void GluonFusion::allocate_luminosity()
{
     pdf_pair_list list_of_pdf_pairs=the_sector->give_list_of_pdf_pairs();
     
     for (unsigned i=0;i<list_of_pdf_pairs.size();i++)
          {
          //cout<<"\n pair #"<<i+1;
          pair<Luminosity::pdf_desc,Luminosity::pdf_desc> cur_pair=list_of_pdf_pairs.give_one_pair(i);
          lumi->add_pair(cur_pair.first,cur_pair.second);
          }
     //lumi->show_necessary_pdfs();
     
}

void GluonFusion::calculate_derived_variables(const UserInterface& UI)
{
     
     
	
     
}

void GluonFusion::set_up_sectors(const UserInterface& UI)
{
     //: linking the matrix elements
     add_gg_sectors();
     add_qg_sectors();
     add_gq_sectors();
     add_qqbar_sectors();
     add_q1q2_sectors();
     add_qq_sectors();
     
     //: building all sectors
     
     av_partons.push_back("gluon");
     av_partons.push_back("quark");
     build_sectors("gluon","gluon");
     
     av_partons.push_back("antiquark");
     build_sectors("quark","gluon");
     build_sectors("gluon","quark");
     build_sectors("quark","antiquark");

     
//     vector<string> av_partons2;
//     av_partons2.push_back("gluon");
//     av_partons2.push_back("quark");
     build_sectors("quark","quark");

     av_partons.push_back("quark2");
     build_sectors("quark","quark2");
     

}



void GluonFusion::build_sectors(const string &parton_left, const string &parton_right)
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

void GluonFusion::build_sectors_with_fixed_a_order(int f1order,int f2order,int Sorder,const string&pleft,const string &pright)
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

vector<FFF> GluonFusion::give_possible_F(const string & parton,int f1order)
{
     vector<FFF> possible_f;
     for (unsigned i=0;i<av_partons.size();i++)
          {
          possible_f.push_back(FFF(av_partons[i],parton,f1order));
          if (f1order==2)
               {
               possible_f.push_back(FFF(av_partons[i],parton,f1order,1));//: adding the 2_1 pdfs
               }
          }
     return possible_f;
}

void GluonFusion::build_sectors_with_fixed_a_order_and_pdfs(const FFF & F1,const FFF & F2,int Sorder)
{
     vector<MatrixElement*> matching_mes;
     for (int ime=0;ime<available_matrix_elements.size();ime++)
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
          if (pleft==available_matrix_elements[ime]->parton_i() and pright==available_matrix_elements[ime]->parton_j())
               {
               matching_mes.push_back(available_matrix_elements[ime]);
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

void GluonFusion::build_sectors_with_fixed_a_order_e_order_and_pdfs(const FFF & F1,const FFF & F2,int Sorder,int Eorder,const vector<MatrixElement*> & matching_mes)
{
     //cout<<"\n specified pdfs "<<F1<<" , "<<F2<<" and e-order = "<<Eorder<<endl;
     vector<ExpansionTerm*> WCET_vector;
     vector<ExpansionTerm*> ZREN_vector;
     
     vector<ExpansionTerm*> AREN_vector;
     vector<ExpansionTerm*> AREN_vector_trivial;
     //: constructing the wilson coefficient factor [a*(c0+a*c1+a^2*c2)]^2
     //vector<ExpansionTerm*> WCET_vector;
     WCET_vector.push_back(new ExpansionTerm("(c0^2 a^2)",pow(WC.c0,2.0),2,0));
     WCET_vector.push_back(new ExpansionTerm("(2*c0*c1* a^3)",2.0*WC.c0*WC.c1,3,0));
     WCET_vector.push_back(new ExpansionTerm("[(c1^2 + 2*c0*c2)*a^4]",
                                             pow(WC.c1,2.0) + 2.0*WC.c0*WC.c2,4,0));
     
     //: a^2 * Z^2 = { a * (1+a*b0*L + a^2*b1*L + a^2*b0^2*L^2) * ( 1 - a*b0/e + a^2 * b0^2/e^2-a^2*b1/e) }^2
     ZREN_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
     ZREN_vector.push_back(new ExpansionTerm("(a)*(-2*b0/e)",-2.0*beta.zero,1,-1));
     ZREN_vector.push_back(new ExpansionTerm("(a)*(2*b0*L)",2.0*beta.zero*log_mur_sq_over_muf_sq,1,0));
     
     ZREN_vector.push_back(new ExpansionTerm("(a^2)*(3*b0^2/e^2)",3.0*pow(beta.zero,2.0),2,-2));
     ZREN_vector.push_back(new ExpansionTerm("(a^2)*(-2*b1/e)",-2.0*beta.one,2,-1));
     ZREN_vector.push_back(new ExpansionTerm("(a^2)*(-4*b0^2*L/e)",-4.0*pow(beta.zero,2.0)*log_mur_sq_over_muf_sq,2,-1));
     ZREN_vector.push_back(new ExpansionTerm("(a^2)*(3*b0^2*L^2)",3.0*pow(beta.zero,2.0)*pow(log_mur_sq_over_muf_sq,2),2,0));
     ZREN_vector.push_back(new ExpansionTerm("(a^2)*(2*b1*L)",2.0*beta.one*log_mur_sq_over_muf_sq,2,0));
     
     AREN_vector_trivial.push_back(new ExpansionTerm("(1)",1.0,0,0));

     AREN_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
     AREN_vector.push_back(new ExpansionTerm("(a)*(-b0/e)",-beta.zero,1,-1));
     AREN_vector.push_back(new ExpansionTerm("(a)*b0*L)",beta.zero*log_mur_sq_over_muf_sq,1,0));

     //AREN_vector.push_back(new ExpansionTerm("(-b0*a*lh)",-beta.zero*lh,1,0));
     for (int ime=0;ime<matching_mes.size();ime++)
          {
          if (matching_mes[ime]->alpha_power()<=Sorder)
               {
               MatrixElement* cur_me=matching_mes[ime];
               //: assigning trivial renormalization factor (1)
               vector<ExpansionTerm*> cur_aren=AREN_vector_trivial;
               //: assigning  -b0 * a exp(e*lh) / e if the matrix element is nlo
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
                              if (total_alpha_order==Sorder and total_epsilon_order==Eorder)
                                   {
                                   vector<ExpansionTerm*> factors;
                                   factors.push_back(WCET_vector[iwc]);
                                   factors.push_back(ZREN_vector[izren]);
                                   factors.push_back(cur_aren[iaren]);
                              
                                   available_sectors.push_back(new SimpleSector(F1,F2,factors,cur_me));
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



void GluonFusion::set_up_wilson_coefficients()
{
     //: QCD plain:
     WC.c0 = 1.0;
     WC.c1 = 11.0/4.0;
     WC.c2 = 2777.0/288.0
                              +(19.0/16.0)*log_muf_sq_over_mt_sq
                              +consts::nf*(
                                             (1.0/3.0)*log_muf_sq_over_mt_sq
                                           -67.0/96.0
                                           );
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
     clear_previously_allocated_events_and_free_memory();
     prepare_phase_space_dependent_quantities();
      (this->*(the_sector->ME->the_ggf_func))();
}

void GluonFusion::clear_previously_allocated_events_and_free_memory()
{
     
          
     for (int i=0;i<production_events.size();i++)
          {
//          if (the_sector->name == "F_gluon_from_gluon_00(*)F_gluon_from_gluon_00(*)(c0^2 a^2)(*)(1)(*)(1)(*)S(gluon,gluon,RR t5,a^2,e^-2 ,dim=6) : a^4,e^-2") cout<<"\n"<<*production_events[i];
//          else cout<<"\n{"<<the_sector->name<<"}";
          delete production_events[i];
          }
    production_events.clear();

    for (int i=0;i<light_events.size();i++)
        {
        //          if (the_sector->name == "F_gluon_from_gluon_00(*)F_gluon_from_gluon_00(*)(c0^2 a^2)(*)(1)(*)(1)(*)S(gluon,gluon,RR t5,a^2,e^-2 ,dim=6) : a^4,e^-2") cout<<"\n"<<*production_events[i];
        //          else cout<<"\n{"<<the_sector->name<<"}";
        delete light_events[i];
        }
    light_events.clear();
}

void GluonFusion:: prepare_phase_space_dependent_quantities()
{
     
     //(this->*pointer_to_function_for_parametrization)();
     (this->*(the_sector->ME->parametrization))();
     
     lumi->set_cur_lumi(ISP.x1LO,ISP.x2LO,cur_lumiLO);//:sets the cur_lumi_LO according to the luminosity initialized in the constructor for this sector
     lumi->set_cur_lumi(ISP.x1,ISP.x2,cur_lumi);       //:sets the cur_lumi according to the luminosity initialized in the constructor for this sector
}

void GluonFusion::LO_parametrization_only()
{
     //:setting x1LO,x2LO,zLO,measLO to LO kinematics
     parametrization_for_LO_kinematics();
}

void GluonFusion::NLO_parametrization()
{
     //:setting x1LO,x2LO,zLO,measLO to LO kinematics
     parametrization_for_LO_kinematics();
     //:setting x1,x2,z,meas to NLO kinematics
     parametrization_for_NLO_kinematics();
    // cout<<"\n** z="<<ISP.z<<"\tx1="<<ISP.x1<<" x1LO="<<ISP.x1LO<<"\t ratio = "<<ISP.x1LO/ISP.x1;
     //:setting lambda
     
}




void GluonFusion::parametrization_for_LO_kinematics()
{
     
     int parametrization_switch=1;
     
     
     if (parametrization_switch==0)
          {
          //: parametrization z/x1
          ISP.x1LO=xx_vegas[0];
          ISP.x2LO=tau/ISP.x1LO;
          ISP.measLO = 1.0/ISP.x1LO;
          ISP.zLO=1.0;
          
          }
     else if (parametrization_switch==1)
          {
          
          //: old parametrization z/x1~rap
          double jac_from_rap_param ;
          double x= generate_x1(jac_from_rap_param);
          
          ISP.measLO = 1.0/x*jac_from_rap_param;
          
         
          
          
          ISP.x1LO=x;
          ISP.zLO=1.0;
          ISP.x2LO= tau/x;
          }
     else if (parametrization_switch==2)
          {
          //: new parametrization u1,u2
          double u2=xx_vegas[0];
          double u1=0.0;//xx_vegas[1];
          
          
          double U = log(tau/(1.0-u1*(1.0-tau)));
          
          ISP.x1LO = exp((1.0-u2)*U);
          ISP.x2LO = exp(u2*U);
          ISP.zLO=1.0;
          ISP.measLO =  log((1.0-u1*(1.0-tau))/tau);
          }
     else if (parametrization_switch==3)
          {
          double zz=1.0;
          double yy=xx_vegas[0];
          ISP.measLO = -log(tau/zz);
          ISP.x1LO = exp(yy*log(tau));
          ISP.x2LO = exp((1.0-yy)*log(tau));
          ISP.zLO = 1.0;
          }
     //cout<<"\n % x1LO="<<ISP.x1LO;

     ISP.cursLO=pow(Model.higgs.m(),2.0)/ISP.zLO;
     //     cout<<"\nhello "<<x1<<x2<<"\t"<<xx_vegas[0]<<"\t"<<xx_vegas[1]<<"\t"<<tau;
}



void GluonFusion::parametrization_for_NLO_kinematics()
{
     int parametrization_switch=1;
     
     if (parametrization_switch==0)
          {
          //: parametrization z/x1
          ISP.x1=xx_vegas[0];
          ISP.x2=xx_vegas[1];
          ISP.meas = 1.0/ISP.x1;
          ISP.z=tau/ISP.x1/ISP.x2;
          
          }
     else if (parametrization_switch==1)
          {
          
          //: old parametrization z/x1~rap
          double jac_from_rap_param ;
          double x= generate_x1(jac_from_rap_param);
          
          ISP.meas = 1.0/x*jac_from_rap_param;
          
          ISP.x1=x;
          //: modification to protect ourselves from z=0
          //ISP.z=xx_vegas[1];
          double z_min=1e-14;
          ISP.z = z_min + xx_vegas[1] * (1.0-z_min);
          ISP.meas = ISP.meas * (1.0-z_min);
          ISP.x2= tau/ISP.x1/ISP.z;
          }
     else if (parametrization_switch==2)
          {
          //: new parametrization u1,u2
          double u2=xx_vegas[0];
          double u1=xx_vegas[1];
          
          
          double U = log(tau/(1.0-u1*(1.0-tau)));
          ISP.meas =  log((1.0-u1*(1.0-tau))/tau);
          ISP.x1 = exp((1.0-u2)*U);
          ISP.x2 = exp(u2*U);
          ISP.z=1.0-u1*(1.0-tau);
          }
     else if (parametrization_switch==3)
          {
          double yy=xx_vegas[0];
          double zz=xx_vegas[1];
          ISP.meas = -log(tau/zz);
          ISP.x1 = exp(yy*log(tau/zz));
          ISP.x2 = exp((1.0-yy)*log(tau/zz));
          ISP.z = zz;
          
          }
     const double almost_zero =0.0;// 1e-23;
     if (ISP.x1>1.0-almost_zero or ISP.x2>1.0-almost_zero or ISP.x1<almost_zero or ISP.x2<almost_zero)
          {
          ISP.meas=0.0;
          }
    // cout<<"\n % x1="<<ISP.x1<<"\t"<<xx_vegas[0]<<"\n tau="<<tau;
     ISP.curs=pow(Model.higgs.m(),2.0)/ISP.z;
     
     ISP.Log_1mz=log(1.0-ISP.z);
     ISP.lambda = xx_vegas[2];
     ISP.phi=xx_vegas[3];
}


double GluonFusion::generate_x1(double & jac_from_rap_param)
{
     double xlambda=xx_vegas[0];
     double umax = -0.5*log(tau);
     double umin = 0.5*log(tau);
     double u=umin+(umax-umin)*xlambda;
     jac_from_rap_param = sqrt(tau)*exp(u)*(umax-umin);
     double x= sqrt(tau)*exp(u);
     return x;
}


void GluonFusion::book_production_event()
{
     //Vegas.set_up_vegas_ff(0.0);
}

void GluonFusion::book_production_event(const double & sigma_central,
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
          <<x1<<" "
          <<x2<<" "
          <<z<<" "
          <<s13<<" "
          <<s23<<" "
          <<s14<<" "
          <<s24<<" "
          <<s34<<" \too==>"
          <<ISP.meas;
          }
     if (sigma_central!=0.0)
          {
          const double shat = pow(Etot,2.0)*x1*x2;
          set_up_event_kinematics(x1,x2,z,s13,s23,s14,s24,s34);
          push_back_event(sigma_central);
          writeEventToFile(x1,all_momenta["h"].pT(),
                           all_momenta["h"].zrap(),sigma_central);

          }
}

void GluonFusion::writeEventToFile(const double & x1,const double &pt,
                                   const double & y,const double & weight)
{

        light_events.push_back(new LightEvent(x1,pt,y,weight));
    
    
}



void GluonFusion::update_smaxmin(int i,double x)
{
     if (x<smin[i]) {smin[i]=x;}
     if (x>smax[i]) {smax[i]=x;}
}

void GluonFusion::Jnlo(const double & res, const double & x1, const double & x2,const double &z, const double & lambda)
{
     //cout<<"\nhello\t"<<x1<<" "<<x2;
     if (x1<1.0 and x2<1.0 and x1>0.0 and x2>0.0)
          {
          
          double  s12 = x1*x2*pow(Etot,2.0);
          double  s13 = s12*(1.0-z)*lambda;
          double  s14 = 0.0;
          double  s23 = s12*(1.0-z)*(1.0-lambda);
          double  s24 = 0.0;
          double  s34 = 0.0;
          book_production_event(res,x1,x2,z,
                                s13,s23,s14,s24,s34);
          }
     else
          {
          cout<<"\n\033[1;31m point with x1 or x2 out of bounds at JNLO \033[0m\n";
          book_production_event();
          }
}

void GluonFusion::JLO(const double & sigma)
{
     double x1=ISP.x1LO;
     double x2=ISP.x2LO;
     all_momenta["p1"].set(x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
     all_momenta["p2"].set(x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
     all_momenta["h"].set((x1+x2)*Etot/2.0,0.0,0.0,(x1-x2)*Etot/2.0);
     all_momenta["pf3"].set(0.0,0.0,0.0,0.0);
     all_momenta["pf4"].set(0.0,0.0,0.0,0.0);
     push_back_event(sigma);
     //production_events.push_back(new Event(sigma,all_momenta));
}

void GluonFusion::push_back_event(const double & sigma)
{
     
     production_events.push_back(new Event(sigma,all_momenta,xx_vegas));
}



void GluonFusion::LO_event_kinematics(const double & x1,
                                      const double & x2)
{
     all_momenta["p1"].set(x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
     all_momenta["p2"].set(x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
     all_momenta["h"].set((x1+x2)*Etot/2.0,0.0,0.0,(x1-x2)*Etot/2.0);
}

void GluonFusion::NLO_event_kinematics(const double & x1,
                                       const double & x2,
                                       const double & z,
                                       const double & s13,
                                       const double & s23)
{
     if (s13==0.0 and s23==0.0)
          {
          //: actually LO kinematics
          LO_event_kinematics(x1,x2);
          }
     else
          {
          
          //     ----------------- Higgs and gluon momenta ----------------------
          const double shat = pow(Etot,2.0)*x1*x2;
          const double pt3 = sqrt(s13*s23/shat); //gluon 1
          all_momenta["p1"].set(x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
          all_momenta["p2"].set(x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
          const double s1H = shat-s13;
          const double s2H = shat-s23;
          const double En3 = 0.5*(s13/x1/Etot+s23/x2/Etot);
          const double pz3 = 0.5*(-s13/x1/Etot+s23/x2/Etot);
          const double En =  0.5*(s1H/x1/Etot+s2H/x2/Etot);
          const double pZ =  0.5*(-s1H/x1/Etot+s2H/x2/Etot);

          if (pt3<=ptbuf )
               {
               // icase="only Higgs";
               all_momenta["h"].set(En,0.0,0.0,pZ);
               all_momenta["pf3"].set(En3,0.0,0.0,pz3);
               //const double phi_rotation_angle = 2.0*consts::Pi*rand();
               //pH.rotate(phi_rotation_angle,unsigned(3));
               //p3.rotate(phi_rotation_angle,unsigned(3));
               //p4.rotate(phi_rotation_angle,unsigned(3));
               }
          else
               {
               // icase=" Higgs + p3";
               double ptsq=s1H*s2H/shat-pow(Model.higgs.m(),2.0);
               if (ptsq<0.0 and abs(ptsq)<1e-5) {ptsq=0.0;}//: taking the absolute value cares
               //: for the case ptsq= -1e-16 which can happen due to roundoff
               const double pT  = sqrt(ptsq); // Higgs
               all_momenta["h"].set(En,pT,0.0,pZ);
               all_momenta["pf3"].set(En3,-pt3,0.0,pz3);
               const double phi_rotation_angle = 2.0*consts::Pi*ISP.phi;
               all_momenta["h"].rotate(phi_rotation_angle,unsigned(3));
               all_momenta["pf3"].rotate(phi_rotation_angle,unsigned(3));
               }
          
          
          }

}

void GluonFusion::set_up_event_kinematics(
                                           const double & x1,
                                           const double & x2,
                                           const double & z,
                                           const double & s13,
                                           const double & s23,
                                           const double & s14,
                                           const double & s24,
                                           const double & s34)
{
     all_momenta.flush();
     
     if (s13==0.0 and s23==0.0)
          {
          //: NLO kinematics: we rename particle 4 to be particle 3 and call NLO_event
          NLO_event_kinematics(x1,x2,z,s14,s24);
          }
     else if (s14==0.0 and s24==0.0)
          {
          //: NLO kinematics: we  call NLO_event
          NLO_event_kinematics(x1,x2,z,s13,s23);
          }
     else
          {
     
     /*
      //: ----- pt_H only !!
      const double shat = pow(Etot,2.0)*x1*x2;
      
      const double s1H = shat-s13-s14;
      const double s2H = shat-s23-s24;
      double ptsq=s1H*s2H/shat-pow(Model.higgs.m,2.0);
      if (ptsq<0.0 and abs(ptsq)<1e-5) {ptsq=0.0;}//: taking the absolute value cares 
      //: for the case ptsq= -1e-16 which can happen due to roundoff
      const double pT  = sqrt(ptsq); // Higgs
      pH.set(0.0,pT,0.0,0.0);
      //:-------- untill here
      */
     
     //string icase="";
     
     //: p1+p2 -> pH + p3 + p4 -> X1+X2+... + p3 + p4
     //: the kinematical variables defined are
     //: s_ij = 2*p_i*p_j
     //: s_iH = 2*p_i*p_H
     const double shat = pow(Etot,2.0)*x1*x2;
     
     const double s1H = shat-s13-s14;
     const double s2H = shat-s23-s24;
     const double s3H = s13+s23-s34;
     //const double s4H = s14+s24-s34;
     
     all_momenta["p1"].set(x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
     all_momenta["p2"].set(x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
     //     ----------------- Higgs and gluon momenta ----------------------
     
     double ptsq=s1H*s2H/shat-pow(Model.higgs.m(),2.0);
     if (ptsq<0.0 and abs(ptsq)<1e-5) {ptsq=0.0;}//: taking the absolute value cares 
     //: for the case ptsq= -1e-16 which can happen due to roundoff
     const double pt3 = sqrt(s13*s23/shat); //gluon 1
     const double pt4 = sqrt(s14*s24/shat); //gluon 2 
     const double pT  = sqrt(ptsq); // Higgs
     const double En3 = 0.5*(s13/x1/Etot+s23/x2/Etot);
     const double En4 = 0.5*(s14/x1/Etot+s24/x2/Etot);
     const double En =  0.5*(s1H/x1/Etot+s2H/x2/Etot);
     const double pz3 = 0.5*(-s13/x1/Etot+s23/x2/Etot);
     const double pz4 = 0.5*(-s14/x1/Etot+s24/x2/Etot);
     const double pZ =  0.5*(-s1H/x1/Etot+s2H/x2/Etot);
     
     
     //: LO kinematics : Higgs + 0 hard partons, with HpT=0
     if ((pt3<=ptbuf and pt4<=ptbuf) )
          {
          // icase="only Higgs";
          all_momenta["h"].set(En,0.0,0.0,pZ);
          all_momenta["pf3"].set(En3,0.0,0.0,pz3);
          all_momenta["pf4"].set(En4,0.0,0.0,pz4);
          //const double phi_rotation_angle = 2.0*consts::Pi*rand();
          //pH.rotate(phi_rotation_angle,unsigned(3));
          //p3.rotate(phi_rotation_angle,unsigned(3));
          //p4.rotate(phi_rotation_angle,unsigned(3));
          }
     //: NLO real kinematics : H + hard parton
     else if (pt3>ptbuf and pt4<ptbuf)
          {
          // icase=" Higgs + p3";
          all_momenta["h"].set(En,pT,0.0,pZ);
          all_momenta["pf3"].set(En3,-pt3,0.0,pz3);
          all_momenta["pf4"].set(En4,0.0,0.0,pz4);
          const double phi_rotation_angle = 2.0*consts::Pi*rand();
          all_momenta["h"].rotate(phi_rotation_angle,unsigned(3));
          all_momenta["pf3"].rotate(phi_rotation_angle,unsigned(3));
          //p4.rotate(phi_rotation_angle,unsigned(3));
          }
     else if (pt4>ptbuf and pt3<ptbuf)
          {
          //  icase=" Higgs + p4";
          all_momenta["h"].set(En,pT,0.0,pZ);
          all_momenta["pf4"].set(En4,-pt4,0.0,pz4);
          all_momenta["pf3"].set(En3,0.0,0.0,pz3);
          const double phi_rotation_angle = 2.0*consts::Pi*rand();
          all_momenta["h"].rotate(phi_rotation_angle,unsigned(3));
          all_momenta["pf4"].rotate(phi_rotation_angle,unsigned(3));
          //p3.rotate(phi_rotation_angle,unsigned(3));
          }
     //: NNLO double real kinematics : H + 2 hard partons
     else if (pt3>ptbuf and pt4>ptbuf)
          {
          if (pT>=0.1*ptbuf)//:the Higgs is not extra soft
               {
               //   icase=" Higgs + p3 + p4";
               //: Higgs is temporary phi-reference
               //: fixing the phi3		
               double cos3H = (En3*En-pz3*pZ-0.5*s3H)/pt3/pT;
               if (cos3H<-1.0) cos3H=-1.0;
               else if (cos3H>1.0) cos3H=1.0;
               double phi3 = acos(cos3H);//: acos returns in [0,Pi]
               //: we now decide if p3T is in the lower or upper semicircle
               //: in the pT plane
               if (rand()<0.5) phi3 = 2.0*consts::Pi - phi3;//: if lower, phi -> 2*Pi-phi
               
               //all_momenta["pf3"].set_phi(phi3);
               //all_momenta["h"].set_phi(0.0);
               all_momenta["pf3"].set(En3,pt3*cos(phi3),pt3*sin(phi3),pz3);
               all_momenta["h"].set(En,pT,0.0,pZ);
               all_momenta["pf4"] = -all_momenta["h"]-all_momenta["pf3"]+all_momenta["p1"]+all_momenta["p2"];
               //: now we rotate all pTs by a random angle 
               const double phi_rotation_angle = 2.0*consts::Pi*rand();
               all_momenta["h"].rotate(phi_rotation_angle,unsigned(3));
               all_momenta["pf4"].rotate(phi_rotation_angle,unsigned(3));
               all_momenta["pf3"].rotate(phi_rotation_angle,unsigned(3));
               }
          else if (pT<0.1*ptbuf)//: soft Higgs accidentally, two jets back to back
               {
               //  icase="  p3 + p4";
               //: p3 is the phi-reference along the x-axis
               //cout<<"\n the soft Higgs case : PTH,pt3,pt4 ="<<pT<<"\t"<<pt3<<"\t"<<pt4;
               all_momenta["h"] = fvector(En,0.0,0.0,pZ);
               all_momenta["pf3"] = fvector(En3,pt3,0.0,pz3);
               all_momenta["pf4"] = fvector(En4,-pt4,0.0,pz4);
               //: now we rotate all pTs by a random angle 
               const double phi_rotation_angle = 2.0*consts::Pi*rand();
               //pH.rotate(phi_rotation_angle,unsigned(3)); //: no need to rotate pH since it has zero pt
               all_momenta["pf4"].rotate(phi_rotation_angle,unsigned(3));
               all_momenta["pf3"].rotate(phi_rotation_angle,unsigned(3));
               }
          else
               {
               cout<<"\n["<<__func__<<"]\tERROR : if you are at this fork, there must be a NAN among the pTs!"<<endl;
               cout<<"\n info on kinematics: ptH="<<pT<<"\tpt3"<<pt3<<"\tpt4="<<pt4
               <<"\t"<<s1H<<" "<<s2H<<" x1="<<x1<<" x2="<<x2;
               }
          }
     else
          {
          cout<<"\n["<<__func__<<"]\tERROR : if you are at this fork, there must be a NAN among the pTs!"<<endl;
          cout<<"\n info on kinematics: ptH="<<pT<<"\tpt3"<<pt3<<"\tpt4="<<pt4
          <<"\t"<<s1H<<" "<<s2H<<" x1="<<x1<<" x2="<<x2;
          }
     //: momentum conservation check
     //
     // fvector PM = p1+p2-p3-p4-pH;
     //for (int i=0;i<4;i++) 
     // {
     //       if (abs(PM[i])>1e-1) 
     //       {
     //           cout<<"\n****";
     //           cout<<"\n case = "<<icase;
     //           cout<<"\nerror in momentum conservation, PM="
     //           <<PM<<"\t"<<p1<<"\t"<<p2<<"\t"<<p3<<"\t"<<p4<<"\t"<<pH;
     //           cout<<"\nptsq="<<s1H*s2H/shat-pow(Model.higgs.m,2.0);
     //           cout<<"\ns1H="<<s1H<<"\ts2H="<<s2H<<"\ts3H="<<s3H<<"\ts4H="<<s4H<<"\ts13="<<s13<<"\ts23="<<s23<<"\ts14="<<s14<<"\ts24="<<s24;
     //           cout<<"\npt3="<<pt3<<"\tpt4="<<pt4<<"\tptbuf="<<ptbuf;
     //       }
     // 
     //}
     
     /*
      if(jetalgorithm)
      {
      //jet algorithm definitions
      const double ptjetmin=20.0;
      //const double yjetmax=2.5;
      const double R=0.4;
      
      all_momenta.jetnumber=0;
      
      
      //: p3 and p4 are ultra soft
      if (pt3<=ptbuf and pt4<=ptbuf)  //0-jet bin
      {
      //:nothing needs to be done here
      }
      //: p3 ultra soft
      if (pt3<=ptbuf and pt4>ptbuf) 
      {
      //const double y4=p4.zrap();
      if (pt4>ptjetmin) //: p4 is hard enough to be a jet
      {
      all_momenta.pjet1=all_momenta["pf4"];
      all_momenta.jetnumber=1;
      }
      }
      //: p4 ultra soft
      if (pt4<=ptbuf and pt3>ptbuf) 
      {
      //const double y3=p3.zrap();
      if (pt3>ptjetmin) //: p3 is hard enough to be a jet
      {
      all_momenta.pjet1=all_momenta["pf3"];
      all_momenta.jetnumber=1;
      }
      }
      //: none ultra soft, this is an event coming from honest RR
      if (pt3>ptbuf and pt4>ptbuf)
      {
      //const double pi = 3.141592653589793;
      //: now we have to calculate angular seperation
      const double y3=all_momenta["pf3"].zrap();
      const double y4=all_momenta["pf4"].zrap();
      //cout<<"\n phi3 "<<acos(0.7);
      //cout<<"\n phi4 "<<acos(-0.7);
      const double delta_phi = all_momenta["pf3"].phi() -  all_momenta["pf4"].phi()  ;
      //: taking care of the dphi>Pi case
      //if (delta_phi>pi) {delta_phi = 2*pi -delta_phi;cout<<"\n it happened";}
      const double d12 = pow(y3-y4,2.0) + pow(delta_phi,2.0);
      //cout<<"\n-----\nd12="<<d12<<"\tRR="<<R<<"\t delta_phi^2="<<delta_phi
      //<<"\t"<<X->p3.phi()<<"\t"<<X->p4.phi();
      //cout<<"\np3="<<X->p3<<"\tp4="<<X->p4;
      if (d12>R*R)//: potential 2-jet case / no parton merging
      {
      all_momenta.pjet1=all_momenta["pf3"];
      all_momenta.pjet2=all_momenta["pf4"];
      all_momenta.jetnumber=2;
      }
      else //: we need to merge the two partons in one jet 
      {
      all_momenta.pjet1 = all_momenta["pf3"]+all_momenta["pf4"];
      all_momenta.jetnumber=1;
      
      }
      }
      }
      */
     } //: end of else from trivial NLO checks at the begining of the function
}







//: ------- gluon fusion LO 
void GluonFusion::LO()
{
     //: the_sector->ME->epsilon_power doesn't matter here because LO has the structure 1+e+e^2
     if (the_sector->ME->epsilon_power()>=0)
          {
          double sigma_central =   pref_sgg
                              *ISP.measLO
                              *cur_lumiLO[0]
                              //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                              *sector_specific_prefactors_from_a_e_expansion;
          
          JLO(sigma_central);
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
          *cur_lumi[0]
          //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
          *sector_specific_prefactors_from_a_e_expansion
          ;
          
          
          //: note that this is a finite sector, so no z-subtraction is needed.
          //: note that it also has one integral
          //          cout<<"\n fix the nlo_me() if you want results here"<<endl;exit(1);
          for (unsigned i=0;i<the_sector->ME->number_of_sectors_in_this_topology;i++)
               {
               double dummyres;
               (*the_sector->ME->franz_func)(i+1,the_sector->ME->epsilon_power(),shat,x1,x2,  z,lh, weight,consts::nf,lambda,0.0,0.0,0.0,dummyres);
               }
          }
}


//: SOFT means virtual + soft real + renormalization
void GluonFusion::gg_NLO_SOFT()
{
     if (the_sector->ME->epsilon_power()==0)
          {
          //double beta_0 = 11.0/4.0-consts::nf/6.0;
          double sigma_soft_nlo = pref_sgg
                    *ISP.measLO
                    //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                    *cur_lumiLO[0]
                    *sector_specific_prefactors_from_a_e_expansion
                    * consts::pi_square;//: the total soft finite term
                    //  + pow(WC.c0,2.0) * (-2.0*beta_0) //: the renormalization term
                    //  + pow(WC.c0,2.0)*2.0*beta_0*log_mur_sq_over_muf_sq //: a log dependent term from a^2 of the born (hence the factor of 2)
                    //  + 2.0*WC.c0*WC.c1 );//: the wilson coefficient contribution
          JLO(sigma_soft_nlo);
          }
     else if (the_sector->ME->epsilon_power()==1)
          {
          //double beta_0 = 11.0/4.0-consts::nf/6.0;
          double sigma_soft_nlo = pref_sgg
          *ISP.measLO
          //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
          *cur_lumiLO[0]
          *sector_specific_prefactors_from_a_e_expansion
          * (consts::pi_square - 3.0); //: the epsilon piece of soft
//            + pow(WC.c0,2.0) * (-2.0*beta_0) //: the renormalization term
//            + pow(WC.c0,2.0)*2.0*beta_0*log_mur_sq_over_muf_sq//: the log dependent term from born
//            + 2.0*WC.c0*WC.c1 //: coming from the epsilon term of the born B(epsilon)
//            );
          JLO(sigma_soft_nlo);
          }
     else
          {
          book_production_event();
          }
}

//: HARD
void GluonFusion::gg_NLO_HARD()
{
     if (abs(the_sector->ME->epsilon_power())>2)
          {
          book_production_event();
          }
     else
          {
          double dummyres;
          double weight = pref_sgg
                         *ISP.meas
                         *cur_lumi[0]
                         //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                         *1.0/(1.0-ISP.z)
                         *sector_specific_prefactors_from_a_e_expansion
                         ;
          double weightLO = pref_sgg
                         *ISP.measLO
                         *cur_lumiLO[0]
                         //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                         *1.0/(1.0-ISP.z)
                         *sector_specific_prefactors_from_a_e_expansion
                         ;
          if (the_sector->ME->epsilon_power()== -1)
               {
               for (int ts=1;ts<3;ts++) //: ts=topology sector (there are 2 in rgg2ght1)
                    {
                    rgg2ght1(ts,-1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,lh, weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,-weightLO,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    }
               }
          else if (the_sector->ME->epsilon_power()==0)
               {
               for (int ts=1;ts<3;ts++) //: ts=topology sector (there are 2 in rgg2ght1)
                    {
                    //: contribution from finite coefficient of rgg2ght1
                    rgg2ght1(ts,0,ISP.curs,ISP.x1,ISP.x2,  ISP.z,lh, weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,0,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,-weightLO,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    //: contribution from pole coefficient times logs of (1-z) and muf/mh
                    //: see documentation for explanation of extra_weight
                    double extra_weight = -2.0*ISP.Log_1mz ;
                    rgg2ght1(ts,-1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,lh, weight*extra_weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,-weightLO*extra_weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    }
               }
          else if (the_sector->ME->epsilon_power()==1)
               {
               for (int ts=1;ts<3;ts++) //: ts=topology sector (there are 2 in rgg2ght1)
                    {
                    
                    //: contribution from finite coefficient of rgg2ght1
                    rgg2ght1(ts,1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,lh, weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,-weightLO,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    //: contribution from pole coefficient times logs of (1-z) and muf/mh
                    //: see documentation for explanation of extra_weight
                    double extra_weight = -2.0*ISP.Log_1mz;
                    rgg2ght1(ts,0,ISP.curs,ISP.x1,ISP.x2,  ISP.z,lh, weight*extra_weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,0,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,-weightLO*extra_weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    
                    double extra_weight2 = 2.0*pow(ISP.Log_1mz,2.0);
                    rgg2ght1(ts,-1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,lh, weight*extra_weight2,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,-weightLO*extra_weight2,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    }

               }
          }
          
}

//: DOUBLE SOFT
void GluonFusion::gg_NNLO_SOFT()
{
     double weight=pref_sgg
                    *ISP.meas
                    //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                    *cur_lumi[0]
                    *sector_specific_prefactors_from_a_e_expansion;
     double weightLO=pref_sgg
                    *ISP.measLO
                    //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                    *cur_lumiLO[0]
                    *sector_specific_prefactors_from_a_e_expansion;
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
          *cur_lumi[0]
          *sector_specific_prefactors_from_a_e_expansion
          //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
          *pow(WC.c0,2.0)
          ;
          //z=0.1;x2=tau/z/x1;shat=pow(Model.higgs.m,2.0)/z; weight=1.0;//: franz's test point
          if (the_sector->ME->is_franz_topology)
               {
               for (int i=0;i<the_sector->ME->number_of_sectors_in_this_topology;i++)
                    {
                    (*the_sector->ME->franz_func)
                         (i+1,//:franz counts from one
                          the_sector->ME->epsilon_power(),
                          shat,x1,x2,  z,lh, weight,consts::nf,
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
     if (the_sector->ME->epsilon_power()<-3 or the_sector->ME->epsilon_power()>1){book_production_event();}
     else
          {
          double dummyres;
          double shat =ISP.curs;
          double x1=ISP.x1;
          double x2 = ISP.x2;
          double z=ISP.z;
          double weight = pref_sgg
                         *ISP.meas
                         *cur_lumi[0]
                         *sector_specific_prefactors_from_a_e_expansion
                         //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                         *1.0/(1.0-ISP.z)
                         *pow(WC.c0,2.0)
                         ;
          double weightLO = pref_sgg
                         *ISP.measLO
                         *cur_lumiLO[0]
                         *sector_specific_prefactors_from_a_e_expansion
                         //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                         *1.0/(1.0-ISP.z)
                         *pow(WC.c0,2.0)
                         ;
          //z=0.1;x2=tau/z/x1;shat=pow(Model.higgs.m,2.0)/z; weight=1.0;//: franz's test point
          if (the_sector->ME->is_franz_topology)
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
                    for (int i=0;i<the_sector->ME->number_of_sectors_in_this_topology;i++)
                         {
//                         cout<<"\n** calling rvgg2ght1 with args "
//                         <<"sector= "<<i+1<<" pole="<<m<<" shat="<<shat<<" x1="<<x1<<" x2="<<x2<<" lh="<<lh
//                         <<" weight="<<weight*thelog<<" nf="<<consts::nf<<" l1="<<lambda1<<" l2="<<lambda2
//                         <<" l3="<<lambda3<<" l4="<<lambda4;
                         (*the_sector->ME->franz_func)
                         (i+1,//:franz counts from one
                          m,
                          shat,x1,x2,  z,lh, weight*thelog,consts::nf,
                          lambda1,lambda2,lambda3,lambda4,dummyres);
//                         cout<<"\n** calling rvgg2ght1 with args "
//                         <<"sector= "<<i+1<<" pole="<<m<<" shat="<<shat<<" x1="<<x1<<" x2="<<x2<<" lh="<<lh
//                         <<" weight="<<weight*thelog<<" nf="<<consts::nf<<" l1="<<lambda1<<" l2="<<lambda2
//                         <<" l3="<<lambda3<<" l4="<<lambda4;
                         (*the_sector->ME->franz_func)
                         (i+1,//:franz counts from one
                          m,
                          ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh, -weightLO*thelog,consts::nf,
                          lambda1,lambda2,lambda3,lambda4,dummyres);
                         }
                    }
               }
          }
}



//: exact MEs below

void GluonFusion::LO_exact()
{
     if (the_sector->ME->epsilon_power()>=0)
          {
          double sigma_central =   pref_sgg
          *ISP.measLO
          *cur_lumiLO[0]
          //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
          *sector_specific_prefactors_from_a_e_expansion
          *LO_exact_coefficient[the_sector->ME->epsilon_power()];//: LO_exact_coefficient is a vector that holds the coefficients calculated once
          //cout<<"\n**LO exact coefficient = "<<LO_exact_coefficient[the_sector->ME->epsilon_power()];
          JLO(sigma_central);
          }
}

void GluonFusion::NLO_soft_exact()
{
     if (the_sector->ME->epsilon_power()==0) //: we do not have an implementation of the e-pieces
          {
          double sigma_central =   pref_sgg
          *ISP.measLO
          *cur_lumiLO[0]
          //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
          *sector_specific_prefactors_from_a_e_expansion
          *NLO_soft_exact_coefficient[the_sector->ME->epsilon_power()];//: LO_exact_coefficient is a vector that holds the coefficients calculated once
          //cout<<"\n** "<<LO_exact_coefficient[the_sector->ME->epsilon_power];
          JLO(sigma_central);
          }
}

void GluonFusion::calculate_exact_born_me_LO()
{
     LO_exact_coefficient.push_back(LO_exact_e0());
     LO_exact_coefficient.push_back(LO_exact_e1());
     LO_exact_coefficient.push_back(LO_exact_e2());
     
     NLO_soft_exact_coefficient.push_back(NLO_soft_exact_e0());
     
}

double GluonFusion::LO_exact_e0()
{

     complex<double> ME ;
     
     for (int i=0;i<Model.quarks.size();i++)
          {
          ME = ME + Model.quarks[i]->Y * born(Model.quarks[i]->X);
          }
    
    cout<<"\n LO exact : "<<pow(abs(ME),2.0);

     return(pow(abs(ME),2.0));
}

double GluonFusion::LO_exact_e1()
{
     
     complex<double> ME ;
     
     for (int i=0;i<Model.quarks.size();i++)
          {
          ME = ME + Model.quarks[i]->Y * born_e(Model.quarks[i]->X);
          }
     return(pow(abs(ME),2.0));
}

double GluonFusion::LO_exact_e2()
{
     
     complex<double> ME ;
     
     for (int i=0;i<Model.quarks.size();i++)
          {
          ME = ME + Model.quarks[i]->Y * born_e(Model.quarks[i]->X);
          }
     return(pow(abs(ME),2.0));
}

complex<double> GluonFusion::born(complex<double> x)
{

     //: the expression below goes to 1 as mq->infty, i.e. as x->1
     complex<double > res=(-3.0)*x/pow(1.0-x,2.0)*(2.0-pow(1.0+x,2.0)/pow(1.0-x,2.0)*HPL2(0,0,x));
     //res = -16.0*pow(1.0+x,2.0)/x*HPL2(0,0,x)+32.0*pow(1.0-x,2.0)/x;
     
     return res;
}

complex<double> GluonFusion::born_e(complex<double> x)
{
     //: copied (and translated) from ihixs, which copied from hggtotal
     complex<double > res=  -3.0*x/pow(1.0-x,4.0) *
                         (
                             +2.0 * (1.0 - x*x) * HPL1(0,x)
                             +2.0 * (1.0 + x*x) * HPL2(0,0,x)
                             + pow(1.0+x,2.0) *
                                   (
                                      1.0/6.0 * consts::pi_square  *HPL1(0,x)
                                    + 2.0 * HPL3(0,-1,0,x)
                                    - HPL3(0,0,0,x)
                                    + 3.0 * consts::z3
                                   )
                              +2.0 * pow(1.0-x,2.0)
                         );
     
     
     
     return res;
}

complex<double> GluonFusion::born_e2(complex<double> x)
{
     //: copied (and translated) from ihixs, which copied from hggtotal

     complex<double > res = -16.0/3.0*pow(1.0+x,2)/x*consts::pi_square *HPL2(0,-1,x)
                         +(-16.0/3.0*(1.0+x*x)/x*consts::pi_square+32.0*pow(1.0+x,2)/x*consts::z3-32.0*(1.0+x)*(-1.0+x)/x)*HPL1(0,x)
                         +32.0*pow(1.0+x,2)/x*HPL4(0,0,-1,0,x)
                         +64.0*(-1.0+x)*(1.0+x)/x*HPL2(-1,0,x)
                         +(8.0/3.0*pow(1.0+x,2)/x*consts::pi_square-16.0*(3.0*x+1.0)*(-1.0+x)/x)*HPL2(0,0,x)
                         -64.0*pow(1.0+x,2)/x*HPL4(0,-1,-1,0,x)
                         +32.0*pow(1.0+x,2)/x*HPL4(0,-1,0,0,x)
                         -64.0*(1.0+x*x)/x*HPL3(0,-1,0,x)
                         +32.0*(1.0+x*x)/x*HPL3(0,0,0,x)
                         -16.0*pow(1.0+x,2)/x*HPL4(0,0,0,0,x)
                         +2.0/9.0*pow(1.0+x,2)/x*pow(consts::pi_square,2.0)
                         +16.0/3.0*(-1.0+x)*(1.0+x)/x*consts::pi_square
                         -96.0*(1.0+x*x)/x*consts::z3
                         +64.0*pow(-1.0+x,2.0)/x;
     return res;
}


double GluonFusion::NLO_soft_exact_e0()
{
     
     complex<double> V(0.0,0.0) ;
     complex<double> Born(0.0,0.0);
     for (int i=0;i<Model.quarks.size();i++)
          {
        //  cout<<"\n quark # "<<i+1<<": "<<Model.quarks[i]->name<<" with mass "<<Model.quarks[i]->m()<<" Y="<<Model.quarks[i]->Y<<" x="<<Model.quarks[i]->X;
          V = V + Model.quarks[i]->Y * ggf_exact_virtual_ep0(Model.quarks[i]->X);
          Born = Born+Model.quarks[i]->Y * born(Model.quarks[i]->X);
          //cout<<"\t\t ME="<<ME;
          
          }
     //cout<<"\n ME = "<<ME;
     double res = real(Born * conj(V)) + pow(abs(Born),2.0) * consts::pi_square;
     cout<<"\n NLO soft= "<<res<<"\t"<<conj(V);
     return(res);
}






void GluonFusion::rgg2ghEXACT(int pole,double s,double x1,double x2,double z,double lh,double  weight,double nf, double lambda)
{
     if (pole==-1)
          {
          //: here only the pole from the collinear term contributes
          double C=(-1.0)*3.0*pow(1.0-z+z*z,2.0)/z * LO_exact_coefficient[0];
          Jnlo(C,x1, x2,z,0.0);
          Jnlo(C,x1, x2,z,1.0);
          }
     if (pole==0)
          {
          double F = 3.0*pow(1.0-z+z*z,2.0)* LO_exact_coefficient[0];
          if (z!=1.0)
               {
               double A1sq = abs_sq_of_sum_over_quarks_of(Aq1,z,lambda);
               double A2sq = abs_sq_of_sum_over_quarks_of(Aq2a,z,lambda);
               double A3sq = abs_sq_of_sum_over_quarks_of(Aq2b,z,lambda);
               double A4sq = abs_sq_of_sum_over_quarks_of(Aq2c,z,lambda);
               double H1 = 3.0/2.0*pow(z,4.0) * (A1sq + A2sq + A3sq + A4sq);
               double H = H1/z/lambda/(1.0-lambda);
               double C0 = -F/lambda/z ;
               double C1 = -F/(1.0-lambda)/z;
               double Crem = F*(log(z)-lh);
               Jnlo(H,x1, x2,z,lambda);
               Jnlo(C0,x1, x2,z,0.0);
               Jnlo(C1,x1, x2,z,1.0);
               Jnlo(Crem,x1, x2,z,0.0);
               Jnlo(Crem,x1, x2,z,1.0);
               }
          else if (z==1.0)
               {
               double Crem = F*(log(z)-lh);
               Jnlo(Crem,x1, x2,z,0.0);
               Jnlo(Crem,x1, x2,z,1.0);
               }
          }
}

double GluonFusion::abs_sq_of_sum_over_quarks_of(complex<double> (*f)(const double& z, const double& lambda,  const complex<double>& M,const double& QQQ),const double & z,const double & lambda)
{
     
     complex<double> ME ;
     
     for (int i=0;i<Model.quarks.size();i++)
          {
          ME = ME + Model.quarks[i]->Y * (f)(z,lambda,Model.quarks[i]->complex_mass_squared_at_mur(),Model.higgs.m())
                                        * Model.quarks[i]->Wq * 3.0/32.0 /pow(Model.higgs.m(),2.0);
          }
     return(pow(abs(ME),2.0));
}

void GluonFusion::gg_NLO_hard_exact()
{
     if (abs(the_sector->ME->epsilon_power())>2)
          {
          book_production_event();
          }
     else
          {
          double weight = pref_sgg
          *ISP.meas
          *cur_lumi[0]
          //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
          *1.0/(1.0-ISP.z)
          *sector_specific_prefactors_from_a_e_expansion
          ;
          double weightLO = pref_sgg
          *ISP.measLO
          *cur_lumiLO[0]
          //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
          *1.0/(1.0-ISP.z)
          *sector_specific_prefactors_from_a_e_expansion
          ;
          double weight_soft = pref_sgg*ISP.measLO*cur_lumiLO[0]*sector_specific_prefactors_from_a_e_expansion/2.0;
          if (the_sector->ME->epsilon_power()== -2)//: double pole from the soft limit
               {
                    rgg2ghEXACT(-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,  1.0,lh, weight_soft,consts::nf,0.0);
               }
          if (the_sector->ME->epsilon_power()== -1)
               {
                    
                    rgg2ghEXACT(-1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,lh, weight,consts::nf,ISP.lambda);
                    rgg2ghEXACT(-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,-weightLO,consts::nf,ISP.lambda);
                    rgg2ghEXACT(-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,weight_soft*ISP.Log_1mz,consts::nf,ISP.lambda);               
               }
          else if (the_sector->ME->epsilon_power()==0)
               {
               
                    //: contribution from finite coefficient of rgg2ght1
                    rgg2ghEXACT(0,ISP.curs,ISP.x1,ISP.x2,  ISP.z,lh, weight,consts::nf,ISP.lambda);
                    rgg2ghEXACT(0,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,-weightLO,consts::nf,ISP.lambda);
                    //: contribution from pole coefficient times logs of (1-z) and muf/mh
                    //: see documentation for explanation of extra_weight
                    double extra_weight = -2.0*ISP.Log_1mz ;
                    rgg2ghEXACT(-1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,lh, weight*extra_weight,consts::nf,ISP.lambda);
                    rgg2ghEXACT(-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,-weightLO*extra_weight,consts::nf,ISP.lambda);
                    rgg2ghEXACT(-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,lh,weight_soft*pow(ISP.Log_1mz,2.0)/2.0,consts::nf,ISP.lambda);
                    
               }
          }
     
}















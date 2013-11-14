#include "gluon_fusion.h"

#ifndef ONCE_PTR_TO_GGF
#define ONCE_PTR_TO_GGF
Production* ptr_to_GGF; //: static global pointer to use as a handle for plugins (like the fortran or c++ NNLO double real pieces)
//Process* ptr_to_process;
#endif


//: external function definitions
#include "nlo_exact_matrix_elements.h"



MeExternalInfo::MeExternalInfo(const string & _pi,const string & _pj,const string& _pord,
               const string & _name, int _epower,
               const string & _me_approximation,int alpha_ew_pow)
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
    alpha_ew_power = alpha_ew_pow;
}

//------------------------------------------------------------------------------

MatrixElement::MatrixElement( MeExternalInfo* info,const string & _kin,
                             const string& _str_param,
                             ptr_to_GluonFusion_function _the_ggf_func,
                             FranzBinder* fr,
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
    _franz = fr;
    epsilon_exponent_in_z_subtraction = _e_exp_in_subtr;
}



ostream& operator<<(ostream& stream, const MatrixElement& ME)
{
    stream<<"S("<<ME.parton_i()<<","<<ME.parton_j()<<","<< ME.name()
    <<",a^"<<ME.alpha_power()<<",e^"<<ME.epsilon_power()
    <<" ,dim="<<ME.dimension<<")";
    
    return stream;
}


//------------------------------------------------------------------------------

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


GluonFusionMatrixElementBox::GluonFusionMatrixElementBox()
{
    //: linking the matrix elements
    add_gg_sectors();
    add_qg_sectors();
    add_gq_sectors();
    add_qqbar_sectors();
    add_q1q2_sectors();
    add_qq_sectors();
}


void GluonFusionMatrixElementBox::add_qqbar_sectors()
{
    
//    
//    
//    const int num_topologies=14;
//    pointer_to_Franz_gluon_fusion rr[num_topologies]={rrqqbar2qqbarht1,rrqqbar2qqbarht2,rrqqbar2qqbarht3,rrqqbar2qqbarht4,rrqqbar2qqbarht5,rrqqbar2qqbarht6,rrqqbar2gght1,rrqqbar2gght2,rrqqbar2gght3,rrqqbar2gght4,rrqqbar2gght5,rrqqbar2gght6,rrq1q1bar2q2q2barht1,rrq1q1bar2q2q2barht2};
//    int num_sect[num_topologies]={1,2,1,4,1,1,1,2,1,1,4,1,1,1};//: number of sectors per topology
    
    push_me("quark","antiquark","NLO","R","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,0,1,new FranzBinder(rqqbar2ght1,1),"effective",0.0);
    push_me("quark","antiquark","NNLO","RV","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-2,0,new FranzBinder(rvqqbar2ght1,1),"effective",0.0);
    
    vector<FranzBinder *> RR_functions;
    RR_functions.push_back(new FranzBinder(rrqqbar2qqbarht1,1));
    RR_functions.push_back(new FranzBinder(rrqqbar2qqbarht2,2));
    RR_functions.push_back(new FranzBinder(rrqqbar2qqbarht3,1));
    RR_functions.push_back(new FranzBinder(rrqqbar2qqbarht4,4));
    RR_functions.push_back(new FranzBinder(rrqqbar2qqbarht5,1));
    RR_functions.push_back(new FranzBinder(rrqqbar2qqbarht6,1));
    
    RR_functions.push_back(new FranzBinder(rrqqbar2gght1,1));
    RR_functions.push_back(new FranzBinder(rrqqbar2gght2,2));
    RR_functions.push_back(new FranzBinder(rrqqbar2gght3,1));
    RR_functions.push_back(new FranzBinder(rrqqbar2gght4,1));
    RR_functions.push_back(new FranzBinder(rrqqbar2gght5,4));
    RR_functions.push_back(new FranzBinder(rrqqbar2gght6,1));
    
    RR_functions.push_back(new FranzBinder(rrq1q1bar2q2q2barht1,1));
    RR_functions.push_back(new FranzBinder(rrq1q1bar2q2q2barht2,1));
    for (unsigned i=0; i<RR_functions.size(); i++)
        {
        stringstream name_str;name_str<<"RR t"<<i+1;
        push_me("quark","antiquark","NNLO",name_str.str(),"kinematics:NNLO",
                "param:NLO",&GluonFusion::NNLO_hard_no_subtraction,-2,0,RR_functions[i],"effective",0.0);
        }
    
    push_me("quark","antiquark","NLO","NLO hard exact","kinematics:NLO","param:NLO",&GluonFusion::qqbar_NLO_hard_exact,0,0,new FranzBinder,"exact",0.0);
}


void GluonFusionMatrixElementBox::add_q1q2_sectors()
{
//    const int num_topologies=2;
//    pointer_to_Franz_gluon_fusion rr[num_topologies]={rrq1q22q1q2ht1,rrq1q22q1q2ht2};
//    int num_sect[num_topologies]={1,1};//: number of sectors per topology
//    
//    
    vector<FranzBinder *> RR_functions;
    RR_functions.push_back(new FranzBinder(rrq1q22q1q2ht1,1));
    RR_functions.push_back(new FranzBinder(rrq1q22q1q2ht2,1));
    for (unsigned i=0; i<RR_functions.size(); i++)
        {
        stringstream name_str;name_str<<"RR t"<<i+1;
        push_me("quark","quark2","NNLO",name_str.str(),"kinematics:NNLO",
                "param:NLO",&GluonFusion::NNLO_hard_no_subtraction,-2,0,RR_functions[i],"effective",0.0);
        }
    
}

void GluonFusionMatrixElementBox::add_qq_sectors()
{
//    const int num_topologies=3;
//    pointer_to_Franz_gluon_fusion rr[num_topologies]={rrqq2qqht1,rrqq2qqht2,rrqq2qqht3};
//    int num_sect[num_topologies]={1,1,8};//: number of sectors per topology
//    
    
    vector<FranzBinder *> RR_functions;
    RR_functions.push_back(new FranzBinder(rrqq2qqht1,1));
    RR_functions.push_back(new FranzBinder(rrqq2qqht2,1));
    RR_functions.push_back(new FranzBinder(rrqq2qqht3,8));
    
    for (unsigned i=0; i<RR_functions.size(); i++)
        {
        stringstream name_str;name_str<<"RR t"<<i+1;
        push_me("quark","quark","NNLO",name_str.str(),"kinematics:NNLO",
                "param:NLO",&GluonFusion::NNLO_hard_no_subtraction,-2,0,RR_functions[i],"effective",0.0);
        }
}

void GluonFusionMatrixElementBox::add_gg_sectors()
{
//    const int num_topologies=29;
//    pointer_to_Franz_gluon_fusion rr[num_topologies]={rrgg2gght1,rrgg2gght2,rrgg2gght3,rrgg2gght4,rrgg2gght5,rrgg2gght6,rrgg2gght7,rrgg2gght8,rrgg2gght9,rrgg2gght10,rrgg2gght11,rrgg2gght12,rrgg2gght13,rrgg2gght14,rrgg2gght15,rrgg2gght16,rrgg2qqbarht1,rrgg2qqbarht2,rrgg2qqbarht3,rrgg2qqbarht4,rrgg2qqbarht5,rrgg2qqbarht6,rrgg2qqbarht7,rrgg2qqbarht8,rrgg2qqbarht9,rrgg2qqbarht10,rrgg2qqbarht11,rrgg2qqbarht12,rrgg2qqbarht13};
//    int num_sect[num_topologies]={2,2,1,1,8,2,2,4,2,1,1,2,1,1,1,1,2,2,1,1,2,2,4,2,1,1,1,2,1};//: number of sectors per topology
//    
    FranzBinder* zero_binder = new FranzBinder;
    push_me("gluon","gluon","LO","B","kinematics:LO","param:LO",
            &GluonFusion::LO,0,2,zero_binder,"effective",0.0);
    push_me("gluon","gluon","NLO","S","kinematics:LO","param:NLO",
            &GluonFusion::gg_NLO_SOFT,0,1,zero_binder,"effective",0.0);//: the soft is finite
    push_me("gluon","gluon","NLO","H","kinematics:NLO","param:NLO",
            &GluonFusion::gg_NLO_HARD,-1,1,zero_binder,"effective",0.0);
    push_me("gluon","gluon","NNLO","DOUBLE SOFT","kinematics:LO",
            "param:NLO",&GluonFusion::gg_NNLO_SOFT,-2,0,zero_binder,"effective",0.0);
    
    
    
    const int rv_no_sub_numtop=1;
    pointer_to_Franz_gluon_fusion rv_no_sub[rv_no_sub_numtop]={rvgg2ght1};
    int rv_no_subnum_sect[rv_no_sub_numtop]={6};
    push_me("gluon","gluon","NNLO","RVt1","kinematics:NLO","param:NLO",
            &GluonFusion::nlo_me,-3,0,new FranzBinder(rvgg2ght1,6),"effective",0.0);
    
    
    
    push_me("gluon","gluon","NNLO","RVt2","kinematics:NLO","param:NLO",
            &GluonFusion::NNLO_rv_with_subtraction,-3,0,
            new FranzBinder(rvgg2ght2,4),"effective",2.0);
    
    push_me("gluon","gluon","NNLO","RVt4","kinematics:NLO","param:NLO",
            &GluonFusion::NNLO_rv_with_subtraction,-3,0,new FranzBinder(rvgg2ght4,2),"effective",4.0);
    
    vector<FranzBinder *> RR_functions;
    RR_functions.push_back(new FranzBinder(rrgg2gght1,2));
    RR_functions.push_back(new FranzBinder(rrgg2gght2,2));
    RR_functions.push_back(new FranzBinder(rrgg2gght3,1));
    RR_functions.push_back(new FranzBinder(rrgg2gght4,1));
    RR_functions.push_back(new FranzBinder(rrgg2gght5,8));
    RR_functions.push_back(new FranzBinder(rrgg2gght6,2));
    RR_functions.push_back(new FranzBinder(rrgg2gght7,2));
    RR_functions.push_back(new FranzBinder(rrgg2gght8,4));
    RR_functions.push_back(new FranzBinder(rrgg2gght9,2));
    RR_functions.push_back(new FranzBinder(rrgg2gght10,1));
    RR_functions.push_back(new FranzBinder(rrgg2gght11,1));
    RR_functions.push_back(new FranzBinder(rrgg2gght12,2));
    RR_functions.push_back(new FranzBinder(rrgg2gght13,1));
    RR_functions.push_back(new FranzBinder(rrgg2gght14,1));
    RR_functions.push_back(new FranzBinder(rrgg2gght15,1));
    RR_functions.push_back(new FranzBinder(rrgg2gght16,1));
    
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht1,2));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht2,2));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht3,1));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht4,1));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht5,2));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht6,2));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht7,4));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht8,2));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht9,1));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht10,1));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht11,1));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht12,2));
    RR_functions.push_back(new FranzBinder(rrgg2qqbarht13,1));
    for (unsigned i=0; i<RR_functions.size(); i++)
        {
        stringstream name_str;name_str<<"RR t"<<i+1;
        push_me("gluon","gluon","NNLO",name_str.str(),
                "kinematics:NNLO","param:NLO",
                &GluonFusion::NNLO_hard_with_subtraction,-3,0,RR_functions[i],"effective",4.0);
        }
    
    
    //: exact matrix elements
    push_me("gluon","gluon","LO","LO exact","kinematics:LO","param:LO",&GluonFusion::LO_exact,0,2,new FranzBinder,"exact",0.0);
    push_me("gluon","gluon","NLO","NLO soft exact","kinematics:LO","param:NLO",&GluonFusion::NLO_soft_exact,0,0,new FranzBinder,"exact",0.0);//: only the e^0 piece
    push_me("gluon","gluon","NLO","NLO hard exact","kinematics:NLO","param:NLO",&GluonFusion::gg_NLO_hard_exact,-2,0,new FranzBinder,"exact",0.0);
    
    //: nlo ew
    available_matrix_elements.push_back(
    new MatrixElement(
    new MeExternalInfo("gluon","gluon","LO","NLO_EWK_S effective",0,"effective",1),
                "kinematics:LO","param:LO",&GluonFusion::NLO_ewk_soft,
                      new FranzBinder,0.0));
    available_matrix_elements.push_back(
    new MatrixElement(
    new MeExternalInfo("gluon","gluon","LO","NLO_EWK_S exact",0,"exact",1),
                "kinematics:LO","param:LO",&GluonFusion::NLO_ewk_soft_exact,
                      new FranzBinder,0.0));
}

void GluonFusionMatrixElementBox::add_qg_sectors()
{
//    const int num_topologies=15;
//    pointer_to_Franz_gluon_fusion rr[num_topologies]={rrqg2qght1,rrqg2qght2,rrqg2qght3,rrqg2qght4,rrqg2qght5,rrqg2qght6,rrqg2qght7,
//        rrqg2qght8,rrqg2qght9,rrqg2qght10,rrqg2qght11,rrqg2qght12,rrqg2qght13,rrqg2qght14,rrqg2qght15};
//    int num_sect[num_topologies]={1,2,1,1,8,2,2,4,2,1,1,1,1,1,1};//: number of sectors per topology
//    
    
    push_me("quark","gluon","NLO","H","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-1,1,new FranzBinder(rqg2qht1,1),"effective",0.0);
    
    push_me("quark","gluon","NNLO","RV","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-3,0,new FranzBinder(rvgq2qht1,3),"effective",0.0);
    
    
    vector<FranzBinder *> RR_functions;
    RR_functions.push_back(new FranzBinder(rrqg2qght1,1));
    RR_functions.push_back(new FranzBinder(rrqg2qght2,2));
    RR_functions.push_back(new FranzBinder(rrqg2qght3,1));
    RR_functions.push_back(new FranzBinder(rrqg2qght4,1));
    RR_functions.push_back(new FranzBinder(rrqg2qght5,8));
    RR_functions.push_back(new FranzBinder(rrqg2qght6,2));
    RR_functions.push_back(new FranzBinder(rrqg2qght7,2));
    RR_functions.push_back(new FranzBinder(rrqg2qght8,4));
    RR_functions.push_back(new FranzBinder(rrqg2qght9,2));
    RR_functions.push_back(new FranzBinder(rrqg2qght10,1));
    RR_functions.push_back(new FranzBinder(rrqg2qght11,1));
    RR_functions.push_back(new FranzBinder(rrqg2qght12,1));
    RR_functions.push_back(new FranzBinder(rrqg2qght13,1));
    RR_functions.push_back(new FranzBinder(rrqg2qght14,1));
    RR_functions.push_back(new FranzBinder(rrqg2qght15,1));
    
    for (unsigned i=0; i<RR_functions.size(); i++)
        {
        stringstream name_str;name_str<<"RR t"<<i+1;
        push_me("quark","gluon","NNLO",name_str.str(),
                "kinematics:NNLO","param:NLO",
                &GluonFusion::NNLO_hard_no_subtraction,-3,0,RR_functions[i],"effective",4.0);
        }
    // qg exact
    push_me("quark","gluon","NLO","NLO hard exact","kinematics:NLO","param:NLO",&GluonFusion::qg_NLO_hard_exact,-1,0,new FranzBinder,"exact",0.0);
    
    
}

void GluonFusionMatrixElementBox::add_gq_sectors()
{
//    
//    const int num_topologies=15;
//    pointer_to_Franz_gluon_fusion rr[num_topologies]={rrgq2qght1,rrgq2qght2,rrgq2qght3,rrgq2qght4,rrgq2qght5,rrgq2qght6,rrgq2qght7,rrgq2qght8,rrgq2qght9,rrgq2qght10,rrgq2qght11,rrgq2qght12,rrgq2qght13,rrgq2qght14,rrgq2qght15};
//    int num_sect[num_topologies]={1,2,1,1,8,2,2,4,2,1,1,1,1,1,1};//: number of sectors per topology
//    
    
    
    push_me("gluon","quark","NLO","H","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-1,1,new FranzBinder(rgq2qht1,1),"effective",0.0);
    
    push_me("gluon","quark","NNLO","RV","kinematics:NLO","param:NLO",&GluonFusion::nlo_me,-3,0,new FranzBinder(rvqg2qht1,3),"effective",0.0);
    
    vector<FranzBinder *> RR_functions;
    RR_functions.push_back(new FranzBinder(rrgq2qght1,1));
    RR_functions.push_back(new FranzBinder(rrgq2qght2,2));
    RR_functions.push_back(new FranzBinder(rrgq2qght3,1));
    RR_functions.push_back(new FranzBinder(rrgq2qght4,1));
    RR_functions.push_back(new FranzBinder(rrgq2qght5,8));
    RR_functions.push_back(new FranzBinder(rrgq2qght6,2));
    RR_functions.push_back(new FranzBinder(rrgq2qght7,2));
    RR_functions.push_back(new FranzBinder(rrgq2qght8,4));
    RR_functions.push_back(new FranzBinder(rrgq2qght9,2));
    RR_functions.push_back(new FranzBinder(rrgq2qght10,1));
    RR_functions.push_back(new FranzBinder(rrgq2qght11,1));
    RR_functions.push_back(new FranzBinder(rrgq2qght12,1));
    RR_functions.push_back(new FranzBinder(rrgq2qght13,1));
    RR_functions.push_back(new FranzBinder(rrgq2qght14,1));
    RR_functions.push_back(new FranzBinder(rrgq2qght15,1));
    
    for (unsigned i=0; i<RR_functions.size(); i++)
        {
        stringstream name_str;name_str<<"RR t"<<i+1;
        push_me("gluon","quark","NNLO",name_str.str(),
                "kinematics:NNLO","param:NLO",
                &GluonFusion::NNLO_hard_no_subtraction,-3,0,RR_functions[i],"effective",4.0);
        }
    
    // gq exact
    push_me("gluon","quark","NLO","NLO hard exact","kinematics:NLO","param:NLO",&GluonFusion::gq_NLO_hard_exact,-1,0,new FranzBinder,"exact",0.0);
    
    
    
}



void GluonFusionMatrixElementBox::push_me(const string & _pi,
                          const string & _pj,
                          const string& _pord,
                          const string & _name,
                          const string & _kin,
                          const string& _str_param,
                          ptr_to_GluonFusion_function _the_ggf_func,
                          int from_k,
                          int to_k,
                          FranzBinder* fb,
                          const string & me_approx,
                          double eps_exp)
{
    for (int k=from_k;k<to_k+1;k++)
        {
        
        available_matrix_elements.push_back(
            new MatrixElement(
                new MeExternalInfo(_pi,_pj,_pord,_name,k,me_approx),
                _kin,_str_param,_the_ggf_func,fb,eps_exp));
        }
    
}


//------------------------------------------------------------------------------

GluonFusionSectorBox::GluonFusionSectorBox(const WilsonCoefficients& WC, const BetaConstants& beta,const double& log_mur_sq_over_muf_sq)
{
    _WC = WC;
    _beta = beta;
    _log_mur_sq_over_muf_sq = log_mur_sq_over_muf_sq;
    //cout<<"\n[GluonFusionSectorBox] : setting up MatrixElements"<<endl;

    available_matrix_elements = new GluonFusionMatrixElementBox;
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
        //: the check
        if (pleft==available_matrix_elements->give_me(ime)->parton_i() and pright==available_matrix_elements->give_me(ime)->parton_j())
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

void GluonFusionSectorBox::build_sectors_with_fixed_a_order_e_order_and_pdfs(const FFF & F1,const FFF & F2,int Sorder,int Eorder,const vector<MatrixElement*> & matching_mes)
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
    cout<<"\n[GluonFusionSectorBox] : looking for necessary sectors"<<endl;
    vector<SimpleSector*> necessary_sectors;
    for (int i=0;i<available_sectors.size();i++)
        {
        bool initial_state_partons_fit=(UI.Fleft==available_sectors[i]->F1.parton_from
                                        and UI.Fright==available_sectors[i]->F2.parton_from
                                        )
        or
        (UI.Fleft=="none" and UI.Fright=="none");
        bool a_power_fits=available_sectors[i]->alpha_power==UI.alpha_s_power or UI.alpha_s_power == -1;
        bool e_power_fits = available_sectors[i]->epsilon_power==UI.pole;
        string real_life_ME_approx = UI.matrix_element_approximation;
        if (available_sectors[i]->alpha_power == 4)
            real_life_ME_approx = "effective";
        bool me_approx_fits = available_sectors[i]->ME->me_approximation()==real_life_ME_approx;
        bool a_weak_power_fits = available_sectors[i]->ME->alpha_ew_power()==0;
        if (UI.ew_soft) a_weak_power_fits = available_sectors[i]->ME->alpha_ew_power()==UI.alpha_ew_power or UI.alpha_ew_power == -1;
        if (initial_state_partons_fit and a_power_fits and e_power_fits and me_approx_fits and a_weak_power_fits)
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

GluonFusionExactCoefficients::GluonFusionExactCoefficients
            (const CModel & themodel)
{
    Model = themodel;
    LO_exact_coefficient.push_back(LO_exact_e0());
    LO_exact_coefficient.push_back(LO_exact_e1());
    LO_exact_coefficient.push_back(LO_exact_e2());
    
    NLO_soft_exact_coefficient.push_back(NLO_soft_exact_e0());
    
}

double GluonFusionExactCoefficients::LO_exact_e0()
{
    
    complex<double> ME ;
    
    for (int i=0;i<Model.quarks.size();i++)
        {
        
        ME = ME + Model.quarks[i]->Y() * born(Model.quarks[i]->X());
        cout<<"\n"<<Model.quarks[i]->name()
        <<"\t"<<Model.quarks[i]->Y() * born(Model.quarks[i]->X())
        <<"\t"<<Model.quarks[i]->X()<<"\t"<<Model.quarks[i]->Wq()
        <<"\t"<<Model.quarks[i]->m()<<"\t"<<Model.quarks[i]->cm_sq();
        }
    
    cout<<"\n LO exact : "<<pow(abs(ME),2.0);
    cout<<"\n Born exact:"<<
            pow(abs(born_exact_summed_over_quarks(&Model)),2.0);
    
    return(pow(abs(ME),2.0));
}

double GluonFusionExactCoefficients::LO_exact_e1()
{
    
    complex<double> ME ;
    
    for (int i=0;i<Model.quarks.size();i++)
        {
        ME = ME + Model.quarks[i]->Y() * born_e(Model.quarks[i]->X());
        }
    return(pow(abs(ME),2.0));
}

double GluonFusionExactCoefficients::LO_exact_e2()
{
    
    complex<double> ME ;
    
    for (int i=0;i<Model.quarks.size();i++)
        {
        ME = ME + Model.quarks[i]->Y() * born_e(Model.quarks[i]->X());
        }
    return(pow(abs(ME),2.0));
}

complex<double> GluonFusionExactCoefficients::born(complex<double> x)
{
    
    //: the expression below goes to 1 as mq->infty, i.e. as x->1
    complex<double > res=(-3.0)*x/pow(1.0-x,2.0)*(2.0-pow(1.0+x,2.0)/pow(1.0-x,2.0)*HPL2(0,0,x));
    //res = -16.0*pow(1.0+x,2.0)/x*HPL2(0,0,x)+32.0*pow(1.0-x,2.0)/x;
    
    return res;
}

complex<double> GluonFusionExactCoefficients::born_e(complex<double> x)
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

complex<double> GluonFusionExactCoefficients::born_e2(complex<double> x)
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


double GluonFusionExactCoefficients::NLO_soft_exact_e0()
{
    
    complex<double> V(0.0,0.0) ;
    complex<double> Born(0.0,0.0);
    for (int i=0;i<Model.quarks.size();i++)
        {
        // every quark has a scheme: on-shell or msbar
        // the ggf_exact_virtual_ep0 below is equal to
        // A + B * scheme_dependent_coeff
        // where scheme_dependent_coeff = 4/3 for on-shell scheme
        // and   scheme_dependent_coeff = log(mq^2/mur^2) for MS_bar
        double scheme_dependent_coeff;
        if (Model.quarks[i]->scheme()=="on-shell")
            scheme_dependent_coeff = 4.0/3.0;
        else if (Model.quarks[i]->scheme()=="msbar")
            {
            scheme_dependent_coeff = 2.0 * log(Model.quarks[i]->m()
                                         / Model.mu_r());
            }
        V = V + Model.quarks[i]->Y() * ggf_exact_virtual_ep0(Model.quarks[i]->X(),scheme_dependent_coeff);
        Born = Born+Model.quarks[i]->Y() * born(Model.quarks[i]->X());
        //cout<<"\t\t ME="<<ME;
        
        }
    //cout<<"\n ME = "<<ME;
    double res = 2.0 * real(Born * conj(V))
                + pow(abs(Born),2.0) * consts::pi_square;
    cout<<"\n NLO soft= "<<res<<"\t"<<conj(V);
    cout<<"\n NLO soft : 2.0*real(B*Vstar) = "<<2.0*real(Born * conj(V))
    <<" |B|^2*pi^2 = "<<pow(abs(Born),2.0) * consts::pi_square;
    return(res);
}

//------------------------------------------------------------------------------

GluonFusionEWCoefficients::GluonFusionEWCoefficients(const CModel& model)
{
    
    #include "electroweak_data.h"
    
    // reads EWK  corrections from Fig. 21 of
    //      http://arXiv.org/pdf/0809.3667
    //  by Actis, Passarino, Sturm, Uccirati
    //   Data in  file "./electroweak.h" provided by the authors.
    //   They have chosen the following paramegters:
    
    //       Mw = 80.398
    //       GammaW = 2.093
    //       Mz = 91.1876
    //       GammaZ = 2.4952
    //       Gfermi = 1.16637e-5 !/GeV^2
    //       a(0) = 1.0/137.0359911.0
    //       alphas_Mz = 0.118.0
    //       Mtop =  170.9
    
    
    cout<<"\nCalcualting ew correction factor";
    const double mtop_pass = 170.9;
    
	const double eff_mh =model.higgs.m()*mtop_pass/model.top.m();
    
    if (eff_mh<100.0 or eff_mh>500.0)
        {
        cout<<"\nSoft ew corrections not available (mh<100 or mh>500). They are set to zero";
        NLO_ew_coeff_ = 0.0;
        }
    else
        {
        int N = ew_data.size();
        int position;
    
        for (int i=0;i<ew_data.size()-1;i++)
            {
            const double mleft = ew_data[i]->mass;
            const double mright = ew_data[i+1]->mass;
            if (mleft < eff_mh and eff_mh<mright)
                {
                position = i;
                if (mright-eff_mh < eff_mh-mleft) {position = i+1;}
                break;
                }
            }
        int ibefore,ihere,iafter;
        if (position==0){ibefore = 0;ihere=1;iafter=2;}
        else if (position==N-1){ibefore = N-3;ihere=N-2;iafter=N-1;}
        else
            {
            
            ibefore = position-1;
            ihere = position;
            iafter = position+1;
            }
        cout<<"\n position = ["<<ibefore<<","<<ihere<<","<<iafter<<"] / "<<N<<endl;
        double x[3]={ew_data[ibefore]->mass,
                     ew_data[ihere]->mass,
                     ew_data[iafter]->mass};
        double y[3]={ew_data[ibefore]->deltaew,
                     ew_data[ihere]->deltaew,
                     ew_data[iafter]->deltaew};
        vector<double> c = givecoeff(x,y);
        double res = (c[2] + c[1] * eff_mh + c[0] * eff_mh*eff_mh)/100.0;
        NLO_ew_coeff_ = sqrt(1.0+res)-1.0 ;
        cout<<"\n NLO ew coeff = "<<NLO_ew_coeff_;
        }
}


vector<double>  GluonFusionEWCoefficients::givecoeff(double x[3],double y[3])
{
    vector<double> res;
    const double dx12 = x[0]-x[1];
    const double dx23 = x[1]-x[2];
    const double dx31 = x[2]-x[0];

    const double den=dx12*dx23*dx31;
    res.push_back((-y[0]*dx23-y[1]*dx31-y[2]*dx12)/den);

    res.push_back(( y[0]*(x[1]*x[1]-x[2]*x[2])
                +y[1]*(x[2]*x[2]-x[0]*x[0])
                +y[2]*(x[0]*x[0]-x[1]*x[1]) ) /den);
    
    res.push_back( (-y[0]*dx23*x[1]*x[2]
                -y[1]*dx31*x[0]*x[2]
                -y[2]*dx12*x[0]*x[1]) / den);
    return res;
}
//------------------------------------------------------------------------------

#include "ggf_cuts.h"

GluonFusion::GluonFusion(const UserInterface & UI) : Production(UI)
{
    ptr_to_GGF = this;
    SetNumberOfParticles();
    SetDecayParticleIdInEventBox();
    for (int i=0;i<7;i++){smax[i]=0.0;smin[i]=1000.0;}
    set_up_wilson_coefficients();
    set_up_beta_constants();
    cout<<"\n[GluonFusion] : setting up sectors"<<endl;
    all_sectors = new GluonFusionSectorBox(WC,beta,log_mur_sq_over_muf_sq);
    
    cout<<"\n hello before Parse"<<endl;
    #include "ggf_cut_initialization.h"
    cuts_->ParseCuts(UI);
    
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
            
            
            
            
            if (UI.matrix_element_approximation=="exact")
                {
                exact_coefficients = new GluonFusionExactCoefficients(Model);
                }
            if (UI.ew_soft)
                {
                electroweak_coefficients = new GluonFusionEWCoefficients(Model);
                }
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
 

void GluonFusion::allocate_luminosity()
{
     pdf_pair_list list_of_pdf_pairs=the_sector->give_list_of_pdf_pairs();
     
     for (unsigned i=0;i<list_of_pdf_pairs.size();i++)
          {
          //cout<<"\n pair #"<<i+1;
          pair<pdf_desc,pdf_desc> cur_pair=list_of_pdf_pairs.give_one_pair(i);
          lumi->add_pair(cur_pair.first,cur_pair.second);
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
     int parametrization_switch=3;
     
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
          if (s13==0.0 and s23==0.0)
              {
              //: NLO kinematics: we rename particle 4 to be particle 3 and call NLO_event
              NLO_event_kinematics(sigma_central,x1,x2,z,s14,s24);
              }
          else if (s14==0.0 and s24==0.0)
              {
              //: NLO kinematics: we  call NLO_event
              NLO_event_kinematics(sigma_central,x1,x2,z,s13,s23);
              }
          else
              {
              NNLO_event_kinematics(sigma_central,x1,x2,z,s13,s23,s14,s24,s34);
              }
          }
}



void GluonFusion::writeEventToFile(const double & x1,const double &pt,
                                   const double & y,const double & weight)
{

        //light_events.push_back(new LightEvent(x1,pt,y,weight));
    
    
}



void GluonFusion::update_smaxmin(int i,double x)
{
     if (x<smin[i]) {smin[i]=x;}
     if (x>smax[i]) {smax[i]=x;}
}

void GluonFusion::Jnlo(const double & sigma, const double & x1, const double & x2,const double &z, const double & lambda)
{
//    if (sigma!=0.0)
//        {
//    all_momenta["p1"].set(x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
//    all_momenta["p2"].set(x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
//    all_momenta["pf4"].set(0.0,0.0,0.0,0.0);
//    if (z==1)
//        {
//        //LO
//        all_momenta["h"].set((x1+x2)*Etot/2.0,0.0,0.0,(x1-x2)*Etot/2.0);
//        all_momenta["pf3"].set(0.0,0.0,0.0,0.0);
//        push_back_event(sigma);
//        }
//    else //NLO
//        {
//        const double pt3 = sqrt(x1*x2*lambda*(1.0-lambda))*Etot*(1.0-z);
//        const double E3 = Etot/2.0 * (1.0-z) * (x1*(1.0-lambda)+x2*lambda);
//        const double Pz3 = Etot/2.0 * (1.0-z) * (x1*(1.0-lambda)-x2*lambda);
//        const double Eh = Etot/2.0 * (x1+x2) - E3;
//        const double Pzh = Etot/2.0 * (x1-x2) -Pz3;
//        if (pt3<=ptbuf)// zero pT event
//            {
//            
//            all_momenta["h"].set(Eh,0.0,0.0,Pzh);
//            all_momenta["pf3"].set(E3,0.0,0.0,Pz3);
//            }
//        else
//            {
//            all_momenta["h"].set(Eh,-pt3,0.0,Pzh);
//            all_momenta["pf3"].set(E3,pt3,0.0,Pz3);
//            const double phi_rotation_angle = 2.0*consts::Pi*ISP.phi;
//            all_momenta["h"].rotate(phi_rotation_angle,unsigned(3));
//            all_momenta["pf3"].rotate(phi_rotation_angle,unsigned(3));
//            }
//        }
//    push_back_event(sigma);
//        }
    double  s12 = x1*x2*pow(Etot,2.0);
    double  s13 = s12*(1.0-z)*lambda;
    double  s14 = 0.0;
    double  s23 = s12*(1.0-z)*(1.0-lambda);
    double  s24 = 0.0;
    double  s34 = 0.0;
    book_production_event(sigma,x1,x2,z,s13,s23,s14,s24,s34);
}

void GluonFusion::JLO(const double & sigma)
{
    double x1=ISP.x1LO;
    double x2=ISP.x2LO;
    event_box.AddNewEvent(sigma);
    event_box.SetP(1,x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
    event_box.SetP(2,x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
    event_box.SetP(3,0.0,0.0,0.0,0.0);
    event_box.SetP(4,0.0,0.0,0.0,0.0);
    event_box.SetP(5,(x1+x2)*Etot/2.0,0.0,0.0,(x1-x2)*Etot/2.0);
}





void GluonFusion::LO_event_kinematics(const double& sigma,
                                      const double & x1,
                                      const double & x2)
{
    event_box.AddNewEvent(sigma);
    event_box.SetP(1,x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
    event_box.SetP(2,x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
    event_box.SetP(3,0.0,0.0,0.0,0.0);
    event_box.SetP(4,0.0,0.0,0.0,0.0);
    event_box.SetP(5,(x1+x2)*Etot/2.0,0.0,0.0,(x1-x2)*Etot/2.0);
}

void GluonFusion::NLO_event_kinematics(const double& sigma,
                                       const double & x1,
                                       const double & x2,
                                       const double & z,
                                       const double & s13,
                                       const double & s23)
{
     if (s13==0.0 and s23==0.0)
          {
          //: actually LO kinematics
          LO_event_kinematics(sigma,x1,x2);
          }
     else
          {
          

          
          //     ----------------- Higgs and gluon momenta ----------------------
          const double shat = pow(Etot,2.0)*x1*x2;
          const double pt3 = sqrt(s13*s23/shat); //gluon 1
          
          const double s1H = shat-s13;
          const double s2H = shat-s23;
          const double En3 = 0.5*(s13/x1/Etot+s23/x2/Etot);
          const double pz3 = 0.5*(-s13/x1/Etot+s23/x2/Etot);
          const double En =  0.5*(s1H/x1/Etot+s2H/x2/Etot);
          const double pZ =  0.5*(-s1H/x1/Etot+s2H/x2/Etot);

          
          const double sinphi = sin(2.0*consts::Pi*ISP.phi);
          const double cosphi = cos(2.0*consts::Pi*ISP.phi);
          event_box.AddNewEvent(sigma);
          event_box.SetP(1,x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
          event_box.SetP(2,x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
          event_box.SetP(3,En3,-pt3*sinphi,-pt3*cosphi,pz3);
          event_box.SetP(4,0.0,0.0,0.0,0.0);
          event_box.SetP(5,En,pt3*sinphi,pt3*cosphi,pZ);
          }

}

void GluonFusion::NNLO_event_kinematics( const double& sigma,
                                           const double & x1,
                                           const double & x2,
                                           const double & z,
                                           const double & s13,
                                           const double & s23,
                                           const double & s14,
                                           const double & s24,
                                           const double & s34)
{
    //: p1+p2 -> pH + p3 + p4 -> X1+X2+... + p3 + p4
    //: the kinematical variables defined are
    //: s_ij = 2*p_i*p_j
    //: s_iH = 2*p_i*p_H
    const double shat = pow(Etot,2.0)*x1*x2;
     
    const double s1H = shat-s13-s14;
    const double s2H = shat-s23-s24;
    const double s3H = s13+s23-s34;
    //const double s4H = s14+s24-s34;
    event_box.AddNewEvent(sigma);
    event_box.SetP(1,x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
    event_box.SetP(2,x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
    //     ----------------- Higgs and gluon momenta ----------------------
     
    double ptsq=s1H*s2H/shat-pow(Model.higgs.m(),2.0);
    if (ptsq<0.0 and abs(ptsq)<1e-5) {ptsq=0.0;}
    //: taking the absolute value cares
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
        event_box.SetP(3,En3,0.0,0.0,pz3);
        event_box.SetP(4,En4,0.0,0.0,pz4);
        event_box.SetP(5,En,0.0,0.0,pZ);

        }
     //: NLO real kinematics : H + hard parton
     else if (pt3>ptbuf and pt4<ptbuf)
        {
        // icase=" Higgs + p3";
        const double sinphi = sin(2.0*consts::Pi*ISP.phi);
        const double cosphi = cos(2.0*consts::Pi*ISP.phi);
        
        event_box.SetP(3,En3,-pt3*sinphi,-pt3*cosphi,pz3);
        event_box.SetP(4,En4,0.0,0.0,pz4);
        event_box.SetP(5,En,pT*sinphi,pT*cosphi,pZ);

        }
     else if (pt4>ptbuf and pt3<ptbuf)
        {
        //  icase=" Higgs + p4";
        const double sinphi = sin(2.0*consts::Pi*ISP.phi);
        const double cosphi = cos(2.0*consts::Pi*ISP.phi);
        
        event_box.SetP(3,En3,0.0,0.0,pz3);
        event_box.SetP(4,En4,-pt4*sinphi,-pt4*cosphi,pz4);
        event_box.SetP(5,En,pT*sinphi,pT*cosphi,pZ);

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
            if (rand()<0.5) phi3 = 2.0*consts::Pi - phi3;
            //: if lower, phi -> 2*Pi-phi
            const double sinphi = sin(2.0*consts::Pi*ISP.phi);
            const double cosphi = cos(2.0*consts::Pi*ISP.phi);
            
            
            event_box.SetP(3,
                            En3,
                            pt3*cos(phi3)*sinphi + pt3*sin(phi3)*cosphi,
                            pt3*cos(phi3)*cosphi + pt3*sin(phi3)*sinphi,
                            pz3);
            event_box.SetP(4,
                            -En-En3,
                            -pt3*cos(phi3)*sinphi - pt3*sin(phi3)*cosphi -pT*sinphi,
                            -pt3*cos(phi3)*cosphi - pt3*sin(phi3)*sinphi-pT*cosphi,
                            -pZ-pz3);
            event_box.SetP(5,En,pT*sinphi,pT*cosphi,pZ);
               }
        else if (pT<0.1*ptbuf)
        //: soft Higgs accidentally, two jets back to back
            {
            //  icase="  p3 + p4";
            //: p3 is the phi-reference along the x-axis
            const double sinphi = sin(2.0*consts::Pi*ISP.phi);
            const double cosphi = cos(2.0*consts::Pi*ISP.phi);
            event_box.SetP(3,En3,pt3*sinphi,pt3*cosphi,pz3);
            event_box.SetP(4,En4,pt4*sinphi,pt4*cosphi,pz4);
            event_box.SetP(5,En,0.0,0.0,pZ);
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
                         *lumi->LL(0)
                         *1.0/(1.0-ISP.z)
                         *the_sector->sector_specific_prefactors_from_a_e_expansion()
                         ;
          double weightLO = pref_sgg
                         *ISP.measLO
                         *lumi->LL_LO(0)
                         //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
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
                             -log_muf_sq_over_mh_sq, weight*extra_weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,-weightLO*extra_weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    }
               }
          else if (the_sector->ME->epsilon_power()==1)
               {
               for (int ts=1;ts<3;ts++) //: ts=topology sector (there are 2 in rgg2ght1)
                    {
                    
                    //: contribution from finite coefficient of rgg2ght1
                    rgg2ght1(ts,1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,
                             -log_muf_sq_over_mh_sq, weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,-weightLO,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    //: contribution from pole coefficient times logs of (1-z) and muf/mh
                    //: see documentation for explanation of extra_weight
                    double extra_weight = -2.0*ISP.Log_1mz;
                    rgg2ght1(ts,0,ISP.curs,ISP.x1,ISP.x2,  ISP.z,
                             -log_muf_sq_over_mh_sq, weight*extra_weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,0,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,-weightLO*extra_weight,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    
                    double extra_weight2 = 2.0*pow(ISP.Log_1mz,2.0);
                    rgg2ght1(ts,-1,ISP.curs,ISP.x1,ISP.x2,  ISP.z,
                             -log_muf_sq_over_mh_sq, weight*extra_weight2,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
                    rgg2ght1(ts,-1,ISP.cursLO,ISP.x1LO,ISP.x2LO,1.0,
                             -log_muf_sq_over_mh_sq,-weightLO*extra_weight2,consts::nf,ISP.lambda,0.0,0.0,0.0,dummyres);
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
          *pow(WC.c0,2.0)
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
                         *lumi->LL(0)
                         *the_sector->sector_specific_prefactors_from_a_e_expansion()
                         //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                         *1.0/(1.0-ISP.z)
                         *pow(WC.c0,2.0)
                         ;
          double weightLO = pref_sgg
                         *ISP.measLO
                         *lumi->LL(0)
                         *the_sector->sector_specific_prefactors_from_a_e_expansion()
                         //*pow(alpha_s_vector[0]/consts::Pi,the_sector->alpha_power)
                         *1.0/(1.0-ISP.z)
                         *pow(WC.c0,2.0)
                         ;
          
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
                    for (int i=0;i<the_sector->ME->number_of_sectors_in_this_topology();i++)
                         {
                         dummyres=0.0;
                         pointer_to_Franz_gluon_fusion the_func = the_sector->ME->franz_func();
                         
                         
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
        double sigma = 32.0 / 27.0 * pow(1.0-ISP.z,3.0) / ISP.z
                        *sum_of_abs_sq_of_Aqqgh(ISP.z,&Model);
        Jnlo(sigma * weight, ISP.z,  ISP.x1,     ISP.x2,   ISP.lambda);
        }
    
}

//: ewk matrix elements below
void GluonFusion::NLO_ewk_soft()
{
    if (the_sector->ME->epsilon_power()==0)
        {
        double sigma_central =   pref_sgg
        *ISP.measLO
        *lumi->LL_LO(0)
        *the_sector->sector_specific_prefactors_from_a_e_expansion()
        *electroweak_coefficients->LO();
        JLO(sigma_central);
        }
}


void GluonFusion::NLO_ewk_soft_exact()
{
    if (the_sector->ME->epsilon_power()==0)
        {
        double sigma_central =   pref_sgg
        *ISP.measLO
        *lumi->LL_LO(0)
        *the_sector->sector_specific_prefactors_from_a_e_expansion()
        *exact_coefficients -> LO_epsilon(0)
        *electroweak_coefficients->LO();
        JLO(sigma_central);
        }
}







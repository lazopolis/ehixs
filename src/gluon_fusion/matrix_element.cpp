
#include "matrix_element.h"

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
                             const string& _the_ggf_func,
                             FranzBinder* fr,
                             double _e_exp_in_subtr)
{
    _info=info;
    if (_kin=="kinematics:LO") dimension=2;
    else if (_kin=="kinematics:NLO") dimension=4;
    else if (_kin=="kinematics:NNLO") dimension=6;
    else {cout<<"\nUnrecognized kinematics when constructing MatrixElement"<<endl;exit(1);}
    parametrization= _str_param;
//    if (_str_param == "param:LO")  parametrization=&GluonFusion::LO_parametrization_only;
//    else if (_str_param=="param:NLO") parametrization=&GluonFusion::NLO_parametrization;
//    else {cout<<"\nerror, param not equal to LO or NLO"<<endl;exit(1);}
    
    
    the_ggf_func = _the_ggf_func;
    _franz = fr;
    epsilon_exponent_in_z_subtraction = _e_exp_in_subtr;
}



ostream& operator<<(ostream& stream, const MatrixElement& ME)
{
    stream<<"S("<<ME.parton_i()<<","<<ME.parton_j()<<","<< ME.name()
    <<",a_s^"<<ME.alpha_power()
    <<",a_w^"<<ME.alpha_ew_power()
    <<",e^"<<ME.epsilon_power()
    <<" ,dim="<<ME.dimension<<")";
    
    return stream;
}






//------------------------------------------------------------------------------



//------------------------------------------------------------------------------


GluonFusionMatrixElementBox::GluonFusionMatrixElementBox(const string& rr_treatment)
{
    rr_treatment_ = rr_treatment;
    //: linking the matrix elements
    add_gg_sectors();
    add_qg_sectors();
    add_gq_sectors();
    add_qqbar_sectors();
    add_q1q2_sectors();
    add_qq_sectors();
}

#include "fortran_interface_for_ggf_amplitudes.h"

void GluonFusionMatrixElementBox::add_qqbar_sectors()
{
    
    //    
    //    
    //    const int num_topologies=14;
    //    pointer_to_Franz_gluon_fusion rr[num_topologies]={rrqqbar2qqbarht1,rrqqbar2qqbarht2,rrqqbar2qqbarht3,rrqqbar2qqbarht4,rrqqbar2qqbarht5,rrqqbar2qqbarht6,rrqqbar2gght1,rrqqbar2gght2,rrqqbar2gght3,rrqqbar2gght4,rrqqbar2gght5,rrqqbar2gght6,rrq1q1bar2q2q2barht1,rrq1q1bar2q2q2barht2};
    //    int num_sect[num_topologies]={1,2,1,4,1,1,1,2,1,1,4,1,1,1};//: number of sectors per topology
    
    push_me("quark","antiquark","NLO","R","kinematics:NLO","param:NLO","nlo_me",0,1,new FranzBinder(rqqbar2ght1,1),"effective",0.0);
    push_me("quark","antiquark","NNLO","RV","kinematics:NLO","param:NLO","nlo_me",-2,0,new FranzBinder(rvqqbar2ght1,1),"effective",0.0);
    
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
                "param:NLO","NNLO_hard_no_subtraction",-2,0,RR_functions[i],"effective",0.0);
    }
    
    push_me("quark","antiquark","NLO","NLO hard exact","kinematics:NLO","param:NLO","qqbar_NLO_hard_exact",0,0,new FranzBinder,"exact",0.0);
    //: electoweak q qbar-> h g
    available_matrix_elements.push_back(
        new MatrixElement(
                          new MeExternalInfo("up","upbar","LO","NLO_EWK h+g exact",0,"exact",1),
                          "kinematics:NLO","param:NLO","NLO_ewk_uubar_h_plus_jet",
                          new FranzBinder,0.0));
    
    available_matrix_elements.push_back(
        new MatrixElement(
                          new MeExternalInfo("down","downbar","LO","NLO_EWK h+g exact",0,"exact",1),
                          "kinematics:NLO","param:NLO","NLO_ewk_ddbar_h_plus_jet",
                          new FranzBinder,0.0));
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
                "param:NLO","NNLO_hard_no_subtraction",-2,0,RR_functions[i],"effective",0.0);
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
                "param:NLO","NNLO_hard_no_subtraction",-2,0,RR_functions[i],"effective",0.0);
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
            "LO",0,2,zero_binder,"effective",0.0);
    push_me("gluon","gluon","NLO","S","kinematics:LO","param:NLO",
            "gg_NLO_SOFT",0,1,zero_binder,"effective",0.0);//: the soft is finite
    push_me("gluon","gluon","NLO","H","kinematics:NLO","param:NLO",
            "gg_NLO_HARD",-1,1,zero_binder,"effective",0.0);
    push_me("gluon","gluon","NNLO","DOUBLE SOFT","kinematics:LO",
            "param:NLO","gg_NNLO_SOFT",-2,0,zero_binder,"effective",0.0);
    
    
    
    const int rv_no_sub_numtop=1;
    pointer_to_Franz_gluon_fusion rv_no_sub[rv_no_sub_numtop]={rvgg2ght1};
    int rv_no_subnum_sect[rv_no_sub_numtop]={6};
    push_me("gluon","gluon","NNLO","RVt1","kinematics:NLO","param:NLO",
            "nlo_me",-3,0,new FranzBinder(rvgg2ght1,6),"effective",0.0);
    
    
    
    push_me("gluon","gluon","NNLO","RVt2","kinematics:NLO","param:NLO",
            "NNLO_rv_with_subtraction",-3,0,
            new FranzBinder(rvgg2ght2,4),"effective",2.0);
    
    push_me("gluon","gluon","NNLO","RVt4","kinematics:NLO","param:NLO",
            "NNLO_rv_with_subtraction",-3,0,new FranzBinder(rvgg2ght4,2),"effective",4.0);
    
    vector<FranzBinder *> RR_functions;
    vector<string> RR_names;
    if (rr_treatment_ == "split")
    {
        RR_functions.push_back(new FranzBinder(rrgg2gght1,1,1));
        RR_names.push_back("RR gg->hgg t1.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght1,2,2));
        RR_names.push_back("RR gg->hgg t1.2");
        RR_functions.push_back(new FranzBinder(rrgg2gght2,1,1));
        RR_names.push_back("RR gg->hgg t2.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght2,2,2));
        RR_names.push_back("RR gg->hgg t2.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght3,1));
        RR_names.push_back("RR gg->hgg t3.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght4,1));
        RR_names.push_back("RR gg->hgg t4.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght5,1,1));
        RR_names.push_back("RR gg->hgg t5.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght5,2,2));
        RR_names.push_back("RR gg->hgg t5.2");
        RR_functions.push_back(new FranzBinder(rrgg2gght5,3,3));
        RR_names.push_back("RR gg->hgg t5.3");
        RR_functions.push_back(new FranzBinder(rrgg2gght5,4,4));
        RR_names.push_back("RR gg->hgg t5.4");
        RR_functions.push_back(new FranzBinder(rrgg2gght5,5,5));
        RR_names.push_back("RR gg->hgg t5.5");
        RR_functions.push_back(new FranzBinder(rrgg2gght5,6,6));
        RR_names.push_back("RR gg->hgg t5.6");
        RR_functions.push_back(new FranzBinder(rrgg2gght5,7,7));
        RR_names.push_back("RR gg->hgg t5.7");
        RR_functions.push_back(new FranzBinder(rrgg2gght5,8,8));
        RR_names.push_back("RR gg->hgg t5.8");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght6,1,1));
        RR_names.push_back("RR gg->hgg t6.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght6,2,2));
        RR_names.push_back("RR gg->hgg t6.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght7,1,1));
        RR_names.push_back("RR gg->hgg t7.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght7,2,2));
        RR_names.push_back("RR gg->hgg t7.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght8,1,1));
        RR_names.push_back("RR gg->hgg t8.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght8,2,2));
        RR_names.push_back("RR gg->hgg t8.2");
        RR_functions.push_back(new FranzBinder(rrgg2gght8,3,3));
        RR_names.push_back("RR gg->hgg t8.3");
        RR_functions.push_back(new FranzBinder(rrgg2gght8,4,4));
        RR_names.push_back("RR gg->hgg t8.4");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght9,1,1));
        RR_names.push_back("RR gg->hgg t9.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght9,2,2));
        RR_names.push_back("RR gg->hgg t9.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght10,1));
        RR_names.push_back("RR gg->hgg t10.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght11,1));
        RR_names.push_back("RR gg->hgg t11.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght12,1,1));
        RR_names.push_back("RR gg->hgg t12.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght12,2,2));
        RR_names.push_back("RR gg->hgg t12.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght13,1));
        RR_names.push_back("RR gg->hgg t13.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght14,1));
        RR_names.push_back("RR gg->hgg t14.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght15,1));
        RR_names.push_back("RR gg->hgg t15.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght16,1));
        RR_names.push_back("RR gg->hgg t16.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht1,1,1));
        RR_names.push_back("RR gg->hqqb t1.1");
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht1,2,2));
        RR_names.push_back("RR gg->hqqb t1.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht2,1,1));
        RR_names.push_back("RR gg->hqqb t2.1");
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht2,2,2));
        RR_names.push_back("RR gg->hqqb t2.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht3,1));
        RR_names.push_back("RR gg->hqqb t3.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht4,1));
        RR_names.push_back("RR gg->hqqb t4.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht5,1,1));
        RR_names.push_back("RR gg->hqqb t5.1");
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht5,2,2));
        RR_names.push_back("RR gg->hqqb t5.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht6,1,1));
        RR_names.push_back("RR gg->hqqb t6.1");
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht6,2,2));
        RR_names.push_back("RR gg->hqqb t6.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht7,1,1));
        RR_names.push_back("RR gg->hqqb t7.1");
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht7,2,2));
        RR_names.push_back("RR gg->hqqb t7.2");
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht7,3,3));
        RR_names.push_back("RR gg->hqqb t7.3");
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht7,4,4));
        RR_names.push_back("RR gg->hqqb t7.4");
        
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht8,1,1));
        RR_names.push_back("RR gg->hqqb t8.1");
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht8,2,2));
        RR_names.push_back("RR gg->hqqb t8.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht9,1));
        RR_names.push_back("RR gg->hqqb t9.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht10,1));
        RR_names.push_back("RR gg->hqqb t10.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht11,1));
        RR_names.push_back("RR gg->hqqb t11.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht12,1,1));
        RR_names.push_back("RR gg->hqqb t12.1");
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht12,2,2));
        RR_names.push_back("RR gg->hqqb t12.2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht13,1));
        RR_names.push_back("RR gg->hqqb t13.1");
    }
    else
    {
        RR_functions.push_back(new FranzBinder(rrgg2gght1,1,2));
        RR_names.push_back("RR gg->hgg t1.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght2,1,2));
        RR_names.push_back("RR gg->hgg t2.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght3,1));
        RR_names.push_back("RR gg->hgg t3.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght4,1));
        RR_names.push_back("RR gg->hgg t4.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght5,1,8));
        RR_names.push_back("RR gg->hgg t5.1-8");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght6,1,2));
        RR_names.push_back("RR gg->hgg t6.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght7,1,2));
        RR_names.push_back("RR gg->hgg t7.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght8,1,4));
        RR_names.push_back("RR gg->hgg t8.1-4");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght9,1,2));
        RR_names.push_back("RR gg->hgg t9.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght10,1));
        RR_names.push_back("RR gg->hgg t10.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght11,1));
        RR_names.push_back("RR gg->hgg t11.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght12,1,2));
        RR_names.push_back("RR gg->hgg t12.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2gght13,1));
        RR_names.push_back("RR gg->hgg t13.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght14,1));
        RR_names.push_back("RR gg->hgg t14.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght15,1));
        RR_names.push_back("RR gg->hgg t15.1");
        RR_functions.push_back(new FranzBinder(rrgg2gght16,1));
        RR_names.push_back("RR gg->hgg t16.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht1,1,2));
        RR_names.push_back("RR gg->hqqb t1.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht2,1,2));
        RR_names.push_back("RR gg->hqqb t2.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht3,1));
        RR_names.push_back("RR gg->hqqb t3.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht4,1));
        RR_names.push_back("RR gg->hqqb t4.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht5,1,2));
        RR_names.push_back("RR gg->hqqb t5.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht6,1,2));
        RR_names.push_back("RR gg->hqqb t6.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht7,1,4));
        RR_names.push_back("RR gg->hqqb t7.1-4");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht8,1,2));
        RR_names.push_back("RR gg->hqqb t8.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht9,1));
        RR_names.push_back("RR gg->hqqb t9.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht10,1));
        RR_names.push_back("RR gg->hqqb t10.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht11,1));
        RR_names.push_back("RR gg->hqqb t11.1");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht12,1,2));
        RR_names.push_back("RR gg->hqqb t12.1-2");
        
        RR_functions.push_back(new FranzBinder(rrgg2qqbarht13,1));
        RR_names.push_back("RR gg->hqqb t13.1");
    }
    for (unsigned i=0; i<RR_functions.size(); i++)
    {
        //stringstream name_str;name_str<<"RR t"<<i+1;
        push_me("gluon","gluon","NNLO",RR_names[i],
                "kinematics:NNLO","param:NLO",
                "NNLO_hard_with_subtraction",-3,0,RR_functions[i],"effective",4.0);
    }
    
    // --- brute force sector
    /*
     for (int k=-3;k<1;k++)
     {
     
     available_matrix_elements.push_back(
     new MatrixElementRR(
     new MeExternalInfo("gluon","gluon","NNLO","RR brute force",k,"effective"),
     "kinematics:NNLO","param:NLO",
     "NNLO_RR_brute_force_wrap",RR_functions,4.0
     )
     );
     }
     */
    //: exact matrix elements
    push_me("gluon","gluon","LO","LO exact","kinematics:LO","param:LO","LO_exact",0,2,new FranzBinder,"exact",0.0);
    push_me("gluon","gluon","NLO","NLO soft exact","kinematics:LO","param:NLO","NLO_soft_exact",0,0,new FranzBinder,"exact",0.0);//: only the e^0 piece
    push_me("gluon","gluon","NLO","NLO hard exact","kinematics:NLO","param:NLO","gg_NLO_hard_exact",-2,0,new FranzBinder,"exact",0.0);
    
    //: nlo ew
    //    available_matrix_elements.push_back(
    //    new MatrixElement(
    //    new MeExternalInfo("gluon","gluon","LO","NLO_EWK_S effective",0,"effective",1),
    //                "kinematics:LO","param:LO","NLO_ewk_soft",
    //                      new FranzBinder,0.0));
    //    available_matrix_elements.push_back(
    //    new MatrixElement(
    //    new MeExternalInfo("gluon","gluon","LO","NLO_EWK_S exact",0,"exact",1),
    //                "kinematics:LO","param:LO","NLO_ewk_soft_exact",
    //                      new FranzBinder,0.0));
    
    
    
    
}

void GluonFusionMatrixElementBox::add_qg_sectors()
{
    //    const int num_topologies=15;
    //    pointer_to_Franz_gluon_fusion rr[num_topologies]={rrqg2qght1,rrqg2qght2,rrqg2qght3,rrqg2qght4,rrqg2qght5,rrqg2qght6,rrqg2qght7,
    //        rrqg2qght8,rrqg2qght9,rrqg2qght10,rrqg2qght11,rrqg2qght12,rrqg2qght13,rrqg2qght14,rrqg2qght15};
    //    int num_sect[num_topologies]={1,2,1,1,8,2,2,4,2,1,1,1,1,1,1};//: number of sectors per topology
    //    
    
    push_me("quark","gluon","NLO","H","kinematics:NLO","param:NLO","nlo_me",-1,1,new FranzBinder(rqg2qht1,1),"effective",0.0);
    
    push_me("quark","gluon","NNLO","RV","kinematics:NLO","param:NLO","nlo_me",-3,0,new FranzBinder(rvgq2qht1,3),"effective",0.0);
    
    
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
                "NNLO_hard_no_subtraction",-3,0,RR_functions[i],"effective",4.0);
    }
    // qg exact
    push_me("quark","gluon","NLO","NLO hard exact","kinematics:NLO","param:NLO","qg_NLO_hard_exact",-1,0,new FranzBinder,"exact",0.0);
    
    //ewk corrections to h+j
    // u g -> u+h
    available_matrix_elements.push_back(
            new MatrixElement(
                              new MeExternalInfo("up","gluon","LO","NLO_EWK h+g exact",0,"exact",1),
                              "kinematics:NLO","param:NLO","NLO_ewk_ug_h_plus_jet",
                              new FranzBinder,0.0));
    // d g -> d+h
    available_matrix_elements.push_back(
            new MatrixElement(
                              new MeExternalInfo("down","gluon","LO","NLO_EWK h+g exact",0,"exact",1),
                              "kinematics:NLO","param:NLO","NLO_ewk_dg_h_plus_jet",
                              new FranzBinder,0.0));
    
}

void GluonFusionMatrixElementBox::add_gq_sectors()
{
    //    
    //    const int num_topologies=15;
    //    pointer_to_Franz_gluon_fusion rr[num_topologies]={rrgq2qght1,rrgq2qght2,rrgq2qght3,rrgq2qght4,rrgq2qght5,rrgq2qght6,rrgq2qght7,rrgq2qght8,rrgq2qght9,rrgq2qght10,rrgq2qght11,rrgq2qght12,rrgq2qght13,rrgq2qght14,rrgq2qght15};
    //    int num_sect[num_topologies]={1,2,1,1,8,2,2,4,2,1,1,1,1,1,1};//: number of sectors per topology
    //    
    
    
    push_me("gluon","quark","NLO","H","kinematics:NLO","param:NLO","nlo_me",-1,1,new FranzBinder(rgq2qht1,1),"effective",0.0);
    
    push_me("gluon","quark","NNLO","RV","kinematics:NLO","param:NLO","nlo_me",-3,0,new FranzBinder(rvqg2qht1,3),"effective",0.0);
    
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
                "NNLO_hard_no_subtraction",-3,0,RR_functions[i],"effective",4.0);
    }
    
    // gq exact
    push_me("gluon","quark","NLO","NLO hard exact","kinematics:NLO","param:NLO","gq_NLO_hard_exact",-1,0,new FranzBinder,"exact",0.0);
    
    //ewk corrections to h+j
    //  g  u -> u+h
    available_matrix_elements.push_back(
        new MatrixElement(
                          new MeExternalInfo("gluon","up","LO","NLO_EWK h+g exact",0,"exact",1),
                          "kinematics:NLO","param:NLO","NLO_ewk_gu_h_plus_jet",
                          new FranzBinder,0.0));
    // g d -> d+h
    available_matrix_elements.push_back(
        new MatrixElement(
                          new MeExternalInfo("gluon","down","LO","NLO_EWK h+g exact",0,"exact",1),
                          "kinematics:NLO","param:NLO","NLO_ewk_gd_h_plus_jet",
                          new FranzBinder,0.0));
    
}



void GluonFusionMatrixElementBox::push_me(const string & _pi,
                                          const string & _pj,
                                          const string& _pord,
                                          const string & _name,
                                          const string & _kin,
                                          const string& _str_param,
                                          const string& _the_ggf_func,
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



#include "GluonFusionInclusive.h"





InclusiveMatrixElement::InclusiveMatrixElement(const string & _pi,const string & _pj,const string& _pord,
                                               const string & _name, const string & _kin,
                                               const string& _str_param,ptr_to_GluonFusionInclusive_function _the_ggf_func)
{
     parton_i=_pi;
     parton_j=_pj;
     name = _name;
     if (_kin=="dim:0") dimension=0;
     else if (_kin=="dim:1") dimension=1;
     else if (_kin=="dim:2") dimension=2;
     else if (_kin=="dim:3") dimension=3;
     else {cout<<"\nUnrecognized kinematics when constructing InclusiveMatrixElement"<<endl;exit(1);}
//     if (_str_param == "param:LO")  parametrization=&GluonFusionInclusive::LO_parametrization_only;
//     else if (_str_param=="param:NLO") parametrization=&GluonFusionInclusive::NLO_parametrization;
//     else {cout<<"\nerror, param not equal to LO or NLO"<<endl;exit(1);}
     
     if (_pord=="LO") alpha_power=0;
     else if (_pord=="NLO") alpha_power=1;
     else if (_pord=="NNLO") alpha_power=2;
     else if (_pord=="N3LO") alpha_power=3;
     else {cout<<"\n unrecognized pord when constructing MatrixElement"<<endl;exit(1);}
     the_ggf_func = _the_ggf_func;
     
}
ostream& operator<<(ostream& stream, const InclusiveMatrixElement& ME)
{
     stream<<"S("<<ME.parton_i<<","<<ME.parton_j<<","<< ME.name<<",a^"<<ME.alpha_power<<" ,dim="<<ME.dimension<<")";
     
     return stream;
}



GluonFusionInclusive::GluonFusionInclusive()
{
     set_up_matrix_elements();
     set_up_sectors();
}

void GluonFusionInclusive::set_up_matrix_elements()
{
     available_me.push_back(new InclusiveMatrixElement
                            ("gluon","gluon","LO","LO delta effective",
                             "dim:0","param:LO",
                             &GluonFusionInclusive::me_gg_delta_LO_effective));
     
     available_me.push_back(new InclusiveMatrixElement
                            ("gluon","gluon","NLO","NLO delta effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_gg_delta_NLO_effective));
     available_me.push_back(new InclusiveMatrixElement
                            ("gluon","gluon","NLO","NLO plus effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_gg_plus_NLO_effective));
     available_me.push_back(new InclusiveMatrixElement
                            ("gluon","gluon","NLO","NLO reg effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_gg_reg_NLO_effective));
     
     available_me.push_back(new InclusiveMatrixElement
                            ("gluon","gluon","NNLO","NNLO delta effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_gg_delta_NNLO_effective));
     available_me.push_back(new InclusiveMatrixElement
                            ("gluon","gluon","NNLO","NNLO plus effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_gg_plus_NNLO_effective));
     available_me.push_back(new InclusiveMatrixElement
                            ("gluon","gluon","NNLO","NNLO reg effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_gg_reg_NNLO_effective));
     
     available_me.push_back(new InclusiveMatrixElement
                            ("gluon","quark","NLO","NLO reg effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_gq_reg_NLO_effective));
     
     
     available_me.push_back(new InclusiveMatrixElement
                            ("gluon","quark","NNLO","NNLO reg effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_gq_reg_NNLO_effective));
     
     available_me.push_back(new InclusiveMatrixElement
                            ("quark","antiquark","NLO","NLO reg effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_qqbar_reg_NLO_effective));
     
     available_me.push_back(new InclusiveMatrixElement
                            ("quark","antiquark","NNLO","NNLO reg effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_qqbar_reg_NNLO_effective));
     
     available_me.push_back(new InclusiveMatrixElement
                            ("quark","quark","NNLO","NNLO reg effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_qq_reg_NNLO_effective));
     
     available_me.push_back(new InclusiveMatrixElement
                            ("quark","quark2","NNLO","NNLO reg effective",
                             "dim:1","param:NLO",
                             &GluonFusionInclusive::me_qqbar_reg_NNLO_effective));

}

void GluonFusionInclusive::set_up_sectors()
{
     
     //: constructing the wilson coefficient factor [a*(c0+a*c1+a^2*c2)]^2
     //vector<ExpansionTerm*> WCET_vector;
     WCET_vector.push_back(new ExpansionTerm("(c0^2 a^2)",pow(WC.c0,2.0),2,0));
     WCET_vector.push_back(new ExpansionTerm("(2*c0*c1* a^3)",2.0*WC.c0*WC.c1,3,0));
     WCET_vector.push_back(new ExpansionTerm("[(c1^2 + 2*c0*c2)*a^4]",pow(WC.c0,2.0),4,0));
     
     
     A_square_LOG_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
     A_square_LOG_vector.push_back(new ExpansionTerm("(2*b0*L*a)",2.0*beta.zero*LL,1,0));
     A_square_LOG_vector.push_back(new ExpansionTerm("(a^2* [3*b0^2*LL^2 + 2*b1*LL)",3.0*pow(beta.zero,2.0)*pow(LL,2.0)+3.0*beta.one*LL,2,0));
     
     A_cube_LOG_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
     A_cube_LOG_vector.push_back(new ExpansionTerm("(3*b0*L*a)",2.0*beta.zero*LL,1,0));
     A_cube_LOG_vector.push_back(new ExpansionTerm("(a^2* [6*b0^2*LL^2 + 3*b1*LL)",3.0*pow(beta.zero,2.0)*pow(LL,2.0)+3.0*beta.one*LL,2,0));
     
     A_fourth_LOG_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
     A_fourth_LOG_vector.push_back(new ExpansionTerm("(4*b0*L*a)",2.0*beta.zero*LL,1,0));
     

   //  AREN_vector.push_back(new ExpansionTerm("(-b0*a*lh)",-beta.zero*lh,1,0));
     
     for (int i=0;i<available_me.size();i++)
          {
          convolute_me(available_me[i]);
          }
     

}


void GluonFusionInclusive::convolute_me(InclusiveMatrixElement* cur_ME)
{
     for (int i=0;i<WCET_vector.size();i++)
          {
          vector<ExpansionTerm*> cur_alpha_log;
          if (cur_ME->alpha_power==0) cur_alpha_log=A_square_LOG_vector;
          else if (cur_ME->alpha_power==1) cur_alpha_log = A_cube_LOG_vector;
          else if (cur_ME->alpha_power==2) cur_alpha_log = A_fourth_LOG_vector;
          else {cout<<"\n too high order from matrix element:"<<cur_ME->alpha_power<<endl;exit(1);}
          for (int j=0;j<cur_alpha_log.size();j++)
               {
               vector<ExpansionTerm*> factors;
               factors.push_back(WCET_vector[i]);
               factors.push_back(cur_alpha_log[j]);
               
               available_sectors.push_back(new InclusiveSector(factors,cur_ME));
               }
          }
}

/*

GluonFusionInclusive::GluonFusionInclusive() : Production()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_g_00,Luminosity::F_g_00);
     sectors.push_back(new InclusiveSector(curlumi,0,0,"delta","effective",
                                           1,
                                           &GluonFusionInclusive::gg_delta_LO_effective,"gg delta effective LO"));
//     sectors.push_back(new Sector(curlumi,&GluonFusionInclusive::gg_plus,2,"gg plus"));
//     sectors.push_back(new Sector(curlumi,&GluonFusionInclusive::gg_reg,2,"gg reg"));
     
     
}


void GluonFusionInclusive::init(const UserInterface& UI,TheHatch* the_hatch)
{
     
     if (UI.info)
          {
          cout<<"\n Info on Gluon Fusion Inclusive\n\nsectors available:\n";
          for (int i=0;i<sectors.size();i++){cout<<"\n"<<i<<":\t"<<sectors[i]->name();}
          cout<<"\n\n";
          exit(0);
          }
     
     lumi = new Luminosity(UI.number_of_flavours,UI.muf_over_mhiggs * UI.m_higgs,UI.mur_over_mhiggs * UI.m_higgs,UI.perturbative_order,UI.pdf_provider,UI.pdf_error);
     sector_control=UI.sector_control;
     if (sector_control>=sectors.size())
          {
          cout<<"\nYou are asking for a sector that does not exist: "<<sector_control
          <<"\t (max sector number = "<<sectors.size()-1<<")"<<endl;
          exit(1);
          }
     else 
          {
          dim_of_integration = sectors[sector_control]->dim();
          my_sector_name = sectors[sector_control]->name();
          pointer_to_function_for_sector =sectors[sector_control]->ptr();
          for (unsigned i=0;i<sectors[sector_control]->pdfsize();i++)
               {
               pair<Luminosity::pdf_desc,Luminosity::pdf_desc> cur_pair=sectors[sector_control]->give_pdf(i);
               lumi->add_pair(cur_pair.first,cur_pair.second);
               }
          }
     //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
     cur_lumi = vector<double>(lumi->pdf_size(),0.0);
     cur_lumiLO = vector<double>(lumi->pdf_size(),0.0);
     //: setting topologies (Franz's functions and number of sectors per function) for the double real
     //set_topologies();
     
     
     
     //: call init_base that sets m_higgs and runs the coupling constants
     init_base(UI,the_hatch);
     
     
//     all_momenta.init_fvector("p1");
//     all_momenta.init_fvector("p2");
//     all_momenta.init_fvector("h");
//     all_momenta.init_fvector("pf3");
//     all_momenta.init_fvector("pf4");
     
     
     //: calculate kinematic variables or Model constants that are input dependent
     //: necessarily after init_base where UI input passes to the Production class.
     calculate_derived_variables();
     Model.set_Xq_for_quarks();
     //: this depends on m_higgs which we take to be the nominal higgs mass
     //: CHANGE the above in case Higgs is off-shell.
     calculate_wilson_coefficients();
}


void GluonFusionInclusive::evaluate_sector()
{
     production_events.clear();
     prepare_phase_space_dependent_quantities();
     (this->*pointer_to_function_for_sector)();
}


void GluonFusionInclusive::prepare_phase_space_dependent_quantities()
{
     //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
	//: Gf = 1.16637*10^{-5} * 0.389379*10^9
     pref_sgg = 35.0309; 

     if (dim_of_integration==1)
          {
          x1 = xx_vegas[0];
          z=xx_vegas[1];
          lambda = xx_vegas[2]; 
          meas=1/x1;
          measLO=1/x1;
          x1LO=x1;
          x2LO=tau/x1LO;
          x2=tau/x1/z;
          }
     if (dim_of_integration>=2)
          {
          x1 = xx_vegas[0];
          z=xx_vegas[1];
          lambda = 0; 
          meas=1/x1;
          measLO=1/x1;
          x1LO=x1;
          x2LO=tau/x1LO;
          x2=tau/x1/z;
          }
     if (dim_of_integration==3)
          {
          lambda = xx_vegas[2];
          }
     
     
     lumi->set_cur_lumi(x1LO,x2LO,cur_lumiLO);//:sets the cur_lumi_LO according to the luminosity initialized in the constructor for this sector
     lumi->set_cur_lumi(x1,x2,cur_lumi);       //:sets the cur_lumi according to the luminosity initialized in the constructor for this sector
     
     lh=log_muf_sq_over_mh_sq;		
//     curs=pow(Model.higgs.m,2.0)/z; 
     
//     Log_1mz=log(1.0-z);
//     cursLO=pow(Model.higgs.m,2.0)/zLO;
}

void GluonFusionInclusive::calculate_wilson_coefficients()
{
     double AC0 = 1.0;
     complex<double> C0 = sum_of_quark_triangles() * AC0;
     
     WC_zero_LO = abs(pow(C0,2.0));
}

complex<double> GluonFusionInclusive::sum_of_quark_triangles()
{
     complex<double> res(0.0);
     for (int i=0;i<Model.quarks.size();i++)
     {
          if (Model.quarks[i]->m>=50.0)
          {
               ParticleObject * cur_quark=Model.quarks[i];
               res +=    cur_quark->Y 
                         * higgs_born(cur_quark->X) 
                         * pow(cur_quark->Wq,2.0)
                         *(-3.0/4.0)/64.0;
          }
     }
     return res;
}


complex<double> GluonFusionInclusive::higgs_born(const complex<double> &x)
{
     //: the born triangle as a function of Xq
     //: 
     //:       (1+x)^2                      (1-x)^2
     //: -16 * ------- * HPL2(0,0,x) + 32 * --------
     //:          x                            x
     //:
     //:
     return(-16.0*pow(1.0+x,2.0)/x * HPL2(0,0,x)+32.0*pow(1.0-x,2.0)/x); 
}


void GluonFusionInclusive::gg_delta_LO_effective()
{
     double sigma_central =	pref_sgg
                              *meas
                              *pow(alpha_s_vector[0]/consts::Pi,2.0)
                              *cur_lumiLO[0]
                              *WC_zero_LO;
     book_production_event(sigma_central);
}


void GluonFusionInclusive::book_production_event(const double & sigma_central)
{
     //cout<<"\n*&* "<<sigma_central;
     if (sigma_central!=0.0)
          {
          production_events.push_back(new Event(sigma_central,all_momenta,xx_vegas));
          //cout<<"\n booking production event event number="<<production_events.size();
          }
}


//
void GluonFusionInclusive::calculate_derived_variables()
{
     
     tau = pow(Model.higgs.m,2.0)/pow(Etot,2.0);	
     //cout<<"\ntau="<<tau<<"\tEtot="<<Etot;
     //g_ew=sqrt(16.0*pow(Model.W.m,4.0)*pow(consts::G_fermi,2.0)/pow(consts::Pi,3.0));
     //: using the real mass
}

*/
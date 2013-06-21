



#include "GluonFusionInclusive.h"




GluonFusionInclusive::GluonFusionInclusive() : Production()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_g_00,Luminosity::F_g_00);
     SingleIntegral gg_delta_0 = new 
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
     
     lumi_with_LO_pdfs = new Luminosity(UI.number_of_flavours,
                                        UI.muf_over_mhiggs * UI.m_higgs,
                                        UI.mur_over_mhiggs * UI.m_higgs,
                                        0,//UI.perturbative_order,
                                        UI.pdf_provider,
                                        UI.pdf_error);
     lumi_with_NLO_pdfs = new Luminosity(UI.number_of_flavours,
                                        UI.muf_over_mhiggs * UI.m_higgs,
                                        UI.mur_over_mhiggs * UI.m_higgs,
                                        1,//UI.perturbative_order,
                                        UI.pdf_provider,
                                        UI.pdf_error);
     lumi_with_NNLO_pdfs = new Luminosity(UI.number_of_flavours,
                                        UI.muf_over_mhiggs * UI.m_higgs,
                                        UI.mur_over_mhiggs * UI.m_higgs,
                                        2;//UI.perturbative_order,
                                        UI.pdf_provider,
                                        UI.pdf_error);
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
               lumi_with_LO_pdfs->add_pair(cur_pair.first,cur_pair.second);
               lumi_with_NLO_pdfs->add_pair(cur_pair.first,cur_pair.second);
               lumi_with_NNLO_pdfs->add_pair(cur_pair.first,cur_pair.second);
               }
          }
     //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
     cur_lumi_with_LO_pdfs = vector<double>(lumi_with_LO_pdfs->pdf_size(),0.0);
     cur_lumi_soft_with_LO_pdfs = vector<double>(lumi_with_LO_pdfs->pdf_size(),0.0);
     
     cur_lumi_with_NLO_pdfs = vector<double>(lumi_with_NLO_pdfs->pdf_size(),0.0);
     cur_lumi_soft_with_NLO_pdfs = vector<double>(lumi_with_NLO_pdfs->pdf_size(),0.0);
     
     cur_lumi_with_NNLO_pdfs = vector<double>(lumi_with_NNLO_pdfs->pdf_size(),0.0);
     cur_lumi_soft_with_NNLO_pdfs = vector<double>(lumi_with_NNLO_pdfs->pdf_size(),0.0);
     
     //: call init_base that sets m_higgs and runs the coupling constants
     init_base(UI,the_hatch);
     //: calculate kinematic variables or Model constants that are input dependent
     //: necessarily after init_base where UI input passes to the Production class.
     calculate_derived_variables();
     Model.set_Xq_for_quarks();
     //: this depends on m_higgs which we take to be the nominal higgs mass
     //: CHANGE the above in case Higgs is off-shell.
     calculate_wilson_coefficients();
}


void GluonFusionInclusive::evaluate_all_components()
{
    production_events.clear();
    prepare_phase_space_dependent_quantities();
    for (int i=0;i<sectors.size();i++)
    {
        (this->*(sectors[sector_control]->ptr()))();
    }
}


void GluonFusionInclusive::prepare_phase_space_dependent_quantities()
{
     //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
	//: Gf = 1.16637*10^{-5} * 0.389379*10^9
     pref_sgg = 35.0309; 

     if (dim_of_integration==1)
          {
          x1 = xx_vegas[0];
          z=xx_vegas[1]; //: what ? 
          lambda = xx_vegas[2]; //: what ? 
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
          production_events.push_back(new Event(sigma_central,all_momenta));
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


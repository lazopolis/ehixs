#include "bottom_fusion.h"

#ifndef ONCE_EC
#define ONCE_EC
BottomFusion* ptr_to_BF; //: static global pointer to use as a handle for plugins (like the fortran or c++ NNLO double real pieces)
//Process* ptr_to_process;
#endif



BottomFusion::BottomFusion() : Production()
{
    
    const string nnlo_variant="franz";
    //: sector disambiguation: determines lumi, pointer_to_function_for_sector, topologies per sector ??  
     add_bbbar_sectors();
     add_bg_sectors();
     add_bq_sectors();
     ptr_to_BF = this;
    
}


void BottomFusion::init(const UserInterface& UI,TheHatch* the_hatch)
{
     
     if (UI.info)
          {
          cout<<"\n Info on Bottom Fusion\n\nsectors available:\n";
          for (int i=0;i<sectors.size();i++){cout<<"\n"<<i<<":\t"<<sectors[i]->name();}
          cout<<"\n\n";
          exit(0);
          }
     
     lumi = new Luminosity(UI.number_of_flavours,UI.muf_over_mhiggs * UI.m_higgs,UI.mur_over_mhiggs * UI.m_higgs,UI.perturbative_order,UI.pdf_provider,UI.pdf_error);
     int sector_control=UI.sector_control;
     if (sector_control>=sectors.size())
          {
          cout<<"\nYou are asking for a sector that does not exist: "<<sector_control
          <<"\t (max sector number = "<<sectors.size()-1<<")"<<endl;
          exit(1);
          }
     else 
          {
          dim_of_integration = sectors[sector_control]->dim();
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
     
     pole = UI.pole;
     sector_control=UI.sector_control;
     all_momenta.init_fvector("p1");
     all_momenta.init_fvector("p2");
     all_momenta.init_fvector("h");
     all_momenta.init_fvector("pf3");
     all_momenta.init_fvector("pf4");

     
     //: calculate kinematic variables or Model constants that are input dependent
     //: necessarily after init_base where UI input passes to the Production class.
     calculate_derived_variables();
}



void BottomFusion::calculate_derived_variables()
{
    
     tau = pow(Model.higgs.m(),2.0)/pow(Etot,2.0);
     //cout<<"\ntau="<<tau<<"\tEtot="<<Etot;
     //g_ew=sqrt(16.0*pow(Model.W.m,4.0)*pow(consts::G_fermi,2.0)/pow(consts::Pi,3.0));
     //: using the real mass
}


void BottomFusion::add_bbbar_LO_NLO_RV_RR()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_00);
     curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_00);
     
     sectors.push_back(new Sector(curlumi,&BottomFusion::bbdeltaLO,2,"b bbar LO"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NLO_SOFT,4,"b bbar NLO SOFT"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NLO_HARD,4,"b bbar NLO HARD"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_SOFT,4,"b bbar NNLO SOFT"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RV_Franz_A1,6,"b bbar NNLO RV 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RV_Franz_A2,6,"b bbar NNLO RV 2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RV_Franz_A3,6,"b bbar NNLO RV 3"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RV_Franz_A4,6,"b bbar NNLO RV 4"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH1n,1,2,"b bbar NNLO RR bb -> ggH 1 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH2n,1,8,"b bbar NNLO RR bb -> ggH 2 secs 1-8"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH3n,1,1,"b bbar NNLO RR bb -> ggH 3 sec 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH3n,2,2,"b bbar NNLO RR bb -> ggH 3 sec 2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH4n,1,1,"b bbar NNLO RR bb -> ggH 4 sec 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH4n,2,2,"b bbar NNLO RR bb -> ggH 4 sec 2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH5n,1,4,"b bbar NNLO RR bb -> ggH 5 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH6n,1,2,"b bbar NNLO RR bb -> ggH 6 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH7n,1,1,"b bbar NNLO RR bb -> ggH 7 sec 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH8n,1,1,"b bbar NNLO RR bb -> ggH 8 sec 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbggH9n,1,2,"b bbar NNLO RR bb -> ggH 9 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarqqbarH1n,1,2,"b bbar NNLO RR bb -> q qbar H 1 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarqqbarH2n,1,1,"b bbar NNLO RR bb -> q qbar H 2 sec 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarqqbarH3n,1,2,"b bbar NNLO RR bb -> q qbar H 3 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarbbarH1n,1,2,"b bbar NNLO RR bb -> b bbar H 1 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarbbarH2n,1,2,"b bbar NNLO RR bb -> b bbar H 2 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarbbarH3n,1,2,"b bbar NNLO RR bb -> b bbar H 3 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarbbarH4n,1,4,"b bbar NNLO RR bb -> b bbar H 4 secs 1-4"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarbbarH5n,1,2,"b bbar NNLO RR bb -> b bbar H 5 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarbbarH6n,1,1,"b bbar NNLO RR bb -> b bbar H 6 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarbbarH7n,1,1,"b bbar NNLO RR bb -> b bbar H 7 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarbbarH8n,1,2,"b bbar NNLO RR bb -> b bbar H 8 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz,6,bbarbbarH9n,1,1,"b bbar NNLO RR bb -> b bbar H 9 secs 1"));
     
}


void BottomFusion::add_bbbar_conv_fb0_fbb1()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_bbar_11);
     curlumi.add_pair(Luminosity::F_bbar_from_bbar_11,Luminosity::F_b_00);
     curlumi.add_pair(Luminosity::F_b_from_b_11,Luminosity::F_bbar_00);
     curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_b_11);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NLO_conv,4,"b bbar conv F_b^0 * F_bb^1 * NLO_conv()"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_bb2gH_soft,4,"b bbar conv (f_b^0 * f_bbar^1) * (R_{1/e} + V_{1/e}) SOFT  (cancels 1/e^2, 1/e of NNLO RV)"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_bb2gH_hard,4,"b bbar conv (f_b^0 * f_bbar^1) * (R_{1/e} + V_{1/e}) HARD  (cancels 1/e^2, 1/e of NNLO RV)"));
}

void BottomFusion::add_bbbar_conv_fb1_fbb1()
{
     pdf_pair_list curlumi;
     
     curlumi.add_pair(Luminosity::F_b_from_b_11,Luminosity::F_bbar_from_bbar_11);
     curlumi.add_pair(Luminosity::F_bbar_from_bbar_11,Luminosity::F_b_from_b_11);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_RRa,4,"b bbar conv (f_b^1*f_bb^1) * sigma_LO : convolutions from two nlo pdf pieces (cancels 1/e^2, 1/e of NNLO RR)"));
}

void BottomFusion::add_bbbar_conv_fb22_fbb0()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_bbar_22);
     curlumi.add_pair(Luminosity::F_bbar_from_bbar_22,Luminosity::F_b_00);
     curlumi.add_pair(Luminosity::F_b_from_b_22,Luminosity::F_bbar_00);
     curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_b_22);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_RRb,4,"b bbar conv (f_b^0 * f_bb^2) * sigma_LO : convolutions from one nnlo pdf piece (cancels 1/e^2, 1/e of NNLO RR)"));
}

void BottomFusion::add_bbbar_conv_fb21_fbb0()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_bbar_21);
     curlumi.add_pair(Luminosity::F_bbar_from_bbar_21,Luminosity::F_b_00);
     curlumi.add_pair(Luminosity::F_b_from_b_21,Luminosity::F_bbar_00);
     curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_b_21);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_RRc,4,"b bbar conv (f_b^0 * f_bb^2_1) * sigma_LO : convolutions from one nnlo pdf piece ( 1/e of NNLO RR)"));
}


void BottomFusion::add_bbbar_conv_fgb21_fbb0()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_g_from_b_11,Luminosity::F_bbar_00);
     curlumi.add_pair(Luminosity::F_g_from_bbar_11,Luminosity::F_b_00);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_RRd,4,"b bbar conv _g->b ^1 * f_bb^0) * sigma_bg_NLO : convolutions from one nlo pdf piece ( 1/e of NNLO RR)"));
}

void BottomFusion::add_bbbar_conv_fb00_fgb11()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_b_11);
     curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_bbar_11);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_RRe,4,"b bbar conv _g->b ^1 * f_bb^0) * sigma_gb_NLO : convolutions from one nlo pdf piece ( 1/e of NNLO RR)"));
}

void BottomFusion::add_bg_NLO_RV_RR()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_00);
     curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_00);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NLO_HARD_1,4,"b g -> b g H NLO"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RV_bg,6,"b g -> b g H NNLO RV"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_renorm_bg,6,"b g -> b g H NNLO renorm"));
     
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,bggbH1n,1,1,"b g NNLO RR bg -> b g H 1 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,bggbH2n,1,2,"b g NNLO RR bg -> b g H 2 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,bggbH3n,1,2,"b g NNLO RR bg -> b g H 3 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,bggbH4n,1,2,"b g NNLO RR bg -> b g H 4 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,bggbH5n,1,4,"b g NNLO RR bg -> b g H 5 secs 1-4"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,bggbH6n,1,2,"b g NNLO RR bg -> b g H 6 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,bggbH7n,1,1,"b g NNLO RR bg -> b g H 7 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,bggbH8n,1,1,"b g NNLO RR bg -> b g H 8 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,bggbH9n,1,1,"b g NNLO RR bg -> b g H 9 secs 1"));
}

void BottomFusion::add_gb_NLO_RV_RR()
{
     pdf_pair_list curlumi;
     curlumi.add_pair(Luminosity::F_g_00,Luminosity::F_b_00);
     curlumi.add_pair(Luminosity::F_g_00,Luminosity::F_bbar_00);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NLO_HARD_2,4,"g b -> b g H NLO"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RV_gb,6,"g b -> b g H NNLO RV "));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_renorm_gb,6,"g b -> b g H NNLO renorm"));
     
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,gbgbH1n,1,1,"g b NNLO RR gb -> b g H 1 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,gbgbH2n,1,2,"g b NNLO RR gb -> b g H 1 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,gbgbH3n,1,2,"g b NNLO RR gb -> b g H 1 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,gbgbH4n,1,2,"g b NNLO RR gb -> b g H 1 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,gbgbH5n,1,4,"g b NNLO RR gb -> b g H 1 secs 1-4"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,gbgbH6n,1,2,"g b NNLO RR gb -> b g H 1 secs 1-2"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,gbgbH7n,1,1,"g b NNLO RR gb -> b g H 1 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,gbgbH8n,1,1,"g b NNLO RR gb -> b g H 1 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bg,6,gbgbH9n,1,1,"g b NNLO RR gb -> b g H 1 secs 1"));
     
     
}


void BottomFusion::add_bg_conv_fb0_fbbarfromg11()
{
     pdf_pair_list fb0_fbbarfromg11;
     fb0_fbbarfromg11.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_g_11);
     fb0_fbbarfromg11.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_g_11);
     sectors.push_back(new Sector(fb0_fbbarfromg11,&BottomFusion::NLO_conv_1,4,"b g conv NLO (cancels 1/e of NLO) "));
}
void BottomFusion::add_bg_conv_fbbarfromg11_fb0()
{
     pdf_pair_list fbbarfromg11_fb0;
     fbbarfromg11_fb0.add_pair(Luminosity::F_bbar_from_g_11,Luminosity::F_b_00);
     fbbarfromg11_fb0.add_pair(Luminosity::F_b_from_g_11,Luminosity::F_bbar_00);
     sectors.push_back(new Sector(fbbarfromg11_fb0,&BottomFusion::NLO_conv_2,4,"g b conv NLO (cancels 1/e of NLO) "));
}
void BottomFusion::add_bg_conv_fb0_fbbarfromg11_for_nlo()
{
     pdf_pair_list fb0_fbbarfromg11_for_nlo;
     fb0_fbbarfromg11_for_nlo.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_g_11);
     fb0_fbbarfromg11_for_nlo.add_pair(Luminosity::F_bbar_from_g_11,Luminosity::F_b_00);
     fb0_fbbarfromg11_for_nlo.add_pair(Luminosity::F_b_from_g_11,Luminosity::F_bbar_00);
     fb0_fbbarfromg11_for_nlo.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_g_11);
     sectors.push_back(new Sector(fb0_fbbarfromg11_for_nlo,&BottomFusion::NNLO_conv_bb_nlo_soft,4,"g b conv f_b x f_bbar_from_g_11  x sigma_bbbar_nlo soft "));
     sectors.push_back(new Sector(fb0_fbbarfromg11_for_nlo,&BottomFusion::NNLO_conv_bb_nlo_hard,4,"g b conv f_b x f_bbar_from_g_11  x sigma_bbbar_nlo hard "));
}

void BottomFusion::add_bg_conv_fb0_fbbarfromg22()
{
     pdf_pair_list fb0_fbbarfromg22;
     fb0_fbbarfromg22.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_g_22);
     fb0_fbbarfromg22.add_pair(Luminosity::F_bbar_from_g_22,Luminosity::F_b_00);
     fb0_fbbarfromg22.add_pair(Luminosity::F_b_from_g_22,Luminosity::F_bbar_00);
     fb0_fbbarfromg22.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_g_22);
     sectors.push_back(new Sector(fb0_fbbarfromg22,&BottomFusion::NNLO_conv_bb_LO_e2,4,"g b conv f_b x f_bbar_from_g_22  x sigma_bbbar_lo "));
}
void BottomFusion::add_bg_conv_fb0_fbbarfromg21()
{
     pdf_pair_list fb0_fbbarfromg21;
     fb0_fbbarfromg21.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_g_21);
     fb0_fbbarfromg21.add_pair(Luminosity::F_bbar_from_g_21,Luminosity::F_b_00);
     fb0_fbbarfromg21.add_pair(Luminosity::F_b_from_g_21,Luminosity::F_bbar_00);
     fb0_fbbarfromg21.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_g_21);
     sectors.push_back(new Sector(fb0_fbbarfromg21,&BottomFusion::NNLO_conv_bb_LO_e1,4,"g b conv f_b x f_bbar_from_g_21  x sigma_bbbar_lo "));
}
void BottomFusion::add_bg_conv_fb11_fbbarfromg11()
{
     pdf_pair_list fb11_fbbarfromg11;
     fb11_fbbarfromg11.add_pair(Luminosity::F_b_from_b_11,Luminosity::F_bbar_from_g_11);
     fb11_fbbarfromg11.add_pair(Luminosity::F_bbar_from_g_11,Luminosity::F_b_from_b_11);
     fb11_fbbarfromg11.add_pair(Luminosity::F_b_from_g_11,Luminosity::F_bbar_from_bbar_11);
     fb11_fbbarfromg11.add_pair(Luminosity::F_bbar_from_bbar_11,Luminosity::F_b_from_g_11);
     sectors.push_back(new Sector(fb11_fbbarfromg11,&BottomFusion::NNLO_conv_bb_LO_e2,4,"g b conv f_b_11 x f_bbar_from_g_11  x sigma_bbbar_lo "));
}
void BottomFusion::add_bg_conv_fb11_fg0()
{
     pdf_pair_list fb11_fg0;
     fb11_fg0.add_pair(Luminosity::F_b_from_b_11,Luminosity::F_g_00);
     fb11_fg0.add_pair(Luminosity::F_bbar_from_bbar_11,Luminosity::F_g_00);
     fb11_fg0.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_g_11);
     fb11_fg0.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_g_11);
     sectors.push_back(new Sector(fb11_fg0,&BottomFusion::NNLO_conv_bg,4,"b g conv f_b_from_b_11 x f_g x sigma_NLO_bg "));
}
void BottomFusion::add_bg_conv_fg0_fb11()
{
     pdf_pair_list fg0_fb11;
     fg0_fb11.add_pair(Luminosity::F_g_00,Luminosity::F_b_from_b_11);
     fg0_fb11.add_pair(Luminosity::F_g_00,Luminosity::F_bbar_from_bbar_11);
     fg0_fb11.add_pair(Luminosity::F_g_from_g_11,Luminosity::F_b_00);
     fg0_fb11.add_pair(Luminosity::F_g_from_g_11,Luminosity::F_bbar_00);
     sectors.push_back(new Sector(fg0_fb11,&BottomFusion::NNLO_conv_gb,4,"g b conv f_g x f_b_from_b_11 x sigma_NLO_gb "));
}

void BottomFusion::add_bg_sectors()
{
     add_bg_NLO_RV_RR();
     add_gb_NLO_RV_RR();
     
     add_bg_conv_fg0_fb11();
     add_bg_conv_fb11_fg0();
     add_bg_conv_fb0_fbbarfromg11();
     add_bg_conv_fbbarfromg11_fb0();
     add_bg_conv_fb0_fbbarfromg11_for_nlo();
     add_bg_conv_fb11_fbbarfromg11();
     add_bg_conv_fb0_fbbarfromg21();
     add_bg_conv_fb0_fbbarfromg22();
}

void BottomFusion::add_bbbar_sectors()
{
     add_bbbar_LO_NLO_RV_RR();
     add_bbbar_conv_fb0_fbb1();
     add_bbbar_conv_fb1_fbb1();
     add_bbbar_conv_fb22_fbb0();
     add_bbbar_conv_fb21_fbb0();
     add_bbbar_conv_fgb21_fbb0();
     add_bbbar_conv_fb00_fgb11();
}


void BottomFusion::add_bq_sectors()
{
     add_qb_RR();
     add_bq_RR();
     add_bq_conv_bg();
     add_bq_conv_gb();
     add_bq_conv_bb_e2();
     add_bq_conv_bb_e1();
     
     
}

void BottomFusion::add_qb_RR()
{
     pdf_pair_list curlumi;
          curlumi.add_pair(Luminosity::F_u_00,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_d_00,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_s_00,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_c_00,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_ubar_00,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_dbar_00,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_sbar_00,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_cbar_00,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_u_00,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_d_00,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_s_00,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_c_00,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_ubar_00,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_dbar_00,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_sbar_00,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_cbar_00,Luminosity::F_bbar_00);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,qbqbH1n,1,1,"q b NNLO RR qb -> qbH 1 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,qbqbH2n,1,1,"q b NNLO RR qb -> qbH 2 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,qbqbH3n,1,1,"q b NNLO RR qb -> qbH 3 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,qbqbH4n,1,1,"q b NNLO RR qb -> qbH 4 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,qbqbH5n,1,1,"q b NNLO RR qb -> qbH 5 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,qbqbH6n,1,1,"q b NNLO RR qb -> qbH 6 secs 1"));
}


void BottomFusion::add_bq_RR()
{
     pdf_pair_list curlumi;

          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_u_00);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_d_00);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_c_00);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_s_00);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_ubar_00);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_dbar_00);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_cbar_00);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_sbar_00);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_u_00);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_d_00);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_c_00);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_s_00);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_ubar_00);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_dbar_00);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_cbar_00);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_sbar_00);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,bqqbH1n,1,1,"q b NNLO RR bq -> qbH 1 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,bqqbH1n,1,1,"q b NNLO RR bq -> qbH 2 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,bqqbH1n,1,1,"q b NNLO RR bq -> qbH 3 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,bqqbH1n,1,1,"q b NNLO RR bq -> qbH 4 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,bqqbH1n,1,1,"q b NNLO RR bq -> qbH 5 secs 1"));
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_RR_Franz_bq,6,bqqbH1n,1,1,"q b NNLO RR bq -> qbH 6 secs 1"));
}
void BottomFusion::add_bq_conv_bg()
{
     pdf_pair_list curlumi;

          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_u_11);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_d_11);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_s_11);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_c_11);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_ubar_11);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_dbar_11);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_sbar_11);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_cbar_11);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_u_11);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_d_11);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_s_11);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_c_11);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_ubar_11);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_dbar_11);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_cbar_11);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_sbar_11);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_bg,6,bqqbH1n,1,1,"q b NNLO conv with bg "));
}



void BottomFusion::add_bq_conv_gb()
{
     pdf_pair_list curlumi;
          curlumi.add_pair(Luminosity::F_g_from_u_11,Luminosity::F_b_00);
          
          curlumi.add_pair(Luminosity::F_g_from_d_11,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_g_from_s_11,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_g_from_c_11,Luminosity::F_b_00);
          
          curlumi.add_pair(Luminosity::F_g_from_ubar_11,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_g_from_dbar_11,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_g_from_sbar_11,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_g_from_cbar_11,Luminosity::F_b_00);
          
          curlumi.add_pair(Luminosity::F_g_from_u_11,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_g_from_d_11,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_g_from_s_11,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_g_from_c_11,Luminosity::F_bbar_00);
          
          curlumi.add_pair(Luminosity::F_g_from_ubar_11,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_g_from_dbar_11,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_g_from_cbar_11,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_g_from_sbar_11,Luminosity::F_bbar_00);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_gb,6,bqqbH1n,1,1,"q b NNLO conv with gb "));
}


void BottomFusion::add_bq_conv_bb_e2()
{
     pdf_pair_list curlumi;

          curlumi.add_pair(Luminosity::F_b_from_u_22,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_d_22,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_s_22,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_c_22,Luminosity::F_bbar_00);
          
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_u_22);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_d_22);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_s_22);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_c_22);
          
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_u_22);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_d_22);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_s_22);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_c_22);
          
          curlumi.add_pair(Luminosity::F_bbar_from_u_22,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_d_22,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_s_22,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_c_22,Luminosity::F_b_00);
          
          curlumi.add_pair(Luminosity::F_b_from_ubar_22,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_dbar_22,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_sbar_22,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_cbar_22,Luminosity::F_bbar_00);
          
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_ubar_22);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_dbar_22);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_sbar_22);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_cbar_22);
          
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_ubar_22);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_dbar_22);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_sbar_22);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_cbar_22);
          
          curlumi.add_pair(Luminosity::F_bbar_from_ubar_22,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_dbar_22,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_sbar_22,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_cbar_22,Luminosity::F_b_00);
     sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_bb_LO_e2,2,bqqbH1n,1,1,"q b NNLO conv with LO bb 1/e^2 "));
}

void BottomFusion::add_bq_conv_bb_e1()
{
     pdf_pair_list curlumi;

          curlumi.add_pair(Luminosity::F_b_from_u_21,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_d_21,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_s_21,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_c_21,Luminosity::F_bbar_00);
          
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_u_21);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_d_21);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_s_21);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_c_21);
          
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_u_21);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_d_21);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_s_21);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_c_21);
          
          curlumi.add_pair(Luminosity::F_bbar_from_u_21,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_d_21,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_s_21,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_c_21,Luminosity::F_b_00);
          
          curlumi.add_pair(Luminosity::F_b_from_ubar_21,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_dbar_21,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_sbar_21,Luminosity::F_bbar_00);
          curlumi.add_pair(Luminosity::F_b_from_cbar_21,Luminosity::F_bbar_00);
          
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_ubar_21);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_dbar_21);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_sbar_21);
          curlumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_cbar_21);
          
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_ubar_21);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_dbar_21);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_sbar_21);
          curlumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_cbar_21);
          
          curlumi.add_pair(Luminosity::F_bbar_from_ubar_21,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_dbar_21,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_sbar_21,Luminosity::F_b_00);
          curlumi.add_pair(Luminosity::F_bbar_from_cbar_21,Luminosity::F_b_00);
          sectors.push_back(new Sector(curlumi,&BottomFusion::NNLO_conv_bb_LO_e1,2,bqqbH1n,1,1,"q b NNLO conv with LO bb 1/e"));
          }








void BottomFusion::evaluate_sector()
{
     production_events.clear();
     prepare_phase_space_dependent_quantities();
     (this->*pointer_to_function_for_sector)();
}


void BottomFusion:: prepare_phase_space_dependent_quantities()
{
    //: pref_sbb = pi * Y^2 / ( 2*Nc*mh^2 )
    //  0.389379*10^9 is the GeV to pb
    pref_sbb = consts::Pi/6.0 * 0.389379e9
						*pow(Model.bottom.Y * yukawa_b_vector[0],2.0)
						/pow(Model.higgs.m(),2.0)
						; 
 //    pref_sbb =  1.0; 
    special_normalization_factor_for_double_real=12.0; 
    parametrization_for_LO_kinematics(x1LO,x2LO,zLO,measLO);
     

    parametrization_for_NLO_kinematics(x1,x2,z,meas);
    
    lambda = xx_vegas[2]; 
    
    lumi->set_cur_lumi(x1LO,x2LO,cur_lumiLO);//:sets the cur_lumi_LO according to the luminosity initialized in the constructor for this sector
    lumi->set_cur_lumi(x1,x2,cur_lumi);       //:sets the cur_lumi according to the luminosity initialized in the constructor for this sector
    
    //: Nij holds various fractional factors that depend on subchannel
    //: like the initial state averaging factor
    Nij_bbbar=1.0/36.0 ; //: averaging factor for the bbbar->bbbarH channel
     Nij_bg= 1.0/96.0; //: averaging factor for bg channel
     Nij_bq = 1.0/36.0;
     
    lh=log_muf_sq_over_mh_sq;		
    curs=pow(Model.higgs.m(),2.0)/z;

    Log_1mz=log(1.0-z);
    cursLO=pow(Model.higgs.m(),2.0)/zLO;
}






void BottomFusion::parametrization_for_LO_kinematics(double & x1, double & x2, double & z, double & meas)
{
     
     int parametrization_switch=3;
     
     
     if (parametrization_switch==0)
          {
          //: parametrization z/x1
          x1=xx_vegas[0];
          x2=tau/x1;
          meas = 1.0/x1;
          z=1.0;
          
          }
     else if (parametrization_switch==1)
          {
          
          //: old parametrization z/x1~rap
          double jac_from_rap_param ;
          double x= generate_x1(jac_from_rap_param);
          
          meas = 1.0/x*jac_from_rap_param;
          
          x1=x;
          z=1.0;
          x2= tau/x1/z;
          }
     else if (parametrization_switch==2)
          {
          //: new parametrization u1,u2
          double u2=xx_vegas[0];
          double u1=0.0;//xx_vegas[1];
          
          
          double U = log(tau/(1.0-u1*(1.0-tau)));
          
          x1 = exp((1.0-u2)*U);
          x2 = exp(u2*U);
          z=1.0;
          meas =  log((1.0-u1*(1.0-tau))/tau);
          }
     else if (parametrization_switch==3)
          {
          double zz=1.0;
          double yy=xx_vegas[1];
          meas = -log(tau/zz);
          x1 = exp(yy*log(tau));
          x2 = exp((1.0-yy)*log(tau));
          z = 1.0;
          }
//     cout<<"\nhello "<<x1<<x2<<"\t"<<xx_vegas[0]<<"\t"<<xx_vegas[1]<<"\t"<<tau;
}


bool BottomFusion::bjorken_x_out_of_range(const double & x1,const double & x2)
{
     double almost_one = 1.0-1e-14;
     const double delta = 1e-14;
     if (x1<almost_one and x2<almost_one and x1>0.0 and x2>0.0 and (x1*x2>=(tau-delta))) 
          {
          
          return false;
          }
     else 
          {
          if (x1*x2<tau-delta)
               {
               cout<<setprecision(16)<<"\n x1*x2="<<x1*x2<<" < "<<tau<<"=tau";
               }
          else
               {
               // cout<<"\nx1="<<x1<<"\tx2="<<x2;
               }
          return true;
          }
}


void BottomFusion::parametrization_for_NLO_kinematics(double & x1, double & x2, double & z, double & meas)
{
     
     int parametrization_switch=3;
     
     vector<double> cur_lumi;
     if (parametrization_switch==0)
          {
          //: parametrization z/x1
          x1=xx_vegas[0];
          x2=xx_vegas[1];
          meas = 1.0/x1;
          z=tau/x1/x2;
          
          }
     else if (parametrization_switch==1)
          {
          
          //: old parametrization z/x1~rap
          double jac_from_rap_param ;
          double x= generate_x1(jac_from_rap_param);
          
          meas = 1.0/x*jac_from_rap_param;
          
          x1=x;
          z=xx_vegas[1];
          x2= tau/x1/z;
          }
     else if (parametrization_switch==2)
          {
          //: new parametrization u1,u2
          double u2=xx_vegas[0];
          double u1=xx_vegas[1];
          
          
          double U = log(tau/(1.0-u1*(1.0-tau)));
          meas =  log((1.0-u1*(1.0-tau))/tau);
          x1 = exp((1.0-u2)*U);
          x2 = exp(u2*U);
          z=1.0-u1*(1.0-tau);
          }
     else if (parametrization_switch==3)
          {
          double zz=xx_vegas[0];
          double yy=xx_vegas[1];
          meas = -log(tau/zz);
          x1 = exp(yy*log(tau/zz));
          x2 = exp((1.0-yy)*log(tau/zz));
          z = zz;
          const double almost_zero =0.0;// 1e-23;
          if (x1>1.0-almost_zero or x2>1.0-almost_zero or x1<almost_zero or x2<almost_zero)
               {
               meas=0.0;
               }
          }
}


double BottomFusion::generate_x1(double & jac_from_rap_param)
{
     double xlambda=xx_vegas[0];
     double umax = -0.5*log(tau);
     double umin = 0.5*log(tau);
     double u=umin+(umax-umin)*xlambda;
     jac_from_rap_param = sqrt(tau)*exp(u)*(umax-umin);
     double x= sqrt(tau)*exp(u);
     //cout<<setprecision(18)<<"\nxx_vegas[0]="<<xx_vegas[0]<<" x="<<x<<" tau/x="<<tau/x<<" tau="<<tau<<" u="<<u<<" umin="<<umin;
     return x;
}





void BottomFusion::Jnlo(const double & res, const double & x1, const double & x2,const double &z, const double & lambda)
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
          book_production_event();
          }
}




//:---------- kinematics
/*
 void BottomFusion::nan_check(const double & x,const string & name)
{
     if (x!=x)
          {
          cout<<"\n nan_check found nan in variable "<<name<<"\t"<<x<<endl;
          }
}
*/
void BottomFusion::set_up_event_kinematics(
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
     /* p1.flush();
      p2.flush();
      p3.flush();
      p4.flush();
      pH.flush();
      */
     
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
}

/*
double BottomFusion::frame(double x)
{
#ifdef debug
     cout<<"\n["<<__func__<<"]";
#endif
     double res=x;
     if (x<-1.0) {res=-1.0;}
     if (x>1.0){res=1.0;}
     return res;
}
*/

void BottomFusion::book_production_event()
{
     //Vegas.set_up_vegas_ff(0.0);
}

void BottomFusion::book_production_event(const double & sigma_central,
                                           const double & x1,
                                           const double & x2,
                                           const double & z,
                                           const double & s13,
                                           const double & s23,
                                           const double & s14,
                                           const double & s24,
                                           const double & s34)
{
     //cout<<"\n*&* "<<sigma_central;
     if (sigma_central!=0.0)
          {
          set_up_event_kinematics(x1,x2,z,s13,s23,s14,s24,s34);
          production_events.push_back(new Event(sigma_central,all_momenta,xx_vegas));
          //cout<<"\n booking production event event number="<<production_events.size();
          }
}










void BottomFusion::bbdeltaLO()
{
     if (pole==0)
          {
          
          double sigma_central =	pref_sbb*measLO*cur_lumiLO[0];
          Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
          }
     else
          {book_production_event();}
}


void BottomFusion::NLO_HARD()
{
     const double prefactor = pref_sbb
               *alpha_s_vector[0]/consts::Pi
               *cur_lumi[0]*meas;
     if (pole==1)
          {
          Jnlo(  prefactor*bb_coll_pole(),x1,x2,z,1.0);
          Jnlo(  prefactor*bb_coll_pole(),x1,x2,z,0.0);
          Jnlo(  prefactor*bb_coll_soft_pole(),x1LO,x2LO,zLO,0.0);
          }
     if (pole==0)
          {
          Jnlo(  prefactor*bb_coll_fin(),x1,x2,z,1.0);
          Jnlo(  prefactor*bb_coll_fin(),x1,x2,z,0.0);
          Jnlo(  prefactor*bb_coll_soft_fin(),x1LO,x2LO,zLO,0.0);
          Jnlo(  prefactor*bb_hard_fin(),x1,x2,z,lambda);
          Jnlo(  -lambda* prefactor*bb_hard_fin(),x1,x2,z,1.0);
          Jnlo(  -(1.0-lambda)*prefactor*bb_hard_fin(),x1,x2,z,0.0);
          }
     
}


void BottomFusion::NLO_SOFT()
{
     //: delta term prefactor
     const double delta_prefactor = pref_sbb
     *alpha_s_vector[0]/consts::Pi
     *cur_lumiLO[0]*measLO;
     //: plus distribution term prefactors. Note that they are all multiplied with a soft jet function (J(z=1))
     const double plus0_prefactor = pref_sbb
     *alpha_s_vector[0]/consts::Pi
     *(cur_lumi[0]*meas-cur_lumiLO[0]*measLO)/(1.0-z);
     const double plus1_prefactor = plus0_prefactor * log(1.0-z);
     
     if (pole==1)
          {
          Jnlo(  bb_soft_pole(delta_prefactor,plus0_prefactor),x1LO,x2LO,zLO,0.0);
          }
     if (pole==0)
          {
          Jnlo(  bb_soft_finite(delta_prefactor,plus0_prefactor,plus1_prefactor),x1LO,x2LO,zLO,0.0);
          }
     
}


void BottomFusion::NNLO_RV_Franz_A1()
{
     //: although here the epsilon_coeff should have been 1.0, Franz has probably expanded over (1-z)^(-1-e) in this
     //: function alone, just for the fun of it.
     double wd=1.0// Note here: no 1/(1.0-z)
     *cur_lumi[0]
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *meas*pref_sbb
     *3.0*(-4.0)/consts::Pi
     *Nij_bbbar;
     double res;
     for (int sector=1;sector<=2;sector++) // A1 has 2 sectors
          {
          if (pole==0)
               {
               RVbbbarggH1n(sector,0,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==1)
               {
               RVbbbarggH1n(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==2)
               {
               RVbbbarggH1n(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==3)
               {
               RVbbbarggH1n(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==4)
               {
               book_production_event();
               }
          }
     
     
     //    for (int cur_sector=1;cur_sector<=2;cur_sector++) //: A2 has 4 sectors
     //	{
     //		
     //		double epsilon_coeff = 0.0;
     //		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH1n,cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
     //		
     //	}
     
}

void BottomFusion::NNLO_RV_Franz_A2()
{
     double wd=1.0/(1.0-z)
     *cur_lumi[0]
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *meas*pref_sbb
     *3.0*(-4.0)/consts::Pi
     *Nij_bbbar;
     
     for (int cur_sector=1;cur_sector<=4;cur_sector++) //: A2 has 4 sectors
          {
		
		double epsilon_coeff = 2.0;
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH2n,cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH2n,cur_sector,zLO,lh,-wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],cursLO,x1LO,x2LO,"no_gluons");
          }
     
}

void BottomFusion::NNLO_RV_Franz_A3()
{
     double wd=1.0/(1.0-z)
     *cur_lumi[0]
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *meas*pref_sbb
     *3.0*(-4.0)/consts::Pi
     *Nij_bbbar;
     for (int cur_sector=1;cur_sector<=4;cur_sector++) //: A3 has 4sectors
          {
		double epsilon_coeff = 3.0;
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH3n,cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
		//: no  1/e^4 pole  - this one is finite
          }
}

void BottomFusion::NNLO_RV_Franz_A4()
{
     double wd=1.0/(1.0-z)
          *cur_lumi[0]
          *pow(alpha_s_vector[0]/consts::Pi,2.0)
          *meas*pref_sbb
          *3.0*(-4.0)/consts::Pi
          *Nij_bbbar;
     
     for (int cur_sector=1;cur_sector<=2;cur_sector++) //: A4 has 2 sectors
          {
		double epsilon_coeff = 4.0;
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH4n,cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH4n,cur_sector,zLO,lh,-wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],cursLO,x1LO,x2LO,"no_gluons");
          }
}


void BottomFusion::NNLO_RR_Franz()
{	
     double epsilon_coeff=4.0;
     
     for (int cur_sector=sectors[sector_control]->sec_init();cur_sector<=sectors[sector_control]->sec_fin();cur_sector++)
          {
          if (cur_lumi[0]==0.0)//: case in which x1 or x2 are out of bounds.
               {
               book_production_event();
               }
          else 
               {
               double wd=1.0/(1.0-z)
               *cur_lumi[0]
               *pow(alpha_s_vector[0]/consts::Pi,2.0)
               *meas*pref_sbb
               *special_normalization_factor_for_double_real
               *Nij_bbbar;
               NNLO_RR_epsilon_expansion(epsilon_coeff,sectors[sector_control]->give_FR(),cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
               NNLO_RR_epsilon_expansion(epsilon_coeff,sectors[sector_control]->give_FR(),cur_sector,zLO,lh,-wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],cursLO,x1LO,x2LO,"no_gluons");
               }
          
          }
}



void BottomFusion::NNLO_RR_Franz_bg()
{	
     if (cur_lumi[0]==0.0)//: case in which x1 or x2 are out of bounds.
          {
          book_production_event();
          }
     else 
          {
          double wd=1.0/(1.0-z)
          *cur_lumi[0]
          *pow(alpha_s_vector[0]/consts::Pi,2.0)
          *meas*pref_sbb
          *special_normalization_factor_for_double_real
          *(-1.0)//: because Franz's routines are coming from crossing the g<->b and mis a minus sign
          *Nij_bg;
          
           
          double epsilon_coeff=4.0;
          for (int cur_sector=sectors[sector_control]->sec_init();cur_sector<=sectors[sector_control]->sec_fin();cur_sector++)
               {
               NNLO_RR_epsilon_expansion(epsilon_coeff,sectors[sector_control]->give_FR(),cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"one_gluon");
               }
          }
}


void BottomFusion::NNLO_RR_Franz_bq()
{	
     if (cur_lumi[0]==0.0)//: case in which x1 or x2 are out of bounds.
          {
          book_production_event();
          }
     else 
          {
          double wd=1.0/(1.0-z)
          *cur_lumi[0]
          *pow(alpha_s_vector[0]/consts::Pi,2.0)
          *meas*pref_sbb
          *special_normalization_factor_for_double_real
          *Nij_bq;

          double epsilon_coeff=4.0;
          for (int cur_sector=sectors[sector_control]->sec_init();cur_sector<=sectors[sector_control]->sec_fin();cur_sector++)
          {
               NNLO_RR_epsilon_expansion(epsilon_coeff,sectors[sector_control]->give_FR(),cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
          }
     }
}




//: NLO matrix elements for b bbar -> H +X. Soft terms from virtual and real have already been combined and the 1/e^2 pole has been 
//: canceled analytically. The remaining 1/e is what gets canceled by the NLO convolution. The matrix elements are organized in 
//: soft (those for which J(1) in integrated counterterms, not necessarily having z=1) and hard, and in 1/e, finite and e pieces

double  BottomFusion::bb_soft_pole(const double & delta,const double &  plus)
{
     const double ME = -2.0 * delta - 8.0/3.0 * plus;
     return(ME);
}

double  BottomFusion::bb_soft_finite(const double &  delta,const double &  plus0,const double &  plus1)
{
     const double ME = 8.0/3.0*(
                                (consts::pi_square/6.0-1.0/2.0)*delta
                                +2.0*plus1
                                -plus0*lh
                                )
     //+ 2.0*log_mur_sq_over_muf_sq*delta  // ***  THIS IS THE ONLY mur - DEPENDENT TERM at NLO and it has migrated to convolutions.cpp
     ;
     return(ME);
}

double  BottomFusion::bb_soft_e(const double &  delta,const double &  plus0,const double &  plus1,const double &  plus2)
{
     const double pisquare = consts::pi_square;
     const double ME = (2.0/3.0)*plus0*(-2.0*pow(lh,2.0)+pisquare)
     +(16.0/3.0)*plus1*lh
     -(16.0/3.0)*plus2
     +(-8.0/3.0-(4.0/3.0)*lh+(4.0/9.0)*lh*pisquare)*delta;
     return(ME);
}



double  BottomFusion::bb_coll_pole()
{
     const double QQQ = 2.0/3.0 /(1.0-z);
     const double ME = -  (1.0+pow(z,2.0)) * QQQ  ;
     return(ME);
}


double  BottomFusion::bb_coll_fin()
{
     const double QQQ = 2.0/3.0 /(1.0-z);
     const double LZ = log(z);
     const double LMZ = log(1.0-z);
     const double ME = -QQQ*(-2.0*LMZ-2.0*LMZ*pow(z,2.0)-1.0+2.0*z-pow(z,2.0)+LZ+LZ*pow(z,2.0)+lh+lh*pow(z,2.0));
     return(ME);
}




double  BottomFusion::bb_coll_e()
{
     const double QQQ = 2.0/3.0 /(1.0-z);
     const double LZ = log(z);
     const double LMZ = log(1.0-z);
     const double ME =  -(1.0/4.0)*QQQ*
     (-4.0*lh
      -8.0*LMZ*LZ*pow(z,2.0)
      +8.0*LZ*z
      -4.0*lh*pow(z,2.0)
      -4.0*LZ
      +8.0*LMZ
      +4.0*LZ*lh*pow(z,2.0)
      -4.0*LZ*pow(z,2.0)
      +8.0*lh*z
      +2.0*pow(LZ,2.0)*pow(z,2.0)
      +4.0*LZ*lh+2*pow(lh,2.0)*pow(z,2.0)
      -8.0*LMZ*lh*pow(z,2.0)
      -pow(z,2.0)*consts::pi_square
      +8.0*pow(LMZ,2.0)*pow(z,2.0)
      +8.0*LMZ*pow(z,2.0)
      +2.0*pow(lh,2.0)
      -consts::pi_square
      +8.0*pow(LMZ,2.0)
      +2.0*pow(LZ,2.0)
      -16.0*LMZ*z
      -8.0*LMZ*LZ
      -8.0*LMZ*lh);
     return(ME);
}


double  BottomFusion::bb_coll_soft_pole()
{
     const double QQQ = 2.0/3.0 /(1.0-z);
     const double ME = 4.0*QQQ ;
     return(ME);
}

double  BottomFusion::bb_coll_soft_fin()
{
     const double QQQ = 2.0/3.0 /(1.0-z);
     const double LZ = log(z);
     const double LMZ = log(1.0-z);
     const double ME= 4.0*QQQ*(lh-2.0*LMZ);
     return(ME);
}


double  BottomFusion::bb_coll_soft_e()
{
     const double QQQ = 2.0/3.0 /(1.0-z);
     const double LZ = log(z);
     const double LMZ = log(1.0-z);
     const double ME = (2.0*pow(lh,2.0)-consts::pi_square+8.0*pow(LMZ,2.0)-8.0*LMZ*lh)*QQQ;
     return(ME);
}


double  BottomFusion::bb_hard_fin()
{
     const double QQQ = 1.0/lambda/(1.0-lambda);
     const double bh1 = (1.0+pow(z,2.0))/(1.0-z) ;
     const double bh2 = 1.0-z;
     const double ME = (2.0/3.0)*bh1*QQQ;
     return(ME);
}

double  BottomFusion::bb_hard_e()
{
     const double QQQ = 1.0/lambda/(1.0-lambda);
     const double bh1 = (1.0+pow(z,2.0))/(1.0-z) ;
     const double bh2 = 1.0-z;
     const double Lz = log(z/pow(1.0-z,2.0));
     const double LL = log(lambda*(1.0-lambda));
     const double ME = (-(2.0/3.0)*bh1*QQQ*LL+(2.0/3.0*(bh2+(Lz+lh)*bh1))*QQQ);
     return(ME);
}




//: NLO matrix elements for b g -> H +g. 

double  BottomFusion::bg_coll_pole()
{
     const double ps = pow(z,2.0)+pow(1.0-z,2.0);
     const double ME = -(1.0/4.0)*ps ;
     return(ME);
}

double  BottomFusion::bg_coll_fin()
{
     const double LZ = log(z/pow(1.0-z,2.0));
     const double ps = (pow(z,2.0)+pow(1.0-z,2.0));
     const double ME = (-1.0/4.0)* ( ps*(LZ+lh)+ps-1.0 + ps/(1.0-lambda));
     return(ME);
}



double  BottomFusion::bg_coll_e()
{
     const double Lz = log(z/pow(1.0-z,2.0));
     const double LLZ = log(z/pow(1.0-z,2.0)/lambda/(1.0-lambda));
     const double ps = (pow(z,2.0)+pow(1.0-z,2.0));
     const double ME =  1.0/4.0 * (1.0-(lh+1.0+LLZ)*ps)/(1.0-lambda)
     +1.0/4.0*(
               -(1.0/2.0)*ps*pow(Lz,2.0)
               +(-ps*lh-ps+1.0)*Lz
               -(1.0/2.0)*ps*pow(lh,2.0)
               +(-ps+1.0)*lh
               +(1.0/4.0)*consts::pi_square
               +(1.0/2.0*((1.0/2.0)*ps-1.0/2.0+z))*consts::pi_square
               -(1.0/2.0)*z*consts::pi_square
               -ps
               +1.0
               );
     return(ME);
}



double  BottomFusion::bg_hard_fin()
{
     const double WW=z+(1.0-z)*lambda;
     const double QQ=1.0-(1.0-z)*(1.0-lambda);
     const double pzL = pow(z,2.0)+pow(1.0-z,2.0)*pow(lambda,2.0);
     const double Az = pzL*QQ/WW;
     const double ME =   (1.0/4.0)*Az/(1.0-lambda) ;
     return(ME);
     
}

double  BottomFusion::bg_hard_e()
{
     const double WW=z+(1.0-z)*lambda;
     const double QQ=1.0-(1.0-z)*(1.0-lambda);
     const double pzL = pow(z,2.0)+pow(1.0-z,2.0)*pow(lambda,2.0);
     const double LLZ = log(z/pow(1.0-z,2.0)/lambda/(1.0-lambda));
     const double Az = (-WW  +(lh+1.0)*pzL/WW)*QQ+LLZ*pzL*QQ/WW;
     const double ME =   (1.0/4.0)*Az/(1.0-lambda) ;
     return(ME);
}


//: NLO matrix elements for g b -> H +g. 
double  BottomFusion::gb_coll_pole()
{
     const double ps = pow(z,2.0)+pow(1.0-z,2.0);
     const double ME = -(1.0/4.0)*ps ;
     return(ME);
}

double  BottomFusion::gb_coll_fin()
{
     const double lambda_bar = 1.0-lambda;
     const double LZ = log(z/pow(1.0-z,2.0));
     const double ps = (pow(z,2.0)+pow(1.0-z,2.0));
     const double ME = (-1.0/4.0)* ( ps*(LZ+lh)+ps-1.0 + ps/(1.0-lambda_bar));
     return(ME);
}



double  BottomFusion::gb_coll_e()
{
     const double lambda_bar = 1.0-lambda;
     const double Lz = log(z/pow(1.0-z,2.0));
     const double LLZ = log(z/pow(1.0-z,2.0)/lambda_bar/(1.0-lambda_bar));
     const double ps = (pow(z,2.0)+pow(1.0-z,2.0));
     const double ME =  1.0/4.0 * (1.0-(lh+1.0+LLZ)*ps)/(1.0-lambda_bar)
     +1.0/4.0*(
               -(1.0/2.0)*ps*pow(Lz,2.0)
               +(-ps*lh-ps+1.0)*Lz
               -(1.0/2.0)*ps*pow(lh,2.0)
               +(-ps+1.0)*lh
               +(1.0/4.0)*consts::pi_square
               +(1.0/2.0*((1.0/2.0)*ps-1.0/2.0+z))*consts::pi_square
               -(1.0/2.0)*z*consts::pi_square
               -ps
               +1.0
               );
     return(ME);
}



double  BottomFusion::gb_hard_fin()
{
     const double lambda_bar = 1.0-lambda;
     const double WW=z+(1.0-z)*lambda_bar;
     const double QQ=1.0-(1.0-z)*(1.0-lambda_bar);
     const double pzL = pow(z,2.0)+pow(1.0-z,2.0)*pow(lambda_bar,2.0);
     const double Az = pzL*QQ/WW;
     const double ME =   (1.0/4.0)*Az/(1.0-lambda_bar) ;
     return(ME);
}

double  BottomFusion::gb_hard_e()
{
     const double lambda_bar = 1.0-lambda;
     const double WW=z+(1.0-z)*lambda_bar;
     const double QQ=1.0-(1.0-z)*(1.0-lambda_bar);
     const double pzL = pow(z,2.0)+pow(1.0-z,2.0)*pow(lambda_bar,2.0);
     const double LLZ = log(z/pow(1.0-z,2.0)/lambda_bar/(1.0-lambda_bar));
     const double Az = (-WW  +(lh+1.0)*pzL/WW)*QQ+LLZ*pzL*QQ/WW;
     const double ME =   (1.0/4.0)*Az/(1.0-lambda_bar) ;
     return(ME);
}







//: The "soft" part of the total cross section is below
//: "soft" here means that it corresponds to soft kinematics, i.e. the jet function is at z=1 (J(1))
//: and the matrix element is at z=1 as well ( f(1) ). This does not imply that it's a z-independent piece:
//: There is a z subtraction on the luminosity function and plus distributions that originate from the expansion
//: of  
//:     ( L(z)-L(1) )                                   f[-3]    f[-2]   f[-1]    
//:     ------------- * exp( - c * e * log(1-z) )  *  ( ----- + ------ + ------ + f[0] )  
//:         1-z                                           e^3     e^2      e
//: where c depends on the particular term of the cross section the contribution comes from (i.e. RR, RV, VV, V^2, etc.)
//:
//: note however that the delta pieces (those coming from the integral  of f(1)*J(1)*L(1)/(1-z)^(1+c*e) ) are 
//: still missing a tau dependent term, since the integration limits are from tau to 1 but the analytic integration
//: performed to get the delta terms assumes limits from 0 to 1
void BottomFusion::NNLO_SOFT()
{
     //: delta term prefactor
     const double delta_prefactor = pref_sbb
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *cur_lumiLO[0]*measLO;
     //: plus distribution term prefactors. Note that they are all multiplied with a soft jet function (J(z=1))
     const double plus0_prefactor = pref_sbb
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *(cur_lumi[0]*meas-cur_lumiLO[0]*measLO)/(1.0-z);
     const double plus1_prefactor = plus0_prefactor * log(1.0-z);
     const double plus2_prefactor = plus0_prefactor * pow(log(1.0-z),2.0);
     const double plus3_prefactor = plus0_prefactor * pow(log(1.0-z),3.0);
     double z3=consts::z3;
     double nf=consts::nf;
     const double pisquare=pow(consts::Pi,2.0);
     const double LH= -lh;
     if (pole==4)
          {
          }
     if (pole==3)
          {
          
          }
     if (pole==2)
          {
          const double delta = -(16.0/27.0)*pisquare-(1.0/6.0)*nf+19.0/4.0;
          const double plus0 = -(2.0/9.0)*nf+9.0;
          const double plus1 = 64.0/9.0;
          Jnlo(  delta*delta_prefactor 
               + plus0*plus0_prefactor 
               + plus1*plus1_prefactor,x1LO,x2LO,zLO,0.0);
          }
     if (pole==1)
          {
          const double delta = ((1.0/36.0+(1.0/27.0)*pisquare)*nf+(32.0/27.0)*LH*pisquare-(23.0/18.0)*pisquare+43.0/24.0-(125.0/9.0)*z3);
          const double plus0 = ((10.0/27.0)*nf+(41.0/27.0)*pisquare-(16.0/3.0)*LH-35.0/9.0);
          const double plus1 = (-32.0/3.0-(128.0/9.0)*LH);
          const double plus2 = -(64.0/3.0);
          Jnlo(  delta*delta_prefactor 
               + plus0*plus0_prefactor 
               + plus1*plus1_prefactor
               + plus2*plus2_prefactor,x1LO,x2LO,zLO,0.0);
          }
     if (pole==0)
          {
          const double delta = (-(5.0/81.0)*pisquare+(2.0/3.0)*z3+2.0/27.0)*nf
          -(32.0/27.0)*pow(LH,2.0)*pisquare
          +((4.0/9.0)*pisquare-17.0/3.0+(250.0/9.0)*z3)*LH
          +(29.0/27.0)*pisquare
          -(17.0/540.0)*pow(consts::Pi,4.0)
          -(26.0/3.0)*z3+211.0/18.0;
          
          const double plus0 =(56.0/81.0-(20.0/27.0)*LH+(2.0/9.0)*pow(LH,2.0)-(4.0/27.0)*pisquare)*nf
          -pow(LH,2.0)
          +(-(82.0/27.0)*pisquare+70.0/9.0)*LH
          +(10.0/9.0)*pisquare
          +(638.0/9.0)*z3
          -212.0/27.0;
          
          const double plus1 =((8.0/9.0)*LH-40.0/27.0)*nf+68.0/3.0+(128.0/9.0)*pow(LH,2.0)-4.0*LH-12.0*pisquare;
          
          const double plus2 = -4.0+(8.0/9.0)*nf+(128.0/3.0)*LH;
          
          const double plus3 =(896.0/27.0);
          Jnlo(  delta*delta_prefactor 
               + plus0*plus0_prefactor 
               + plus1*plus1_prefactor
               + plus2*plus2_prefactor
               + plus3*plus3_prefactor,x1LO,x2LO,zLO,0.0);
          }
     
}


void BottomFusion::NNLO_RR_epsilon_expansion(double epsilon_coeff,
                                               pointer_to_Franz my_func,
                                               int sector,double z,double lh,double wd,double L,
                                               double lambda1,double lambda2,double lambda3,double lambda4,double s,double x1,double x2,const string& number_of_II_gluons)
{
     //cout<<"\n["<<__func__<<"]"<<endl;
     double res;
     double c1,c2,c3;
     if (number_of_II_gluons=="no_gluons")
          {
          c1=(-epsilon_coeff*L);
          c2=(pow(epsilon_coeff,2.0)/2.0*L*L);
          c3=(-pow(epsilon_coeff,3.0)/6.0*L*L*L);
          }
     else if (number_of_II_gluons=="one_gluon")
          {
          c1=(1.0-epsilon_coeff*L);
          c2=(1.0-epsilon_coeff*L+pow(epsilon_coeff,2.0)/2.0*L*L);
          c3=(1.0-epsilon_coeff*L+pow(epsilon_coeff,2.0)/2.0*L*L-pow(epsilon_coeff,3.0)/6.0*L*L*L);
          }
     else if (number_of_II_gluons=="two_gluons")
          {
          c1=(2.0-epsilon_coeff*L);
          c2=(3.0-2.0*epsilon_coeff*L+pow(epsilon_coeff,2.0)/2.0*L*L);
          c3= -3.0*L*epsilon_coeff+pow(L,2.0)*pow(epsilon_coeff,2.0)+4.0-L*L*L*pow(epsilon_coeff,3.0)/6.0;
          }
     if (pole==0)
          {
          (*my_func)(sector,0,s,x1,x2,z, -lh, wd, lambda1, lambda2, lambda3, lambda4,res);
          (*my_func)(sector,-1,s,x1,x2,z, -lh, wd*c1, lambda1, lambda2, lambda3, lambda4,res);
          (*my_func)(sector,-2,s,x1,x2,z, -lh, wd*c2, lambda1, lambda2, lambda3, lambda4,res);
          (*my_func)(sector,-3,s,x1,x2,z, -lh, wd*c3, lambda1, lambda2, lambda3, lambda4,res);
          }
     if (pole==1)
          {
          (*my_func)(sector,-1,s,x1,x2,z, -lh, wd, lambda1, lambda2, lambda3, lambda4,res);
          (*my_func)(sector,-2,s,x1,x2,z, -lh, wd*c1, lambda1, lambda2, lambda3, lambda4,res);
          (*my_func)(sector,-3,s,x1,x2,z, -lh, wd*c2, lambda1, lambda2, lambda3, lambda4,res);
          }
     if (pole==2)
          {
          (*my_func)(sector,-2,s,x1,x2,z, -lh, wd, lambda1, lambda2, lambda3, lambda4,res);
          (*my_func)(sector,-3,s,x1,x2,z, -lh, wd*c1, lambda1, lambda2, lambda3, lambda4,res);
          }
     if (pole==3)
          {
          (*my_func)(sector,-3,s,x1,x2,z, -lh, wd, lambda1, lambda2, lambda3, lambda4,res);
          }
}




void BottomFusion::NNLO_RR_epsilon_expansion_nf(double epsilon_coeff,
                                                  void (*my_func)(const int&, const int&, const double&,const double&,const double&,
                                                                  const double&, const double&, const double&, const double&, const double&, const double&),
                                                  int sector,double z,double lh,double wd,double L,
                                                  double nf,double lambda1,double s,double x1,double x2,const string& number_of_II_gluons)
{
     //cout<<"\n["<<__func__<<"]"<<endl;
     double res;
     double c1,c2,c3;
     if (number_of_II_gluons=="no_gluons")
          {
          c1=(-epsilon_coeff*L);
          c2=(pow(epsilon_coeff,2.0)/2.0*L*L);
          c3=(-pow(epsilon_coeff,3.0)/6.0*L*L*L);
          }
     else if (number_of_II_gluons=="one_gluon")
          {
          c1=(1.0-epsilon_coeff*L);
          c2=(1.0-epsilon_coeff*L+pow(epsilon_coeff,2.0)/2.0*L*L);
          c3=(1.0-epsilon_coeff*L+pow(epsilon_coeff,2.0)/2.0*L*L-pow(epsilon_coeff,3.0)/6.0*L*L*L);
          }
     else if (number_of_II_gluons=="two_gluons")
          {
          c1=(2.0-epsilon_coeff*L);
          c2=(3.0-2.0*epsilon_coeff*L+pow(epsilon_coeff,2.0)/2.0*L*L);
          c3= -3.0*L*epsilon_coeff+pow(L,2.0)*pow(epsilon_coeff,2.0)+4.0-L*L*L*pow(epsilon_coeff,3.0)/6.0;
          }
     if (pole==0)
          {
          (*my_func)(sector,0,s,x1,x2,z, -lh, wd, nf,lambda1,res);
          (*my_func)(sector,-1,s,x1,x2,z, -lh, wd*c1,  nf,lambda1,res);
          (*my_func)(sector,-2,s,x1,x2,z, -lh, wd*c2,  nf,lambda1,res);
          (*my_func)(sector,-3,s,x1,x2,z, -lh, wd*c3,  nf,lambda1,res);
          }
     if (pole==1)
          {
          (*my_func)(sector,-1,s,x1,x2,z, -lh, wd,  nf,lambda1,res);
          (*my_func)(sector,-2,s,x1,x2,z, -lh, wd*c1,  nf,lambda1,res);
          (*my_func)(sector,-3,s,x1,x2,z, -lh, wd*c2,  nf,lambda1,res);
          }
     if (pole==2)
          {
          (*my_func)(sector,-2,s,x1,x2,z, -lh, wd,  nf,lambda1,res);
          (*my_func)(sector,-3,s,x1,x2,z, -lh, wd*c1,  nf,lambda1,res);
          }
     if (pole==3)
          {
          (*my_func)(sector,-3,s,x1,x2,z, -lh, wd,  nf,lambda1,res);
          }
}




void BottomFusion::NLO_conv()
{
     
     if (pole==1)
          {
          double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*alpha_s_vector[0]/consts::Pi;//: note that this term is of order a_s
          Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
          }
}

void BottomFusion::NNLO_conv_RRa()
{
     
     if (pole==2)
          {
          double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
          Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
          }
     else
          {book_production_event();}
}


void BottomFusion::NNLO_conv_RRb()
{
     if (pole==2)
          {
          double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
          Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
          }
     else
          {book_production_event();}
}

void BottomFusion::NNLO_conv_RRc()
{
     if (pole==1)
          {
          double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
          Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
          }
     else
          {book_production_event();}
}


void BottomFusion::NNLO_conv_RRd()//: F_g->b x F_bbar * sigma_bg with gluon from beam 1
{
     
     double sigma_central =	pref_sbb*meas*cur_lumi[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
     if (pole==2)
          {
          Jnlo(sigma_central*gb_coll_pole(),x1,x2,z,0.0);
          }
     else if (pole==1)
          {
          Jnlo(sigma_central*gb_coll_fin(),x1,x2,z,0.0);
          Jnlo(sigma_central*gb_hard_fin(),x1,x2,z,lambda);
          }
     else if (pole==0)
          {
          Jnlo(sigma_central*gb_coll_e(),x1,x2,z,0.0);
          Jnlo(sigma_central*gb_hard_e(),x1,x2,z,lambda);
          }
}

void BottomFusion::NNLO_conv_RRe()//: this is the same convolution as above but with gluon from beam 2
{
     
     double sigma_central =	pref_sbb*meas*cur_lumi[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
     if (pole==2)
          {
          Jnlo(sigma_central*bg_coll_pole(),x1,x2,z,1.0);
          }
     else if (pole==1)
          {
          Jnlo(sigma_central*bg_coll_fin(),x1,x2,z,1.0);
          Jnlo(sigma_central*bg_hard_fin(),x1,x2,z,lambda);
          }
     else if (pole==0)
          {
          Jnlo(sigma_central*bg_coll_e(),x1,x2,z,1.0);
          Jnlo(sigma_central*bg_hard_e(),x1,x2,z,lambda);
          }
}

void BottomFusion::NNLO_conv_bb2gH_soft()
{
     
     //: delta term prefactor
     const double delta_prefactor = pref_sbb
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *cur_lumiLO[0]*measLO;
     //: plus distribution term prefactors. Note that they are all multiplied with a soft jet function (J(z=1))
     const double plus0_prefactor = pref_sbb
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *(cur_lumi[0]*meas-cur_lumiLO[0]*measLO)/(1.0-z);
     const double plus1_prefactor = plus0_prefactor * log(1.0-z);
     const double plus2_prefactor = plus0_prefactor * pow(log(1.0-z),2.0);
     const double plus3_prefactor = plus0_prefactor * pow(log(1.0-z),3.0);
     if (pole==2)
          {
          Jnlo(  bb_soft_pole(delta_prefactor,plus0_prefactor),x1LO,x2LO,zLO,0.0);
          }
     if (pole==1)
          {
          Jnlo(  bb_soft_finite(delta_prefactor,plus0_prefactor,plus1_prefactor),x1LO,x2LO,zLO,0.0);
          }
     if (pole==0)
          {
          Jnlo(  bb_soft_e(delta_prefactor,plus0_prefactor,plus1_prefactor,plus2_prefactor),x1LO,x2LO,zLO,0.0);
          }
     
}


void BottomFusion::NNLO_conv_bb2gH_hard()
{
     lambda = xx_vegas[2]; 
     
     //:  prefactor for jet
     const double prefactor = pref_sbb
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *cur_lumi[0]*meas;
     if (pole==2)
          {
          Jnlo(  prefactor*bb_coll_pole(),x1,x2,z,1.0);
          Jnlo(  prefactor*bb_coll_pole(),x1,x2,z,0.0);
          Jnlo(  prefactor*bb_coll_soft_pole(),x1LO,x2LO,zLO,0.0);
          }
     if (pole==1)
          {
          Jnlo(  prefactor*bb_coll_fin(),x1,x2,z,1.0);
          Jnlo(  prefactor*bb_coll_fin(),x1,x2,z,0.0);
          Jnlo(  prefactor*bb_coll_soft_fin(),x1LO,x2LO,zLO,0.0);
          Jnlo(  prefactor*bb_hard_fin(),x1,x2,z,lambda);
          Jnlo(  -lambda* prefactor*bb_hard_fin(),x1,x2,z,1.0);
          Jnlo(  -(1.0-lambda)*prefactor*bb_hard_fin(),x1,x2,z,0.0);
          }
     if (pole==0)
          {
          Jnlo(  prefactor*bb_coll_e(),x1,x2,z,1.0);
          Jnlo(  prefactor*bb_coll_e(),x1,x2,z,0.0);
          Jnlo(  prefactor*bb_coll_soft_e(),x1LO,x2LO,zLO,0.0);
          Jnlo(  prefactor*bb_hard_e(),x1,x2,z,lambda);
          Jnlo(  -lambda* prefactor*bb_hard_e(),x1,x2,z,1.0);
          Jnlo(  -(1.0-lambda)*prefactor*bb_hard_e(),x1,x2,z,0.0);
          }
     
}


//:---------------------    bg channel

void BottomFusion::NNLO_conv_bb_nlo_soft()
{
     
     //: delta term prefactor
     const double delta_prefactor = pref_sbb
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *cur_lumiLO[0]*measLO;
     //: plus distribution term prefactors. Note that they are all multiplied with a soft jet function (J(z=1))
     const double plus0_prefactor = pref_sbb
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *(cur_lumi[0]*meas-cur_lumiLO[0]*measLO)/(1.0-z);
     const double plus1_prefactor = plus0_prefactor * log(1.0-z);
     const double plus2_prefactor = plus0_prefactor * pow(log(1.0-z),2.0);
     const double plus3_prefactor = plus0_prefactor * pow(log(1.0-z),3.0);
     if (pole==2)
          {
          Jnlo(  bb_soft_pole(delta_prefactor,plus0_prefactor),x1LO,x2LO,zLO,0.0);
          }
     if (pole==1)
          {
          Jnlo(  bb_soft_finite(delta_prefactor,plus0_prefactor,plus1_prefactor),x1LO,x2LO,zLO,0.0);
          }
     if (pole==0)
          {
          Jnlo(  bb_soft_e(delta_prefactor,plus0_prefactor,plus1_prefactor,plus2_prefactor),x1LO,x2LO,zLO,0.0);
          }
     
}


void BottomFusion::NNLO_conv_bb_nlo_hard()
{
     lambda = xx_vegas[2]; 
     
     //:  prefactor for jet
     const double prefactor = pref_sbb
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *cur_lumi[0]*meas;
     if (pole==2)
          {
          Jnlo(  prefactor*bb_coll_pole(),x1,x2,z,1.0);
          Jnlo(  prefactor*bb_coll_pole(),x1,x2,z,0.0);
          Jnlo(  prefactor*bb_coll_soft_pole(),x1LO,x2LO,zLO,0.0);
          }
     if (pole==1)
          {
          Jnlo(  prefactor*bb_coll_fin(),x1,x2,z,1.0);
          Jnlo(  prefactor*bb_coll_fin(),x1,x2,z,0.0);
          Jnlo(  prefactor*bb_coll_soft_fin(),x1LO,x2LO,zLO,0.0);
          Jnlo(  prefactor*bb_hard_fin(),x1,x2,z,lambda);
          Jnlo(  -lambda* prefactor*bb_hard_fin(),x1,x2,z,1.0);
          Jnlo(  -(1.0-lambda)*prefactor*bb_hard_fin(),x1,x2,z,0.0);
          }
     if (pole==0)
          {
          Jnlo(  prefactor*bb_coll_e(),x1,x2,z,1.0);
          Jnlo(  prefactor*bb_coll_e(),x1,x2,z,0.0);
          Jnlo(  prefactor*bb_coll_soft_e(),x1LO,x2LO,zLO,0.0);
          Jnlo(  prefactor*bb_hard_e(),x1,x2,z,lambda);
          Jnlo(  -lambda* prefactor*bb_hard_e(),x1,x2,z,1.0);
          Jnlo(  -(1.0-lambda)*prefactor*bb_hard_e(),x1,x2,z,0.0);
          }
     
}

void BottomFusion::NNLO_conv_bg()//: F_g->b x F_bbar * sigma_bg with gluon from beam 1
{
     
     double sigma_central =	pref_sbb*meas*cur_lumi[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
     if (pole==2)
          {
          Jnlo(sigma_central*gb_coll_pole(),x1,x2,z,0.0);
          }
     else if (pole==1)
          {
          Jnlo(sigma_central*gb_coll_fin(),x1,x2,z,0.0);
          Jnlo(sigma_central*gb_hard_fin(),x1,x2,z,lambda);
          }
     else if (pole==0)
          {
          Jnlo(sigma_central*gb_coll_e(),x1,x2,z,0.0);
          Jnlo(sigma_central*gb_hard_e(),x1,x2,z,lambda);
          }
}




void BottomFusion::NNLO_conv_gb()//: this is the same convolution as above but with gluon from beam 2
{
     
     double sigma_central =	pref_sbb*meas*cur_lumi[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
     if (pole==2)
          {
          Jnlo(sigma_central*bg_coll_pole(),x1,x2,z,1.0);
          }
     else if (pole==1)
          {
          Jnlo(sigma_central*bg_coll_fin(),x1,x2,z,1.0);
          Jnlo(sigma_central*bg_hard_fin(),x1,x2,z,lambda);
          }
     else if (pole==0)
          {
          Jnlo(sigma_central*bg_coll_e(),x1,x2,z,1.0);
          Jnlo(sigma_central*bg_hard_e(),x1,x2,z,lambda);
          }
}



void BottomFusion::NLO_HARD_1()//: F_b x F_g * sigma_bg  //: gluon from beam 2
{
     double sigma_central =	pref_sbb*meas*cur_lumi[0]*alpha_s_vector[0]/consts::Pi;
     if (pole==1)
          {
          Jnlo(sigma_central*bg_coll_pole(),x1,x2,z,1.0);
          }
     else if (pole==0)
          {
          Jnlo(sigma_central*bg_coll_fin(),x1,x2,z,1.0);
          Jnlo(sigma_central*bg_hard_fin(),x1,x2,z,lambda);
          }
}

void BottomFusion::NLO_HARD_2()//: F_b x F_g * sigma_bg  //: gluon from beam 1
{
     
     double sigma_central =	pref_sbb*meas*cur_lumi[0]*alpha_s_vector[0]/consts::Pi;
     if (pole==1)
          {
          Jnlo(sigma_central*gb_coll_pole(),x1,x2,z,0.0);
          }
     else if (pole==0)
          {
          Jnlo(sigma_central*gb_coll_fin(),x1,x2,z,0.0);
          Jnlo(sigma_central*gb_hard_fin(),x1,x2,z,lambda);
          }
}


void BottomFusion::NNLO_renorm_bg()//: F_b x F_g * sigma_bg  //: gluon from beam 2
{
     double sigma_central =	pref_sbb*meas*cur_lumi[0]
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     //*3.0/consts::Pi
     //*Nij
     *(-1.0/2.0)
     *(19.0/2.0-consts::nf/3.0);
     if (pole==2)
          {
          Jnlo(sigma_central*bg_coll_pole(),x1,x2,z,1.0);
          }
     else if (pole==1)
          {
          Jnlo(sigma_central*bg_coll_fin(),x1,x2,z,1.0);
          Jnlo(sigma_central*bg_hard_fin(),x1,x2,z,lambda);
          }
     else if (pole==0)
          {
          Jnlo(sigma_central*bg_coll_e(),x1,x2,z,1.0);
          Jnlo(sigma_central*bg_hard_e(),x1,x2,z,lambda);
          }
}

void BottomFusion::NNLO_renorm_gb()//: F_b x F_g * sigma_bg  //: gluon from beam 1
{
     
     double sigma_central =	pref_sbb*meas*cur_lumi[0]
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     //*3.0/consts::Pi
     //*Nij
     *(-1.0/2.0)
     *(19.0/2.0-consts::nf/3.0);
     if (pole==2)
          {
          Jnlo(sigma_central*gb_coll_pole(),x1,x2,z,0.0);
          }
     else if (pole==1)
          {
          Jnlo(sigma_central*gb_coll_fin(),x1,x2,z,0.0);
          Jnlo(sigma_central*gb_hard_fin(),x1,x2,z,lambda);
          }
     else if (pole==0)
          {
          Jnlo(sigma_central*gb_coll_e(),x1,x2,z,0.0);
          Jnlo(sigma_central*gb_hard_e(),x1,x2,z,lambda);
          }
}



void BottomFusion::NLO_conv_1()//: F_b x F_g->bar * sigma_bbar  //: gluon from beam 2
{
     double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*alpha_s_vector[0]/consts::Pi;
     if (pole==1)
          {
          Jnlo(sigma_central,x1LO,x2LO,1.0,1.0);
          }
     else if (pole==0)
          {
          
          }
}

void BottomFusion::NLO_conv_2()//: F_b x F_g->bar * sigma_bbar  //: gluon from beam 1
{
     
     double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*alpha_s_vector[0]/consts::Pi;
     if (pole==1)
          {
          Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
          }
     else if (pole==0)
          {
          
          }
}

void BottomFusion::NNLO_conv_bb_LO_e2()//: F_b x F_g->bar * sigma_bbar  //: gluon from beam 1
{
     
     double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]
     *pow(alpha_s_vector[0]/consts::Pi,2.0);
     if (pole==2)
          {
          Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
          }
     else if (pole==0)
          {
          
          }
}

void BottomFusion::NNLO_conv_bb_LO_e1()//: F_b x F_g->bar * sigma_bbar  //: gluon from beam 1
{
     
     double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
     if (pole==1)
          {
          Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
          }
}

void BottomFusion::NNLO_RV_bg()//: Real virtual with gluon at beam 2
{
     
     double wd=1.0// Note here: no 1/(1.0-z)
     *cur_lumi[0]
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *meas*pref_sbb
     *3.0*(4.0)/consts::Pi
     *Nij_bg;
     double res;
     for (int sector=1;sector<=3;sector++) // A1 has 2 sectors
          {
          if (pole==0)
               {
               RVbggH(sector,0,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVbggH(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVbggH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVbggH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==1)
               {
               RVbggH(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVbggH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVbggH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==2)
               {
               RVbggH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVbggH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==3)
               {
               RVbggH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==4)
               {
               book_production_event();
               }
          }
}

void BottomFusion::NNLO_RV_gb()//: Real virtual with gluon at beam 1
{
     
     double wd=1.0// Note here: no 1/(1.0-z)
     *cur_lumi[0]
     *pow(alpha_s_vector[0]/consts::Pi,2.0)
     *meas*pref_sbb
     *3.0*(4.0)/consts::Pi
     *Nij_bg;
     double res;
     for (int sector=1;sector<=3;sector++) // A1 has 2 sectors
          {
          if (pole==0)
               {
               RVgbgH(sector,0,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVgbgH(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVgbgH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVgbgH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==1)
               {
               RVgbgH(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVgbgH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVgbgH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==2)
               {
               RVgbgH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               RVgbgH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==3)
               {
               RVgbgH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
               }
          if (pole==4)
               {
               book_production_event();
               }
          }
}


////: Franz's functions communicate with the rest of the code via this fjet_ . Note that there is a global pointer passed around!
////: THIS IS NOW DEFINED IN PRODUCTION.CPP
//
//double fjet_(double* x1,double *x2,double *s12,double *s13,double *s23,double *s14,double *s24,double *s34,double *w)
//{
//     double ns13= -(*s13);
//     double ns14= -(*s14);
//     double ns23= -(*s23);
//     double ns24= -(*s24);
//     double ns34=(*s34);
//     double z=(*s12-ns13-ns14-ns23-ns24+ns34)/(*s12);
//     
//     ptr_to_BF->book_production_event(*w,
//                               *x1,
//                               *x2,
//                               z,
//                               ns13,ns23,ns14,ns24,ns34);
//     
//     //cout<<"\n*w="<<*w;
//     return 0;
//}
















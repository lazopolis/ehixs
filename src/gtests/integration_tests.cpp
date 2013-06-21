/** testing ehixs: 
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>
#include "Decay.h"
#include "Decay_bb.h"

#include "Production.h"
#include "BottomFusion.h"
#include "GluonFusionInclusive.h"
#include "GluonFusion.h"
#include "Process.h"
using namespace std;

#include "gtest/gtest.h"

/*

TEST(ProcessTest,DecayOnly)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=0.5;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=0.5;
     UI.perturbative_order=1;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=0;
     UI.pole=0;
     
     UI.epsrel=0.1; 
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=1000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Decay* my_decay=new Decay_bb();
     the_process.set_decay(my_decay);
     //: do everything
     the_process.perform();
     double xs=the_process.total_xs();
     double is_zero=abs(xs - 0.002140649295888349);
     EXPECT_LT(is_zero,1e-8);
     
}



TEST(ProcessTest,ProductionOnly)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=0.5;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=0.5;
     UI.perturbative_order=1;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=0;
     UI.pole=0;
     
     UI.epsrel=0.1; 
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=1000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_production=new BottomFusion();
     the_process.set_production(my_production);
     //: do everything
     the_process.perform();
     double xs=the_process.total_xs();
     double is_zero=abs(xs - 0.2765033726);
     EXPECT_LT(is_zero,1e-6);
     
}





TEST(ProcessTest,LOxLO)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=0.5;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=0.5;
     UI.perturbative_order=1;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=0;
     UI.pole=0;
     
     UI.epsrel=0.1; 
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=1000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_prod=new BottomFusion();
     Decay* my_decay=new Decay_bb();
     the_process.set_production(my_prod);
     the_process.set_decay(my_decay);
     //: do everything
     the_process.perform();
     double xs=the_process.total_xs();
     double is_zero=abs(xs - 0.0005918967496);
     EXPECT_LT(is_zero,1e-6);
     
}




TEST(ProcessTest,ggFInclusive)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=0.5;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=0.5;
     UI.perturbative_order=0;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=0;
     UI.pole=0;
     
     UI.epsrel=0.01; 
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=1000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_prod=new GluonFusionInclusive();
     the_process.set_production(my_prod);
     //: do everything
     the_process.perform();
     double xs=the_process.total_xs();
     double is_zero=abs(xs - 10.315);
     EXPECT_LT(is_zero,1e-3);
     
}


TEST(ProcessTest,ggF)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=0.5;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=0.5;
     UI.perturbative_order=0;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=0;
     UI.pole=0;
     
     UI.epsrel=0.01; 
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=1000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_prod=new GluonFusion();
     the_process.set_production(my_prod);
     //: do everything
     the_process.perform();
     double xs=the_process.total_xs();
     double is_zero=abs(xs - 10.315);
     EXPECT_LT(is_zero,1e-3);
     
}
 
  **/

/*
TEST(ProcessTest,ggF_x_bb_decay_sector_names)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=0.5;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=0.5;
     UI.perturbative_order=0;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=0;
     UI.decay_sector=0;
     UI.pole=0;
     
     UI.epsrel=0.01; 
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=1000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_prod=new GluonFusion();
     the_process.set_production(my_prod);
     Decay* my_decay=new Decay_bb();
     the_process.set_decay(my_decay);
     //: do everything
     //the_process.perform();
     //double xs=the_process.total_xs();
     //double is_zero=abs(xs - 10.315);
     string sector_information = the_process.sector_info();
     EXPECT_EQ(sector_information,"ggF LO :: h->bb LO");
     
}

TEST(ProcessTest,ggF_x_bb_decay_PT_b1)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=0.5;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=0.5;
     UI.perturbative_order=0;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=0;
     UI.decay_sector=0;
     UI.pole=0;
     
     UI.epsrel=0.001; 
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_prod=new GluonFusion();
     the_process.set_production(my_prod);
     Decay* my_decay=new Decay_bb();
     the_process.set_decay(my_decay);
     //: do everything
     the_process.perform();
     //double xs=the_process.total_xs();
     //double is_zero=abs(xs - 10.315);
     string sector_information = the_process.sector_info();
     EXPECT_EQ(sector_information,"ggF LO :: h->bb LO");
     
}
*/

/*
TEST(ProcessTest,ggF_ggNLO_hard)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=1.0;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=1.0;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=1;//:gg NLO hard
     UI.decay_sector=0;
     UI.pole=1;//: 1/e
     
     UI.epsrel=0.01;
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_prod=new GluonFusion();
     the_process.set_production(my_prod);
     //Decay* my_decay=new Decay_bb();
     //the_process.set_decay(my_decay);
     //: do everything
     the_process.perform();
     //double xs=the_process.total_xs();
     //double is_zero=abs(xs - 10.315);
     string sector_information = the_process.sector_info();
     EXPECT_EQ(sector_information,"production: gg NLO HARD | decay: none");
     
}
*/


/*
TEST(ProcessTest,ggF_ggNLO_conv_from_gluon)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=1.0;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=1.0;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=2;//:gg NLO conv at gg channel from gluon
     UI.decay_sector=0;
     UI.pole=1;//: 1/e
     
     UI.epsrel=0.01;
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_prod=new GluonFusion();
     the_process.set_production(my_prod);
     //Decay* my_decay=new Decay_bb();
     //the_process.set_decay(my_decay);
     //: do everything
     the_process.perform();
     //double xs=the_process.total_xs();
     //double is_zero=abs(xs - 10.315);
     string sector_information = the_process.sector_info();
     EXPECT_EQ(sector_information,"production: gg NLO CONV (fg_0 x fg_from_g_11 x sigma_gg_LO) | decay: none");
     
}
*/

/*
TEST(ProcessTest,ggF_ggNLO_conv_from_quark)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=1.0;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=1.0;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=3;//:gg NLO conv at gg channel from quark
     UI.decay_sector=0;
     UI.pole=1;//: 1/e
     
     UI.epsrel=0.01;
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_prod=new GluonFusion();
     the_process.set_production(my_prod);
     //Decay* my_decay=new Decay_bb();
     //the_process.set_decay(my_decay);
     //: do everything
     the_process.perform();
     //double xs=the_process.total_xs();
     //double is_zero=abs(xs - 10.315);
     string sector_information = the_process.sector_info();
     EXPECT_EQ(sector_information,"production: gg NLO CONV (fg_0 x fg_from_g_11 x sigma_gg_LO) | decay: none");
     
}


TEST(ProcessTest,ggF_qgNLO_hard)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=1.0;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=1.0;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     UI.sector_control=4;//:qg NLO hard
     UI.decay_sector=0;
     UI.pole=1;//: 1/e
     
     UI.epsrel=0.01;
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     UI.requested_histograms.push_back("higgs_rapidity");
     
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     Process the_process(UI);
     Production* my_prod=new GluonFusion();
     the_process.set_production(my_prod);
     //Decay* my_decay=new Decay_bb();
     //the_process.set_decay(my_decay);
     //: do everything
     the_process.perform();
     //double xs=the_process.total_xs();
     //double is_zero=abs(xs - 10.315);
     string sector_information = the_process.sector_info();
     EXPECT_EQ(sector_information,"production: qg NLO HARD | decay: none");
     
}
*/

TEST(ProcessTest,ggF_qgNLO_pole_cancelation)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=1.0;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=1.0;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     
     UI.decay_sector=0;
     UI.pole=1;//: 1/e
     
     UI.epsrel=0.01;
     UI.epsabs=0.0;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     UI.requested_histograms.push_back("higgs_rapidity");
     
     //UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     UI.sector_control=6;//:qg NLO hard
     
     Process qg_hard_NLO(UI);
     Production* my_prod=new GluonFusion();
     qg_hard_NLO.set_production(my_prod);
     qg_hard_NLO.perform();

     
     UI.sector_control=7;
     Process nlo_conv_g_from_quark(UI);
     nlo_conv_g_from_quark.set_production(my_prod);
     nlo_conv_g_from_quark.perform();
     
     
     CHistogram* H1=qg_hard_NLO.histogram_vector[0];
     CHistogram* H2=nlo_conv_g_from_quark.histogram_vector[0];
     cout<<compare_histograms(H1,H2,"+");
     
     
     string sector_information = qg_hard_NLO.sector_info();
     EXPECT_EQ(sector_information,"production: qg NLO HARD | decay: none");
     
}





int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}
































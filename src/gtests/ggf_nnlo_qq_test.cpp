/** testing ehixs:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>


#include "ggf_headers.h"

using namespace std;

#include "gtest/gtest.h"


TEST(ggF_nnlo_qq_test,one_over_e_squared)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-2;
     chs.push_back(channel_name("quark","quark","NNLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     cout<<"\n** xs="<<xs;
     double expected_xs = 0.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
}


TEST(ggF_nnlo_qq_test,one_over_e)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-1;
     chs.push_back(channel_name("quark","quark","NNLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     cout<<"\n** xs="<<xs;
     double expected_xs = 0.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
}


TEST(ggF_nnlo_qq_test,finite)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("quark","quark","NNLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     cout<<"\n** xs="<<xs;
     double expected_xs = 0.00310;
     EXPECT_LT(fabs(xs-expected_xs),err);
}

TEST(ggF_nnlo_qq_test,finite_weird_mu)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("quark","quark","NNLO",pole));
     double mur=0.125;
     double muf=0.56789;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     cout<<"\n** xs="<<xs;
     double expected_xs = 0.020438;
     EXPECT_LT(fabs(xs-expected_xs),err);
}

 

 /*
 TEST(ggF_nnlo_qq_test,one_over_e_squared)
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
 UI.pole=2;//: 1/e^2
 
 UI.epsrel=0.01;
 UI.epsabs=1e-14;
 UI.verbose=2;
 UI.mineval=100000;
 UI.maxeval=500000000;
 UI.nstart=10000;
 UI.nincrease=1000;
 
 UI.requested_histograms.push_back("higgs_rapidity");
 
 //UI.info=true;//: shows the sector numbers with descriptions
 //UI.histogram_info = true;
 
 //UI.cut_info = true;
 UI.sector_control=1;//:gg NLO hard
 UI.sector_name="qq RR t1";
 Process p1(UI);
 Production* my_prod=new GluonFusion();
 p1.set_production(my_prod);
 p1.perform();
 CHistogram* H1=p1.histogram_vector[0];
 
 UI.sector_name="qq RR t2";
 Process p2(UI);
 p2.set_production(my_prod);
 p2.perform();
 CHistogram* H2=p2.histogram_vector[0];
 
 UI.sector_name="qq RR t3";
 Process p6(UI);
 p6.set_production(my_prod);
 p6.perform();
 CHistogram* H6=p6.histogram_vector[0];
 
 UI.sector_name="qq conv  fq x f_g_from_q x sigma_NLO_qg";
 Process p3(UI);
 p3.set_production(my_prod);
 p3.perform();
 CHistogram* H3=p3.histogram_vector[0];
 
 UI.sector_name="qq conv  f_g_from_q1 x fq2 x sigma_NLO_gq";
 Process p4(UI);
 p4.set_production(my_prod);
 p4.perform();
 CHistogram* H4=p4.histogram_vector[0];
 
 UI.sector_name="qq conv  f_g_from_q1 x f_g_from_q1 x sigma_LO_gg";
 Process p5(UI);
 p5.set_production(my_prod);
 p5.perform();
 CHistogram* H5=p5.histogram_vector[0];
 
 vector<CHistogram*> all_hists;
 all_hists.push_back(H1);
 all_hists.push_back(H2);
 all_hists.push_back(H6);
 all_hists.push_back(H3);
 all_hists.push_back(H4);
 all_hists.push_back(H5);
  bool color_on =true;
 cout<<compare_histograms(all_hists,color_on);
 
 
 double xs = p1.total_xs()+p2.total_xs()+p6.total_xs()+p3.total_xs()+p4.total_xs()+p5.total_xs();
 double err = sqrt(
 pow(p1.total_err(),2.0)
 +pow(p2.total_err(),2.0)
 +pow(p3.total_err(),2.0)
 +pow(p4.total_err(),2.0)
 +pow(p5.total_err(),2.0)
 +pow(p6.total_err(),2.0)
 );
 double expected_xs = 0.0;
 EXPECT_LT(fabs(xs-expected_xs),err);
 
 }
  */
 /*
TEST(ggF_nnlo_qq_test,one_over_e)
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
     
     UI.epsrel=0.1;
     UI.epsabs=1e-14;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=20000;
     UI.nincrease=1000;
     
     UI.requested_histograms.push_back("higgs_rapidity");
     
     //UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     UI.sector_control=1;//:gg NLO hard
     UI.sector_name="qq RR t1";
     Process p1(UI);
     Production* my_prod=new GluonFusion();
     p1.set_production(my_prod);
     p1.perform();
     CHistogram* H1=p1.histogram_vector[0];
     
     UI.sector_name="qq RR t2";
     Process p2(UI);
     p2.set_production(my_prod);
     p2.perform();
     CHistogram* H2=p2.histogram_vector[0];
     
     UI.sector_name="qq RR t3";
     Process p6(UI);
     p6.set_production(my_prod);
     p6.perform();
     CHistogram* H6=p6.histogram_vector[0];
     
     UI.sector_name="qq conv  fq x f_g_from_q x sigma_NLO_qg";
     Process p3(UI);
     p3.set_production(my_prod);
     p3.perform();
     CHistogram* H3=p3.histogram_vector[0];
     
     UI.sector_name="qq conv  f_g_from_q1 x fq2 x sigma_NLO_gq";
     Process p4(UI);
     p4.set_production(my_prod);
     p4.perform();
     CHistogram* H4=p4.histogram_vector[0];
     
     UI.sector_name="qq conv  f_g_from_q1 x f_g_from_q1 x sigma_LO_gg";
     Process p5(UI);
     p5.set_production(my_prod);
     p5.perform();
     CHistogram* H5=p5.histogram_vector[0];
     
     vector<CHistogram*> all_hists;
     all_hists.push_back(H1);
     all_hists.push_back(H2);
     all_hists.push_back(H6);
     all_hists.push_back(H3);
     all_hists.push_back(H4);
     all_hists.push_back(H5);
  
     bool color_on=true;
     cout<<compare_histograms(all_hists,color_on);
     
     
     double xs = p1.total_xs()+p2.total_xs()+p6.total_xs()+p3.total_xs()+p4.total_xs()+p5.total_xs();
     double err = sqrt(
                       pow(p1.total_err(),2.0)
                       +pow(p2.total_err(),2.0)
                       +pow(p3.total_err(),2.0)
                       +pow(p4.total_err(),2.0)
                       +pow(p5.total_err(),2.0)
                       +pow(p6.total_err(),2.0)
                       );
     double expected_xs = 0.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
     
}

*/


/*
TEST(ggF_nnlo_qq_test,finite_weird_mu)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.muf_over_mhiggs=0.56789;
     UI.m_higgs=125.0;
     UI.mur_over_mhiggs=0.125;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     
     UI.decay_sector=0;
     UI.pole=0;//: fin
     
     UI.epsrel=0.05;
     UI.epsabs=1e-14;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=20000;
     UI.nincrease=1000;
     
     UI.requested_histograms.push_back("higgs_rapidity");
     
     //UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     UI.sector_control=1;//:gg NLO hard
     UI.sector_name="qq RR t1";
     Process p1(UI);
     Production* my_prod=new GluonFusion();
     p1.set_production(my_prod);
     p1.perform();
     CHistogram* H1=p1.histogram_vector[0];
     
     UI.sector_name="qq RR t2";
     Process p2(UI);
     p2.set_production(my_prod);
     p2.perform();
     CHistogram* H2=p2.histogram_vector[0];
     
     UI.sector_name="qq RR t3";
     Process p6(UI);
     p6.set_production(my_prod);
     p6.perform();
     CHistogram* H6=p6.histogram_vector[0];
     
     UI.sector_name="qq conv  fq x f_g_from_q x sigma_NLO_qg";
     Process p3(UI);
     p3.set_production(my_prod);
     p3.perform();
     CHistogram* H3=p3.histogram_vector[0];
     
     UI.sector_name="qq conv  f_g_from_q1 x fq2 x sigma_NLO_gq";
     Process p4(UI);
     p4.set_production(my_prod);
     p4.perform();
     CHistogram* H4=p4.histogram_vector[0];
     
     UI.sector_name="qq conv  f_g_from_q1 x f_g_from_q1 x sigma_LO_gg";
     Process p5(UI);
     p5.set_production(my_prod);
     p5.perform();
     CHistogram* H5=p5.histogram_vector[0];
     
     vector<CHistogram*> all_hists;
     all_hists.push_back(H1);
     all_hists.push_back(H2);
     all_hists.push_back(H6);
     all_hists.push_back(H3);
     all_hists.push_back(H4);
     all_hists.push_back(H5);
     cout<<compare_histograms(all_hists);
     
     
     double xs = p1.total_xs()+p2.total_xs()+p6.total_xs()+p3.total_xs()+p4.total_xs()+p5.total_xs();
     double err = sqrt(
                       pow(p1.total_err(),2.0)
                       +pow(p2.total_err(),2.0)
                       +pow(p3.total_err(),2.0)
                       +pow(p4.total_err(),2.0)
                       +pow(p5.total_err(),2.0)
                       +pow(p6.total_err(),2.0)
                       );
     double expected_xs = 0.020438;
     EXPECT_LT(fabs(xs-expected_xs),err);
     
}
*/

int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}
































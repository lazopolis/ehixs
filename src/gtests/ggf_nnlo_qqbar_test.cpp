/** testing ehixs:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>


#include "Production.h"
//#include "GluonFusionInclusive.h"
#include "GluonFusion.h"
#include "Process.h"
#include "test_utils.h"
using namespace std;

#include "gtest/gtest.h"



TEST(ggF_nnlo_qqbar_test,one_over_e_squared)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-2;
     chs.push_back(channel_name("quark","antiquark","NNLO",pole));
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


TEST(ggF_nnlo_qqbar_test,one_over_e)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-1;
     chs.push_back(channel_name("quark","antiquark","NNLO",pole));
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

TEST(ggF_nnlo_qqbar_test,finite)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("quark","antiquark","NNLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     cout<<"\n** xs="<<xs<<" +- "<<err;
     double expected_xs = 0.00552;
     EXPECT_LT(fabs(xs-expected_xs),err);
}

/*
TEST(ggF_nnlo_qqbar_test,finite_weird_mu)
{
     vector<channel_name> chs;
     double res[2];
     chs.push_back(channel_name("quark","antiquark","NNLO"));
     double mur=0.125;
     double muf=0.56789;
     int pole=0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     cout<<"\n** xs="<<xs<<"\tbut here we don;t know yet the ihixs value";
     double expected_xs = 0.020438;
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/



/*
 TEST(ggF_nnlo_qqbar_test,one_over_e_squared)
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
 
 UI.epsrel=0.005;
 UI.epsabs=1e-14;
 UI.verbose=2;
 UI.mineval=100000;
 UI.maxeval=500000000;
 UI.nstart=10000;
 UI.nincrease=1000;
 
 UI.requested_histograms.push_back("higgs_pT");
 
 //UI.info=true;//: shows the sector numbers with descriptions
 //UI.histogram_info = true;
 
 //UI.cut_info = true;

 vector<string> sector_names;
 sector_names.push_back("q qbar NNLO RV");
 sector_names.push_back("q qbar NNLO RENORM");
 sector_names.push_back("q qbar -> q qbar NNLO RR t1");
 sector_names.push_back("q qbar -> q qbar NNLO RR t2");
 sector_names.push_back("q qbar -> q qbar NNLO RR t3");
 sector_names.push_back("q qbar -> q qbar NNLO RR t4");
 sector_names.push_back("q qbar -> q qbar NNLO RR t5");
 sector_names.push_back("q qbar -> q qbar NNLO RR t6");

 sector_names.push_back("q qbar conv  fq x f_g_from_q x sigma_NLO_qg");
 sector_names.push_back("q qbar conv  f_g_from_q1 x fq2 x sigma_NLO_gq");
 sector_names.push_back("q qbar conv  f_g_from_q1 x f_g_from_q1 x sigma_LO_gg");
 sector_names.push_back("q qbar conv  f_q_from_q x fqbar x sigma_NLO_qqbar");

 
 
 sector_names.push_back("q qbar -> g g NNLO RR t1");
 sector_names.push_back("q qbar -> g g NNLO RR t2");
 sector_names.push_back("q qbar -> g g NNLO RR t3");
 sector_names.push_back("q qbar -> g g NNLO RR t4");
 sector_names.push_back("q qbar -> g g NNLO RR t5");
 sector_names.push_back("q qbar -> g g NNLO RR t6");

 sector_names.push_back("q qbar -> q' qbar' NNLO RR t1");
 sector_names.push_back("q qbar -> q' qbar' NNLO RR t2");


 
 
 vector<Process* > procs;
 vector<CHistogram*> all_hists;
 vector<double> sec_xs;
 vector<double> sec_err;
 
 double xs=0.0;
 double err=0.0;
 for (int i=0;i<sector_names.size();i++)
      {
      UI.sector_name=sector_names[i];
      Process* cur_process = new Process(UI);
      cur_process->set_production(new GluonFusion());
      cur_process->perform();
      procs.push_back(cur_process);
      all_hists.push_back(cur_process->histogram_vector[0]);
      xs += cur_process->total_xs();
      err += pow(cur_process->total_err(),2.0);
      
      }
 err = sqrt(err);
 
 bool color_on =true;
 cout<<compare_histograms(all_hists,color_on);
 double expected_xs = 0.0;
 EXPECT_LT(fabs(xs-expected_xs),err);
 
 }



TEST(ggF_nnlo_qqbar_test,one_over_e)
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
     UI.epsabs=5e-7;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=30000;
     UI.nincrease=1000;
     
//     UI.requested_histograms.push_back("vegas x[0]");
//     UI.requested_histograms.push_back("vegas x[1]");
//     UI.requested_histograms.push_back("vegas x[2]");
//     UI.requested_histograms.push_back("vegas x[3]");
//     UI.requested_histograms.push_back("vegas x[4]");
//     UI.requested_histograms.push_back("vegas x[5]");
     
     UI.requested_histograms.push_back("higgs_rapidity");

     //UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     vector<string> sector_names;
     sector_names.push_back("q qbar NNLO RV");
     sector_names.push_back("q qbar NNLO RENORM");
     sector_names.push_back("q qbar -> q qbar h NNLO RR t1");
     sector_names.push_back("q qbar -> q qbar h NNLO RR t2");
     sector_names.push_back("q qbar -> q qbar h NNLO RR t3");
     sector_names.push_back("q qbar -> q qbar h NNLO RR t4");
     sector_names.push_back("q qbar -> q qbar h NNLO RR t5");
     sector_names.push_back("q qbar -> q qbar h NNLO RR t6");

     sector_names.push_back("q qbar conv  fq x f_g_from_q x sigma_NLO_qg");
     sector_names.push_back("q qbar conv  f_g_from_q1 x fq2 x sigma_NLO_gq");
     sector_names.push_back("q qbar conv  f_g_from_q1 x f_g_from_q1 x sigma_LO_gg");
     sector_names.push_back("q qbar conv  f_q_from_q x fqbar x sigma_NLO_qqbar");

     
     
     sector_names.push_back("q qbar -> g g h NNLO RR t1");
     sector_names.push_back("q qbar -> g g h NNLO RR t2");
     sector_names.push_back("q qbar -> g g h NNLO RR t3");
     sector_names.push_back("q qbar -> g g h NNLO RR t4");
     sector_names.push_back("q qbar -> g g h NNLO RR t5");
     sector_names.push_back("q qbar -> g g h NNLO RR t6");

     sector_names.push_back("q qbar -> q' qbar' NNLO RR t1");
     sector_names.push_back("q qbar -> q' qbar' NNLO RR t2");
     
     
     
     
     vector<Process* > procs;
     vector<CHistogram*> all_hists;
     vector<double> sec_xs;
     vector<double> sec_err;
     
     double xs=0.0;
     double err=0.0;
     for (int i=0;i<sector_names.size();i++)
          {
          UI.sector_name=sector_names[i];
          Process* cur_process = new Process(UI);
          cur_process->set_production(new GluonFusion());
          cur_process->perform();
          procs.push_back(cur_process);
          all_hists.push_back(cur_process->histogram_vector[0]);
          xs += cur_process->total_xs();
          err += pow(cur_process->total_err(),2.0);
          
          }
     err = sqrt(err);
     
     bool color_on =true;
     cout<<compare_histograms(all_hists,color_on);
     double expected_xs = 0.0;
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
































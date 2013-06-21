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

/*
TEST(q1q2,one_over_e_squared)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-2;
     chs.push_back(channel_name("quark","quark2","NNLO",pole));
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
 */

/*
TEST(q1q2,one_over_e)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-1;
     chs.push_back(channel_name("quark","quark2","NNLO",pole));
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
 */

TEST(q1q2,finite)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("quark","quark2","NNLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     cout<<"\n** xs="<<xs<<" +- "<<err;
     double expected_xs = 0.01074;
     EXPECT_LT(fabs(xs-expected_xs),err);
}

/*
TEST(ggF_NNLO_q1q2_pole_cancelation,one_over_e_squared)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.m_higgs=125.0;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     
     UI.decay_sector=0;
     
     UI.muf_over_mhiggs=1.0;
     UI.mur_over_mhiggs=1.0;
     
     UI.epsrel=0.005;
     UI.epsabs=1e-10;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     UI.requested_histograms.push_back("higgs_rapidity");
     
     //UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     
     //: finding sectors
     //UI.info=true;
     Process* dummyprocess = new Process(UI);
     dummyprocess->set_production(new GluonFusion());
     vector<string> sector_names=dummyprocess->give_sector_names("quark","quark2","NNLO");
//     vector<string> additional_names=dummyprocess->give_sector_names("gluon","quark","NLO");
//     sector_names.insert(sector_names.end(),additional_names.begin(),additional_names.end());
     delete dummyprocess;
     UI.info=false;
     
     UI.pole=2;
     
     for (int i=0;i<sector_names.size();i++) cout<<"\nsectors: "<<sector_names[i];
     
     
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
          if (UI.requested_histograms.size()>0)
               {
               all_hists.push_back(cur_process->histogram_vector[0]);
               }
          xs += cur_process->total_xs();
          err += pow(cur_process->total_err(),2.0);
          
          }
     err = sqrt(err);
     
     if (UI.requested_histograms.size()>0)
          {
          bool color_on =true;
          cout<<compare_histograms(all_hists,color_on);
          }

     double expected_xs = 0.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
     
}
*/
/*
TEST(ggF_NNLO_q1q2_pole_cancelation,one_over_e)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.m_higgs=125.0;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     
     UI.decay_sector=0;
     
     UI.muf_over_mhiggs=1.0;
     UI.mur_over_mhiggs=1.0;
     
     UI.epsrel=0.005;
     UI.epsabs=1e-10;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     UI.requested_histograms.push_back("higgs_rapidity");
     
     //UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     
     //: finding sectors
     //UI.info=true;
     Process* dummyprocess = new Process(UI);
     dummyprocess->set_production(new GluonFusion());
     vector<string> sector_names=dummyprocess->give_sector_names("quark","quark2","NNLO");
     //     vector<string> additional_names=dummyprocess->give_sector_names("gluon","quark","NLO");
     //     sector_names.insert(sector_names.end(),additional_names.begin(),additional_names.end());
     delete dummyprocess;
     UI.info=false;
     
     UI.pole=1;
     
     for (int i=0;i<sector_names.size();i++) cout<<"\nsectors: "<<sector_names[i];
     
     
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
          if (UI.requested_histograms.size()>0)
               {
               all_hists.push_back(cur_process->histogram_vector[0]);
               }
          xs += cur_process->total_xs();
          err += pow(cur_process->total_err(),2.0);
          
          }
     err = sqrt(err);
     
     if (UI.requested_histograms.size()>0)
          {
          bool color_on =true;
          cout<<compare_histograms(all_hists,color_on);
          }
     
     double expected_xs = 0.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
     
}
*/
/*
TEST(ggF_NNLO_q1q2_pole_cancelation,finite)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.m_higgs=125.0;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     
     UI.decay_sector=0;
     
     UI.muf_over_mhiggs=1.0;
     UI.mur_over_mhiggs=1.0;
     
     UI.epsrel=0.005;
     UI.epsabs=1e-10;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     UI.requested_histograms.push_back("higgs_rapidity");
     
     //UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     
     //: finding sectors
     //UI.info=true;
     Process* dummyprocess = new Process(UI);
     dummyprocess->set_production(new GluonFusion());
     vector<string> sector_names=dummyprocess->give_sector_names("quark","quark2","NNLO");
     //     vector<string> additional_names=dummyprocess->give_sector_names("gluon","quark","NLO");
     //     sector_names.insert(sector_names.end(),additional_names.begin(),additional_names.end());
     delete dummyprocess;
     UI.info=false;
     
     UI.pole=0;
     
     for (int i=0;i<sector_names.size();i++) cout<<"\nsectors: "<<sector_names[i];
     
     
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
          if (UI.requested_histograms.size()>0)
               {
               all_hists.push_back(cur_process->histogram_vector[0]);
               }
          xs += cur_process->total_xs();
          err += pow(cur_process->total_err(),2.0);
          
          }
     err = sqrt(err);
     
     if (UI.requested_histograms.size()>0)
          {
          bool color_on =true;
          cout<<compare_histograms(all_hists,color_on);
          }
     

     double expected_xs = 0.010795;
     EXPECT_LT(fabs(xs-expected_xs),err);
     
}
*/
/*
TEST(ggF_NNLO_q1q2_pole_cancelation,finite_weird_mu)
{
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.m_higgs=125.0;
     UI.perturbative_order=2;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     
     UI.decay_sector=0;
     
     UI.muf_over_mhiggs=0.56789;
     UI.mur_over_mhiggs=0.125;
     
     UI.epsrel=0.005;
     UI.epsabs=1e-10;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     UI.requested_histograms.push_back("higgs_rapidity");
     
     //UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     
     //: finding sectors
     //UI.info=true;
     Process* dummyprocess = new Process(UI);
     dummyprocess->set_production(new GluonFusion());
     vector<string> sector_names=dummyprocess->give_sector_names("quark","quark2","NNLO");
     //     vector<string> additional_names=dummyprocess->give_sector_names("gluon","quark","NLO");
     //     sector_names.insert(sector_names.end(),additional_names.begin(),additional_names.end());
     delete dummyprocess;
     UI.info=false;
     
     UI.pole=0;
     
     for (int i=0;i<sector_names.size();i++) cout<<"\nsectors: "<<sector_names[i];
     
     
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
          if (UI.requested_histograms.size()>0)
               {
               all_hists.push_back(cur_process->histogram_vector[0]);
               }
          xs += cur_process->total_xs();
          err += pow(cur_process->total_err(),2.0);
          
          }
     err = sqrt(err);
     
     if (UI.requested_histograms.size()>0)
          {
          bool color_on =true;
          cout<<compare_histograms(all_hists,color_on);
          }
//     
//     
//     UserInterface UI;
//     UI.number_of_flavours=5;
//     UI.muf_over_mhiggs=0.56789;
//     UI.m_higgs=125.0;
//     UI.mur_over_mhiggs=0.125;
//     UI.perturbative_order=2;
//     UI.pdf_provider="MSTW";
//     UI.pdf_error=0;
//     UI.Etot = 8000.0;
//     
//     UI.decay_sector=0;
//     UI.pole=0;//: Finite
//     
//     UI.epsrel=0.005;
//     UI.epsabs=0.0;
//     UI.verbose=2;
//     UI.mineval=100000;
//     UI.maxeval=500000000;
//     UI.nstart=10000;
//     UI.nincrease=1000;
//     
//     UI.requested_histograms.push_back("higgs_rapidity");
//     
//     //UI.info=true;//: shows the sector numbers with descriptions
//     //UI.histogram_info = true;
//     
//     //UI.cut_info = true;
//     UI.sector_control=1;//:gg NLO hard
//     UI.sector_name="q1q2 RR t1";
//     Process p1(UI);
//     Production* my_prod=new GluonFusion();
//     p1.set_production(my_prod);
//     p1.perform();
//     CHistogram* H1=p1.histogram_vector[0];
//     
//     UI.sector_name="q1q2 RR t2";
//     Process p2(UI);
//     p2.set_production(my_prod);
//     p2.perform();
//     CHistogram* H2=p2.histogram_vector[0];
//     
//     UI.sector_name="q1q2 conv x fq1 f_g_from_q2 x sigma_NLO_qg";
//     Process p3(UI);
//     p3.set_production(my_prod);
//     p3.perform();
//     CHistogram* H3=p3.histogram_vector[0];
//     
//     UI.sector_name="q1q2 conv  f_g_from_q1 x fq2 x sigma_NLO_gq";
//     Process p4(UI);
//     p4.set_production(my_prod);
//     p4.perform();
//     CHistogram* H4=p4.histogram_vector[0];
//     
//     UI.sector_name="q1q2 conv  f_g_from_q1 x f_g_from_q1 x sigma_LO_gg";
//     Process p5(UI);
//     p5.set_production(my_prod);
//     p5.perform();
//     CHistogram* H5=p5.histogram_vector[0];
//     
//     vector<CHistogram*> all_hists;
//     all_hists.push_back(H1);
//     all_hists.push_back(H2);
//     all_hists.push_back(H3);
//     all_hists.push_back(H4);
//     all_hists.push_back(H5);
//     cout<<compare_histograms(all_hists);
//     
//     
//     double xs = p1.total_xs()+p2.total_xs()+p3.total_xs()+p4.total_xs()+p5.total_xs();
//     double err = sqrt(
//                       pow(p1.total_err(),2.0)
//                       +pow(p2.total_err(),2.0)
//                       +pow(p3.total_err(),2.0)
//                       +pow(p4.total_err(),2.0)
//                       +pow(p5.total_err(),2.0)
//                       );
     double expected_xs = 0.048477;
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
































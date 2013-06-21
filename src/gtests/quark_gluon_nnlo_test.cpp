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
using namespace std;

#include "gtest/gtest.h"



/*
TEST(nnlo_quark_gluon_test,one_over_e_cubed)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-3;
     chs.push_back(channel_name("quark","gluon","NNLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs = 0.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/

/*
 TEST(nnlo_quark_gluon_test,one_over_e_cube)
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
 UI.pole=3;//: 1/e^3
 
 UI.epsrel=0.005;
 UI.epsabs=1e-6;
 UI.verbose=2;
 UI.mineval=100000;
 UI.maxeval=500000000;
 UI.nstart=10000;
 UI.nincrease=1000;
 
 //UI.requested_histograms.push_back("higgs_pT");
 
// UI.info=true;//: shows the sector numbers with descriptions
 //UI.histogram_info = true;
 
 //UI.cut_info = true;


 vector<string> sector_names;
 sector_names.push_back("quark gluon NNLO RV");
// sector_names.push_back("gluon quark NNLO RV");
 string generic_rr_name="quark gluon -> quark gluon h NNLO RR t";
 for (unsigned i=1;i<16;i++)
      {
      stringstream loc_name;loc_name<<generic_rr_name<<i;
      sector_names.push_back(loc_name.str());
      }
 
 
// UI.requested_histograms.push_back("higgs_pT");
 UI.requested_histograms.push_back("higgs_rapidity");

 
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
 if (cur_process->histogram_vector.size()>0)
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
 

TEST(nnlo_gluon_quark_test,one_over_e_cube)
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
     UI.pole=3;//: 1/e^3
     
     UI.epsrel=0.005;
     UI.epsabs=1e-6;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     //UI.requested_histograms.push_back("higgs_pT");
     
     // UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     
     vector<string> sector_names;
     sector_names.push_back("gluon quark NNLO RV");
     string generic_rr_name="gluon quark -> quark gluon h NNLO RR t";
     for (unsigned i=1;i<16;i++)
          {
          stringstream loc_name;loc_name<<generic_rr_name<<i;
          sector_names.push_back(loc_name.str());
          }
     
     
     // UI.requested_histograms.push_back("higgs_pT");
     UI.requested_histograms.push_back("higgs_rapidity");
     
     
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
          if (cur_process->histogram_vector.size()>0)
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

/*
TEST(nnlo_quark_gluon_test,one_over_e_squared)
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
     
     UI.epsrel=0.03;
     UI.epsabs=1e-6;
     UI.verbose=2;
     UI.mineval=100000;
     UI.maxeval=500000000;
     UI.nstart=10000;
     UI.nincrease=1000;
     
     //UI.requested_histograms.push_back("higgs_pT");
     
      UI.info=true;//: shows the sector numbers with descriptions
     //UI.histogram_info = true;
     
     //UI.cut_info = true;
     
     
     vector<string> sector_names;
     sector_names.push_back("quark gluon NNLO RV");
     string generic_rr_name="quark gluon -> quark gluon h NNLO RR t";
     for (unsigned i=1;i<16;i++)
          {
          stringstream loc_name;loc_name<<generic_rr_name<<i;
          sector_names.push_back(loc_name.str());
          }
     
     
     // UI.requested_histograms.push_back("higgs_pT");
     UI.requested_histograms.push_back("higgs_rapidity");
     
     
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
          if (cur_process->histogram_vector.size()>0)
               all_hists.push_back(cur_process->histogram_vector[0]);
          xs += cur_process->total_xs();
          err += pow(cur_process->total_err(),2.0);
          sec_xs.push_back(cur_process->total_xs());
          }
     err = sqrt(err);
     
     bool color_on =true;
     cout<<compare_histograms(all_hists,color_on);
     for (int i=0;i<sector_names.size();i++)
          {
          cout<<"\n"<<i<<" "<<sector_names[i]<<" : "<<sec_xs[i];
          }

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
































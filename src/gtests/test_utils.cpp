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
//#include <pthread.h>
using namespace std;

//#define NUM_THREADS     5


void  run_sector(void * args);



vector<string> give_sectors(vector<channel_name> channels)
{
     UserInterface UI;
     UI.decay_sector=0;
     Process* dummyprocess = new Process(UI);
     dummyprocess->set_production(new GluonFusion());
     vector<string> sector_names;
     for (unsigned i=0;i<channels.size();i++)
          {
          vector<string> curnames=dummyprocess->give_sector_names(channels[i].p1,channels[i].p2,channels[i].pord,channels[i].e_ord,channels[i].me_approx);
          sector_names.insert(sector_names.end(),curnames.begin(),curnames.end());
          }
     delete dummyprocess;
     return sector_names;
}


MultiThreadArgumentKeeper::MultiThreadArgumentKeeper(int number_of_sectors)
{
     for (int i=0;i<number_of_sectors;i++)
          {
          xs.push_back(0.0);
          err.push_back(0.0);
          all_hists.push_back(NULL);
          }
}


void proceed_to_check(const vector<string>& sector_names, double mur,double muf,int pole,int pertord,double* res,const string & _me_approx)
{
     for (int i=0;i<sector_names.size();i++) cout<<"\nsectors: "<<sector_names[i];
     
     //     vector<Process* > procs;
     //     vector<CHistogram*> all_hists;
     //     vector<double> sec_xs;
     //     vector<double> sec_err;
     MultiThreadArgumentKeeper all_data(sector_names.size());
     MultiThreadArgumentKeeper* the_keeper = &all_data;
     the_keeper->muf=muf;
     the_keeper->mur=mur;
     the_keeper->pole=pole;
     the_keeper->pertord=pertord;
     the_keeper->me_approx = _me_approx;
     //: ---- sequential sector running
     for (int i=0;i<sector_names.size();i++)
          {
          //if (i!=44) continue;
          SingleThreadId cur_id_obj;
          cur_id_obj.the_keeper=the_keeper;
          cur_id_obj.ID=i;
          cur_id_obj.current_sector_name=sector_names[i];
          SingleThreadId* cur_id=&cur_id_obj;
          run_sector(cur_id);
          }
     //  //: ----- multithreading
     //     pthread_t threads[NUM_THREADS];
     //     int rc;
     //     int i;
     //     for( i=0; i < NUM_THREADS; i++ )
     //          {
     //          SingleThreadId cur_id_obj;
     //          cur_id_obj.the_keeper=the_keeper;
     //          cur_id_obj.ID=i;
     //          cur_id_obj.current_sector_name=sector_names[i];
     //
     //          SingleThreadId* cur_id=&cur_id_obj;
     //
     //
     //          rc = pthread_create(&threads[i], NULL,
     //                              run_sector, cur_id);
     //          if (rc)
     //               {
     //               cout << "Error:unable to create thread," << rc << endl;
     //               exit(-1);
     //               }
     //          }
     //     pthread_exit(NULL);
     
     //: after every sector has finished
     
     if (the_keeper->all_hists.size()>0)
          {
          bool color_on =true;
          cout<<compare_histograms(the_keeper->all_hists,color_on);
          }
     res[0]=0.0;
     res[1]=0.0;
     for (int i=0;i<sector_names.size();i++)
          {
          res[0] += the_keeper->xs[i];
          res[1] += the_keeper->err[i];
          cout<<"\n"<<i+1<<" : "<<the_keeper->xs[i]<<" +- "<<sqrt(the_keeper->err[i])<<"\t"<<sector_names[i];
          }
     res[1] = sqrt(res[1]);
     cout<<"\n********\tTotal for Franz: "<<res[0]<<" +- "<<res[1];
     //     EXPECT_LT(fabs(xs-expected_xs),err);
}

void check_sectors(vector<channel_name> channels,double mur,double muf,int pole,int pertord,double*res,const vector<int> & specific_sector_numbers)
{
     vector<string> sector_names=give_sectors(channels);
     vector<string> restricted_sector_names;
     for (int i=0;i<specific_sector_numbers.size();i++)
          {
          restricted_sector_names.push_back(sector_names[specific_sector_numbers[i]]);
          }
     proceed_to_check(restricted_sector_names,mur,muf,pole,pertord,res,"effective");
}



void check_sectors(vector<channel_name> channels,double mur,double muf,int pole,int pertord,double*res)
{
     vector<string> sector_names=give_sectors(channels);
     proceed_to_check(sector_names,mur,muf,pole,pertord,res,"effective");
     
     
     
}

void check_sectors(vector<channel_name> channels,double mur,double muf,int pole,int pertord,double*res,const string & _me_approx)
{
     vector<string> sector_names=give_sectors(channels);
     proceed_to_check(sector_names,mur,muf,pole,pertord,res,_me_approx);
     
     
     
}
void check_sectors(vector<channel_name> channels,double mur,double muf,int pole,int pertord,double*res,const string & _me_approx,const vector<int> & specific_sector_numbers)
{
     vector<string> sector_names=give_sectors(channels);
     vector<string> restricted_sector_names;
     for (int i=0;i<specific_sector_numbers.size();i++)
          {
          restricted_sector_names.push_back(sector_names[specific_sector_numbers[i]]);
          }
     proceed_to_check(restricted_sector_names,mur,muf,pole,pertord,res,_me_approx);
}



void run_sector(void * args)
{
     struct SingleThreadId *mydata;
     mydata = (struct SingleThreadId*) args;
     UserInterface UI;
     UI.number_of_flavours=5;
     UI.m_higgs=125.0;
     UI.perturbative_order=mydata->the_keeper->pertord;
     UI.pdf_provider="MSTW";
     UI.pdf_error=0;
     UI.Etot = 8000.0;
     
     UI.decay_sector=0;
     
     UI.muf_over_mhiggs=mydata->the_keeper->muf;
     UI.mur_over_mhiggs=mydata->the_keeper->mur;
     UI.matrix_element_approximation = mydata->the_keeper->me_approx;
     
     UI.epsrel=1e-3;
     UI.epsabs=1e-2;
     UI.verbose=2;
     UI.mineval=200000;
     UI.maxeval=1000000;
     UI.nstart=50000;
     UI.nincrease=1000;
     UI.info=false;
     
     UI.pole=mydata->the_keeper->pole;
     UI.sector_name=mydata->current_sector_name;
//     UI.requested_histograms.push_back("higgs_rapidity");
     UI.requested_histograms.push_back("higgs_pT");
   //  UI.requested_histograms.push_back("vegas x[1]");

     
     //UI.requested_cuts.push_back("pt_cut_pH");
     //UI.requested_cuts.push_back("pt_bin_pH");
     //UI.requested_cuts.push_back("pt_zero_bin_pH");

     Process* cur_process = new Process(UI);
     cur_process->set_production(new GluonFusion());
     cur_process->perform();
     
     int id = mydata->ID;
     if (UI.requested_histograms.size()>0)
          {
          mydata->the_keeper->all_hists[id]=cur_process->histogram_vector[0];
          }
     mydata->the_keeper->xs[id]=cur_process->total_xs();
     mydata->the_keeper->err[id]= pow(cur_process->total_err(),2.0);

 //    pthread_exit(NULL);
}























/** \file ehixs.cpp
 *
 * Main/Entry of the program. 
 */



#include <stdlib.h> 
#include <string.h>
#include <fstream>
#include<vector>
#include <iostream>
#include <iomanip>
#include <map>
#include <math.h>
#include "inclusive_process.h"
#include "timekeeper.h"

#include "user_interface.h"
#include "ehixs_config.h"
using namespace std;





void print_logo()
{
    int vmajor = 0;
    int vminor = 0;
#ifdef EHIXS_VERSION_MAJOR
    vmajor = EHIXS_VERSION_MAJOR;
    vminor = EHIXS_VERSION_MINOR;
#endif
    cout<<"\n* * * * * * * * * * * * * * * * * * * * * * * * *";
    cout<<"\n*                                               *";
    cout<<"\n*                                               *";
    cout<<"\n*                                               *";
    cout<<"\n*                ihixs                          *";
    cout<<"\n*                                               *";
    cout<<"\n*                version "<<vmajor<<"."<<vminor<<"                    *";
    cout<<"\n*                                               *";
    cout<<"\n*                                               *";
    cout<<"\n* * * * * * * * * * * * * * * * * * * * * * * * *";
    cout<<"\n\n";
}

void single_scale_running(const UserInterface& UI);
void full_multiscale_running(UserInterface& UI);
void restricted_multiscale_running(UserInterface& UI);
void compute_plain_lumi(UserInterface& UI, const vector<double>& scales);
void compute_delta_sigma_multiscale(UserInterface& UI);



int main(int argc, char** argv)
{
    // get init time
    //clock_t t_init = clock();
    TimeKeeper myclock;
    myclock.StartMeasurement();
    
    print_logo();
    // UI initialization
    UserInterface UI;
    
    
    try
    {
        UI.ParseInput(argc,argv);
        UI.RunSanityChecks();
        // Prints processes list: functionality that we lack currently: how to
        if(UI.list_processes)
        {
            //ProcessList();
            cout<<"\nggF\t:Higgs production via gluon fusion"<<endl;
            exit(0);
        }
        else{
            restricted_multiscale_running(UI);
            //full_multiscale_running(UI);
            //single_scale_running(UI);
            //compute_delta_sigma_multiscale(UI);
        }
        
    }
    catch(const char* s)
    {
        cerr << endl << argv[0] << ": " << s << endl;
    }
    catch(const string s)
    {
        cerr << endl << argv[0] << ": " << s << endl;
    }
    catch(...)
    {
        cerr<<endl<<"Something went wrong but the exception thrown was not a string"<<endl;
    }
    cout << "Total running time  = " << myclock.GiveMeasurement()
    //float(clock()-t_init)/CLOCKS_PER_SEC
    << " s" << endl;
    return 0;
}

void single_scale_running(const UserInterface& UI)
{
    InclusiveProcess* cur_process = new InclusiveProcess(UI);
    cur_process->Evaluate();
    cout<<cur_process->DetailedResults();
    cout<<endl<<endl;
}

void multiscale_running( UserInterface& UI, const vector<double>& scales);


void restricted_multiscale_running(UserInterface& UI)
{
    double scales_array[]={1.0,0.5,0.25};//0.25,0.5,1.0};
    
    vector<double> scales (scales_array, scales_array
                           + sizeof(scales_array) / sizeof(double) );
    multiscale_running(UI,scales);
}

void full_multiscale_running(UserInterface& UI)
{
    double scales_array[]={0.03125,0.04,0.05,0.0625,0.08,0.1,0.125,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.75,0.8,0.9,1.0,1.2,1.5,2.0};
    
    vector<double> scales (scales_array, scales_array
                           + sizeof(scales_array) / sizeof(double) );
    multiscale_running(UI,scales);
}

void compute_delta_sigma_multiscale(UserInterface& UI)
{
    double scales_array[]={0.03125,0.04,0.05,0.0625,0.08,0.1,0.125,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.75,0.8,0.9,1.0,1.2,1.5,2.0};
    
    vector<double> scales (scales_array, scales_array
                           + sizeof(scales_array) / sizeof(double) );
    compute_plain_lumi(UI,scales);
}

void compute_plain_lumi(UserInterface& UI, const vector<double>& scales)
{
    const int num_of_scales=scales.size();
    
    double central_scale = UI.mur;
    vector<InclusiveProcess*> all_procs;
    
    InclusiveProcess* cur_process = new InclusiveProcess(UI);
    cur_process->Evaluate("N3LO L(z)*a^5");
    cur_process->Evaluate("qg N3LO L(z)*a^5");
    
    cur_process->Evaluate("N3LO L*(F - NS)*a^5");
    cur_process->Evaluate("qg N3LO L*(F - NS)*a^5");
    
    cur_process->Evaluate("N3LO L(z)*(1-z)*a^5");
    cur_process->Evaluate("qg N3LO L(z)*(1-z)*a^5");
    
    
    ResultPair dsgg = cur_process->find_term("N3LO L*(F - NS)*a^5")->Result().term_of_order(5);
    ResultPair dsqg = cur_process->find_term("qg N3LO L*(F - NS)*a^5")->Result().term_of_order(5);
    ResultPair dengg = cur_process->find_term("N3LO L(z)*(1-z)*a^5")->Result().term_of_order(5);
    ResultPair denqg = cur_process->find_term("qg N3LO L(z)*(1-z)*a^5")->Result().term_of_order(5);
    
    
    
    
    ResultPair cgg = dsgg / dengg;
    ResultPair cqg = dsqg / denqg;
    
    ResultPair dsigma = dengg * cgg + denqg * cqg;
    
    
    cout<<"mu = "<<UI.muf<<endl;
    cout<<"L(z)*(1-z)*a^5 gg = "<<dengg<<"\tqg = "<<denqg<<endl;
    cout<<"F-NS gg = "<<dsgg<<"\tqg = "<<dsqg<<endl;
    cout<<"C gg = "<<dsgg/dengg
        <<"\t qg = "<<dsqg/denqg
    <<endl;
    cout<<"dsigma = "<<dsigma <<endl;
    cout<<"dsigma rescaled = "<<dsigma * cur_process->RescalingCoeff()<<endl;
    cout<<endl<<endl;

    for (int i=0;i<num_of_scales;i++)
    {
        double cur_scale = central_scale * scales[i];
        UI.mur = cur_scale;
        UI.muf = cur_scale;
        InclusiveProcess* cur_process = new InclusiveProcess(UI);
        cur_process->Evaluate("N3LO L(z)*(1-z)*a^5");
        cur_process->Evaluate("qg N3LO L(z)*(1-z)*a^5");
        all_procs.push_back(cur_process);
    }
    for (int i=0;i<num_of_scales;i++)
    {
        InclusiveProcess* cur_process = all_procs[i];
        ResultPair curLa5gg = cur_process->find_term("N3LO L(z)*(1-z)*a^5")->Result().term_of_order(5);
        ResultPair curLa5qg = cur_process->find_term("qg N3LO L(z)*(1-z)*a^5")->Result().term_of_order(5);
        cout<<"mu"<<scales[i]
            <<setw(20)<< (curLa5gg * cgg + curLa5qg * cqg)*cur_process->RescalingCoeff()
            <<setw(20)<< curLa5gg
            <<setw(20)<< curLa5qg
            <<endl;
    }
 
    cout<<endl<<endl;
}

void multiscale_running( UserInterface& UI, const vector<double>& scales)
{
    const int num_of_scales=scales.size();
    
    double central_scale = UI.mur;
    vector<InclusiveProcess*> all_procs;
    
    InclusiveProcess* central_scale_process;
    for (int i=0;i<num_of_scales;i++)
    {
        double cur_scale = central_scale * scales[i];
        UI.mur = cur_scale;
        UI.muf = cur_scale;
        InclusiveProcess* cur_process = new InclusiveProcess(UI);
        if (scales[i]==1) central_scale_process = cur_process;
        cur_process->Evaluate();
        all_procs.push_back(cur_process);
    }
    
    ResultPair dsgg = central_scale_process->find_term("N3LO L*(F - NS)*a^5")->Result().term_of_order(5);
    ResultPair dsqg = central_scale_process->find_term("qg N3LO L*(F - NS)*a^5")->Result().term_of_order(5);
    ResultPair dengg = central_scale_process->find_term("N3LO L(z)*(1-z)*a^5")->Result().term_of_order(5);
    ResultPair denqg = central_scale_process->find_term("qg N3LO L(z)*(1-z)*a^5")->Result().term_of_order(5);
    
    
    
    
    ResultPair cgg = dsgg / dengg;
    ResultPair cqg = dsqg / denqg;
    
    
    
    vector<ResultPair> deltaSigma;
    for (int i=0;i<num_of_scales;i++)
    {
    
    ResultPair curLa5gg = all_procs[i]->find_term("N3LO L(z)*(1-z)*a^5")->Result().term_of_order(5);
    ResultPair curLa5qg = all_procs[i]->find_term("qg N3LO L(z)*(1-z)*a^5")->Result().term_of_order(5);
        ResultPair ds = (curLa5gg * cgg + curLa5qg * cqg)*all_procs[i]->RescalingCoeff();
        deltaSigma.push_back(ds);
    }
    
    
    
    
    stringstream output;
    
    for (int i=0;i<num_of_scales;i++)
    {
        output<<all_procs[i]->DetailedResults();
        output<<setw(20)<<"delta sigma "<<setw(24)<<deltaSigma[i]<<endl;
    }
    output<<"\nCollected results"<<endl;
    int width=19;
    output<<setw(5)<<left<<"mu"<<right
            <<setw(width)<<"best LO"
            <<setw(width)<<"best NLO"
            <<setw(width)<<"best NNLO"
            <<setw(width)<<"best N3LO"
            <<setw(width)<<"best total"
            <<setw(width)<<"Lf=0 FULL-NS"
            <<setw(width)<<"delta sigma"
            <<endl;
    for (int i=0;i<num_of_scales;i++)
    {
        
        
        
        output<<left
        <<setprecision(4)<<setw(6)<<scales[i]
            <<setprecision(4)<<all_procs[i]->OneLineResults()
            <<setw(24)<<deltaSigma[i]<<endl;
    }
    
    output<<"\nCollected results top only"<<endl;
    output<<setw(5)<<left<<"mu"<<right
    <<setw(width)<<"top only LO"
    <<setw(width)<<"top only NLO"
    <<setw(width)<<"top only NNLO"
    <<setw(width)<<"top only N3LO"
    <<setw(width)<<"top only Sum"
    <<endl;
    for (int i=0;i<num_of_scales;i++)
    {
        output<<left
        <<setprecision(4)<<setw(6)<<scales[i]
        <<setprecision(4)<<all_procs[i]->OneLineResultsTopOnly()<<endl;
    }
    
    output<<endl<<endl;
    
    
    width=35;
    
    
    cout<<output.str();
    if (not(UI.output_filename.empty()))
    {
        cout << "[ihixs] writing output at " << UI.output_filename << endl;
        const char * output_fname = UI.output_filename.c_str();
        fstream my_local_outfile(output_fname, fstream::out);
        if(my_local_outfile.is_open())
        {
            my_local_outfile.precision(5);
            my_local_outfile << "ihixs results " << endl;
            
            
            my_local_outfile << "\n runcard_name=\""<<UI.input_filename
            <<"\""<<endl;
            
            my_local_outfile << output.str() << endl;
            
            //cout << "\noutput written in "<< my_UI.output_filename<< endl;
        }
        else
        {
            cout<<"\nfailbit = "<<my_local_outfile.fail()<<endl;
            cout << "Error opening file "<<UI.output_filename.c_str()<<endl;
        }
        my_local_outfile.close();
    }
    
}



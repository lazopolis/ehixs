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
            
            InclusiveProcess* cur_process = new InclusiveProcess(UI);
            cur_process->Evaluate();
            cout<<"--------"<<endl;
            const double Kfactor = 1.065;
            cout<<"LO delta = "<<cur_process->gg_delta_LO * Kfactor<<endl;
            cout<<"NLO delta = "<<cur_process->gg_delta_NLO * Kfactor<<endl;
            cout<<"NLO D0 = "<<cur_process->gg_D0_NLO * Kfactor<<endl;
            cout<<"NLO D1 = "<<cur_process->gg_D1_NLO * Kfactor<<endl;
            cout<<"NLO +  = "<<cur_process->gg_D0_NLO * Kfactor
                            +cur_process->gg_D1_NLO * Kfactor<<endl;
            
            cout<<"N2LO delta = "<<cur_process->gg_delta_NNLO * Kfactor<<endl;
            cout<<"N2LO D0 = "<<cur_process->gg_D0_NNLO * Kfactor<<endl;
            cout<<"N2LO D1 = "<<cur_process->gg_D1_NNLO * Kfactor<<endl;
            cout<<"N2LO D2 = "<<cur_process->gg_D2_NNLO * Kfactor<<endl;
            cout<<"N2LO D3 = "<<cur_process->gg_D3_NNLO * Kfactor<<endl;
            cout<<"N2LO +  = "<<(cur_process->gg_D0_NNLO
                                 + cur_process->gg_D1_NNLO
                                 + cur_process->gg_D2_NNLO
                                 + cur_process->gg_D3_NNLO) * Kfactor<<endl;
            
            cout<<"N3LO delta = "<<cur_process->gg_delta_N3LO * Kfactor<<endl;
            cout<<"N3LO D0 = "<<cur_process->gg_D0_N3LO * Kfactor<<endl;
            cout<<"N3LO D1 = "<<cur_process->gg_D1_N3LO * Kfactor<<endl;
            cout<<"N3LO D2 = "<<cur_process->gg_D2_N3LO * Kfactor<<endl;
            cout<<"N3LO D3 = "<<cur_process->gg_D3_N3LO * Kfactor<<endl;
            cout<<"N3LO D4 = "<<cur_process->gg_D4_N3LO * Kfactor<<endl;
            cout<<"N3LO D5 = "<<cur_process->gg_D5_N3LO * Kfactor<<endl;
            cout<<"N3LO +  = "<< cur_process->gg_D0_N3LO * Kfactor
                                +cur_process->gg_D1_N3LO * Kfactor
                                +cur_process->gg_D2_N3LO * Kfactor
                                +cur_process->gg_D3_N3LO * Kfactor
                                +cur_process->gg_D4_N3LO * Kfactor
                                +cur_process->gg_D5_N3LO * Kfactor<<endl;
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




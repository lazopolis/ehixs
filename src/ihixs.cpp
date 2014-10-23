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
            cout<< *cur_process<<endl;
            cout<<"-----------------------------------------------------"
                <<"-----------------------------------------------------"
                <<"---------"
                <<endl;
            cout<<"Total contributions for the N3LO cross section"
                <<" at central scale mur="<<UI.mur<<endl;
            cout<<setprecision(4)
            <<setw(6)<<"LO"<<setw(24)<<cur_process->CoefficientAlphaS(2)<<endl
            <<setw(6)<<"NLO"<<setw(24)<<cur_process->CoefficientAlphaS(3)<<endl
            <<setw(6)<<"NNLO"<<setw(24)<<cur_process->CoefficientAlphaS(4)<<endl
            <<setw(6)<<"N3LO"<<setw(25)<<cur_process->CoefficientAlphaS(5)<<endl
            <<"-----------------"<<endl
            <<setw(6)<<"Total"<<cur_process->TotalCentral()
            <<endl<<endl;
            
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




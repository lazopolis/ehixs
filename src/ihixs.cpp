

#include <stdlib.h>
#include <string.h>
#include <fstream>
#include<vector>
#include <iostream>
#include <iomanip>
#include <map>
#include <math.h>
#include "process.h"


#include "user_interface.h"
#include "exact_lo_inclusive.h"
using namespace std;





void print_logo()
{
    cout<<"\n* * * * * * * * * * * * * * * * * * * * * * * * *";
    cout<<"\n*                                               *";
    cout<<"\n*                                               *";
    cout<<"\n*                                               *";
    cout<<"\n*                ihixs++                        *";
    cout<<"\n*                                               *";
    cout<<"\n*                version 1.0                    *";
    cout<<"\n*                                               *";
    cout<<"\n*                                               *";
    cout<<"\n* * * * * * * * * * * * * * * * * * * * * * * * *";
    cout<<"\n\n";
}




int main(int argc, char** argv)
{
    // get init time
    clock_t t_init = clock();
    
    // UI initialization
    UserInterface UI;
    
    
    try
        {
        UI.ParseInput(argc,argv);
        

        CModel *Model = new CModel;
        //Model->quarks[0]->set_m_at_ref_scale(20.0);
        Model->quarks[1]->Y = 0.0;
        
        Exact_LO_Inclusive default_LO(Model, &UI);
        default_LO.perform();
        double xs = default_LO.cross_section();
        double err = default_LO.mc_error();
        cout<<"\n ihixs results:\n-------------------------------"
            <<"\n LO exact cross-section = "<<xs<<" +- "<<err
        <<"\t(PDF+a_s) error = "<<default_LO.pdf_error_in_string_format()
            <<"\n \ninput used:"
            <<"\na_s("<<Model->mu_r()<<") = "<<Model->alpha_strong[0];
        
        for (unsigned i=0;i<Model->quarks.size();i++)
            {
            cout<<"\nm_"<<Model->quarks[i]->name<<"("<<Model->mu_r()
            <<")= "<<Model->quarks[i]->m();
            if (Model->quarks[i]->Y!=0.0) cout<<"\t Y="<<Model->quarks[i]->Y;
            else cout<<"\t INACTIVE";
            }

        cout<<"\nm_h = "<<Model->higgs.m()
            <<"\nEtot = "<<UI.Etot
            <<"\npdf = "<<UI.pdf_provider
            <<" at order a^"<<UI.perturbative_order
            <<"\n-------------------------------"
            <<endl;
        }
    catch(const char* s)
    {
    cerr << endl << argv[0] << ": " << s << endl;
    }
    catch(...)
    {
    cerr<<endl<<"Something went wrong but the exception thrown was not a string";
    }
    cout << "Total running time  = " << float(clock()-t_init)/CLOCKS_PER_SEC << " s" << endl;
    return 0;
}




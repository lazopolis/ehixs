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
#include "process.h"


#include "user_interface.h"

using namespace std;





void print_logo()
{
  cout<<"\n* * * * * * * * * * * * * * * * * * * * * * * * *";
  cout<<"\n*                                               *";
  cout<<"\n*                                               *";
  cout<<"\n*                                               *";
  cout<<"\n*                ehixs                          *";
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
               Process* cur_process = new Process(UI);
               cur_process->perform();
               }
          
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




/** \file hub.cpp
  *
  */

//#include "headers.hpp"

#include "hub.hpp"


#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include<iostream>
using namespace std;
namespace Hub {
  /** Pointer to data */
  double  x    [NVAR_MAX];
  /* Types of variables (see VarType) */
  VarType types[NVAR_MAX];
  /* Count for number of variables */
  unsigned n=0;

     void RequestVar(Hub::VarType type)
     {
          //// Wonderful usage of post-increment (dedicated to Julian)
          // types[n++] = type;
          //: something went wonderfully wrong here
          cout<<"\n request var call\nn="<<n;
          types[n] = type;
          n++;
     // Error checking
    if(n>=NVAR_MAX)
      throw "too many variables requested. Change HUB_NVAR_MAX in headers.hpp (yes, it is dirty).";
//          cout<<"\n request var call"<<endl;
  }

  double* RequestPtr()
  { return x+n; }

  void SetVars(const double* x_vegas)
  {
    for(unsigned i=0, c=0; i<n; ++i)
      switch(types[i])
      {
        case VEGAS:
          x[i] = x_vegas[c++];
          break;
        case FLAT:
           srand(time(NULL));
           x[i] = (double) rand()/ (RAND_MAX);
          break;
      }
  }

  unsigned GetVEGASDim()
  {
    unsigned c=0;
    for(unsigned i=0; i<n; ++i)
      if(types[i] == VEGAS)
        ++c;

    return c;
  }
}
//:=================================   TheHatch
void TheHatch::RequestVar(string type)
{
    if(types.size()>=NVAR_MAX)
        throw "[TheHatch] too many variables requested. Change HUB_NVAR_MAX in headers.hpp (yes, it is dirty).";
    else
        {
        if (type=="VEGAS")
            {
            types.push_back(0);
            number_of_vegas_variables++;
            //cout<<"\n[TheHatch] new Vegas variable requested."
            //    <<" Number of Vegas variables now = "
            //    <<number_of_vegas_variables<<endl;
            }
        else types.push_back(1);
        }
}

double* TheHatch::RequestPtr()
{ return data+types.size(); }

void TheHatch::SetVars(const double* x_vegas)
{
     int c=0;
     srand(time(NULL));
     for (unsigned i=0;i<types.size();i++)
     {
          switch (types[i])
          {
               case 0:
                    data[i]=open_interval(x_vegas[c]);
                    c++;//: how cool is this?
                    break;
               case 1:
                    double randomnumber=(double) rand()/ (RAND_MAX);
                    data[i]=randomnumber;
                    break;
          }
          
     }
     
}

double TheHatch::open_interval(const double& x)
{
    const double technical_cuttoff = 0.5e-15;
    return 2.*technical_cuttoff + (1.-technical_cuttoff)*x;
}





/** \file CBin.cpp
  *
  * Implement CBin.h
  */

#include <math.h>
#include <iostream>

using namespace std;

#include "CBin.h"

/*

inline int Factorial(int x) {
    return (x == 1 ? x : x * Factorial(x - 1));
}

inline int my_factorial(const double & x)
{
    if (fmod(x,1.0)==0.0)
    {
        return Factorial(x);
    }
    else if (fmod(x,1.0)==0.5)
    {
        return(Factorial(2*unsigned(x)));
    }
    else
    {
        cout<<"\nnon (semi) integer input in factorial"<<endl;
        //exit(0);
        return 0;
    }
}
*/

void Bin::update(unsigned NOP)
{
  if (running_f!=0.0)
  {
    // Cumulated sums...
    // fabs takes care of the situation in which the integrand is constant and NOP*f2-f^2 is marginally negative due to round-off
    const double sigma=sqrt(fabs((NOP*running_f2-running_f*running_f)/(NOP-1.0)));
    // If first iteration
    if (avg_err==0.0)
    {
      // Values after first iteration
      avg_f           = running_f;
      avg_err         = sigma;
      running_chi_sq  = 0.0;
      iteration_number= 1;
    }
    else
    {
      // Save values
      const double old_err=avg_err;
      const double old_f=avg_f;
      // Update
      avg_err            = 1.0/sqrt(1.0/(avg_err*avg_err) + 1.0/(sigma*sigma));
      avg_f              = (avg_f/(old_err*old_err) + running_f/(sigma*sigma))*avg_err*avg_err;

     ++iteration_number;
     running_chi_sq    += pow(running_f - avg_f, 2.0)/avg_err;
     chi_sq             = running_chi_sq*iteration_number/(iteration_number-1.0);
    }
    // Reset running variables
    running_f=0.0;
    running_f2=0.0;
  }
  else
     {
       ++iteration_number;
       running_chi_sq    += pow( avg_f, 2.0)/avg_err;
       chi_sq             = running_chi_sq*iteration_number/(iteration_number-1.0);
     }

}
/*
double Bin::give_chi_sq() const
{   
    const double x=chi_sq;
    const int k=iteration_number;
    return(pow(x,k/2-1.0)*exp(-x/2.0)/pow(2.0,k/2.0)/my_factorial(k/2.0));
}
*/

void Bin::end()
{
  // Add values
  running_f     += intra_point_f;
  running_f2    += pow(intra_point_f, 2.0);
  // Reset values
  intra_point_f  = 0.0;
}

ostream& operator<<(ostream& stream, const Bin& bin)
{
  
  return stream << bin.avg_f 
                << " +- " << bin.avg_err 
     << " \t| " << bin.chi_sq <<" / "<<bin.iteration_number;
                ;
}

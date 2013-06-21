
#include "SingleIntegral.h"
#include "cuba.h"

#ifndef ONCE_PTR_2_SINGLEINTEGRAL
#define ONCE_PTR_2_SINGLEINTEGRAL

SingleIntegral* ptr_to_SingleIntegral;
#endif



int Integrand(const int *ndim, const double xx[],
              const int *ncomp, double ff[],void * therun, double* weight, int* iteration_number)
{

     ptr_to_SingleIntegral->evaluate_integrand(xx,ff);
     return(0);
}


ostream& operator<<(ostream& stream, const SingleIntegral& Vegas)
{
     
     stream << "\n-------------------------------------------"
     << "\n ehixs output" << endl
     << "\n sigma = " << Vegas.vegas_integral_output[0]
     << " +- " << Vegas.vegas_error_output[0]
     << " \t prob = " << Vegas.vegas_prob_output[0];
     return stream;
}


void SingleIntegral::call_vegas()
{
     ptr_to_SingleIntegral=this;
     
     double integral[number_of_components];
     double error[number_of_components];
     double prob[number_of_components];
     int neval,fail;
     int gridno=0;
     int seed=0;
     //:0: Sobol
     //:>0 Ranlux
     int nbatch=10;
     cout<<"-------------------- Vegas  --------------------\n";
     //
     Vegas(
           dimensions_of_integration,
           number_of_components,
           integrand_t(Integrand),
           NULL,
           myVegasArgs.epsrel,
           myVegasArgs.epsabs,
           myVegasArgs.verbose,
           seed,
           myVegasArgs.mineval, // = mineval
           myVegasArgs.maxeval,
           myVegasArgs.nstart,
           myVegasArgs.nincrease,
           nbatch,
           gridno,
           NULL,//grid_file_name.c_str(),
           &neval,
           &fail,
           integral,
           error,
           prob);
     
     for (int i=0;i<number_of_components;i++)
          {
          vegas_integral_output.push_back(integral[i]);
          vegas_error_output.push_back(error[i]);
          vegas_prob_output.push_back(prob[i]);
          }
     
}




SingleIntegral::SingleIntegral(VegasArgs& Vargs)
{
     myVegasArgs=Vargs;
     
}







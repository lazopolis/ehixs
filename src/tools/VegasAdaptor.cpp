
#include "VegasAdaptor.h"
#include "cuba.h"




int Integrand(const int *ndim, const double xx[],
                     const int *ncomp, double ff[],void * therun, double* weight, int* iteration_number);


ostream& operator<<(ostream& stream, const VegasAdaptor& Vegas)
{
     
     stream << "\n-------------------------------------------"
     << "\n ehixs output" << endl
     << "\n sigma = " << Vegas.vegas_integral_output[0]
     << " +- " << Vegas.vegas_error_output[0]
     << " \t prob = " << Vegas.vegas_prob_output[0];
     return stream;
}


void VegasAdaptor::call_vegas()
{
#ifdef debug
     cout<<"\n["<<__func__<<"]";
#endif
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
           the_hatch->GetVEGASDim(), 
           number_of_components,
           integrand_t(my_integrand),
           NULL,
           epsrel, 
           epsabs,
           verbose,
           seed,  
           mineval, // = mineval
           maxeval,
           nstart,
           nincrease,
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

void VegasAdaptor::set_up_vegas_ff(double res)
{
     //: only central value passed to vegas here
     //: all other values of histograms are set in the histogram do_binning routine.
     ff_vegas[0]=ff_vegas[0]+res;
     
     //cout<<"\t\t sigma_0 += "<<res;
}



VegasAdaptor::VegasAdaptor(const UserInterface & UI )
{
     //: MC, setting options
     epsrel=UI.epsrel; 
     epsabs=UI.epsabs;
     verbose=UI.verbose;
     mineval=UI.mineval;
     maxeval=UI.maxeval;
     nstart=UI.nstart;
     nincrease=UI.nincrease;
     

}



bool VegasAdaptor::new_iteration_has_started()
{
     //:flushing ff_vegas[]
     for(int p = 0; p <= number_of_components; ++p)
		{
		ff_vegas[p]=0.0;// cleaning up ff_vegas
		}
     //:checking whether this is the first point of new iteration
     if (vegas_iteration_number!=vegas_iteration_number_old)
     {
          NOP_in_previous_iteration=vegas_NOP_in_current_iteration;
          vegas_NOP_in_current_iteration = 1;
          vegas_iteration_number_old=vegas_iteration_number;
          return true;
     }
     else 
     {
          vegas_NOP_in_current_iteration++;
          return false;
     }
}



/**
 *
 * \file    vegasadaptor.cpp
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2015
 *
 */

#include "vegasadaptor.h"
#include "mincuba.h"
#include "math.h"
#include "sstream"

int Integrand(
              const int* ndim, const double xx[],
              const int* ncomp, double ff[],
              void* therun, double* weight, int* iteration_number
              );

VegasAdaptor::VegasAdaptor(pointer_to_Integrand ptr, const int ndim)
: IVegas(), number_of_dims(ndim),
epsrel_cur(epsrel_therm),
epsabs_cur(epsabs_therm),
mineval_cur(mineval_therm),
maxeval_cur(maxeval_therm),
nstart_cur(nstart_therm),
nincrease_cur(nincrease_therm),
the_hatch(NULL),
my_integrand(ptr),
total_number_of_points_(0),
grid_file_name_("grid"),
gridno(0),
ff(),
number_of_components(0),
integral_output(), error_output(), prob_output(),
weight(), iteration_number(), NOP_in_current_iteration(),
NOP_in_previous_iteration(), iteration_number_old()
{}

void VegasAdaptor::call()
{
    cout<<"\n["<<__func__<<"] Calling Vegas "<<endl;
    cout<<"\n["<<__func__<<"] dimension of integration =  "
        <<number_of_dims <<endl;
    
     double integral[number_of_components];
     double error[number_of_components];
     double prob[number_of_components];
     int neval,fail; 
       
     gridno = 1;
     int seed=0;
     //:0: Sobol
     //:>0 Ranlux
     int nbatch=10;
     //
     Vegas(
           number_of_dims,
           number_of_components,
           integrand_t(my_integrand),
           NULL,
           epsrel_cur,
           epsabs_cur,
           verbose,
           seed,  
           mineval_cur, // = mineval
           maxeval_cur,
           nstart_cur,
           nincrease_cur,
           nbatch,
           gridno, 
           grid_file_name_.c_str(),
           &neval,
           &fail,
           integral, 
           error, 
           prob);
     
     for (int i=0;i<number_of_components;i++)
          {
          integral_output.push_back(integral[i]);
          error_output.push_back(error[i]);
          prob_output.push_back(prob[i]);
          }
}

void VegasAdaptor::prepare_for_final_iteration()
{
    epsrel_cur=epsrel;
    epsabs_cur=epsabs;
    mineval_cur=mineval;
    maxeval_cur=maxeval;
    nstart_cur=nstart;
    nincrease_cur=nincrease;
    ff[0]=0.0;
    integral_output.clear();
    error_output.clear();
    prob_output.clear();
}

void VegasAdaptor::set_up_ff(double res)
{
     //: only central value passed to vegas here
     //: all other values of histograms are set in the histogram do_binning routine.
     ff[0]=ff[0]+res;
     //cout<<"\n[Vegas] added weight "<<res;
}

bool VegasAdaptor::new_iteration_has_started()
{
    //:flushing ff[]
    for(vector<double>::iterator p = ff.begin(); p != ff.end(); ++p)
        *p = 0.;
    //:checking whether this is the first point of new iteration
    if (iteration_number!=iteration_number_old) {
        NOP_in_previous_iteration=NOP_in_current_iteration;
        NOP_in_current_iteration = 1;
        iteration_number_old=iteration_number;
        return true;
    } else {
        total_number_of_points_++;
        NOP_in_current_iteration++;
        return false;
    }
}

void VegasAdaptor::flush()
{
    ff[0]=0.0;
    total_number_of_points_ =0;
    integral_output.clear();
    error_output.clear();
    prob_output.clear();
    //iteration_info_.str(""); // emptying the iteration_info_
}

string VegasAdaptor::xml()
{
    stringstream stream;
    stream<< "\n sigma=\"" << integral_output[0]<<"\""
    << "\nerror=\"" << error_output[0]<<"\""
    << " \nprob=\"" << prob_output[0]<<"\""
    <<"\ntotal_number_of_points=\""<<total_number_of_points()<<"\""
    <<endl;
    return stream.str();
}

ostream& operator<<(ostream& stream, const VegasAdaptor& Vegas)
{
    stream << "\n\n[ehixs] cross section [pb]" << endl;
    for (int i = 0; i < number_of_components; ++i)
    {
        stream << "[ehixs] sigma = " << Vegas.integral_output[i]
        << " +- " << Vegas.error_output[i]
        << " prob = " << Vegas.prob_output[i];
    }
    stream << " total # of points = " << Vegas.total_number_of_points() << "\n" << endl;
    return stream;
}

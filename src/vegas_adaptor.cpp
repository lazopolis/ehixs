
#include "vegas_adaptor.h"
#include "cuba.h"
#include "math.h"




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
           number_of_dims,
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




VegasAdaptor::VegasAdaptor(const UserInterface & UI,pointer_to_Integrand ptr,int ndim)
{
    //: MC, setting options
    epsrel=UI.epsrel;
    epsabs=UI.epsabs;
    verbose=UI.verbose;
    mineval=UI.mineval;
    maxeval=UI.maxeval;
    nstart=UI.nstart;
    nincrease=UI.nincrease;

    my_integrand = static_cast<pointer_to_Integrand>(ptr);
    number_of_dims = ndim;
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
    number_of_dims = 0;

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




//  ---------------------------------------------------------------------------



double CoolInt::evaluateIntegral(const double xx[])
{
    //ptr_to_inclusive_process->Vegas.vegas_weight = *weight;
	//ptr_to_inclusive_process->Vegas.vegas_iteration_number = *iteration_number;
	//ptr_to_inclusive_process->Evaluate_integral(xx);
    
    double lambda = xx[0];
    return pow(lambda,3.0);
    
}


CoolInt::CoolInt()
{
    _gridno=0;
    _seed=0;//:0: Sobol,  >0 Ranlux
    _nbatch=10;
    _mineval=10000;
    _maxeval=50000000;
    _nstart=5000;
    _nincrease=1000;
    _verbose = 2;
   // ptr_to_cool_int=this;
    _number_of_dims = 1;
    _epsrel = 1e-3;
    _epsabs = 0.0;
   // ptr_to_cool_int=this;
    _vector_of_xs.push_back(new Bin);
    _running_xs_values.push_back(0.0);
}


void CoolInt::setParams(int number_of_dims,double epsrel,double epsabs,
                        int mineval,int maxeval,int nstart,int nincrease)

{
    _gridno=0;
    _seed=0;//:0: Sobol,  >0 Ranlux
    _nbatch=10;
    _verbose = 2;
    _number_of_dims = number_of_dims;
    _epsabs = epsabs;
    _epsrel = epsrel;
    _mineval = mineval;
    _maxeval = maxeval;
    _nstart = nstart;
    _nincrease = nincrease;
}

void CoolInt::setParams(int number_of_dims,double epsrel,double epsabs)
{
    _gridno=0;
    _seed=0;//:0: Sobol,  >0 Ranlux
    _nbatch=10;
    _mineval=10000;
    _maxeval=50000000;
    _nstart=5000;
    _nincrease=1000;
    _verbose = 2;
    _number_of_dims = number_of_dims;
    _epsrel = epsrel;
    _epsabs = epsabs;
}


void CoolInt::call_vegas()
{
    Vegas(
          _number_of_dims,
          1,// number of components
          integrand_t(&cool_integral),
          this,
          _epsrel,
          _epsabs,
          _verbose,
          _seed,
          _mineval, // = mineval
          _maxeval,
          _nstart,
          _nincrease,
          _nbatch,
          _gridno,
          NULL,//grid_file_name.c_str(),
          &_neval,
          &_fail,
          _central_value,
          _vegas_error,
          _prob);
    // update the bins after the last iteration
    for (unsigned i=0;i<_vector_of_xs.size();i++)
        _vector_of_xs[i]->end_of_iteration_update();
}

void CoolInt::set_number_of_xs_values_we_keep(unsigned nox)
{
    // there is already one bin in _vector_of_xs,
    // and one double in _running_xs_values from constructor
    for (unsigned i=0;i<nox-1;i++)
        {
        _vector_of_xs.push_back(new Bin);
        _running_xs_values.push_back(0.0);
        }
    
}

void CoolInt::check_whether_we_need_to_update_bins(unsigned new_iter_number)
{
    if(new_iter_number!=_iter_num)
        {
        for (unsigned i=0;i<_vector_of_xs.size();i++)
            _vector_of_xs[i]->end_of_iteration_update();
        cout<<"\n new iteration. Previous iteration value = "
            <<_vector_of_xs[0]->value()<<" +- "<<_vector_of_xs[0]->error();
        }
}

void CoolInt::add_to_bins(const double & w)
{
    for (unsigned i=0;i<_vector_of_xs.size();i++)
        {
        _vector_of_xs[i]->add_single_point_package(w* _running_xs_values[i]);
        }
}




const vector<double>  CoolInt::give_vector_of_res()
{
    vector<double> res;
    for (unsigned i=0;i<_vector_of_xs.size();i++)
        res.push_back(_vector_of_xs[i]->value());
    return res;
}

double CoolInt::give_res_component(unsigned i)
{
    if (i<_vector_of_xs.size())
        return _vector_of_xs[i]->value();
    else
        {
        cerr<<"\nError: you asked from CoolInt a result for component #"
            <<i<<" while it only has "<<_vector_of_xs.size()<<" compnents";
        return 0.0;
        }
}

double CoolInt::give_err_component(unsigned i)
{
    if (i<_vector_of_xs.size())
        return _vector_of_xs[i]->error();
    else
        {
        cerr<<"\nError: you asked from CoolInt a result for component #"
        <<i<<" while it only has "<<_vector_of_xs.size()<<" compnents";
        return 0.0;
        }
}


AnotherInt::AnotherInt():CoolInt(){};//{ptr_to_cool_int=this;};


double AnotherInt::evaluateIntegral(const double xx[])
{
    double z=0.732;
    double lambda = xx[0];
    double res = 3.0*pow(lambda,2.0)*123.456789;
    _running_xs_values[0] = res;
    return res;
    
}


int cool_integral(const int *ndim, const double xx[],
                  const int *ncomp, double ff[],void * theclass,
                  double* weight_from_vegas,
                  int* iteration_number_from_vegas)
{

    CoolInt* ptr_to_class = static_cast<CoolInt*>(theclass);
	ff[0]=ptr_to_class->evaluateIntegral(xx);
    
    ptr_to_class->check_whether_we_need_to_update_bins(*iteration_number_from_vegas);
    ptr_to_class->set_iteration_number(*iteration_number_from_vegas);
    ptr_to_class->add_to_bins(*weight_from_vegas);
    
    return(0);
}











#include "vegas_adaptor.h"
#include "mincuba.h"
#include "math.h"
#include "sstream"



int Integrand(const int *ndim, const double xx[],
                     const int *ncomp, double ff[],void * therun, double* weight, int* iteration_number);


ostream& operator<<(ostream& stream, const VegasAdaptor& Vegas)
{
     
     stream <<"[ehixs]"<<endl
            <<"[ehixs]"<<endl
            <<"[ehixs] cross section [pb]"<<endl
        <<"[ehixs] sigma = " << Vegas.vegas_integral_output[0]
     << " +- " << Vegas.vegas_error_output[0]
     << " prob = " << Vegas.vegas_prob_output[0]
    <<" total # of points = "<<Vegas.total_number_of_points()
    <<endl<<"[ehixs]"<<endl;
     return stream;
}

string VegasAdaptor::xml()
{
    stringstream stream;
    stream<< "\n sigma=\"" << vegas_integral_output[0]<<"\""
    << "\nerror=\"" << vegas_error_output[0]<<"\""
    << " \nprob=\"" << vegas_prob_output[0]<<"\""
    <<"\ntotal_number_of_points=\""<<total_number_of_points()<<"\""
    <<endl;
    return stream.str();
}


void VegasAdaptor::prepare_for_final_iteration()
{
    epsrel_cur=epsrel;
    epsabs_cur=epsabs;
    mineval_cur=mineval;
    maxeval_cur=maxeval;
    nstart_cur=nstart;
    nincrease_cur=nincrease;
    ff_vegas[0]=0.0;
    vegas_integral_output.clear();
    vegas_error_output.clear();
    vegas_prob_output.clear();
}

void VegasAdaptor::ConfigureNumberOfComponents(int n)
{
    // the first "Nmember" components are for the pdf error
    number_of_components=n;
    
    for (int i=0;i<number_of_components;i++)
    {
        ff_vegas.push_back(0.0);
    }   
}


void VegasAdaptor::call_vegas()
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
    
     //cout<<"\n[Vegas] added weight "<<res;
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

    epsrel_therm=UI.epsrel_therm;
    epsabs_therm=UI.epsabs_therm;
    mineval_therm=UI.mineval_therm;
    maxeval_therm=UI.maxeval_therm;
    nstart_therm=UI.nstart_therm;
    nincrease_therm=UI.nincrease_therm;
    
    // initial run with thermalized variables
    epsrel_cur=epsrel_therm;
    epsabs_cur=epsabs_therm;
    mineval_cur=mineval_therm;
    maxeval_cur=maxeval_therm;
    nstart_cur=nstart_therm;
    nincrease_cur=nincrease_therm;
    
    my_integrand = static_cast<pointer_to_Integrand>(ptr);
    number_of_dims = ndim;
    total_number_of_points_ =0;
    grid_file_name_ = "vegas_grid";
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
    total_number_of_points_ =0;
    
    
    epsrel_therm=UI.epsrel_therm;
    epsabs_therm=UI.epsabs_therm;
    mineval_therm=UI.mineval_therm;
    maxeval_therm=UI.maxeval_therm;
    nstart_therm=UI.nstart_therm;
    nincrease_therm=UI.nincrease_therm;
    
    // initial run with thermalized variables
    epsrel_cur=epsrel_therm;
    epsabs_cur=epsabs_therm;
    mineval_cur=mineval_therm;
    maxeval_cur=maxeval_therm;
    nstart_cur=nstart_therm;
    nincrease_cur=nincrease_therm;
    grid_file_name_ = "vegas_grid";

    
    
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
     total_number_of_points_ ++;
          vegas_NOP_in_current_iteration++;
          return false;
     }
}

void VegasAdaptor::flush()
{
    ff_vegas[0]=0.0;
    total_number_of_points_ =0;
    vegas_integral_output.clear();
    vegas_error_output.clear();
    vegas_prob_output.clear();
    //iteration_info_.str(""); // emptying the iteration_info_
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
        //cout<<"\n new iteration. Previous iteration value = "
        //    <<_vector_of_xs[0]->value()<<" +- "<<_vector_of_xs[0]->error();
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











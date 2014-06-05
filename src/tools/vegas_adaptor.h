

#ifndef VEGAS_ADAPTOR_H
#define VEGAS_ADAPTOR_H

#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>
#include <vector>
using namespace std;
#include "user_interface.h"
#include "hub.hpp"
#include "bin.h"



typedef int (*pointer_to_Integrand)(const int *ndim, const double xx[],
const int *ncomp, double ff[],void * therun, double* weight, int* iteration_number);

class VegasAdaptor{
public:
    VegasAdaptor(const UserInterface & UI);
    VegasAdaptor(const UserInterface & UI,const pointer_to_Integrand ptr,int dim);
     vector<double> ff_vegas; //: public because it has to be accessed by Integrand
     int number_of_components;//: public because it has to be accessed by Integrand
     void ConfigureNumberOfComponents(int);
     void call_vegas();
     void call_vegas_final();
    void prepare_for_final_iteration();

     void  set_up_vegas_ff(double res);
     vector<double> vegas_integral_output;//: public because they need to be accessed by CHistogram::print()
     vector<double> vegas_error_output;//: public because they need to be accessed by CHistogram::print()
     vector<double> vegas_prob_output;//: public because they need to be accessed by CHistogram::print()
     double vegas_weight;
     int vegas_iteration_number;
     int vegas_NOP_in_current_iteration;
     int NOP_in_previous_iteration;
     int vegas_iteration_number_old;
     bool new_iteration_has_started();
     void flush();
    void set_ptr_to_the_hatch(TheHatch* in_hatch)
        {the_hatch=in_hatch;}
    void set_number_of_dimensions(int dd){number_of_dims=dd;}
     void set_ptr_to_integrand(pointer_to_Integrand ptr){my_integrand=ptr;}
    int total_number_of_points() const {return total_number_of_points_;}
     friend ostream& operator<<(ostream&, const VegasAdaptor&);
    string xml();
    //string iteration_info(){return iteration_info_.str();}
private:
    double epsrel,epsabs;
    double epsrel_therm,epsabs_therm;
    double epsrel_cur,epsabs_cur;
     int verbose,mineval,maxeval,nstart,nincrease,number_of_dims;
    int mineval_therm,maxeval_therm,nstart_therm,nincrease_therm;
    int mineval_cur,maxeval_cur,nstart_cur,nincrease_cur;
     TheHatch* the_hatch;
     pointer_to_Integrand my_integrand;
    int total_number_of_points_;
    string grid_file_name_;
    int gridno;
    
   // ..stringstream iteration_info_;
     
};

//  ---------------------------------------------------------------------------
//
//  classes for easy vegas integration
//
//  usage:  subclass CoolInt to overload the function
//          double evaluateIntegral(const double xx[]);
//          Then use by constructing an object of the derived class
//          setting it up potentially
//          and then call_vegas();
//
//          see example below AnotherInt
//

class CoolInt;

#ifndef POINTER_TO_COOL_INTEGRAL
#define POINTER_TO_COOL_INTEGRAL
//CoolInt * ptr_to_cool_int;
#endif

int cool_integral(const int *ndim, const double xx[],
                  const int *ncomp, double ff[],void * therun,
                  double* weight, int* iteration_number);


class CoolInt{
public: //data
    

public: //functions
    // default constructor setting default 
    CoolInt();
    ~CoolInt(){};
    
    void setParams(int number_of_dims,double epsrel,double epsabs,
                   int mineval,int maxeval,int nstart,int nincrease);
    void setParams(int number_of_dims,double epsrel,double epsabs);
    
    virtual double evaluateIntegral(const double xx[]);
    void call_vegas();
    
    double result(){return _central_value[0];}
    double error(){return _vegas_error[0];}
    double central_value(){return _vector_of_xs[0]->value();}
    double mc_error_of_central_value(){return _vector_of_xs[0]->error();}
    void check_whether_we_need_to_update_bins(unsigned new_iter_number);

    void set_iteration_number(unsigned int ii){_iter_num = ii;}
    void add_to_bins(const double & ww);
    
    const vector<double>  give_vector_of_res();
    unsigned long number_of_components(){return _vector_of_xs.size();}
    double give_res_component(unsigned i);
    double give_err_component(unsigned i);

    void set_number_of_xs_values_we_keep(unsigned);
protected: //functions
    
    unsigned iteration_number(){return _iter_num;}
    void set_dimensions(int dim){_number_of_dims=dim;}
private: //data
    int _number_of_dims;
    double _central_value[1];
    double _vegas_error[1];
    double _prob[1];
    int _neval,_fail;
    int _gridno;
    int _seed;
    
    int _nbatch,_verbose;
    double _epsrel,_epsabs;
    int _mineval,_maxeval,_nstart,_nincrease;
    unsigned _iter_num;
    vector<Bin*> _vector_of_xs;
protected://data
    vector<double> _running_xs_values;
};



class AnotherInt: public CoolInt
{
public:
    AnotherInt();
    double evaluateIntegral(const double xx[]);
private:

};

#endif




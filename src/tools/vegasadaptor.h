/**
 *
 * \file    vegasadaptor.h
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2015
 *
 */

#ifndef VEGAS_ADAPTOR_H
#define VEGAS_ADAPTOR_H

#include "option.h"
#include "thehatch.h"
#include "bin.h"

#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>
#include <vector>
using namespace std;

typedef int (*pointer_to_Integrand)(
    const int* ndim, const double xx[],
    const int* ncomp, double ff[],
    void* therun, double* weight, int* iteration_number
);

struct IVegas : public OptionSet
{
    double epsrel, epsabs, epsrel_therm, epsabs_therm;
    size_t maxeval, mineval, nstart, nincrease;
    size_t maxeval_therm, mineval_therm, nstart_therm, nincrease_therm;
    bool   bin_by_bin_integration, no_grid_adaptation;
    int    verbose;
};

/// \todo this is how it should be organized, left for later because of compatibility with CHistogram

//struct IntegralOutput
//{
//    double value;
//    double error;
//    double prob;
//
//    IntegralOutput(const double& ivalue = 0., const double& ierror = 0., const double& iprob = 0.)
//    : value(ivalue), error(ierror), prob(iprob)
//    {}
//
//};

class VegasAdaptor : public IVegas
{

private:

    int _number_of_dims;
    double _epsrel_cur, _epsabs_cur;
    size_t _mineval_cur, _maxeval_cur, _nstart_cur, _nincrease_cur;
    TheHatch* the_hatch;
    pointer_to_Integrand my_integrand;
    size_t total_number_of_points_;
    string grid_file_name_;
    int gridno;

public:

    vector<double> ff; //: public because it has to be accessed by Integrand
    int number_of_components;//: public because it has to be accessed by Integrand
    vector<double> integral_output;//: public because they need to be accessed by CHistogram::print()
    vector<double> error_output;//: public because they need to be accessed by CHistogram::print()
    vector<double> prob_output;//: public because they need to be accessed by CHistogram::print()
    double weight;
    int iteration_number;
    int NOP_in_current_iteration;
    int NOP_in_previous_iteration;
    int iteration_number_old;

    VegasAdaptor(pointer_to_Integrand ptr = NULL, const size_t ndim = 0);
    void call();
    void prepare_for_final_iteration();

    void set_up_ff(double res);
    bool new_iteration_has_started();
    void flush();
    void set_ptr_to_the_hatch(TheHatch* in_hatch) {the_hatch=in_hatch;}
    void set_number_of_dimensions(int dd) {number_of_dims=dd;}
    void set_ptr_to_integrand(pointer_to_Integrand ptr) {my_integrand=ptr;}
    int total_number_of_points() const {return total_number_of_points_;}
    string xml();
    friend ostream& operator<<(ostream&, const VegasAdaptor&);
    //string iteration_info(){return iteration_info_.str();}

    // ..stringstream iteration_info_;

};

#endif

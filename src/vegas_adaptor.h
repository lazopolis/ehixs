

#ifndef VEGAS_ADAPTOR_H
#define VEGAS_ADAPTOR_H

#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>
#include <vector>
using namespace std;
#include "UserInterface.h"
#include "hub.hpp"


typedef int (*pointer_to_Integrand)(const int *ndim, const double xx[],
const int *ncomp, double ff[],void * therun, double* weight, int* iteration_number);

class VegasAdaptor{
public:
     VegasAdaptor(const UserInterface & UI);
     vector<double> ff_vegas; //: public because it has to be accessed by Integrand
     int number_of_components;//: public because it has to be accessed by Integrand
     void call_vegas();
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
     
     void set_ptr_to_the_hatch(TheHatch* in_hatch){the_hatch=in_hatch;}
     void set_ptr_to_integrand(pointer_to_Integrand ptr){my_integrand=ptr;}
     
     friend ostream& operator<<(ostream&, const VegasAdaptor&);
private:
     double epsrel,epsabs;
     int verbose,mineval,maxeval,nstart,nincrease;
     TheHatch* the_hatch;
     pointer_to_Integrand my_integrand;
     
};


#endif




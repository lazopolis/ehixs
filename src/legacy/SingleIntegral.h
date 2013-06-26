

#ifndef SINGLE_INTEGRAL_H
#define SINGLE_INTEGRAL_H



#include <iostream>
#include <iomanip>
#include <string>
#include <fstream>
#include <vector>
#include <math.h>
using namespace std;


struct VegasArgs{
     double  epsrel,epsabs ;
     int  verbose, mineval,maxeval,nstart,nincrease;

};

class SingleIntegral{
public:
     SingleIntegral(VegasArgs&);
     ~SingleIntegral(){};
     virtual void evaluate_integrand(const double xx[], double ff[])=0;
     
     friend ostream& operator<<(ostream&, const SingleIntegral&);
     void call_vegas();
     
     vector<double> vegas_integral_output;
     vector<double> vegas_error_output;
     vector<double> vegas_prob_output;
protected:
     VegasArgs myVegasArgs;
     double epsrel,epsabs;
     int verbose,mineval,maxeval,nstart,nincrease;
     vector<double> ff_vegas;
     
     
     
     
     double vegas_weight;
     int dimensions_of_integration,number_of_components;
    
};


class x1_plus_x2_squared: public SingleIntegral{
public:
     x1_plus_x2_squared(VegasArgs vv,int number_of_comps,double * LL): SingleIntegral(vv)
          {
          number_of_components=number_of_comps;
          dimensions_of_integration=2;
          lumi=LL;
          }
     void evaluate_integrand(const double xx[],double ff[])
          {
               for(int p = 0; p < number_of_components; ++p)
               {
               	ff[p]=pow(xx[0]+xx[1],2.0) * lumi[p];
               }
          }
     
private:
     double * lumi;
};


class SI_delta_0: public SingleIntegral{
public:
     SI_delta_0(VegasArgs vv,int number_of_comps,Luminosity * LL): SingleIntegral(vv)
     {
     number_of_components=number_of_comps;
     dimensions_of_integration=2;
     lumi=LL;
     }
     void evaluate_integrand(const double xx[],double ff[])
     {
     for(int p = 0; p < number_of_components; ++p)
          {
          ff[p]=pow(xx[0]+xx[1],2.0) * lumi[p];
          }
     }
     
private:
     Luminosity * lumi;
};

#endif


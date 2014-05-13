/** \file hub.hpp
  *
  * Dispatch random variables. This deserves a lot more explanations.
  *
  * It actually just deserves the following:
  * 
  *       usage:
  *
  *       x = Hub::RequestPtr();
  *       Hub::RequestVar(Hub::FLAT);
  *       Hub::RequestVar(Hub::VEGAS);
 */

#ifndef HUB_HPP
#define HUB_HPP
#define NVAR_MAX 100
namespace Hub {

  /** Types of random variables 
    *
    * VEGAS variables are sampled by the VEGAS algorithm while FLAT variables are uniform random variables in the range [0, 1], see random(). */
  enum VarType {
    VEGAS,
    FLAT
  };

  /** Request pointer to random variables
    *
    * Return a pointer to a location not already used by another part of the program. */
  double*  RequestPtr();

  /** Request random variable 
    *
    * Request a random variable of the type type() for the pointer just obtained from RequestPtr(). This function has to be called just after obtaining a pointer using RequestPtr(). */
  void     RequestVar(Hub::VarType type);

  /** Dispatch function
    *
    * Takes the VEGAS random variables array and dispatch it at the right memory place, while adding FLAT variables where necessary. */
  void     SetVars(const double*);

  /** Return number of VEGAS variables currently asked */
  unsigned GetVEGASDim();
}

#include <vector>
#include <string>
using namespace std;

class TheHatch
{
public:
     TheHatch(){number_of_vegas_variables=0;}
     /** Request pointer to random variables
      *
      * Return a pointer to a location not already used by another part of the program. */
     double*  RequestPtr();
     
     /** Request random variable 
      *
      * Request a random variable of the type type() for the pointer just obtained from RequestPtr(). This function has to be called just after obtaining a pointer using RequestPtr(). */
     void     RequestVar(string type);
     
     /** Dispatch function
      *
      * Takes the VEGAS random variables array and dispatch it at the right memory place, while adding FLAT variables where necessary. */
     void     SetVars(const double*);
     
     /** Return number of VEGAS variables currently asked */
     unsigned GetVEGASDim(){return number_of_vegas_variables;}
private:
     vector<int> types;
     double data[NVAR_MAX];
     unsigned number_of_vegas_variables;
    double open_interval(const double&);

};


#endif

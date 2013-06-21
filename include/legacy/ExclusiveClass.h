#ifndef EXCLUSIVE_H
#define EXCLUSIVE_H


//#include "CConstants.h"

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <vector>
#include <math.h>
#include <cassert>
#include <complex>
#include <cstdlib>
#include <time.h>

using namespace std;

// ==================================================
// First includes
//#include "CPDF.h"
//#include "CMonteCarlo.h"
#include "Model.h"
#include "chaplin.h"
#include "fvector.h"
#include "CConstants.h"
#include "CCut.h"
#include "Interface_to_amplitudes.h"
#include "UserInterface.h"
#include "luminosity.h"
#include "hub.hpp"
#include "Decay.h"
#include "Production.h"
#include "VegasAdaptor.h"
// ==================================================
// Forward declarations
class CHistogram;
class CHistogram2d;
class AverageObservable;
#ifndef ONCE_ROMAIN
#define ONCE_ROMAIN

// Don't look at this it might hurt
#define DECLARE_INVARIANTS \
  const double S12 = P[5];\
  const double S13 = P.s13();\
  const double S14 = P.s14();\
  const double S23 = P.s23();\
  const double S24 = P.s24();\
  const double S34 = P.s34();\
  const double S134 = P.s134();\
  const double S234 = P.s234();\
  const double S124 = P.s124();\
  const double S123 = P.s123();\
  const double mh = sqrt(P[4]*P[5])

//:
// Parametrizations
#include "parametrizations.hpp"
// Topology subtraction
#include "subtraction.hpp"
// Definition of all possible topologies
#include "topologies.hpp"
// Definition of the jet function
#include "jet.hpp"
// Creates all the Tii::Call functions you need
//#include "input.hpp"

#endif


#include "Momenta.h"







class Process
{
public:
     Process(const UserInterface& UI);
	~Process();
     
     void  perform();
     
     void  Evaluate_integral(const double xx[]);//: public because it has to be accessed by Integrand
	void set_production(Production * theproduction){my_production = theproduction;my_production->init(my_UI);}
     void set_decay(Decay * thedecay){my_decay = thedecay;my_decay->init(my_UI);}
     VegasAdaptor Vegas;
     
	
private:
     UserInterface my_UI;
     double* xx_vegas;
     void  book_histograms( Event *);
     void book_null_event();
     void book_event(Event *);
     void proceed_to_production_phase();
     void proceed_to_decay_phase(Event*);
     void book_decay_event(Event *);
     void print_output_intermediate();
     void  calculate_number_of_components();
     void  print_output();
     string input_filename,output_filename;
	//
	
	//
	
	//:
     int perturbative_order;
	int dimension_of_integration;
	void calculate_dimension_of_integration();
     //: histograms
     void  setup_histograms();
     void update_histograms_end_of_iteration(int NOP);
	void update_histograms_end_of_vegas_point();
     vector<CHistogram*> histogram_vector;
     vector<CHistogram2d*>  histogram2d_vector;
     vector<AverageObservable*> average_observable_vector;
     
     //: cuts 
     void  setup_cuts();
	vector<CCut*> production_cuts;
	vector<CCut*> decay_cuts;
     
     Decay *my_decay;
     Production* my_production;
};






// ==================================================
// Last includes
#include "CBin.h"
#include "CHistogram.h"

#endif

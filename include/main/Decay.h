#ifndef DECAY_H
#define DECAY_H

#include <string>
#include <complex>
using namespace std;

#include "UserInterface.h"
#include "fvector.h"
#include "CConstants.h"
#include "hub.hpp"
#include "Momenta.h"
#include "Model.h"
#include "chaplin.h"

/** \brief Decay base class 
 It takes care of decay vegas variables, 
 it hosts the decay_events vector of pointers,
 and the generic PSP routine.
 It does not know about sectors, so sector disambiguation should be implemented within init(), which is called at the end of the ExclusiveClass constructor
 To inherit from it you need to define 
     a void init(CModel&); that at least sets up the dimensionality of integration 
     and a void do_decay(Momenta&); which computes the decay weight and fills up the decay_events with events.
 
 */
class Decay
{
public:
     Decay(){my_sector_name = "If you see this, the specific decay hasn't declared it's sector_name yet.";sector_defined=false;}
     //double  decay_WW();
     
	
	 
     //Momenta decay_momenta;
     //CModel Model;
     //string decay_mode;
     virtual void init(const UserInterface&,TheHatch*)=0;
     virtual void do_decay(const fvector& PH)=0;
     virtual void do_decay()=0;//: default decay at rest frame
     vector<Event*> decay_events;
     int dimension_of_integration(){return dimension_of_integration_for_decay;}
     string sector_name(){return my_sector_name;}
     void set_alpha_s(vector<double> a_s_in){alpha_s=a_s_in;}
     void set_y_b(vector<double> yb_in){y_b=yb_in;}
     CModel Model;
     bool is_sector_defined(){return sector_defined;}

protected:
     string decay_mode;
     double* decay_xx_vegas;
     Momenta my_momenta;
     string my_sector_name;
     void setup_hatch(TheHatch*);
     double  One_to_two_PSP(const fvector & ,const double & ,const double & ,
                            fvector & ,fvector & ,
                            const double & ,const double & );
     double  PSP_lambda(const double & ,const double & ,const double & );
     int dimension_of_integration_for_decay;
     vector<double> alpha_s;
     vector<double> y_b;
     bool sector_defined;

};





class Decay_gammagamma: public Decay
{
public:
     void do_decay(const fvector& PH);
     void do_decay();
     void init(const UserInterface&,TheHatch* );
private:
     void perform();
     complex<double> born_for_gamma_gamma(complex<double> x);
     complex<double> bosonloop(complex<double> x);
};


#endif


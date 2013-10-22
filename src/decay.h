#ifndef DECAY_H
#define DECAY_H

#include <string>
#include <complex>
using namespace std;

#include "user_interface.h"
#include "fvector.h"
#include "constants.h"
#include "hub.hpp"
#include "momenta.h"
#include "model.h"
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
     virtual void do_decay(const fvector& PH)=0;
     virtual void do_decay()=0;//: default decay at rest frame
     vector<Event*> decay_events;
     int dimension_of_integration(){return dimension_of_integration_for_decay;}
     string sector_name(){return my_sector_name;}
     void set_alpha_s(double a_s_in){alpha_s=a_s_in;}
     void set_y_b(double yb_in){y_b=yb_in;}
     CModel Model;
     bool is_sector_defined(){return sector_defined;}
    void set_up_the_hatch(TheHatch*);

protected:
     string decay_mode;
     double* decay_xx_vegas;
     Momenta my_momenta;
     string my_sector_name;
     double  One_to_two_PSP(const fvector & ,const double & ,const double & ,
                            fvector & ,fvector & ,
                            const double & ,const double & );
     double  PSP_lambda(const double & ,const double & ,const double & );
     int dimension_of_integration_for_decay;
     double alpha_s;
     double y_b;
     bool sector_defined;

};

//=================== WW/ZZ ===============================

class Decay_WWZZ: public Decay
{
public:
    void do_decay(const fvector& PH);
    void do_decay();
    Decay_WWZZ(const UserInterface&);
private:
    void perform();
    complex<double> born_for_gamma_gamma(complex<double> x);
    complex<double> bosonloop(complex<double> x);
};


//=================== gamma gamma ===============================

class Decay_gammagamma: public Decay
{
public:
     void do_decay(const fvector& PH);
     void do_decay();
     Decay_gammagamma(const UserInterface&);
private:
     void perform();
     complex<double> born_for_gamma_gamma(complex<double> x);
     complex<double> bosonloop(complex<double> x);
};


#endif


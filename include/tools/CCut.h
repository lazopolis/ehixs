/** \file CCut.h  */

#ifndef CCUT_H
#define CCUT_H

#include "Momenta.h"

/** \brief Base class for cuts.
  *
  * This is basically just a callable object that should perform a test on a certain kinematical configuration of ExclusiveClass. */
class CCut
{
  /** \brief Cut function
    *
    * This function performs the cut, i.e. return true or false depending on whether the kinematics has passed the cut. */
public:
	virtual bool operator()(Event *) = 0;
     CCut(string _name, const double& _v){name=_name;min=_v;}
     string name;
     string give_name(){return name;}
     void set_value(const double & _v){min=_v;}
     double min;
     string info(){stringstream stream;stream<<name<<" : "<<min;return stream.str();}
};

#endif

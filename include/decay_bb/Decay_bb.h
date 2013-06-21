#ifndef DECAY_BB
#define DECAY_BB

#include "Decay.h"
#include "Sector.h"
//#include <string>
//#include <complex>
#include <iostream>
using namespace std;

//#include "UserInterface.h"
//#include "fvector.h"
//#include "CConstants.h"
//#include "hub.hpp"
//#include "Momenta.h"
//#include "Model.h"
//#include "chaplin.h"

class Decay_bb;

typedef void (Decay_bb::*ptr_to_Decay_bb_function)();


#include "Sector.h"


class Decay_bb: public Decay
{
public:
     Decay_bb();
     void do_decay(const fvector& );
     void do_decay();//: default rest frame decay
     void init(const UserInterface&,TheHatch *);
     int give_sector(){return my_sector;}
     
     void set_yb(double in_yb){yb=in_yb;}
private:
     void perform();
     vector<DecaySector<ptr_to_Decay_bb_function> *> sectors;
     int my_sector;
     void LO();
     void NLOV();
     void set_LO_momenta();
     double yb;
     fvector PH;
     
};


#endif
#ifndef DECAY_BB
#define DECAY_BB

#include "decay.h"
#include "sector.h"

#include <iostream>
using namespace std;



class Decay_bb;

typedef void (Decay_bb::*ptr_to_Decay_bb_function)();




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
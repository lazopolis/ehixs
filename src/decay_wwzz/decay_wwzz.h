#ifndef DECAY_H_TO_WWZZ
#define DECAY_H_TO_WWZZ

#include "decay.h"

class Decay_WWZZ: public Decay
{
public:
    void do_decay(double* PH);
    void do_decay();
    Decay_WWZZ(const UserInterface&);
    int NumberOfParticles(){return 4;}
private:
    void perform();
    int decay_mode_;
};

#endif

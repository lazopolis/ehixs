#ifndef DECAY_H_TO_Z_GAMMA
#define DECAY_H_TO_Z_GAMMA

#include "decay.h"

class Decay_H_to_Z_Gamma: public Decay
{
public:
    void do_decay(double* PH);
    void do_decay();
    Decay_H_to_Z_Gamma(const UserInterface&);
    int NumberOfParticles(){return 4;}
private:
    void perform();
    int decay_mode_;
};



#endif
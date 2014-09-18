#ifndef DECAY_H_TO_Z_GAMMA
#define DECAY_H_TO_Z_GAMMA

#include "decay.h"

/// \todo Review const-correctness of decay!!!

class Decay_H_to_Z_Gamma: public Decay
{
public:
    void do_decay(FourVector PH);
    void do_decay();
    Decay_H_to_Z_Gamma(const UserInterface&);
    int NumberOfParticles(){return 4;}
private:
    void perform();
    int decay_mode_;
};



#endif
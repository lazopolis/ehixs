#ifndef GSTAR2_AMPLITUDE_H
#define GSTAR2_AMPLITUDE_H

#include "kinematic_invariants.h"

class Gstar2Amplitude{
public:
    double born(const KinematicInvariants& kk);
    double born_e(const KinematicInvariants& kk);
    double born_e2(const KinematicInvariants& kk);
    double born_e3(const KinematicInvariants& kk);
    
    double R(const KinematicInvariants& kk);
    double RR (const KinematicInvariants & kk);
    
};

#endif
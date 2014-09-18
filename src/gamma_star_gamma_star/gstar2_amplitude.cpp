#include "gstar2_amplitude.h"


#include "math.h" /* pow */

double Gstar2Amplitude::born(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double q13 = kk.q(1,3);
    const double q23 = kk.q(2,3);
    
    
    return 8.0*3.0*(
                    q23/q13 - 2.0 * (q3+q4)/q13
                    - q3*q4/q13/q13
                    +q13/q23 - 2.0 * (q3+q4)/q23
                    - q3*q4/q23/q23
                    +2.0*pow(q3+q4,2.0)/ (q13*q23)
                    );
}

double Gstar2Amplitude::born_e(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double t = kk.q(1,3);
    const double u = kk.q(2,3);
    
    
    return 48.0*(-q3*q3/(t*u)-q4*q4/(t*u)
                 +q3*q4/pow(u,2.0)-q3*q4/(t*u)+q3*q4/pow(t,2.0)
                 +q3/u+q3/t+q4/u+q4/t
                 -t/u-1.0-u/t );
}


double Gstar2Amplitude::born_e2(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double t = kk.q(1,3);
    const double u = kk.q(2,3);
    
    return 24.0*(-q3*q4/pow(u,2.0)
                 -2.0*q3*q4/(t*u)-q3*q4/pow(t,2.0)+t/u+2.0+u/t);
}

double Gstar2Amplitude::born_e3(const KinematicInvariants& kk)
{
    return 0.0;
}



#include "real.cpp"
#include "double_real.cpp"



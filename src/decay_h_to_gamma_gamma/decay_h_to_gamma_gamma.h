#ifndef DECAY_HIGGS_TO_GAMMA_GAMMA
#define DECAY_HIGGS_TO_GAMMA_GAMMA

/// \todo Factor out the common structure of all do_decay functions!!!
///       (It is repeated at least 3 times!)

#include "decay.h"

class Decay_gammagamma: public Decay
{
public:
    void do_decay(FourVector PH);
    void do_decay();
    Decay_gammagamma(const UserInterface&);
    int NumberOfParticles(){return 2;}
private:
    void perform();
    int decay_mode_;
};



/*
#include "event.h"
#include "decay.h"
//=================== gamma gamma ===============================

//class HiggsToGammaGammaEvent: public Event
//{
//public:
//    HiggsToGammaGammaEvent(const double& weight,
//                           FourMomentum* gamma1,FourMomentum* gamma2)
//    :Event(weight),gamma1_(gamma1),gamma2_(gamma2){};
//    FourMomentum* gamma1(){return gamma1_;}
//    FourMomentum* gamma2(){return gamma1_;}
//    
//private:
//    FourMomentum* gamma1_;
//    FourMomentum* gamma2_;
//    
//    
//};
//

class Decay_gammagamma: public Decay
{
public:
    void do_decay(double* PH);
    void do_decay();
    Decay_gammagamma(const UserInterface&);
    int NumberOfParticles(){return 2;}
private:
    void perform();
    complex<double> born_for_gamma_gamma(complex<double> x);
    complex<double> bosonloop(complex<double> x);
    double  One_to_two_PSP(double* ,const double & ,const double & ,
                           double*  ,double*  ,
                           const double & ,const double & );
    double  PSP_lambda(const double & ,const double & ,const double & );
};
*/

#endif
#ifndef GLUON_FUSION_EVENT_RECONSTRUCTOR
#define GLUON_FUSION_EVENT_RECONSTRUCTOR
#include "event.h"

class EventReconstructor{
public:
    void Configure(const double& Etotal,EventBox* evbox,const double& mh);
    void  NNLO_event_kinematics(const double& sigma,
                                const double & x1,
                                const double & x2,
                                const double & z,
                                const double & s13,
                                const double & s23,
                                const double & s14,
                                const double & s24,
                                const double & s34,
                                const double & phi);
    void LO_event_kinematics(const double& sigma,const double & x1,const double & x2);
    void NLO_event_kinematics(const double& sigma,const double & x1,
                              const double & x2,
                              const double & z,
                              const double & s13,
                              const double & s23,
                              const double & phi);
private:
    double Etot;
    EventBox* event_box;
    double mh_;
    double ptbuf;
};


#endif
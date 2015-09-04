#ifndef GAMMAGAMMA_CUTS_H
#define GAMMAGAMMA_CUTS_H

#include "cut.h"

/// \todo Review this, I think it's plain wrong

// In order for the cross section to be finite,
// it is necessary to ensure that the two photons
// be isolated from each other and from the beam.

// Isolation from hadronic radiation is also a problem
// but it's taken care of by the measurement function

// Cut on the invariant mass of the two gammas
class GammaGammaCut_Q2 : public CCut
{
public:
    GammaGammaCut_Q2(const double& cutvalue) :
    CCut("GammaGamma_Q2",cutvalue)
    {};

    bool operator()(Event* the_event)
    {
        if (square(the_event->p[3]+the_event->p[4])>min) return true;
        return false;
    }
};

// Cut on the transverse momentum of the hardest photon
class GammaGammaCut_pT1 : public CCut
{
public:
    GammaGammaCut_pT1(const double& cutvalue) :
    CCut("GammaGamma_pT1",cutvalue)
    {};

    bool operator()(Event* the_event)
    {
        if (max(the_event->p[3].T(),the_event->p[4].T())>min) return true;
        return false;
    }
};

// Cut on the transverse momentum of the softest photon
class GammaGammaCut_pT2 : public CCut
{
public:
    GammaGammaCut_pT2(const double& cutvalue) :
    CCut("GammaGamma_pT2",cutvalue)
    {};

    bool operator()(Event* the_event)
    {
        if (std::min(the_event->p[3].T(),the_event->p[4].T())>min) return true;
        return false;
    }
};

// Cut on the transverse momentum of the jet
class GammaGammaCut_pT1jet : public CCut
{
public:
    GammaGammaCut_pT1jet(const double& cutvalue) :
    CCut("GammaGamma_pT1jet",cutvalue)
    {};

    bool operator()(Event* the_event)
    {
        if (the_event->p[5].T()>min) return true;
        return false;
    }
};


#endif
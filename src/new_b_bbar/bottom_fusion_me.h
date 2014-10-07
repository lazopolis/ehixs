#ifndef BOTTOM_FUSION_ME_H
#define BOTTOM_FUSION_ME_H

#include <stdlib.h>
#include "xsectionmaker.h"
#include "parametrizations.h"
#include "xgenerator.h"

/// \todo Move this to either Constants, UserInterface, Model or whatever
constexpr double yukawa_bottom = 1.0;

template<size_t N>
class BottomFusion_bb;

/**
 *
 * \class BottomFusion_bb<0>
 * \brief Mother class for subprocesses with bbar initial state and delta-like kinematics
 *
 */

template<>
class BottomFusion_bb<0> : public XSection
{

public:

    BottomFusion_bb<0>(const UserInterface& UI, const SectorInfo& info) :
    XSection(UI, info), _p(), _xg(_x), _pg(_p, _x), _tau(pow(UI.m_higgs/UI.Etot,2))
    {
        _p.resize(3);
        _prefactor *= consts::Pi * pow(yukawa_bottom,2) / (2. * QCD::Nc * pow(UI.m_higgs,2));
        _xg.setParameters(_tau);
        _pg.setParameters(UI.Etot*UI.Etot, vector<double>({UI.m_higgs}));
        return;
    }

    double generateXs(vector<double>& randoms)
    {
        return _xg(randoms);
    }

protected:

    Momenta _p;
    OneXGenerator _xg;
    DeltaPG _pg;
    double _tau;

};

/**
 *
 * \class BottomFusion_bb<1>
 * \brief Mother class for subprocesses with bbar initial state and one extra particle in the final state
 *
 */

template<>
class BottomFusion_bb<1> : public XSection
{

public:

    BottomFusion_bb<1>(const UserInterface& UI, const SectorInfo& info) :
    XSection(UI, info), _p(), _xg(_x), _pg(_p, _x), _tau(pow(UI.m_higgs/UI.Etot,2))
    {
        _p.resize(4);
        _prefactor *= consts::Pi * pow(yukawa_bottom,2) / (2. * QCD::Nc * pow(UI.m_higgs,2));
        _xg.setParameters(_tau);
        _pg.setParameters(UI.Etot*UI.Etot, vector<double>({UI.m_higgs}));
        return;
    }

    double generateXs(vector<double>& randoms)
    {
        return _xg(randoms);
    }

protected:

    Momenta _p;
    TwoXGenerator _xg;
    ZlambdaPG _pg;
    double _tau;

};

/**
 *
 * \class BottomFusion_bb_LO
 * \brief LO sector for bbar->H
 *
 */

class BottomFusion_bb_LO : public BottomFusion_bb<0>
{

public:


    BottomFusion_bb_LO(const UserInterface& UI) :
    BottomFusion_bb<0>(UI, XSectionMaker<BottomFusion_bb_LO>::_info)
    {}

    void generateEvents(vector<double>& randoms);

};

/**
 *
 * \class BottomFusion_bb_NLO_real
 * \brief LO sector for bbar->H
 *
 */

class BottomFusion_bb_NLO_real : public BottomFusion_bb<1>
{

public:


    BottomFusion_bb_NLO_real(const UserInterface& UI) :
    BottomFusion_bb<1>(UI, XSectionMaker<BottomFusion_bb_NLO_real>::_info)
    {
        _prefactor *= 8. * consts::Pi * QCD::CF /*alphas*/;
        return;
    }

    void generateEvents(vector<double>& randoms);

private:

    double regularME(const double& z) const
    {
        return (1.+z*z)/(1.-z)/(1.-z);
    }
    
};

/**
 *
 * \class BottomFusion_bb_NLO_soft
 * \brief NLO soft sector for bbar->H
 * \todo  Divide stuff between soft and collinear sectors
 *
 */

class BottomFusion_bb_NLO_soft : public BottomFusion_bb<0>
{

public:


    BottomFusion_bb_NLO_soft(const UserInterface& UI) :
    BottomFusion_bb<0>(UI, XSectionMaker<BottomFusion_bb_NLO_soft>::_info)
    {
        _prefactor *= 2. * QCD::CF /*alphas*/;
        return;
    }

    void generateEvents(vector<double>& randoms);

private:

    double f0(const double& z) const
    {
        const double zbar = 1.-z;
        return zbar*zbar + log(z);
    }

    double f1(const double& z) const
    {
        return 2.*(1.+z*z);
    }

};



#endif

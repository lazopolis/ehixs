/**
 *
 * \file    gammagamma_me.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#ifndef GAMMAGAMMA_ME_H
#define GAMMAGAMMA_ME_H

#include <stdlib.h>
#include "qq2gammagammaX.h"
#include "xsectionmaker.h"
#include "parametrizations.h"
#include "xgenerator.h"
#include "counterforge.h"

template<size_t N>
class GammaGamma_qq;

/**
 *
 * \class GammaGamma_qq<0>
 * \brief Mother class for subprocesses with qqbar initial state and delta-like kinematics
 *
 */

template<>
class GammaGamma_qq<0> : public XSection
{

public:

    GammaGamma_qq<0>(const UserInterface& UI, const SectorInfo& info) :
    XSection(UI, info), _p(), _xg(_x), _pg(_p, _x), _tau(pow(2.*UI.m_higgs/UI.Etot,2))
    {
        _p.resize(3);
        _prefactor *= consts::Pi * pow(alpha,2) / (2. * QCD::Nc * pow(UI.m_higgs,2));
        _xg.setParameters(_tau);
        _pg.setParameters(UI.Etot*UI.Etot*0.25, vector<double>({UI.m_higgs}));
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
 * \class GammaGamma_qq<1>
 * \brief Mother class for subprocesses with qqbar initial state and one extra particle in the final state
 *
 */

template<>
class GammaGamma_qq<1> : public XSection
{

public:

    GammaGamma_qq<1>(const UserInterface& UI, const SectorInfo& info) :
    XSection(UI, info), _p(), _xg(_x), _pg(_p, _x), _tau(pow(2.*UI.m_higgs/UI.Etot,2))
    {
        _p.resize(4);
        cout << "\nmH:\t" << UI.m_higgs << "\nEtot:\t" << UI.Etot << "\ntau:\t" << _tau << endl;
        _prefactor *= consts::Pi * pow(alpha,2) / (2. * QCD::Nc * pow(UI.m_higgs,2));
        _xg.setParameters(_tau);
        _pg.setParameters(UI.Etot*UI.Etot*0.25, vector<double>({UI.m_higgs}));
        return;
    }

    double generateXs(vector<double>& randoms)
    {
        return _xg(randoms);
    }

protected:

    Momenta _p;
    FlatXGenerator _xg;
    ZlambdaPG _pg;
    double _tau;

};

/**
 *
 * \class GammaGamma_qq_LO
 * \brief LO sector for qqbar->gammagamma
 *
 */

class GammaGamma_qq_LO : public GammaGamma_qq<0>
{

public:


    GammaGamma_qq_LO(const UserInterface& UI) :
    GammaGamma_qq<0>(UI, XSectionMaker<GammaGamma_qq_LO>::_info)
    {}

    void generateEvents(vector<double>& randoms);

};

#endif

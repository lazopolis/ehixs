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

#include "qq2gammagammaX.h"
#include "xsectionmaker.h"
#include "parametrizations.h"
#include "isolationcone.h"
#include "xgenerator.h"
#include "counterforge.h"
#include <stdlib.h>
#include <random>
#include <chrono>

template<size_t N>
class GammaGamma_qq;

/**
 *
 * \class GammaGamma_qq<0>
 * \brief Mother class for subprocesses with qqbar initial state and 2->2 kinematics
 *
 */

template<>
class GammaGamma_qq<0> : public XSection
{

public:

    GammaGamma_qq<0>(const UserInterface& UI, const SectorInfo& info) :
    XSection(UI, info), _p(), _xg(_x), _pg(_p, _x)
    {
        _p.resize(4);
        _prefactor *= 2. * pow(alpha,2) / static_cast<double>(QCD::Nc);
        _xg.setParameters(0.);
        _pg.setParameters(UI.Etot*UI.Etot*0.25, vector<double>({0.,0.}));
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
    XSection(UI, info), _p(), _xg(_x), _pg(_p, _x), _cone(1.), _lambda(0.5)
    {
        _p.resize(5);
        _prefactor *= 2. * pow(alpha,2) / static_cast<double>(QCD::Nc);
        _xg.setParameters(0.);
        _pg.setParameters(UI.Etot*UI.Etot*0.25, vector<double>({0.,0.}));
        return;
    }

    double generateXs(vector<double>& randoms)
    {
        return _xg(randoms);
    }

protected:

    Momenta _p;
    FlatXGenerator _xg;
    Zlambda3PG _pg;
    SmoothPhotonIsolation _cone;
    double _lambda;

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

/**
 *
 * \class GammaGamma_qq_NLO_real
 * \brief NLO hard real emission sector for qqbar->gammagammag
 *
 */

class GammaGamma_qq_NLO_real : public GammaGamma_qq<1>
{

public:


    GammaGamma_qq_NLO_real(const UserInterface& UI) :
    GammaGamma_qq<1>(UI, XSectionMaker<GammaGamma_qq_NLO_real>::_info)
    {}

    void generateEvents(vector<double>& randoms);
    void test(vector<double>& randoms);

};

/**
 *
 * \class GammaGamma_qq_NNLO_RV
 * \brief NLO hard real emission sector for qqbar->gammagammag
 *
 */

class GammaGamma_qq_NNLO_RV : public GammaGamma_qq<1>
{

public:


    GammaGamma_qq_NNLO_RV(const UserInterface& UI) :
    GammaGamma_qq<1>(UI, XSectionMaker<GammaGamma_qq_NLO_real>::_info), _discard(2)
    {}

    void generateEvents(vector<double>& randoms);
    void test(vector<double>& randoms);

private:

    size_t _discard;
    bool _cut(const bool verbose = false) const;
    bool _tech_cut(const bool verbose = false) const;
    static double _coll(
                 const double& z,
                 const double& lambda,
                 const double& ratio,
                 const bool LCf = true,
                 const bool SCf = true
                 );
    static double _coll1(
                 const double& z,
                 const double& lambda,
                 const double& ratio,
                 const bool LCf = true,
                 const bool SCf = true
                 );
    static double _coll2(
                  const double& z,
                  const double& lambda,
                  const double& ratio,
                  const bool LCf = true,
                  const bool SCf = true
                  );
    static double _fullsoft(
                     const double& z,
                     const double& lambda,
                     const double& ratio,
                     const bool LCf = true,
                     const bool SCf = true
                     );
    static double _fullsoft1(
                      const double& z,
                      const double& lambda,
                      const double& ratio,
                      const bool LCf = true,
                      const bool SCf = true
                      );
    static double _fullsoft2(
                      const double& z,
                      const double& lambda,
                      const double& ratio,
                      const bool LCf = true,
                      const bool SCf = true
                      );
    double _fullsoftcoll(const double& z, const double& lambda, const double& ratio) const;
    double _soft(const double& z, const double& lambda, const double& ratio) const;
    double _softcoll(const double& z, const double& lambda, const double& ratio) const;

};

#endif

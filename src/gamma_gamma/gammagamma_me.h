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
 * \fn    Rdist
 * \brief Distance in eta-phi plane of two particles
 *
 */

double Rdist(const FourVector& p1, const FourVector& p2);

/**
 *
 * \class SmoothPhotonIsolation
 * \brief This class implements an smooth isolation cone as in arXiv:9801442
 *
 */

class SmoothPhotonIsolation
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    SmoothPhotonIsolation(const double& delta0 = 0.5, const double& n = 1., const double& epsilongamma = 1.) :
    _delta0(delta0), _n(n), _epsilongamma(epsilongamma)
    {}

    /// Destructor
    ~SmoothPhotonIsolation()
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Criterion
    /// \note 1 parton only, needs to be rewritten for more (overload const Momenta& as 2nd arg?)
    bool inside(const FourVector& pgamma, const FourVector& pparton) const
    {
        const double Rig = Rdist(pgamma,pparton);
        return Rig<_delta0 && pparton[0]<Chi(pgamma[0],Rig);
    }

    /// @}
    
private:

    /// \name Data members
    /// @{

    const double _delta0;       ///< Cone angle
    const double _n;            ///< Exponent
    const double _epsilongamma; ///< Energy fraction

    /// @}

    /// \name Auxiliary functions
    /// @{

    /// This is Frixione's Chi function eq. (3.4)
    /// Modified to contain the minimum in eq. (3.10)
    double Chi(const double& Egamma, const double& delta) const
    {
        if (delta<_delta0) return Egamma*_epsilongamma;
        return Egamma*_epsilongamma*pow((1.-cos(delta))/(1.-cos(_delta0)),_n);
    }

    /// @}

};

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
    XSection(UI, info), _p(), _xg(_x), _pg(_p, _x)
    {
        _p.resize(4);
        _prefactor *= 2. * pow(alpha,2) / static_cast<double>(QCD::Nc);
        _xg.setParameters(0.);
        _pg.setParameters(UI.Etot*UI.Etot*0.25, vector<double>({0./*,0.*/}));
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
        _prefactor *= 2. * pow(alpha,2) /** alphas_pi*/ / static_cast<double>(QCD::Nc);
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
    GammaGamma_qq<1>(UI, XSectionMaker<GammaGamma_qq_NLO_real>::_info), _hackIsFirstEvent(true)
    {}

    void generateEvents(vector<double>& randoms);
    void test(vector<double>& randoms);

private:

    bool _hackIsFirstEvent;
    
};

#endif

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
    GammaGamma_qq<1>(UI, XSectionMaker<GammaGamma_qq_NLO_real>::_info), _hackIsFirstEvent(true)
    {
        cout << "\nStarting phase-space scan to switch between double and quadruple." << endl;

        // Initializing own random number generator
        size_t seed = chrono::system_clock::now().time_since_epoch().count();
        default_random_engine generator(static_cast<unsigned int>(seed));
        uniform_real_distribution<double> distribution(0.,1.);

        vector<double> rands(2,1.);
        _xg(rands);
        rands.resize(5);
        rands[2] = 0.0; // global phi: irrelevant
        for (size_t i = 0; i<100; ++i)
        {
            // Generating born phase-space randomly
            rands[0] = distribution(generator); // phi_gamma
            rands[1] = distribution(generator); // cos_theta_gamma
            // Scanning real emission phase space
            for (double zbar = 0.5; zbar > 1.e-8; zbar /= 1.5)
            {
                cout << "zbar = " << zbar << endl;
                rands[3] = 1.-zbar;
                for (double lambda_safe = 0.5; lambda_safe > _lambda_quad; lambda_safe /= 1.5)
                {
                    cout << "lambda = " << lambda_safe << endl;
                    rands[4] = lambda_safe;
//                    for (vector<double>::const_iterator it = rands.begin(); it != rands.end(); ++it)
//                        cout << *it << "\t";
                    cout << endl;
                    _pg(rands);
//                    for (Momenta::const_iterator it = _p.begin(); it != _p.end(); ++it)
//                        cout << *it << "\n";
                    // Computing invariants
                    double s12 = square(_p[1]+_p[2]);
                    const double s13 = square(_p[1]-_p[3])/s12;
                    const double s14 = square(_p[1]-_p[4])/s12;
                    const double s23 = square(_p[2]-_p[3])/s12;
                    const double s24 = square(_p[2]-_p[4])/s12;
                    const __float128 s13q = s13;
                    const __float128 s14q = s14;
                    const __float128 s23q = s23;
                    const __float128 s24q = s24;
                    const double qq2yygd1 = qq2yygz1col(s13,s14,s23,s24);
                    const double qq2yygq1 = qq2yygz1col(s13q,s14q,s23q,s24q);
                    const double scaledreldiff1 = (
                                                  zbar*lambda_safe *
                                                  abs(qq2yygd1-qq2yygq1) /
                                                  (abs(qq2yygd1)+abs(qq2yygq1))
                                                  );
                    cout << qq2yygd1 << "\t" << qq2yygq1 << "\t" << scaledreldiff1 << endl;
                    if ( !_cut() && !(scaledreldiff1<_tolerance) )
                        _lambda_quad = lambda_safe;
                    rands[4] = 1.-lambda_safe;
                    _pg(rands);
                    const double qq2yygd2 = qq2yygz1col(s23,s24,s13,s14);
                    const double qq2yygq2 = qq2yygz1col(s23q,s24q,s13q,s14q);
                    const double scaledreldiff2 = (
                                                   zbar*(1.-lambda_safe) *
                                                   abs(qq2yygd2-qq2yygq2) /
                                                   (abs(qq2yygd2)+abs(qq2yygq2))
                                                   );
                    cout << qq2yygd2 << "\t" << qq2yygq2 << "\t" << scaledreldiff2 << endl;
                    if ( !_cut() && !(scaledreldiff2<_tolerance) )
                        _lambda_quad = lambda_safe;
                }
            }
        }
        cout << "\nThe switch between double and quadruple precision for lambda is " << _lambda_quad << endl;
    }

    void generateEvents(vector<double>& randoms);
    void test(vector<double>& randoms);

private:

    bool _hackIsFirstEvent;
    bool _cut(const bool verbose = false) const;
    double _lambda_quad = 0.;
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

    const double _lambda_tech_cutoff = 1.e-16;
    const double _delta = 2.5e-3;
    const double _tolerance = 1.e-4;
    size_t _i = 0;

};

#endif

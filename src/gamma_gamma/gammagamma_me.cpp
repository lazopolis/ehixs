/**
 *
 * \file    gammagamma_me.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#include "gammagamma_me.h"
#include <iostream>

/// \fn    Rdist

double Rdist(const FourVector& p1, const FourVector& p2)
{
    return sqrt(pow(p1.eta()-p2.eta(),2)+pow(p1.phi()-p2.phi(),2));
}

/// \class GammaGamma_qq_LO

void GammaGamma_qq_LO::generateEvents(vector<double>& randoms)
{
    _pg(randoms);
    const double w = _prefactor * _factor;
    _eventBox->push_back(
                         Event(
                               w*qq2gammagamma<0,0>(square(_p[1]-_p[3])/square(_p[1]-_p[4])),
                               _p
                               )
                         );
    return;
}

template<>
const SectorInfo XSectionMaker<GammaGamma_qq_LO>::_info(
                                                   "Born",
                                                    InitialStateFlavors::quarks,
                                                    0,
                                                    4
                                                        );

/// \class GammaGamma_qq_NLO_real

void GammaGamma_qq_NLO_real::generateEvents(vector<double>& randoms)
{
    // Defining auxiliary names
    double& lambdaR = randoms.back();
    const double lambda = randoms.back();
    const double& z = randoms[3];
    // Generating momenta
    const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
    // Photon isolation criterion: measurement function
    if (_cone.inside(_p[3],_p[5])||_cone.inside(_p[4],_p[5])||z*square(_p[1]+_p[2])<20.||_p[3].T()<20.||_p[4].T()<20.)
    {
        _eventBox->push_back(Event(0.,_p));
        return;
    } else {
    // Pushing back main event
    _eventBox->push_back(Event(
                                w*qq2gammagammag<0,0>(
                                                     square(_p[1]+_p[2]),
                                                     square(_p[1]-_p[3]),
                                                     square(_p[1]-_p[4]),
                                                     square(_p[2]-_p[3]),
                                                     square(_p[2]-_p[4])
                                                     ),
                               _p
                               ));
    // Pushing back collinear counterterms
    lambdaR = 0.;
    const double cw = _prefactor * _factor * (CounterForge::Pqq<0>(z)).getCoefficient(0) * _pg(randoms);
    _eventBox->push_back(Event(
                               -cw*qq2gammagamma<0,0>(square(_p[1]-_p[3])/square(_p[1]-_p[4]))/
                               (z*square(_p[1]+_p[2])*lambda),
                               _p
                               ));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(
                               -cw*qq2gammagamma<0,0>(square(_p[2]-_p[3])/square(_p[2]-_p[4]))/
                               (z*square(_p[1]+_p[2])*(1.-lambda)),
                               _p
                               ));
    randoms.clear();
    return;
    }
}

template<>
const SectorInfo XSectionMaker<GammaGamma_qq_NLO_real>::_info(
                                                        "NLO real",
                                                        InitialStateFlavors::quarks,
                                                        1,
                                                        7
                                                        );

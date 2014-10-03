/**
 *
 * \file    parametrizations.cpp
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "parametrizations.h"

/// \class ZlambdaPG

/// Sets the intermediate variable _tau for computational speed gain
void ZlambdaPG::computeConstants()
{
    // Check number of masses passed to PGenerator::setParameters
    if (_m.size() != 1)
    {
        cerr << "[ZlambdaPG] Error: a wrong number of masses arrived at the momenta generator.\n";
        exit(1);
    }
    _tau = _m[0]*_m[0]/(4.*_E*_E);
    return;
}

/// Implements the parametrization
double ZlambdaPG::generateFSMomenta(vector<double>& randoms) const
{
    const double phi = 2.*consts::Pi*randoms.back();
    randoms.pop_back();
    const double lambda = randoms.back();
    randoms.pop_back();
    const double z = _tau / (x1*x2);
    const double sllbar = sqrt(lambda*(1.-lambda))*x1*x2*_E;
    _p[4] = (1.-z)*(
                    (1.-lambda) * _p[1] +
                    lambda * _p[2] +
                    sllbar * LightCone::eperp(phi)
                    );
    _p[3] = _p[1] + _p[2] - _p[4];
    //jacobian = 1
    return 1.;
}

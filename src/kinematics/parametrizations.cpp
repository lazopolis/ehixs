/**
 *
 * \file    parametrizations.cpp
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "parametrizations.h"

/// \class PGenerator

/// \name Member functions

/// Sets the parameters needed for generation
void PGenerator::setParameters(const double& S, const vector<double>& masses)
{
    _E = sqrt(S)/2.;
    _m = masses;
    computeConstants();
    return;
}

/// Generates the first two momenta that are common for all processes,
/// then passes the job on to generateFSMomenta
double PGenerator::operator()(const double* const randoms) const
{
    _p[1] = x1 * _E * LightCone::n;
    _p[2] = x2 * _E * LightCone::nbar;
    return generateFSMomenta(randoms);
};

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
double ZlambdaPG::generateFSMomenta(const double* const randoms) const
{
    const double phi = 2.*consts::Pi*randoms[0];
    const double lambda = randoms[1];
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

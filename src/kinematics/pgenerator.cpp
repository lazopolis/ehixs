/**
 *
 * \file    pgenerator.cpp
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "pgenerator.h"

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
double PGenerator::operator()(vector<double>& randoms) const
{
    _p[1] = x1 * _E * LightCone::n;
    _p[2] = x2 * _E * LightCone::nbar;
    return generateFSMomenta(randoms);
};

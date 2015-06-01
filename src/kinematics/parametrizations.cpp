/**
 *
 * \file    parametrizations.cpp
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "parametrizations.h"

/// \fn    ThetaPhi

FourVector ThetaPhi(const double& theta, const double& phi, const double& absp, const double& m)
{
    return FourVector(
                      sqrt(absp*absp+m*m),
                      absp*sin(theta)*cos(phi),
                      absp*sin(theta)*sin(phi),
                      absp*cos(theta)
                      );
}

/// \fn    KaellenLambda
double KaellenLambda(const double& a, const double& b, const double& c)
{
    return sqrt(a*a+b*b+c*c-2.*a*b-2.*b*c-2.*a*c);
}

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
double ZlambdaPG::generateFSMomenta(const vector<double>& randoms) const
{
    const double phi = 2.*consts::Pi*randoms[1];
    const double lambda = randoms[0];
    const double z = _tau / (x1*x2);
    // Warning: this was x1*x2*_E before becoming sqrt(x1*x2)*2.*_E.
    // Think it was a bug!! Affects p_T spectrum, not total Xsec.
    const double sllbar = sqrt(lambda*(1.-lambda)*x1*x2)*2.*_E;
    _p[4] = (1.-z)*(
                    (1.-lambda) * _p[1] +
                    lambda * _p[2] +
                    sllbar * LightCone::eperp(phi)
                    );
    _p[3] = _p[1] + _p[2] - _p[4];
    //jacobian = 1
    return 1.;
}

/// \class Zlambda3PG

/// Sets the intermediate variable _tau for computational speed gain
void Zlambda3PG::computeConstants()
{
    // Check number of masses passed to PGenerator::setParameters
    if (_m.size() != 2)
    {
        cerr << "[ZlambdaPG] Error: wrong masses arrived at the momenta generator.\n";
        exit(1);
    }
    _tau = (_m[0]*_m[0]+_m[1]*_m[1])/(4.*_E*_E);
    return;
}

/// Implements the parametrization
double Zlambda3PG::generateFSMomenta(const vector<double>& randoms) const
{
    // Generating fraction of energy going into pseudo-particle 34
    // This could be optimized to not-reject by inserting a jacobian
    const double z = randoms[3];
    if (z < _tau) return 0.;
    // Generating gluon momentum in the partonic com
    const double s12 = 4.*_E*_E*x1*x2;
    const double sqrts12_2 = _E*sqrt(x1*x2);
    const double phi = 2.*consts::Pi*randoms[2];
    const double lambda = randoms[4];
    const double sllbar = sqrt(lambda*(1.-lambda)*s12);
    _p[5] = (1.-z)*(
                    (1.-lambda) * sqrts12_2 * LightCone::n +
                    lambda * sqrts12_2 * LightCone::nbar +
                    sllbar * LightCone::eperp(phi)
                    );
    // Generating p4 in the 34 rest frame
    const double Q2 = z*s12;
    const double absp = KaellenLambda(Q2,_m[0]*_m[0],_m[1]*_m[1])/(2.*sqrt(Q2));
    const double theta_gamma = consts::Pi*randoms[1];
    const double phi_gamma = 2.*consts::Pi*randoms[0];
    _p[3] = ThetaPhi(theta_gamma,phi_gamma,absp,_m[0]);
    // Boosting p3 to the partonic com (boost by velocity of p34)
    const double p34_0 = 2.*sqrts12_2-_p[5][0];
    _p[3].boost(-_p[5][1]/p34_0,-_p[5][2]/p34_0,-_p[5][3]/p34_0);
    // Boosting p3 and p5 to the lab
    const double Y = 0.5*log(x1/x2);
    _p[5].rapBoost(Y);
    _p[3].rapBoost(Y);
    // Recovering p4 by momentum conservation
    _p[4] = _p[1]+_p[2]-_p[3]-_p[5];
    // cout << _p[4]*_p[4] << endl;
    //jacobian = sin(theta_gamma)
    return sin(theta_gamma);
}

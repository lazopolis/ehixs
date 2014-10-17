/**
 *
 * \file    xgenerator.cpp
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "xgenerator.h"

/// \class TwoXGenerator

/// Generates the product x1*x2 in the range [_x1x2min, 1] and x1 in [x1*x2, 1]
double TwoXGenerator::operator()(vector<double>& randoms)
    {
        const double x1x2 = _x1x2min + (1.-_x1x2min) * randoms.back();
        randoms.pop_back();
        _x.x1 = x1x2 + (1.-x1x2) * randoms.back();
        randoms.pop_back();
        _x.x2 = x1x2/_x.x1;
        return 1./_x.x1;
    }

/// \class ZXGenerator

/// Given tau=_x1x2min, generates z=tau/(x1*x2) in the range [tau, 1] and the log of their ratio in [-log(tau), log(tau)]
double ZXGenerator::operator()(vector<double>& randoms)
{
    const double& tau = _x1x2min;
    const double logtau = log(tau);
    const double z = randoms.back()*(1.-tau)+tau;
    randoms.pop_back();
    const double Y = (2.*randoms.back()-1.)*logtau;
    randoms.pop_back();
    const double s_x1x2 = sqrt(tau/z);
    const double s_x1_x2 = exp(Y);
    _x.x1 = s_x1x2 * s_x1_x2;
    _x.x2 = s_x1x2 / s_x1_x2;
    return fabs(2.*logtau*tau/(z*z));
}

/// \class FlatXGenerator

/// Generates x1 and x2 between 0 and 1, returning 0. jacobian if their product is too small
double FlatXGenerator::operator()(vector<double>& randoms)
{
    _x.x1 = randoms.back();
    randoms.pop_back();
    _x.x2 = randoms.back();
    randoms.pop_back();
    return ( _x.x1*_x.x2 > _x1x2min );
}

/// \class OneXGenerator

/// Sets the product x1*x2 to _x1x2min (using the delta), then generates x1 in the range [x1*x2, 1]
double OneXGenerator::operator()(vector<double>& randoms)
    {
        _x.x1 = _x1x2min + (1.-_x1x2min) * randoms.back();
        randoms.pop_back();
        _x.x2 = _x1x2min/_x.x1;
        return (1.-_x1x2min)/_x.x1;
    }

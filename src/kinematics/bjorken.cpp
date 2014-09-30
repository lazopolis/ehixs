/**
 *
 * \file    bjorken.cpp
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "bjorken.h"

/// \class TwoXGenerator

/// Generates the product x1*x2 in the range [_x1x2min, 1] and x1 in [x1*x2, 1]
double TwoXGenerator::operator()(const double* const randoms)
    {
        const double x1x2 = _x1x2min + (1.-_x1x2min) * randoms[0];
        _x.x1 = x1x2 + (1.-x1x2) * randoms[1];
        _x.x2 = x1x2/_x.x1;
        return 1./_x.x1;
    }

/// \class OneXGenerator

/// Sets the product x1*x2 to _x1x2min (using the delta), then generates x1 in the range [x1*x2, 1]
double OneXGenerator::operator()(const double* const randoms)
    {
        _x.x1 = _x1x2min + (1.-_x1x2min) * randoms[0];
        _x.x2 = _x1x2min/_x.x1;
        return (1.-_x1x2min)/_x.x1;
    }

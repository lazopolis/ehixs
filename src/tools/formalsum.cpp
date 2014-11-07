/**
 *
 * \file    formalsum.cpp
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    November 2014
 *
 */

#include "formalsum.h"

/// Multiplications

FormalSum<double> operator*(const FormalSum<double>& myExpansion, const double& factor)
{
    if ( factor == 0. ) {
        return FormalSum<double>();
    } else {
        FormalSum<double> foo;
        for (int i = myExpansion.minTerm(); i <= myExpansion.maxTerm(); ++i)
            foo.setCoefficient(i, myExpansion.getCoefficient(i) * factor);
        return foo;
    }
}

FormalSum<double> operator*(const double& factor, FormalSum<double>& myExpansion)
{
    return myExpansion * factor;
}

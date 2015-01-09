/**
 *
 * \file    qq2gammagammaX.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */


#include "qq2gammagammaX.h"

/// \brief Matrix elements for bb->H

/// Tree

template<>
double qq2gammagamma<0,0>()
{
    return 0.;
}

/// End of tree

/// One loop (to be multiplied by c_Gamma)

template<>
double qq2gammagamma<1,-2>()
{
    return 0.;
}

template<>
double qq2gammagamma<1,-1>()
{
    return 0.;
}

template<>
double qq2gammagamma<1,0>()
{
    return 0.;
}

/// \brief Shorthand for full epsilon expansion of bb->H

template<>
Expansion<Parameter::epsilon, double> qq2gammagamma<0>()
{
    return Expansion<Parameter::epsilon, double>(0,{qq2gammagamma<0,0>()},true);
}

template<>
Expansion<Parameter::epsilon, double> qq2gammagamma<1>()
{
    return Expansion<Parameter::epsilon, double>(-2,{
        qq2gammagamma<1,-2>(),
        qq2gammagamma<1,-1>(),
        qq2gammagamma<1, 0>()
    });
}

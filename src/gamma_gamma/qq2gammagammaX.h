/**
 *
 * \file    qq2gammagammaX.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */


#ifndef QQ2GAMMAGAMMAX_H
#define QQ2GAMMAGAMMAX_H

#include "chaplin.h"   // HPL
#include "constants.h" // QCD::CF, etc...
#include "expansion.h" // Expansion<>

/// \todo Move this to either Constants, UserInterface, Model or whatever
constexpr double yukawa_bottom = 1.0;
constexpr double alphas_pi = 1.0;

/// \brief Matrix elements for bb->H
template<size_t loop, int eps>
double qq2gammagamma();

/// \brief Shorthand for full epsilon expansion of bb->H
template<size_t loop>
Expansion<Parameter::epsilon, double> qq2gammagamma();

#endif

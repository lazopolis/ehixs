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
#include "qq2yyg/allcoeffs.h"

/// \todo Move this to either Constants, UserInterface, Model or whatever
constexpr double alpha = 1.0;

/// \brief Matrix elements for qq->gammagamma
template<size_t loop, int eps>
double qq2gammagamma(const double& s13_s14);

/// \brief Matrix elements for qq->gammagamma+g
template<size_t loop, int eps>
double qq2gammagammag(
                      const double& s12,
                      const double& s13,
                      const double& s14,
                      const double& s23,
                      const double& s24
                      );

/// \brief Shorthand for full epsilon expansion of qq->gammagamma
template<size_t loop>
Expansion<Parameter::epsilon, double> qq2gammagamma(const double& s13_s14);

/// \brief Shorthand for full epsilon expansion of qq->gammagamma+g
template<size_t loop>
Expansion<Parameter::epsilon, double> qq2gammagammag(
                                                     const double& s12,
                                                     const double& s13,
                                                     const double& s14,
                                                     const double& s23,
                                                     const double& s24
                                                     );

#endif

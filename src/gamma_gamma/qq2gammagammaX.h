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
#include "expansion.h" // EpsExp
#include "qq2yyg/qq2yyg0.h"
#include "qq2yyg/qq2yyg1.h"

/// \todo Move this to either Constants, UserInterface, Model or whatever
constexpr double alpha = 1.;

/// \brief Matrix elements for qq->yy
template<size_t loop, int eps>
double qq2yy(const double& s13_s14);

/// \brief Shorthand for full epsilon expansion of qq->yy
template<size_t loop>
EpsExp qq2yy(const double& s13_s14);

#endif

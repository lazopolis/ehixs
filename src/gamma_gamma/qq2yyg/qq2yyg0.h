/**
 *
 * \file    qq2yyg0.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#ifndef QQ2YYG0_H
#define QQ2YYG0_H

#include "expansion.h"

/// \brief Matrix elements for qq->yyg
template<int eps>
double qq2yyg0(
               const double& zb,
               const double& t12,
               const double& t34,
               const double& u
               );

/// \brief Shorthand for full epsilon expansion of qq->yyg
EpsExp qq2yyg0(
               const double& zb,
               const double& t12,
               const double& t34,
               const double& u
               );

#endif

/**
 *
 * \file    coeff6ESC.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    April 2015
 *
 */

#ifndef COEFF6ESC_H
#define COEFF6ESC_H

#include "expansion.h" // Expansion<>
#include "cln/rational.h"
#include "cln/float.h"
#define BaseT cln::cl_RA
#include "mypow.h"
#undef BaseT

/**
 * \fn    qq2yyg6ESC
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, subleading color
 */

template<size_t master, int eps>
double qq2yyg6ESC(
              const double& s12,
              const double& s13,
              const double& s14,
              const double& s23,
              const double& s24
              );

/**
 * \fn    qq2yyg6ESC
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, subleading color
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yyg6ESC(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

#endif

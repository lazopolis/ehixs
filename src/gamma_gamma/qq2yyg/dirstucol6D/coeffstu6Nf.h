/**
 *
 * \file    coeffstu6Nf.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#ifndef COEFFSTU6NF_H
#define COEFFSTU6NF_H

#include "expansion.h"          // Expansion<>
#include "floatconversions.h"   // todouble<>, flog<>
#define BaseT my_float
#include "mypow.h"              // pow<>
#undef BaseT

/**
 * \fn    qq2yygstu6Nf
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, Nf piece
 */

template<size_t master, int eps>
double qq2yygstu6Nf(
		    const my_float& zb,
		    const my_float& t12,
		    const my_float& t34,
		    const my_float& u
		    );

/**
 * \fn    qq2yygstu6Nf
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, Nf piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygstu6Nf(
						   const my_float& zb,
						   const my_float& t12,
						   const my_float& t34,
						   const my_float& u
						   );

#endif

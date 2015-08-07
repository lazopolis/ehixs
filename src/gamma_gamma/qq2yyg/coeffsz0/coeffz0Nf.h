/**
 *
 * \file    coeffz0Nf.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#ifndef COEFF_Z0_NF_H
#define COEFF_Z0_NF_H

#include "expansion.h"          // Expansion<>
#include "floatconversions.h"   // todouble<>, flog<>
#define BaseT my_float
#include "mypow.h"              // pow<>
#undef BaseT

/**
 * \fn    qq2yygz0Nf
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, Nf piece
 */

template<size_t master, int eps>
double qq2yygz0Nf(
		    const my_float& z,
		    const my_float& t12,
		    const my_float& t34,
		    const my_float& u
		    );

/**
 * \fn    qq2yygz0Nf
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, Nf piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygz0Nf(
						   const my_float& z,
						   const my_float& t12,
						   const my_float& t34,
						   const my_float& u
						   );

#endif

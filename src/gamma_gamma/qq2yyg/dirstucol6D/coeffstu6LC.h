/**
 *
 * \file    coeffstu6LC.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#ifndef COEFFSTU6LC_H
#define COEFFSTU6LC_H

#include "expansion.h"          // Expansion<>
#include "floatconversions.h"   // todouble<>, flog<>
#define BaseT my_float
#include "mypow.h"              // pow<>
#undef BaseT

/**
 * \fn    qq2yygstu6LC
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, leading color
 */

template<size_t master, int eps>
double qq2yygstu6LC(
		    const my_float& zb,
		    const my_float& t12,
		    const my_float& t34,
		    const my_float& u
		    );

/**
 * \fn    qq2yygstu6LC
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, leading color
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygstu6LC(
						   const my_float& zb,
						   const my_float& t12,
						   const my_float& t34,
						   const my_float& u
						   );

/**
 * \fn    qq2yygstu6LCbub1325
 * \brief First term of the Taylor series of c13*bub(s13)+c25*bub(s25) around s13=s25,
 *        power by power in epsilon, leading color
 */

template<int eps>
double qq2yygstu6LCbub1325(const my_float& zb, const my_float& t12, const my_float& t34);

/**
 * \fn    qq2yygstu6LCbub1325
 * \brief First term of the Taylor series of c13*bub(s13)+c25*bub(s25) around s13=s25,
 *        series in epsilon, leading color
 */

double qq2yygstu6LCbub1325(const my_float& zb, const my_float& t12, const my_float& t34);

/**
 * \fn    qq2yygstu6LCbub1324
 * \brief First term of the Taylor series of c13*bub(s13)+c24*bub(s24) around s13=s24,
 *        power by power in epsilon, leading color
 */

template<int eps>
double qq2yygstu6LCbub1324(const my_float& zb, const my_float& t12, const my_float& u);

/**
 * \fn    qq2yygstu6LCbub1324
 * \brief First term of the Taylor series of c13*bub(s13)+c24*bub(s24) around s13=s24,
 *        series in epsilon, leading color
 */

double qq2yygstu6LCbub1324(const my_float& zb, const my_float& t12, const my_float& u);

#endif

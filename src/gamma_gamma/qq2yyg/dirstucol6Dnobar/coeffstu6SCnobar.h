/**
 *
 * \file    coeffstu6SCnobar.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#ifndef COEFFSTU6SCNOBAR_H
#define COEFFSTU6SCNOBAR_H

#include "expansion.h" // Expansion<>
#define BaseT double
#include "mypow.h"
#undef BaseT

/**
 * \fn    qq2yygstu6SCnobar
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, subleading color
 */

template<size_t master, int eps>
double qq2yygstu6SCnobar(
			 const double& z,
			 const double& t12,
			 const double& t34,
			 const double& u
			 );

/**
 * \fn    qq2yygstu6SCnobar
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, subleading color
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygstu6SCnobar(
							const double& z,
							const double& t12,
							const double& t34,
							const double& u
							);

#endif

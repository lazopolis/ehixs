/**
 *
 * \file    coeffs4D.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#ifndef COEFFS4D_H
#define COEFFS4D_H

/**
 * \fn    qq2yyg4CA
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, CA piece
 */

template<size_t master, int eps>
double qq2yyg4CA(
              const double& s12,
              const double& s13,
              const double& s14,
              const double& s23,
              const double& s24
              );

/**
 * \fn    qq2yyg4CA
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, CA piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yyg4CA(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg4CF
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, CF piece
 */

template<size_t master, int eps>
double qq2yyg4CF(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg4CF
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, CF piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yyg4CF(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg4CAm2CF
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, CA-2CF piece
 */

template<size_t master, int eps>
double qq2yyg4CAm2CF(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg4CAm2CF
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, CA-2CF piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yyg4CAm2CF(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg4CAbub
 */

double qq2yyg4CAbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4CAbox
 */

double qq2yyg4CAbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4CFbub
 */

double qq2yyg4CFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4CFbox
 */

double qq2yyg4CFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4AFbub
 */

double qq2yyg4AFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4AFbox
 */

double qq2yyg4AFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yyg4(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

#endif

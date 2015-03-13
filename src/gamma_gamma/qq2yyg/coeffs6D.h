/**
 *
 * \file    coeffs6D.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    February 2015
 *
 */

#ifndef COEFFS6D_H
#define COEFFS6D_H

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

/**
 * \fn    qq2yyg6CA
 * \brief Master coefficients for qq->gammagamma+g at one loop, power by power in epsilon, CA piece
 */

template<size_t master, int eps>
double qq2yyg6CA(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg6CA
 * \brief Master coefficients for qq->gammagamma+g at one loop, series in epsilon, CA piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yyg6CA(
                                               const double& s12,
                                               const double& s13,
                                               const double& s14,
                                               const double& s23,
                                               const double& s24
                                               );

/**
 * \fn    qq2yyg6CF
 * \brief Master coefficients for qq->gammagamma+g at one loop, power by power in epsilon, CF piece
 */

template<size_t master, int eps>
double qq2yyg6CF(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg6CF
 * \brief Master coefficients for qq->gammagamma+g at one loop, series in epsilon, CF piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yyg6CF(
                                               const double& s12,
                                               const double& s13,
                                               const double& s14,
                                               const double& s23,
                                               const double& s24
                                               );

/**
 * \fn    qq2yyg6CAm2CF
 * \brief Master coefficients for qq->gammagamma+g at one loop, power by power in epsilon, CA-2CF piece
 */

template<size_t master, int eps>
double qq2yyg6CAm2CF(
                    const double& s12,
                    const double& s13,
                    const double& s14,
                    const double& s23,
                    const double& s24
                    );

/**
 * \fn    qq2yyg6CAm2CF
 * \brief Master coefficients for qq->gammagamma+g at one loop, series in epsilon, CA-2CF piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yyg6CAm2CF(
                                                   const double& s12,
                                                   const double& s13,
                                                   const double& s14,
                                                   const double& s23,
                                                   const double& s24
                                                   );

/**
 * \fn    qq2yyg6CAbub
 */

double qq2yyg6CAbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6CAbox
 */

double qq2yyg6CAbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6CFbub
 */

double qq2yyg6CFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6CFbox
 */

double qq2yyg6CFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6AFbub
 */

double qq2yyg6AFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6AFbox
 */

double qq2yyg6AFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg6
 * \brief Order epsilon^0 part of the qq->gammagamma+g matrix element
 */

double qq2yyg6(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

#endif

/**
 *
 * \file    coeffsz6D.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#ifndef COEFFSZ6D_H
#define COEFFSZ6D_H

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

/**
 * \fn    qq2yygz6CA
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, CA piece
 */

template<size_t master, int eps>
double qq2yygz6CA(
                  const double& s12,
                  const double& s13,
                  const double& s15n,
                  const double& s25n,
                  const double& s35n,
                  const double& zb
                  );

/**
 * \fn    qq2yygz6CA
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, CA piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygz6CA(
                                                 const double& s12,
                                                 const double& s13,
                                                 const double& s15n,
                                                 const double& s25n,
                                                 const double& s35n,
                                                 const double& zb
                                                 );

/**
 * \fn    qq2yygz6CF
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, CF piece
 */

template<size_t master, int eps>
double qq2yygz6CF(
                  const double& s12,
                  const double& s13,
                  const double& s15n,
                  const double& s25n,
                  const double& s35n,
                  const double& zb
                  );

/**
 * \fn    qq2yygz6CF
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, CF piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygz6CF(
                                                 const double& s12,
                                                 const double& s13,
                                                 const double& s15n,
                                                 const double& s25n,
                                                 const double& s35n,
                                                 const double& zb
                                                 );

/**
 * \fn    qq2yygz6CAm2CF
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, CA-2CF piece
 */

template<size_t master, int eps>
double qq2yygz6CAm2CF(
                      const double& s12,
                      const double& s13,
                      const double& s15n,
                      const double& s25n,
                      const double& s35n,
                      const double& zb
                      );

/**
 * \fn    qq2yygz6CAm2CF
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, CA-2CF piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygz6CAm2CF(
                                                     const double& s12,
                                                     const double& s13,
                                                     const double& s15n,
                                                     const double& s25n,
                                                     const double& s35n,
                                                     const double& zb
                                                     );

/**
 * \fn    qq2yygz6CAbub
 */

double qq2yygz6CAbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6CAbox
 */

double qq2yygz6CAbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6CFbub
 */

double qq2yygz6CFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6CFbox
 */

double qq2yygz6CFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6AFbub
 */

double qq2yygz6AFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6AFbox
 */

double qq2yygz6AFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yygz6(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

#endif

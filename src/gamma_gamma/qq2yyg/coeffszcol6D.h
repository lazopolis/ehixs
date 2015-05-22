/**
 *
 * \file    coeffszcol6D.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#ifndef COEFFSZCOL6D_H
#define COEFFSZCOL6D_H

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

/**
 * \fn    qq2yygz6LC
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, LC piece
 */

template<size_t master, int eps>
double qq2yygz6LC(
                  const double& s13,
                  const double& s35n,
                  const double& lam,
                  const double& zb
                  );

/**
 * \fn    qq2yygz6LC
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, LC piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygz6LC(
                                                 const double& s13,
                                                 const double& s35n,
                                                 const double& lam,
                                                 const double& zb
                                                 );

/**
 * \fn    qq2yygz6SC
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, SC piece
 */

template<size_t master, int eps>
double qq2yygz6SC(
                  const double& s13,
                  const double& s35n,
                  const double& lam,
                  const double& zb
                  );

/**
 * \fn    qq2yygz6SC
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, SC piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygz6SC(
                                                 const double& s13,
                                                 const double& s35n,
                                                 const double& lam,
                                                 const double& zb
                                                 );

/**
 * \fn    qq2yygz6LCbub
 */

double qq2yygz6LCbub(const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6LCbox
 */

double qq2yygz6LCbox(const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6SCbub
 */

double qq2yygz6SCbub(const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6SCbox
 */

double qq2yygz6SCbox(const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygz6col
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yygz6col(const double& s13, const double& s14, const double& s23, const double& s24);

#endif

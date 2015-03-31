/**
 *
 * \file    coeffscol4D.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    March 2015
 *
 */

#ifndef COEFFSCOL4D_H
#define COEFFSCOL4D_H

/**
 * \fn    qq2yyg4LC
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, leading color
 */

template<size_t master, int eps>
double qq2yyg4LC(
              const double& s12,
              const double& s13,
              const double& s14,
              const double& s23,
              const double& s24
              );

/**
 * \fn    qq2yyg4LC
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, leading color
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yyg4LC(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg4SC
 * \brief Master coefficients for qq->yyg at one loop, power by power in epsilon, subleading color
 */

template<size_t master, int eps>
double qq2yyg4SC(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg4SC
 * \brief Master coefficients for qq->yyg at one loop, series in epsilon, subleading color
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yyg4SC(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yyg4LCbub
 */

double qq2yyg4LCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4LCbox
 */

double qq2yyg4LCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4SCbub
 */

double qq2yyg4SCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4SCbox
 */

double qq2yyg4SCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg4col
 * \brief Order epsilon^0 part of the qq->yyg matrix element
 */

double qq2yyg4col(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

#endif

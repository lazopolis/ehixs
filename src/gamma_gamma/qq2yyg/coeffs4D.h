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
 * \fn    qq2yygCA
 * \brief Master coefficients for qq->gammagamma+g at one loop, power by power in epsilon, CA piece
 */

template<size_t master, int eps>
double qq2yygCA(
              const double& s12,
              const double& s13,
              const double& s14,
              const double& s23,
              const double& s24
              );

/**
 * \fn    qq2yygCA
 * \brief Master coefficients for qq->gammagamma+g at one loop, series in epsilon, CA piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygCA(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yygCF
 * \brief Master coefficients for qq->gammagamma+g at one loop, power by power in epsilon, CF piece
 */

template<size_t master, int eps>
double qq2yygCF(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yygCF
 * \brief Master coefficients for qq->gammagamma+g at one loop, series in epsilon, CF piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygCF(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yygCAm2CF
 * \brief Master coefficients for qq->gammagamma+g at one loop, power by power in epsilon, CA-2CF piece
 */

template<size_t master, int eps>
double qq2yygCAm2CF(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yygCAm2CF
 * \brief Master coefficients for qq->gammagamma+g at one loop, series in epsilon, CA-2CF piece
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygCAm2CF(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yygCAbub
 */

double qq2yygCAbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygCAbox
 */

double qq2yygCAbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygCFbub
 */

double qq2yygCFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygCFbox
 */

double qq2yygCFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygAFbub
 */

double qq2yygAFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygAFbox
 */

double qq2yygAFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yyg
 * \brief Order epsilon^0 part of the qq->gammagamma+g matrix element
 */

double qq2yyg(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

#endif

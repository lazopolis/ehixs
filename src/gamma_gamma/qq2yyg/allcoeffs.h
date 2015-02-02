/**
 *
 * \file    allcoeffs.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#ifndef ALLCOEFFS_H
#define ALLCOEFFS_H

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

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
 * \fn    qq2yyg
 * \brief Order epsilon^0 part of the qq->gammagamma+g matrix element
 */

double qq2yyg(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

#endif

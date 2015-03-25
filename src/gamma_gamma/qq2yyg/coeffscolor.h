/**
 *
 * \file    coeffscolor.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    March 2015
 *
 */

#ifndef COEFFSCOLOR_H
#define COEFFSCOLOR_H

/**
 * \fn    qq2yygLC
 * \brief Master coefficients for qq->gammagamma+g at one loop, power by power in epsilon, leading color
 */

template<size_t master, int eps>
double qq2yygLC(
              const double& s12,
              const double& s13,
              const double& s14,
              const double& s23,
              const double& s24
              );

/**
 * \fn    qq2yygLC
 * \brief Master coefficients for qq->gammagamma+g at one loop, series in epsilon, leading color
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygLC(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yygSC
 * \brief Master coefficients for qq->gammagamma+g at one loop, power by power in epsilon, subleading color
 */

template<size_t master, int eps>
double qq2yygSC(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yygSC
 * \brief Master coefficients for qq->gammagamma+g at one loop, series in epsilon, subleading color
 */

template<size_t master>
Expansion<Parameter::epsilon, double> qq2yygSC(
                const double& s12,
                const double& s13,
                const double& s14,
                const double& s23,
                const double& s24
                );

/**
 * \fn    qq2yygLCbub
 */

double qq2yygLCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygLCbox
 */

double qq2yygLCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygSCbub
 */

double qq2yygSCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygSCbox
 */

double qq2yygSCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

/**
 * \fn    qq2yygcol
 * \brief Order epsilon^0 part of the qq->gammagamma+g matrix element
 */

double qq2yygcol(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24);

#endif

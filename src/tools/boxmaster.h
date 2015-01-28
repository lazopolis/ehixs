/**
 *
 * \file    boxmaster.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#ifndef BOXMASTER_H
#define BOXMASTER_H

#include "expansion.h"     // Expansion
#include "chaplin.h"       // HPL
#include "counterforge.h"  // cotan
#include <cmath>           // NAN

/**
 *
 * \fn    m1n
 * \brief Shorthand for (-1)^n
 *
 */
template <typename intype, typename outtype>
outtype m1n(const intype& n)
{
    if (n%2==0) return 1;
    else return -1;
}

/**
 *
 * \fn    polyLog
 * \brief Shorthand to call polylogarithms from Chaplin
 * \todo  Consider moving into chaplin.h
 * \todo  Consider using HPLreal functions (not working?!?)
 *
 */

double polyLog(const size_t n, const double& z);

/**
 *
 * \fn    twoFone
 * \brief Hypergeometric function 2F1(1,-e;1-e,z) with automatic analytic continuation
 *
 */

Expansion<Parameter::epsilon, double> twoFone(const double& z, const size_t n = Expansion<Parameter::epsilon, double>::accuracy);

#endif

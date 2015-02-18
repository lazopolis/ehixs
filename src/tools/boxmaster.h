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
#include "counterforge.h"  // cGamma, cotan, cosec
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
 * \fn    continuedExp
 * \brief Expansion of Re(z^(a*epsilon)), automatised for z<0
 *
 */

Expansion<Parameter::epsilon, double> continuedExp(const double& z, const double& a,
                                                   const size_t n = Expansion<Parameter::epsilon, double>::accuracy);

/**
 *
 * \fn    bubble
 * \brief Returns the ordinary bubble master as an expansion in epsilon for given invariants
 *
 */

Expansion<Parameter::epsilon, double> bubble(const double& s,
                                             const size_t n = Expansion<Parameter::epsilon, double>::accuracy);

/**
 *
 * \fn    twoFone
 * \brief Hypergeometric function 2F1(1,-e;1-e,z) with automatic analytic continuation
 *
 */

Expansion<Parameter::epsilon, double> twoFone(const double& z, const size_t n = Expansion<Parameter::epsilon, double>::accuracy);

/**
 *
 * \fn    box
 * \brief Returns the Babis box master (eq. 4.33 of his PhD thesis) as an expansion in epsilon for given invariants
 *
 */

Expansion<Parameter::epsilon, double> box(const double& s, const double&t, const double& M2,
                                           const size_t n = Expansion<Parameter::epsilon, double>::accuracy);

/**
 *
 * \fn    box6
 * \brief Returns the box master in 6 dimensions as an expansion in epsilon for given invariants
 *        Beware: this is already multiplied by -epsilon/u in order to avoid *u/u
 *
 */

Expansion<Parameter::epsilon, double> box6(const double& s, const double&t, const double& M2,
                                          const size_t n = Expansion<Parameter::epsilon, double>::accuracy);

#endif

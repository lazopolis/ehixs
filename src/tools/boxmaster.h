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
#include "mpl.h"
using namespace mpl;

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
 * \brief Expansion of Re(z^(a*epsilon)), automated for z<0
 *
 */

EpsExp continuedExp(const double& z, const double& a,
                    const size_t n = EpsExp::accuracy);

/**
 *
 * \fn    bubble
 * \brief Returns the ordinary bubble master as an expansion in epsilon for given invariants
 *
 */

EpsExp bubble(const double& s,
              const size_t n = EpsExp::accuracy);
template<class T>
EpsExp bubble(const T& s,
              const size_t n = EpsExp::accuracy)
{
    return bubble(todouble(s),n);
}

/**
 *
 * \fn    tri2
 * \brief Returns the triangle with 2 external masses as an expansion in epsilon for given invariants
 * \note  The series representation for very close masses is disabled (commented) in the .cpp
 *
 */

EpsExp tri2(const double& p12,
            const double& p22,
            const size_t n = EpsExp::accuracy);

/**
 *
 * \fn    twoFone
 * \brief Hypergeometric function 2F1(1,-e;1-e,z) with automatic analytic continuation
 *
 */

EpsExp twoFone(const double& z, const size_t n = EpsExp::accuracy);

/**
 *
 * \fn    box
 * \brief Returns the Babis box master (eq. 4.33 of his PhD thesis) as an expansion in epsilon for given invariants
 *
 */

EpsExp box(const double& s, const double&t, const double& M2,
           const size_t n = EpsExp::accuracy);

/**
 *
 * \fn    box6
 * \brief Returns the box master in 6 dimensions as an expansion in epsilon for given invariants
 *        Beware: this is already multiplied by -epsilon/u in order to avoid *u/u
 *
 */

EpsExp box6(const double& s, const double&t, const double& M2,
            const size_t n = EpsExp::accuracy);
template<class T>
EpsExp box6(const T& s, const T&t, const T& M2,
            const size_t n = EpsExp::accuracy)
{
    return box6(todouble(s),todouble(t),todouble(M2),n);
}

#endif

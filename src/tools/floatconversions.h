/**
 *
 * \file    floatconversions.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    June 2015
 *
 */

#ifndef FLOAT_CONVERSIONS_H
#define FLOAT_CONVERSIONS_H

#include "cln/rational.h"
#include "cln/float.h"
#include <quadmath.h>

// Cross-type floating point logarithm

template<class T>
inline T flog(const T& that)
{
    return log(that);
}

template<>
inline __float128 flog(const __float128& that)
{
    return logq(that);
}

template<>
inline cln::cl_RA flog(const cln::cl_RA& that)
{
    // this is ugly
    return cln::rational(std::log(cln::double_approx(that)));
}

// Cross-type floating point absolute value

template<class T>
inline T fabs(const T& that)
{
    return abs(that);
}

template<>
inline __float128 fabs(const __float128& that)
{
    return fabsq(that);
}

// Cross-type floating point conversion to double

template<class T>
inline double todouble(const T& that)
{
    return static_cast<double>(that);
}

template<>
inline double todouble(const cln::cl_RA& that)
{
    return cln::double_approx(that);
}

template<>
inline double todouble(const double& that)
{
    return that;
}

#endif

/**
 *
 * \file    mpl.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    June 2015
 *
 */

#ifndef MPL_H
#define MPL_H

/**
 *
 * \namespace mpl
 * \brief     This namespace acts as a wrapper for the different type of arithmetics
 *            that can be used in a calculation (e.g. quadruple precision, rational etc..)
 *            The acronym "mpl" stands for "multiple" (precision arithmetics).
 *
 */

namespace mpl{

    // Type definitions

    typedef double dbl;

    // Cross-type floating point conversion to double

    template<class T>
    inline double todouble(const T& that)
    {
        return static_cast<double>(that);
    }

    // Specialization of todouble for doubles - nothing to do

    template<>
    inline double todouble(const dbl& that)
    {
        return that;
    }

    // Cross-type floating point conversion from double

    template<class T>
    inline T fromdouble(const double& that)
    {
        return static_cast<T>(that);
    }

    // Specialization of fromdouble for doubles - nothing to do

    template<>
    inline dbl fromdouble(const double& that)
    {
        return that;
    }

    // Cross-type floating point logarithm, assuming "log" is the right function

    template<class T>
    inline T flog(const T& that)
    {
        return log(that);
    }

    // Cross-type floating point absolute value, assuming "abs" is the right function

    template<class T>
    inline T fabs(const T& that)
    {
        return abs(that);
    }

    // Include the pow<> function for doubles
    #define BaseT double
    #include "mypow.h"
    #undef BaseT

}

// Bridging from gcc __float128 for quadruple arithmetics

#ifdef WITH_GCC

#include <quadmath.h>

namespace mpl{

    typedef __float128 qpl;

    template<>
    inline qpl flog(const qpl& that)
    {
        return logq(that);
    }

    template<>
    inline qpl fabs(const qpl& that)
    {
        return fabsq(that);
    }

    // Include the pow<> function for this type
    #define BaseT qpl
    #include "mypow.h"
    #undef BaseT

}

#endif

// Bridging from cln::cl_RA for rational arithmetics

#ifdef WITH_CLN

#include "cln/rational.h"
#include "cln/float.h"

namespace mpl{

    typedef cln::cl_RA rtn;

    template<>
    inline double todouble(const rtn& that)
    {
        return cln::double_approx(that);
    }

    template<>
    inline rtn fromdouble(const double& that)
    {
        return cln::rational(that);
    }

    template<>
    inline rtn flog(const rtn& that)
    {
        // this is ugly, but log is intrinsically trascendental
        return fromdouble<rtn>(std::log(todouble<rtn>(that)));
    }

    // Include the pow<> function for this type
    #define BaseT rtn
    #include "mypow.h"
    #undef BaseT

}

#endif

#endif

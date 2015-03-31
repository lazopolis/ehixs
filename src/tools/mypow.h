/**
 *
 * \file      mypow.h
 * \ingroup   tools
 * \author    Simone Lionetti
 * \date      April 2015
 * \attention BaseT must be defined before this file is loaded
 *
 */

#ifndef MYPOW_H
#define MYPOW_H

/**
 *
 * \def   BaseT
 * \brief BaseT is the type the pow function will be generated for
 *
 */

/**
 *
 * \fn    pow
 * \brief Overload of pow for integer exponents and any argument
 *
 */

template <int n>
BaseT pow(BaseT base)
{
    BaseT result(1);
    size_t exp = abs(n);
    while (exp)
    {
        if (exp&1) result*=base;
        exp>>=1;
        base *= base;
    }
    if (n>0) return result;
    else return BaseT(1)/result;
}

template <>
inline BaseT pow<-2>(BaseT base)
{
    return BaseT(1)/(base*base);
}

template <>
inline BaseT pow<-1>(BaseT base)
{
    return BaseT(1)/base;
}

template <>
inline BaseT pow<0>(BaseT base)
{
    return BaseT(1);
}

template <>
inline BaseT pow<1>(BaseT base)
{
    return base;
}

template <>
inline BaseT pow<2>(BaseT base)
{
    return base*base;
}

template <>
inline BaseT pow<3>(BaseT base)
{
    return base*base*base;
}

#endif

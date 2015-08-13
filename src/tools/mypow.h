/**
 *
 * \file      mypow.h
 * \ingroup   tools
 * \author    Simone Lionetti
 * \date      April 2015
 * \attention BaseT must be defined before this file is loaded
 *
 */

////////////////////////////////////////////////////////////////////////////////
// This file has no header guard because it should NOT be #included directly! //
////////////////////////////////////////////////////////////////////////////////

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
BaseT pow(const BaseT base)
{
    BaseT result(1), bas(base);
    size_t exp = abs(n);
    while (exp)
    {
        if (exp&1) result*=bas;
        exp>>=1;
        bas *= bas;
    }
    if (n>0) return result;
    else return BaseT(1)/result;
}

template <>
inline BaseT pow<-2>(const BaseT base)
{
    return BaseT(1)/(base*base);
}

template <>
inline BaseT pow<-1>(const BaseT base)
{
    return BaseT(1)/base;
}

template <>
inline BaseT pow<0>(const BaseT base)
{
    return BaseT(1);
}

template <>
inline BaseT pow<1>(const BaseT base)
{
    return base;
}

template <>
inline BaseT pow<2>(const BaseT base)
{
    return base*base;
}

template <>
inline BaseT pow<3>(const BaseT base)
{
    return base*base*base;
}

template <>
inline BaseT pow<4>(const BaseT base)
{
    const BaseT pow2=base*base;
    return pow2*pow2;
}

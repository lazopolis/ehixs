/**
 *
 * \file    mastersums.inc
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#include "mastersums.h"

// Computes the product of a pair of epsilon expansions keeping terms up to order eps^0
inline EpsExp x2Finite(const EpsExp2& pair)
{
    const int startorder = pair.first.minTerm() + pair.second.minTerm();
    if (startorder>0) return EpsExp();
    else return times(pair.first,pair.second,static_cast<size_t>(abs(startorder)+1));
}

// Computes a single term in the product of a pair of epsilon expansions
inline double xTerm(const EpsExp2& pair, const int term)
{
    return productCoeff(pair.first,pair.second,term);
}

template<typename IT, typename T>
double accTerm(IT begin, const IT end, const T& x, const int term, double init = 0)
{
    while (begin != end)
    {
        init += xTerm((*begin)(x),term);
        ++begin;
    }
    return init;
}

template<typename IT, typename T>
double selAccTerm(IT begin, const IT end, bool* check, const T& x, const int term, double init = 0)
{
    while (begin != end)
    {
        if (*check) init += xTerm((*begin)(x),term);
        ++begin;
        ++check;
    }
    return init;
}

template<typename IT, typename T>
double selAccPatchTerm(IT begin, const IT end, bool* check, const T& x, const int term, double init = 0)
{
    while (begin != end)
    {
        if (*check) init += (*begin)(x).getCoefficient(term);
        ++begin;
        ++check;
    }
    return init;
}

template<typename IT, typename T>
EpsExp acc2Finite(IT begin, const IT end, const T& x, EpsExp init = EpsExp())
{
    while (begin != end)
    {
        init += x2Finite((*begin)(x));
        ++begin;
    }
    return init;
}

template<typename IT, typename T>
EpsExp selAcc2Finite(IT begin, const IT end, bool* check, const T& x, EpsExp init = EpsExp())
{
    while (begin != end)
    {
        if(*check) init += x2Finite((*begin)(x));
        ++begin;
        ++check;
    }
    return init;
}

template<typename IT, typename T>
EpsExp selAccPatch2Finite(IT begin, const IT end, bool* check, const T& x, EpsExp init = EpsExp())
{
    while (begin != end)
    {
        if(*check) init += (*begin)(x);
        ++begin;
        ++check;
    }
    return init;
}

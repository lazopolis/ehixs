/**
 *
 * \file    qq2yyg1rescue.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#include "qq2yyg/qq2yyg1rescue.h"
#include <cmath>    // std::isfinite

/**
 * \note Cheating a bit with the poles, because the \beta_0 term cancels
 *       between renormalization counterterm and IR poles.
 *       Thus, it is here not included at all.
 */

// qq2yyg1_rescue

template<typename T>
EpsExp qq2yyg1_rescue::evalT(const Momenta& ps, const bool t)
{
    return qq2yyg1<T>::eval(typename qq2yyg1<T>::PSpoint(ps),t);
}

std::vector<typename qq2yyg1_rescue::alt> qq2yyg1_rescue::_options()
{
    static std::vector<typename qq2yyg1_rescue::alt>* _opt =
    new std::vector<typename qq2yyg1_rescue::alt>({
        evalT<dbl>, evalT<qpl>, evalT<rtn>
    });
    return *_opt;
}

EpsExp qq2yyg1_rescue::eval(const Momenta& ps, const bool taylor)
{
    PSpoint p(ps);
    EpsExp mypoles = poles(p);
    EpsExp foo;
    for (size_t i = 0; i != _options().size(); ++i)
    {
        alt f = _options()[i];
        foo = f(ps,taylor);
        if (isfinite(foo.getCoefficient(0)) && _checkpoles(mypoles,32./3.*consts::Pi*foo)) return foo;
//        cout << "no!" << endl;
    }
//    for (vector<alt>::iterator it = _options().begin(); it != _options().end(); ++it)
//    {
//        std::cout << "Computing" << std::endl;
//        foo = (*it)(ps,taylor);
//        std::cout << "done" << std::endl;
//        if (isfinite(foo.getCoefficient(0)) && _checkpoles(mypoles,32./3.*consts::Pi*foo)) return foo;
//        cout << "no!" << endl;
//    }
    std::cerr << "Could not compute PS point with enough precision!!" << std::endl;
    std::cerr << "PS point: " << p.s13 << ", " << p.s14 << ", " << p.s23 << ", " << p.s24 << std::endl;
    std::cerr << "Catani poles   : " << mypoles << std::endl;
    std::cerr << "Matrix element : " << 32./3.*consts::Pi*foo << std::endl;
    throw;
    return foo;
}

double qq2yyg1_rescue::eval(const Momenta& p, const int i, const bool taylor)
{
    return (eval(p,taylor)).getCoefficient(i);
}

EpsExp qq2yyg1_rescue::polecoeffs(const double s15, const double s25)
{
    return (
            qq2yyg1<dbl>::LC::factor()*LC::polecoeffs(s15,s25)+
            qq2yyg1<dbl>::SC::factor()*SC::polecoeffs()
            );
}

EpsExp qq2yyg1_rescue::poles(const PSpoint& p)
{
    const EpsExp foo = EpsExp(0, {
        qq2yyg0<0>(p.zb,p.t12,p.t34,p.u),
        qq2yyg0<1>(p.zb,p.t12,p.t34,p.u)
    });
    return p.zb*times(polecoeffs(p.s15,p.s25),foo,2);
}

// LC

EpsExp qq2yyg1_rescue::LC::polecoeffs(const double s15, const double s25)
{
    return EpsExp(-2, {-2., -1.5 + log(s15*s25)});
}

// SC

EpsExp qq2yyg1_rescue::SC::polecoeffs()
{
    return EpsExp(-2, {1., 1.5});
}

// qq2yyg1_rescue

bool qq2yyg1_rescue::_checkpoles(const EpsExp& poles, const EpsExp& me)
{
//    cout << "Checking poles..." << poles << "   vs   " << me << "   ...   ";
    /// \todo Move this in header
    static double reltolerance = 1.e-5;
    for (int i = poles.minTerm(); i != poles.maxTerm()+1; ++i)
    {
        double x(poles.getCoefficient(i)), y(me.getCoefficient(i));
        // Trying to catch the exceptional case where a pole vanishes analytically
        if (x==0 && y!=0 && y > reltolerance*me.getCoefficient(i+1)) return false;
        // Checking relative difference in normal case
        if (2.*abs(x-y)/(abs(x)+abs(y)) > reltolerance) return false;
    }
//    cout << "yes!" << endl;
    return true;
}

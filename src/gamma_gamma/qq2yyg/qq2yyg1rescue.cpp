/**
 *
 * \file    qq2yyg1rescue.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#include "qq2yyg/qq2yyg1rescue.h"

/**
 * \note Cheating a bit with the poles, because the \beta_0 term cancels
 *       between renormalization counterterm and IR poles.
 *       Thus, it is here not included at all.
 */

// LC bub

//static typename qq2yyg_rescue::alt alt1 = qq2yyg1<dbl>::LC::bub::eval;

std::vector<typename qq2yyg1_rescue::alt> qq2yyg1_rescue::LC::bub::_options =
vector<qq2yyg1_rescue::alt>({
    [](const PSpoint& p, const bool t){
        return qq2yyg1<dbl>::LC::bub::eval(p,t);
    },
    [](const PSpoint& p, const bool t){
        return qq2yyg1<qpl>::LC::bub::eval(qq2yyg1<qpl>::PSpoint(p.s13,p.s14,p.s23,p.s24),t);
    },
    [](const PSpoint& p, const bool t){
        return qq2yyg1<rtn>::LC::bub::eval(qq2yyg1<rtn>::PSpoint(p.s13,p.s14,p.s23,p.s24),t);
    }
});

EpsExp qq2yyg1_rescue::LC::bub::eval(const PSpoint& p, const bool taylor)
{
    EpsExp mypoles = poles(p);
    EpsExp foo;
    for (vector<alt>::iterator it = _options.begin(); it != _options.end(); ++it)
    {
        foo = (*it)(p,taylor);
        if (_checkpoles(mypoles,32./9.*consts::Pi*foo)) return foo;
        cout << "no!" << endl;
    }
    std::cerr << "Could not compute PS point with enough precision!!" << std::endl;
    std::cerr << "PS point: " << p.s13 << ", " << p.s14 << ", " << p.s23 << ", " << p.s24 << std::endl;
    std::cerr << "Catani poles   : " << mypoles << std::endl;
    std::cerr << "Matrix element : " << foo << std::endl;
    throw;
    return foo;
}

EpsExp qq2yyg1_rescue::LC::polecoeffs(const double s15, const double s25)
{
    return EpsExp(-2, {-2., -1.5 + log(s15*s25)});
}

EpsExp qq2yyg1_rescue::LC::poles(const PSpoint& p)
{
    const EpsExp foo = EpsExp(0, {
        qq2yyg0<0>(p.zb,p.t12,p.t34,p.u),
        qq2yyg0<1>(p.zb,p.t12,p.t34,p.u)
    });
    return p.zb*times(polecoeffs(p.s15,p.s25),foo,2);
}

// SC bub

EpsExp qq2yyg1_rescue::SC::bub::eval(const PSpoint& p, const bool taylor)
{
    /// \bug Temporary, WRONG, without rescue system
    return qq2yyg1<dbl>::SC::bub::eval(p,taylor);
}

// qq2yyg1_rescue

bool qq2yyg1_rescue::_checkpoles(const EpsExp& poles, const EpsExp& me)
{
    cout << "Checking poles..." << poles << "   vs   " << me << "   ...   ";
    /// \todo Move this in header
    static double reltolerance = 1.e-4;
    for (int i = poles.minTerm(); i != poles.maxTerm()+1; ++i)
    {
        double x(poles.getCoefficient(i)), y(me.getCoefficient(i));
        // Trying to catch the exceptional case where a pole vanishes analytically
        if (x==0 && y!=0 && y > reltolerance*me.getCoefficient(i+1)) return false;
        // Checking relative difference in normal case
        if (2.*abs(x-y)/(abs(x)+abs(y)) > reltolerance) return false;
    }
    cout << "yes!" << endl;
    return true;
}

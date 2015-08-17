/**
 *
 * \file    qq2yyg1rescue.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#ifndef QQ2YYG1_RESCUE_H
#define QQ2YYG1_RESCUE_H

#include "qq2yyg/qq2yyg0.h"
#include "qq2yyg/qq2yyg1.h"

struct qq2yyg1_rescue
{

    qq2yyg1_rescue() = delete;

    typedef qq2yyg1<dbl>::PSpoint PSpoint;
    using alt = EpsExp(*)(const PSpoint&, const bool);

    /// Evaluate matrix element, term
    static double eval(const PSpoint& p, const int i, const bool taylor = true);
    /// Evaluate matrix element, epsilon expansion
    static EpsExp eval(const PSpoint& p, const bool taylor = true);

    struct LC
    {

        LC() = delete;

        /// Coefficients of Catani's formula
        static EpsExp polecoeffs(const double s15, const double s25);

    };

    struct SC
    {

        SC() = delete;

        /// Coefficients of Catani's formula
        static EpsExp polecoeffs();
    };

    /// Poles as predicted by Catani
    static EpsExp poles(const PSpoint& p);

private:

    static std::vector<alt> _options;

    static bool _checkpoles(const EpsExp& poles, const EpsExp& me);

};

#endif

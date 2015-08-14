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
    static double eval(const PSpoint& p, const int i);
    /// Evaluate matrix element, epsilon expansion
    static EpsExp eval(const PSpoint& p);

    struct LC
    {

        LC() = delete;

        /// Evaluate Nf matrix element piece, term
        static double eval(const PSpoint& p, const int i);
        /// Evaluate Nf matrix element piece, epsilon expansion
        static EpsExp eval(const PSpoint& p);

        struct bub
        {

            bub() = delete;

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i, const bool taylor = true);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p, const bool taylor = true);

        private:

            static std::vector<alt> _options;

        };

        struct box
        {

            box() = delete;

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p);

        };

        /// Coefficients of Catani's formula
        static EpsExp polecoeffs(const double s15, const double s25);
        /// Poles as predicted by Catani
        static EpsExp poles(const PSpoint& p);

    };

    struct SC
    {

        SC() = delete;

        /// Evaluate Nf matrix element piece, term
        static double eval(const PSpoint& p, const int i);
        /// Evaluate Nf matrix element piece, epsilon expansion
        static EpsExp eval(const PSpoint& p);

        struct bub
        {

            bub() = delete;

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i, const bool taylor = true);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p, const bool taylor = true);

        };

        struct box
        {

            box() = delete;

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p);

        };
        
    };

    struct Nf
    {

        Nf() = delete;

        /// Evaluate Nf matrix element piece, term
        static double eval(const PSpoint& p, const int i);
        /// Evaluate Nf matrix element piece, epsilon expansion
        static EpsExp eval(const PSpoint& p);

        struct bub
        {

            bub() = delete;

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p);

        };

        struct box
        {

            box() = delete;

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p);
            
        };

    };

private:

    static bool _checkpoles(const EpsExp& poles, const EpsExp& me);

};

#include "qq2yyg/qq2yyg1rescue.inl"

#endif

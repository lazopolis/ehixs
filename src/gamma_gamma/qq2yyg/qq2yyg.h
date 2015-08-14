/**
 *
 * \file    qq2yyg.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#ifndef QQ2YYG_H
#define QQ2YYG_H

#include "fourvector.h"
#include "expansion.h"
#include "mpl.h"
using namespace mpl;


template<typename T>
struct qq2yyg1
{

    qq2yyg1() = delete;

    struct PSpoint
    {

        const T s12, s13, s14, s23, s24, s15, s25, s34, s35, s45;
        const T zb, t12, t34, u;

        PSpoint(const double& is13, const double& is14, const double& is23, const double& is24)
        : s12(static_cast<T>(1)),
        s13(fromdouble<T>(is13)), s14(fromdouble<T>(is14)),
        s23(fromdouble<T>(is23)), s24(fromdouble<T>(is24)),
        s15(-s12-s13-s14), s25(-s12-s23-s24), s34(-s12-s13-s14-s23-s24),
        s35(s12+s14+s24), s45(s12+s13+s23), zb(-s15-s25),
        t12((s15-s25)/zb), t34((s35-s45)/zb), u(s13-s14-s23+s24)
        {}

        PSpoint(const double& is12, const double& is13, const double& is14, const double& is23, const double& is24)
        : PSpoint(is13/is12,is14/is12,is23/is12,is24/is12)
        {}

        PSpoint(const Momenta& p)
        : PSpoint(square(p[1]+p[2]),square(p[1]-p[3]),square(p[1]-p[4]),square(p[2]-p[3]),square(p[2]-p[4]))
        {}
        
    };

    struct LC
    {
        LC() = delete;
        struct bub
        {
            bub() = delete;

            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);

            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            template<int m>
            static double c1324(const T& zb, const T& t12, const T& u);

            static EpsExp c1324(const T& zb, const T& t12, const T& u);

            template<int m>
            static double c1325(const T& zb, const T& t12, const T& t34);

            static EpsExp c1325(const T& zb, const T& t12, const T& t34);
            
            static EpsExp eval(const PSpoint& p);
            static double eval(const PSpoint& p, const int i);
        };

        struct box
        {
            box() = delete;

            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);

            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            static EpsExp eval(const PSpoint& p);
            static double eval(const PSpoint& p, const int i);
        };

        static EpsExp eval(const PSpoint& p);
        static double eval(const PSpoint& p, const int i);
    };

    struct SC
    {
        SC() = delete;
        struct bub
        {
            bub() = delete;

            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);

            template<int n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            template<int m>
            static double c1324(const T& zb, const T& t12, const T& u);

            static EpsExp c1324(const T& zb, const T& t12, const T& u);

            template<int m>
            static double c1325(const T& zb, const T& t12, const T& t34);

            static EpsExp c1325(const T& zb, const T& t12, const T& t34);
            
            static EpsExp eval(const PSpoint& p);
            static double eval(const PSpoint& p, const int i);
        };

        struct box
        {
            box() = delete;

            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);

            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            static EpsExp eval(const PSpoint& p);
            static double eval(const PSpoint& p, const int i);
        };

        static EpsExp eval(const PSpoint& p);
        static double eval(const PSpoint& p, const int i);
    };

    struct Nf
    {
        Nf() = delete;

        struct bub
        {
            bub() = delete;

            template<size_t n, size_t m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);

            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            static EpsExp eval(const PSpoint& p);
            static double eval(const PSpoint& p, const int i);
        };

        struct box
        {
            box() = delete;

            template<size_t n, size_t m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);

            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            static EpsExp eval(const PSpoint& p);
            static double eval(const PSpoint& p, const int i);
        };

        static EpsExp eval(const PSpoint& p);
        static double eval(const PSpoint& p, const int i);
    };

    static Expansion<Parameter::epsilon, double> eval(const PSpoint& p);
    static double eval(const PSpoint& p, const int i);

};

#endif

/**
 *
 * \file    qq2yyg1.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#ifndef QQ2YYG1_H
#define QQ2YYG1_H

#include "fourvector.h"
#include "mastersums.h"
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

    using Master = EpsExp2(*)(const PSpoint&);
    using Patch  = EpsExp (*)(const PSpoint&);

    /// Evaluate matrix element, term
    static double eval(const PSpoint& p, const int i, const bool taylor = true);
    /// Evaluate matrix element, epsilon expansion
    static EpsExp eval(const PSpoint& p, const bool taylor = true);

    struct LC
    {

        LC() = delete;

        /// Global factor for this matrix element piece
        static const double& factor();

        /// Evaluate Nf matrix element piece, term
        static double eval(const PSpoint& p, const int i, const bool taylor = true);
        /// Evaluate Nf matrix element piece, epsilon expansion
        static EpsExp eval(const PSpoint& p, const bool taylor = true);

        struct bub
        {

            bub() = delete;

            /// Basic coefficients, term
            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);
            /// Basic coefficients, epsilon expansion
            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            /// Patch expansion 1324 for large cancellations, term
            template<int m>
            static double c1324(const T& zb, const T& t12, const T& u);
            /// Patch expansion 1324 for large cancellations, epsilon expansion
            static EpsExp c1324(const T& zb, const T& t12, const T& u);

            /// Patch expansion 1325 for large cancellations, term
            template<int m>
            static double c1325(const T& zb, const T& t12, const T& t34);
            /// Patch expansion 1325 for large cancellations, epsilon expansion
            static EpsExp c1325(const T& zb, const T& t12, const T& t34);

            /// Single coefficient x master contribution, term
            static double master(const size_t i, const PSpoint& p, const int j);
            /// Single coefficient x master contribution, epsilon expansion
            static EpsExp master(const size_t i, const PSpoint& p);

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i, const bool taylor = true);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p, const bool taylor = true);

        private:

            /// Details of the combinations coefficient x master
            static array<Master,7>& masters();
            /// Details of patches for large cancellations
            static array<Patch,6>& patches();
            /// Disable masters for large cancellations
            static array<bool,7> _on;
            /// Enable patches for large cancellations
            static array<bool,6> _patch;
            /// Set conditional arrays
            static void patch(const PSpoint& p, const bool taylor = true);

        };

        struct box
        {
            box() = delete;

            /// Basic coefficients, term
            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);
            /// Basic coefficients, epsilon expansion
            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            /// Single coefficient x master contribution, term
            static double master(const size_t i, const PSpoint& p, const int j);
            /// Single coefficient x master contribution, epsilon expansion
            static EpsExp master(const size_t i, const PSpoint& p);

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p);


        private:

            /// Details of the combinations coefficient x master
            static array<Master,9>& masters();

        };

    };

    struct SC
    {

        SC() = delete;

        /// Global factor for this matrix element piece
        static const double& factor();

        /// Evaluate Nf matrix element piece, term
        static double eval(const PSpoint& p, const int i, const bool taylor = true);
        /// Evaluate Nf matrix element piece, epsilon expansion
        static EpsExp eval(const PSpoint& p, const bool taylor = true);

        struct bub
        {

            bub() = delete;

            /// Basic coefficients, term
            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);
            /// Basic coefficients, epsilon expansion
            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            /// Patch expansion 1324 for large cancellations, term
            template<int m>
            static double c1324(const T& zb, const T& t12, const T& u);
            /// Patch expansion 1324 for large cancellations, epsilon expansion
            static EpsExp c1324(const T& zb, const T& t12, const T& u);

            /// Patch expansion 1325 for large cancellations, term
            template<int m>
            static double c1325(const T& zb, const T& t12, const T& t34);
            /// Patch expansion 1325 for large cancellations, epsilon expansion
            static EpsExp c1325(const T& zb, const T& t12, const T& t34);

            /// Single coefficient x master contribution, term
            static double master(const size_t i, const PSpoint& p, const int j);
            /// Single coefficient x master contribution, epsilon expansion
            static EpsExp master(const size_t i, const PSpoint& p);

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i, const bool taylor = true);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p, const bool taylor = true);

        private:

            /// Details of the combinations coefficient x master
            static array<Master,10>& masters();
            /// Details of patches for large cancellations
            static array<Patch,6 >& patches();
            /// Disable masters for large cancellations
            static array<bool,10> _on;
            /// Enable patches for large cancellations
            static array<bool,6 > _patch;
            /// Set conditional arrays
            static void patch(const PSpoint& p, const bool taylor = true);

        };

        struct box
        {
            box() = delete;

            /// Basic coefficients, term
            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);
            /// Basic coefficients, epsilon expansion
            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            /// Single coefficient x master contribution, term
            static double master(const size_t i, const PSpoint& p, const int j);
            /// Single coefficient x master contribution, epsilon expansion
            static EpsExp master(const size_t i, const PSpoint& p);

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p);
            
            
        private:
            
            /// Details of the combinations coefficient x master
            static array<Master,21>& masters();
            
        };
        
    };

    struct Nf
    {

        Nf() = delete;

        /// Global factor for this matrix element piece
        static const double& factor();

        /// Evaluate Nf matrix element piece, term
        static double eval(const PSpoint& p, const int i);
        /// Evaluate Nf matrix element piece, epsilon expansion
        static EpsExp eval(const PSpoint& p);

        struct bub
        {

            bub() = delete;

            /// Basic coefficients, term
            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);
            /// Basic coefficients, epsilon expansion
            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            /// Single coefficient x master contribution, term
            static double master(const size_t i, const PSpoint& p, const int j);
            /// Single coefficient x master contribution, epsilon expansion
            static EpsExp master(const size_t i, const PSpoint& p);

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p);

        private:

            /// Details of the combinations coefficient x master
            static array<Master,4>& masters();
            
        };

        struct box
        {
            box() = delete;

            /// Basic coefficients, term
            template<size_t n, int m>
            static double c(const T& zb, const T& t12, const T& t34, const T& u);
            /// Basic coefficients, epsilon expansion
            template<size_t n>
            static EpsExp c(const T& zb, const T& t12, const T& t34, const T& u);

            /// Single coefficient x master contribution, term
            static double master(const size_t i, const PSpoint& p, const int j);
            /// Single coefficient x master contribution, epsilon expansion
            static EpsExp master(const size_t i, const PSpoint& p);

            /// Sum of coefficients x masters, term
            static double eval(const PSpoint& p, const int i);
            /// Sum of coefficients x masters, epsilon expansion
            static EpsExp eval(const PSpoint& p);
            

        private:

            /// Details of the combinations coefficient x master
            static array<Master,3>& masters();

        };

    };

};

#endif

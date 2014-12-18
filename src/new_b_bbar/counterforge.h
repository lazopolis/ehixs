/**
 *
 * \file    counterforge.h
 * \author  Simone Lionetti
 * \date    November 2014
 *
 */

#ifndef COUNTER_FORGE_H
#define COUNTER_FORGE_H

#include "expansion.h"
#include "constants.h"
#include "chaplin.h"

/**
 *
 * \class CounterForge
 * \brief Contains functions that are useful to build counterterms
 *
 */

class CounterForge
{

public:

    /**
     *
     * \enum  Scheme
     * \brief Semantic switch for dimensional regularization scheme variant
     *
     */
    enum Scheme {
        CDR = 0,
        HV = 1
    };

    /// \name Static functions
    /// @{

    CounterForge(const Scheme inScheme = Scheme::CDR) :
    _acc(Expansion<Parameter::epsilon, double>::accuracy), _scheme(inScheme),
    _f2(f2()), _r4(r4())
    {}

    /// @}

    /// \name Static functions
    /// @{

    /// One-loop conventional prefactor (4pi)^e Gamma(1+e) Gamma^2(1-e) / Gamma(1-2e)
    /// Numerically implemented: only combinations of EulerGamma,
    /// zeta(n) and log(4pi), not particularly meaningful
    static const Expansion<Parameter::epsilon, double> cGamma;
    
    /// Series espansion of (pi epsilon) cot(pi epsilon)
    static const Expansion<Parameter::epsilon, double> cotan;

    /// Quick alias to current accuracy
    static size_t& acc;

    /// Quark-quark splitting function, already multiplied by 1-z
    template <size_t loop>
    static Expansion<Parameter::epsilon, double> Pqq(const double& z);

    /// Kosower's auxiliary factor r3
    static Expansion<Parameter::epsilon, double> r3(const double& z, const size_t trunc = acc);

    /// Kosower's auxiliary factor r4
    static Expansion<Parameter::epsilon, double> r4(const Scheme& s = Scheme::CDR, const size_t trunc = acc);

    /// Epsilon series of Kosower's auxiliary function 1/z f1(1/z)
    static Expansion<Parameter::epsilon, double> f1_1overz(const double& z, const size_t trunc = acc);

    /// Epsilon series of Kosower's auxiliary function (1-1/z) f1(1-1/z)
    static Expansion<Parameter::epsilon, double> f1_1minus1overz(const double& z, const size_t trunc = acc);

    /// Epsilon series of Kosower's auxiliary 'function' f2
    static Expansion<Parameter::epsilon, double> f2();

    /// Quark-quark splitting function, already multiplied by 1-z, faster member implementation
    template <size_t loop>
    Expansion<Parameter::epsilon, double> fastPqq(const double& z);

    /// Kosower's auxiliary factor r3
    Expansion<Parameter::epsilon, double> fastr3(const double& z);

    /// Epsilon series of the soft current
    template <size_t loop>
    static Expansion<Parameter::epsilon, double> soft(const double&z, const double& lambda, const size_t trunc = acc);

    /// Epsilon series of the soft-collinear limit
    template <size_t loop>
    static Expansion<Parameter::epsilon, double> softcoll(const double&z, const double& lambda, const size_t trunc = acc);

    /// @}

private:

    /// \name Private data members
    /// @{

    const size_t _acc;                          ///< Accuracy
    const Scheme _scheme;                       ///< Dim reg scheme
    Expansion<Parameter::epsilon,double> _r4;   ///< Storage location for precomputed r4
    Expansion<Parameter::epsilon,double> _f2;   ///< Storage location for precomputed f2
    static const double _2CF;

    /// @}

    /// \name Auxiliary static functions
    /// @{

    /// \brief Elementary pieces of the quark-quark splitting function, already multiplied by 1-z
    /// These have color factors stripped of and represent the new spin structures
    /// appearing at each loop
    template <size_t loop>
    static Expansion<Parameter::epsilon, double> _Pqq(const double& z);

    /// Series expansion of 2F1(1, -a*eps; 1-a*eps; z)
    static Expansion<Parameter::epsilon, double> _2F1(const double& a, const double& z, const size_t trunc = acc);

    /// @}

};

#endif

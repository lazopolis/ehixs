/**
 *
 * \file    counterforge.h
 * \ingroup tools
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

    /// \name Constructors and destructor
    /// @{

    CounterForge(const Scheme inScheme = Scheme::CDR) :
    _acc(EpsExp::accuracy), _scheme(inScheme),
    _f2(f2()), _r4(r4())
    {}

    /// @}

    /// \name Static functions
    /// @{

    /// One-loop conventional prefactor (4pi)^e Gamma(1+e) Gamma^2(1-e) / Gamma(1-2e)
    /// Numerically implemented: only combinations of EulerGamma,
    /// zeta(n) and log(4pi), not particularly meaningful
    static const EpsExp& cGamma();
    
    /// Series espansion of (pi epsilon) cot(pi epsilon)
    static const EpsExp& cotan();

    /// Series espansion of (pi epsilon) / sin(pi epsilon)
    static const EpsExp& cosec();

    /// Series espansion of cos(pi epsilon)
    static const EpsExp& cos();

    /// Quick alias to current accuracy
    static size_t& acc;

    /// Quark-quark splitting function
    /// Already multiplied by 1-z, still needs to be multiplied by alpha_s
    template <size_t loop>
    static EpsExp Pqq(
                                                     const double& z,
                                                     const bool LCf = true,
                                                     const bool SCf = true,
                                                     const size_t trunc = acc
                                                     );

    /// Kosower's auxiliary factor r3
    static EpsExp r3(
                                                    const double& z,
                                                    const bool LCf = true,
                                                    const bool SCf = true,
                                                    const Scheme& s = Scheme::CDR,
                                                    const size_t trunc = acc
                                                    );

    /// Kosower's auxiliary factor r4
    static EpsExp r4(
                                                    const bool LCf = true,
                                                    const bool SCf = true,
                                                    const Scheme& s = Scheme::CDR,
                                                    const size_t trunc = acc
                                                    );

    /// Epsilon series of Kosower's auxiliary function 1/z f1(1/z)
    static EpsExp f1_1overz(const double& z, const size_t trunc = acc);

    /// Epsilon series of Kosower's auxiliary function (1-1/z) f1(1-1/z)
    static EpsExp f1_1minus1overz(const double& z, const size_t trunc = acc);

    /// Epsilon series of Kosower's auxiliary 'function' f2
    static EpsExp f2(const size_t trunc = acc);

    /// Quark-quark splitting function, already multiplied by 1-z, faster member implementation
    template <size_t loop>
    EpsExp fastPqq(const double& z);

    /// Kosower's auxiliary factor r3
    EpsExp fastr3(const double& z);

    /// Epsilon series of the soft current
    template <size_t loop>
    static EpsExp soft(const double&z, const double& lambda, const size_t trunc = acc);

    /// Epsilon series of the soft-collinear limit
    template <size_t loop>
    static EpsExp softcoll(const double&z, const double& lambda, const size_t trunc = acc);

    /// @}

private:

    /// \name Private data members
    /// @{

    const size_t _acc;                          ///< Accuracy
    const Scheme _scheme;                       ///< Dim reg scheme
    EpsExp _r4;   ///< Storage location for precomputed r4
    EpsExp _f2;   ///< Storage location for precomputed f2
    static const double _2CF;                   ///< Shorthand for double 2.*QCD::CF
    static const double _4pi;                   ///< Shorthand for double 4.*consts::Pi

    /// @}

    /// \name Auxiliary static functions
    /// @{

    /// \brief Elementary pieces of the quark-quark splitting function, already multiplied by 1-z
    /// These have color factors stripped of and represent the new spin structures
    /// appearing at each loop
    template <size_t loop>
    static EpsExp _Pqq(const double& z);

    /// Series expansion of 2F1(1, -a*eps; 1-a*eps; z)
    static EpsExp _2F1(const double& a, const double& z, const size_t trunc = acc);

    /// @}

};

#endif

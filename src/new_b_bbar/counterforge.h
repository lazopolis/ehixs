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
 * \namespace CounterForge
 * \brief     Contains functions that are useful to build counterterms
 *
 */

namespace CounterForge
{

    /// Quick alias to current accuracy
    static const size_t& acc = Expansion<Parameter::epsilon, double>::accuracy;

    /// Switch for conventional dimensional regularization
    static bool CDR = true;

    /// \fn    geometric
    /// \brief Geometric series expansion
    /// Expansion in Par of the function 1/(1 - a Par)
    /// Type has to be double, int, complex<>, etc...
    template<Parameter Par, class Type>
    Expansion<Par,Type> geometric(const Type& a, const size_t trunc = Expansion<Par,Type>::accuracy)
    {
        vector<Type> foo({1});
        foo.reserve(trunc);
        while (foo.size()<trunc)
            foo.push_back(foo.back()*a);
        return Expansion<Par,Type>(0,foo);
    }

    /// \fn    exp
    /// \todo CHANGE NAME
    /// \brief Exponential series expansion
    /// Expansion in Par of the function a^Par
    /// Type has to be double, int, complex<>, etc...
    template<Parameter Par, class Type>
    Expansion<Par,Type> exp(const Type& a, const size_t trunc = Expansion<Par,Type>::accuracy)
    {
        vector<Type> foo({1});
        foo.reserve(trunc);
        while (foo.size()<trunc)
            foo.push_back(foo.back()*a/foo.size());
        return Expansion<Par,Type>(0,foo);
    }

    /// \fn    _2F1
    /// \brief Series expansion of 2F1(1, -a*eps; 1-a*eps; z)
    Expansion<Parameter::epsilon, double> _2F1(const double& a, const double& z, const size_t trunc = acc);

    /// \fn    Pqq
    /// \brief Quark-quark splitting function, already multiplied by 1-z
    template <size_t loop>
    Expansion<Parameter::epsilon, double> Pqq(const double& z, const double& lambda);

    /// Kosower's auxiliary factor r3
    Expansion<Parameter::epsilon, double> r3(const double& z);
    /// Kosower's auxiliary factor r4
    Expansion<Parameter::epsilon, double> r4();

    /// \fn    f1_1overz
    /// \brief Epsilon series of Kosower's auxiliary function 1/z f1(1/z)
    Expansion<Parameter::epsilon, double> f1_1overz(const double& z, const size_t trunc = acc);

    /// \fn    f1_1minus1overz
    /// \brief Epsilon series of Kosower's auxiliary function (1-1/z) f1(1-1/z)
    Expansion<Parameter::epsilon, double> f1_1minus1overz(const double& z, const size_t trunc = acc);

    /// \fn    f2
    /// \brief Epsilon series of Kosower's auxiliary 'function' f2
    Expansion<Parameter::epsilon, double> f2();



    /// \name Auxiliary functions
    /// @{

    /// \fn    _Pqq
    /// \brief Elementary pieces of the quark-quark splitting function, already multiplied by 1-z
    /// These have color factors stripped of and represent the new spin structures
    /// appearing at each loop
    template <size_t loop>
    Expansion<Parameter::epsilon, double> _Pqq(const double& z);

    /// @}

};

#endif

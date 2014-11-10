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

    /// Type has to be double, int, complex<>, etc...
    template<Parameter Par, class Type>
    Expansion<Par,Type> geometric(const Type& a, const size_t trunc)
    {
        vector<Type> foo({1});
        foo.resize(trunc); //check this
        while (foo.size()<trunc)
            foo.push_back(foo.back()*a);
        return Expansion<Par,Type>(0,foo);
    }

    /// \fn    Pqq
    /// \brief Quark-quark splitting function, already multiplied by 1-z
    template <size_t loop>
    Expansion<Parameter::epsilon, double> Pqq(const double& z, const double& lambda);

    /// Kosower's auxiliary factor r3
    Expansion<Parameter::epsilon, double> r3(const double& z);
    /// Kosower's auxiliary factor r4
    Expansion<Parameter::epsilon, double> r4();

    /**
     *
     * \class f1Type
     * \brief Kosower's auxiliary function z*f1(z)
     *
     */
    class f1Type
    {

    public:

        Expansion<Parameter::epsilon, double> operator()(const double& z)
        {
            // Alias
            const size_t& acc = Expansion<Parameter::epsilon, double>::accuracy;
            // Computing the appropriate logs and HPLs once and for all
            _computeLogs(acc, z);
            // Building f1 to the appropriate order
            Expansion<Parameter::epsilon, double> foo(-2,-2.);
            if (acc>1) foo.setCoefficient(-1, 2.*_lz);
            if (acc>2) foo.setCoefficient(0,-_lz*_lz+2.*(_lz*_l1mz+_li2z-consts::z3));
            if (acc>3) throw "Not enough coefficients were implemented in f1Type";
            return foo;
        }


    private:

        double _lz;
        double _l1mz;
        double _li2z;

        void _computeLogs(const size_t n, const double&z)
        {
            if (n==1) return;
            _lz = log(z);
            if (n==2) return;
            _l1mz = log(1.-z);
            _li2z = HPL2(0,1,z).real();
            if (n==3) return;
            throw "f1 is not implemented at this order";
            return;
        }

    };

    /// Kosower's auxiliary function f2
    Expansion<Parameter::epsilon, double> f2();

    /// \name Auxiliary functions
    /// @{

    /// \fn    _Pqq
    /// \brief Elementary pieces of the quark-quark splitting function, already multiplied by 1-z
    /// These have color factors stripped of and represent the new spin structures
    /// appearing at each loop
    template <size_t loop>
    Expansion<Parameter::epsilon, double> _Pqq(const double& z, const double& lambda);

    /// \fn _f1
    /// Coefficients of the powers of f1 as functions (for optimization)
    template<size_t power>
    double _f1(const double& z);

    /// @}

};

#endif

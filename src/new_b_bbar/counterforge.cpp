/**
 *
 * \file    counterforge.cpp
 * \author  Simone Lionetti
 * \date    November 2014
 *
 */

#include "counterforge.h"

/// \namespace CounterForge

namespace CounterForge
{

    /// One-loop conventional prefactor (4pi)^e Gamma(1+e) Gamma^2(1-e) / Gamma(1-2e)
    /// Numerically implemented: only combinations of EulerGamma,
    /// zeta(n) and log(4pi), not particularly meaningful
    Expansion<Parameter::epsilon, double> cGamma(0,{
        1.00000000000000000},true);/*,
        1.95380858206775793,
        1.08621695425669817,
        -3.16867487054886856,
        -9.62201628020990803,
        -16.4724841528570519,
        -21.6399889269925095,
        -24.8375320531365908,
        -26.0913434909829986,
        -26.4200672536006831,
        -26.1171450608306706,
        -25.8690562981899906,
        -25.5150688770234950,
        -25.4090746469793326,
        -25.2184891075254644,
        -25.2367509972286645
    });*/

    f1Type f1;

    /// \name Auxiliary functions

    /// \fn _Pqq

    template<>
    Expansion<Parameter::epsilon, double> _Pqq<0>(const double& z, const double& lambda)
    {
//        cout << "About to compute _Pqq<0>" << endl;
        return Expansion<Parameter::epsilon, double>(0,{(1.+z*z)/(1.-z),z-1.},true);
    }

    template<>
    Expansion<Parameter::epsilon, double> _Pqq<1>(const double& z, const double& lambda)
    {
//        cout << "About to compute _Pqq<1>" << endl;
        return Expansion<Parameter::epsilon, double>(0,{(1.+z)/(1.-z)},true);
    }
    
    /// \fn Pqq

    template<>
    Expansion<Parameter::epsilon, double> Pqq<0>(const double& z, const double& lambda)
    {
        const double foo = 2.*QCD::CF/lambda;
        return foo*_Pqq<0>(z,lambda);
    }

    template<>
    Expansion<Parameter::epsilon, double> Pqq<1>(const double& z, const double& lambda)
    {
        return (r3(z)*_Pqq<0>(z,lambda)+r4()*_Pqq<1>(z,lambda))/lambda;
    }

}

/// Kosower's auxiliary factor r3
Expansion<Parameter::epsilon, double> CounterForge::r3(const double& z)
{
    return 0.5*(1.*QCD::Nc*f1(1.-z)-1./QCD::Nc*(f1(z)-2.*f2()))-r4();
}

/// Kosower's auxiliary factor r4
Expansion<Parameter::epsilon, double> CounterForge::r4()
{
    const size_t& acc = Expansion<Parameter::epsilon, double>::accuracy;
    Expansion<Parameter::epsilon, double> foo(0,1.,true);
    if (!CDR) foo = geometric<Parameter::epsilon, double>(1.,acc);
    return (QCD::Nc+1./QCD::Nc) * f2()
        * foo
        * geometric<Parameter::epsilon, double>(2.,acc)
        * Expansion<Parameter::epsilon, double>(2,0.5,true);
}

/// Kosower's auxiliary function f2
Expansion<Parameter::epsilon, double> CounterForge::f2()
{
    return Expansion<Parameter::epsilon, double>(-2,-1.,true)*cGamma;
}

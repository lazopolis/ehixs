/**
 *
 * \file    counterforge.cpp
 * \author  Simone Lionetti
 * \date    November 2014
 *
 */

#include "counterforge.h"

/// \class CounterForge

/// Quick alias to current accuracy
size_t& CounterForge::acc = Expansion<Parameter::epsilon, double>::accuracy;

/// Shorthand for double 2.*QCD::CF
const double CounterForge::_2CF = 2.*QCD::CF;

/// One-loop conventional prefactor (4pi)^e Gamma(1+e) Gamma^2(1-e) / Gamma(1-2e)
/// Numerically implemented: only combinations of EulerGamma,
/// zeta(n) and log(4pi), not particularly meaningful
const Expansion<Parameter::epsilon, double> CounterForge::cGamma = Expansion<Parameter::epsilon, double>(0,{
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

/// Series espansion of (pi epsilon) cot(pi epsilon)
const Expansion<Parameter::epsilon, double> CounterForge::cotan = Expansion<Parameter::epsilon, double>(0,{
    1.000000000000000, 0.,
    -3.289868133696453, 0.,
    -2.164646467422276, 0.,
    -2.034686123968898, 0.,
    -2.008154712395889, 0.,
    -2.001989150255636, 0.});

/// \fn _Pqq

template<>
Expansion<Parameter::epsilon, double> CounterForge::_Pqq<0>(const double& z)
{
    return _2CF*Expansion<Parameter::epsilon, double>(0,{(1.+z*z)/(1.-z),z-1.},true);
}

template<>
Expansion<Parameter::epsilon, double> CounterForge::_Pqq<1>(const double& z)
{
    return _2CF*Expansion<Parameter::epsilon, double>(0,{(1.+z)/(1.-z)},true);
}

/// \fn Pqq

template<>
Expansion<Parameter::epsilon, double> CounterForge::Pqq<0>(const double& z)
{
    return _Pqq<0>(z);
}

template<>
Expansion<Parameter::epsilon, double> CounterForge::Pqq<1>(const double& z)
{
    return r3(z)*_Pqq<0>(z)+r4()*_Pqq<1>(z);
}

/// \fn    _2F1
/// \brief Series expansion of 2F1(1, -a*eps; 1-a*eps; z)-1
Expansion<Parameter::epsilon, double> CounterForge::_2F1(const double& a, const double& z, const size_t trunc)
{
    vector<double> foo({0.});
    if (trunc>1) { foo.push_back(a*log(1.-z));
    if (trunc>2) { foo.push_back(-a*a*HPL2real(0, 1, z, 0.));
    if (trunc>3) { foo.push_back(-pow(a,3)*HPL3real(0, 0, 1, z, 0.));
    if (trunc>4) { foo.push_back(-pow(a,4)*HPL4real(0, 0, 0, 1, z, 0.));
    if (trunc>5) { throw "Chaplin is not capable of computing Polylogs of weight 5."; }
    }}}}
    return Expansion<Parameter::epsilon, double>(0,foo);
}

/// Kosower's auxiliary factor r3
Expansion<Parameter::epsilon, double> CounterForge::r3(const double& z, const size_t trunc)
{
    return 0.5*(static_cast<double>(QCD::Nc)*f1_1minus1overz(z,trunc)-1./QCD::Nc*f1_1overz(z,trunc))-r4();
}

/// Kosower's auxiliary factor r4
Expansion<Parameter::epsilon, double> CounterForge::r4(const Scheme& s, const size_t trunc)
{
    Expansion<Parameter::epsilon, double> foo(0,1.,true);
    if (s == Scheme::HV) foo = Expansion<Parameter::epsilon, double>::geometric(1.,trunc);
    return (QCD::Nc+1./QCD::Nc) * f2()
        * foo
        * Expansion<Parameter::epsilon, double>::geometric(2.,trunc)
        * Expansion<Parameter::epsilon, double>(2,0.5,true);
}

/// Soft current at tree level
template<>
Expansion<Parameter::epsilon, double> CounterForge::soft<0>(const double& z, const double& lambda, const size_t trunc)
{
    return Expansion<Parameter::epsilon, double>(0,4.*QCD::CF/((1.-z)*lambda*(1.-lambda)),true);
}

/// Soft current at one loop
template<>
Expansion<Parameter::epsilon, double> CounterForge::soft<1>(const double& z, const double& lambda, const size_t trunc)
{
    return cGamma*Expansion<Parameter::epsilon, double>(-2,8.*QCD::CA*QCD::CF,true)*cotan/((1.-z)*lambda*(1.-lambda))*
    Expansion<Parameter::epsilon, double>::exp(-log(lambda))*
    Expansion<Parameter::epsilon, double>::exp(-log(1.-lambda))*
    Expansion<Parameter::epsilon, double>::exp(-2.*log(1.-z));
}

/// Soft-collinear current at tree level
template<>
Expansion<Parameter::epsilon, double> CounterForge::softcoll<0>(const double& z, const double& lambda, const size_t trunc)
{
    return Expansion<Parameter::epsilon, double>(0,4.*QCD::CF/((1.-z)*lambda),true);
}

/// Soft-collinear current at one loop
template<>
Expansion<Parameter::epsilon, double> CounterForge::softcoll<1>(const double& z, const double& lambda, const size_t trunc)
{
    return cGamma*Expansion<Parameter::epsilon, double>(-2,8.*QCD::CA*QCD::CF,true)*cotan/((1.-z)*lambda)*
    Expansion<Parameter::epsilon, double>::exp(-log(lambda))*
    Expansion<Parameter::epsilon, double>::exp(-2.*log(1.-z));
}

/// \fn    f1_1overz
Expansion<Parameter::epsilon, double> CounterForge::f1_1overz(const double& z, const size_t trunc)
{
    return cGamma*Expansion<Parameter::epsilon, double>(-2,{-2.},true)*_2F1(1.,1.-z,trunc);
}

/// \fn    f1_1minus1overz
Expansion<Parameter::epsilon, double> CounterForge::f1_1minus1overz(const double& z, const size_t trunc)
{
    return cGamma*Expansion<Parameter::epsilon, double>(-2,{2.},true)*(
                _2F1(-1.,1.-z,trunc)
                -Expansion<Parameter::epsilon, double>::exp(-log(1.-z),trunc)*cotan
                );
}

/// \fn    f2
Expansion<Parameter::epsilon, double> CounterForge::f2()
{
    return cGamma*Expansion<Parameter::epsilon, double>(-2,-1.,true);
}

/// \fn fastPqq

template<>
Expansion<Parameter::epsilon, double> CounterForge::fastPqq<0>(const double& z)
{
    return Pqq<0>(z);
}

template<>
Expansion<Parameter::epsilon, double> CounterForge::fastPqq<1>(const double& z)
{
    return fastr3(z)*_Pqq<0>(z)+_r4*_Pqq<1>(z);
}

Expansion<Parameter::epsilon, double> CounterForge::fastr3(const double& z)
{
    const double mNc = -static_cast<double>(QCD::Nc);
    const Expansion<Parameter::epsilon, double> f1_1overz_foo = _2F1(1.,1.-z,_acc);
    const Expansion<Parameter::epsilon, double> f1_1minus1overz_foo =
        _2F1(-1.,1.-z,_acc)
        - Expansion<Parameter::epsilon, double>::exp(-log(1.-z),_acc)*cotan;
    return _f2*(mNc*f1_1minus1overz_foo+1./mNc*f1_1overz_foo)-_r4;
}

/**
 *
 * \file    counterforge.cpp
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    November 2014
 *
 */

#include "counterforge.h"

/// \class CounterForge

/// Quick alias to current accuracy
size_t& CounterForge::acc = EpsExp::accuracy;

/// Shorthand for double 2.*QCD::CF
const double CounterForge::_2CF = 2.*QCD::CF;

/// Shorthand for double 4.*consts::Pi
const double CounterForge::_4pi = 4.*consts::Pi;

/// One-loop conventional prefactor (4pi)^e Gamma(1+e) Gamma^2(1-e) / Gamma(1-2e)
/// Numerically implemented: only combinations of EulerGamma,
/// zeta(n) and log(4pi), not particularly meaningful
const EpsExp& CounterForge::cGamma()
{
    static EpsExp* _cGamma =
    new EpsExp(0,{
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
    return *_cGamma;
}

/// Series expansion of (pi epsilon) cot(pi epsilon)
const EpsExp& CounterForge::cotan()
{
    static EpsExp* _cotan =
    new EpsExp(0,{
        +1.000000000000000, 0.,
        -3.289868133696453, 0.,
        -2.164646467422276, 0.,
        -2.034686123968898, 0.,
        -2.008154712395889, 0.,
        -2.001989150255636, 0.});
    return *_cotan;
}

/// Series expansion of (pi epsilon) / sin(pi epsilon)
const EpsExp& CounterForge::cosec()
{
    static EpsExp* _cosec =
    new EpsExp(0,{
        1.000000000000000, 0.,
        1.644934066848226, 0.,
        1.894065658994492, 0.,
        1.971102182594870, 0.,
        1.992466003705296, 0.,
        1.998079015196543, 0.});
    return *_cosec;
}

/// Series expansion of cos(pi epsilon)
const EpsExp& CounterForge::cos()
{
    static EpsExp* _cos =
    new EpsExp(0,{
        +1.0000000000000000,   0.,
        -4.934802200544679,    0.,
        +4.058712126416768,    0.,
        -1.335262768854589,    0.,
        +0.2353306303588932,   0.,
        -0.02580689139001406,  0.,
        +0.001929574309403923, 0.});
    return *_cos;
}

/// \fn _Pqq

template<>
EpsExp CounterForge::_Pqq<0>(const double& z)
{
    return EpsExp(0,{(1.+z*z)/(1.-z),z-1.},true);
}

template<>
EpsExp CounterForge::_Pqq<1>(const double& z)
{
    return EpsExp(0,{(1.+z)/(1.-z)},true);
}

/// \fn Pqq

template<>
EpsExp CounterForge::Pqq<0>(
                            const double& z,
                            const bool LCf,
                            const bool SCf,
                            const size_t trunc
                            )
{
    return _4pi*(LCf*QCD::CA-SCf/QCD::CA)*_Pqq<0>(z);
}

template<>
EpsExp CounterForge::Pqq<1>(
                            const double& z,
                            const bool LCf,
                            const bool SCf,
                            const size_t trunc
                            )
{
    // This generates mayhem if trunc < 3 .
    return 2.*_2CF*(
               times(r3(z,LCf,SCf,Scheme::CDR,trunc),_Pqq<0>(z),trunc) +
               times(r4(LCf,SCf,Scheme::CDR,trunc),_Pqq<1>(z),trunc).cut(trunc-2)
               );
}

/// \fn    _2F1
/// \brief Series expansion of 2F1(1, -a*eps; 1-a*eps; z)-1
/// \todo  Eliminate this implementation in favor of the one in boxmaster?!?
EpsExp CounterForge::_2F1(const double& a, const double& z, const size_t trunc)
{
    vector<double> foo({0.});
    if (trunc>1) { foo.push_back(a*log(1.-z));
    if (trunc>2) { foo.push_back(-a*a*HPL2real(0, 1, z, 0.));
    if (trunc>3) { foo.push_back(-pow(a,3)*HPL3real(0, 0, 1, z, 0.));
    if (trunc>4) { foo.push_back(-pow(a,4)*HPL4real(0, 0, 0, 1, z, 0.));
    if (trunc>5) { throw "Chaplin is not capable of computing Polylogs of weight 5."; }
    }}}}
    return EpsExp(0,foo);
}

/// Kosower's auxiliary factor r3
EpsExp CounterForge::r3(
                        const double& z,
                        const bool LCf,
                        const bool SCf,
                        const Scheme& s,
                        const size_t trunc
                        )
{
    // This generates mayhem if trunc < 3 .
    return 0.5*(LCf*QCD::CA*f1_1minus1overz(z,trunc)-SCf/QCD::CA*f1_1overz(z,trunc))-r4(LCf,SCf,s,trunc)/*.cut(trunc-2)*/;
}

/// Kosower's auxiliary factor r4
EpsExp CounterForge::r4(
                        const bool LCt,
                        const bool SCt,
                        const Scheme& s,
                        const size_t trunc
                        )
{
    EpsExp foo(0,1.,true);
    if (s == Scheme::HV) foo = EpsExp::geometric(1.,trunc);
    return (LCt*QCD::CA+SCt/QCD::CA) * f2()
        * foo
        * EpsExp::geometric(2.,trunc)
        * EpsExp(2,0.5,true);
}

/// Soft current at tree level
template<>
EpsExp CounterForge::soft<0>(const double& z, const double& lambda, const size_t trunc)
{
    return EpsExp(0,4.*QCD::CF*_4pi/((1.-z)*lambda*(1.-lambda)),true);
}

/// Soft current at one loop
template<>
EpsExp CounterForge::soft<1>(const double& z, const double& lambda, const size_t trunc)
{
    const double zbar = 1.-z;
    const double zll = zbar*lambda*(1.-lambda);
    return times(
                 times(cGamma(),cotan(),trunc),
                 times(
                       EpsExp(-2,-4.*QCD::CA*QCD::CF,true),
                       EpsExp::exp(-log(zll*zbar),trunc),
                       trunc
                       ),
                 trunc
                 )/zll;
}

/// Soft-collinear current at tree level
template<>
EpsExp CounterForge::softcoll<0>(const double& z, const double& lambda, const size_t trunc)
{
    return EpsExp(0,4.*_4pi*QCD::CF/((1.-z)*lambda),true);
}

/// Soft-collinear current at one loop
template<>
EpsExp CounterForge::softcoll<1>(const double& z, const double& lambda, const size_t trunc)
{
    const double zbar = 1.-z;
    const double zl = zbar*lambda;
    return times(
                 times(cGamma(),cotan(),trunc),
                 times(
                       EpsExp(-2,-8.*QCD::CA*QCD::CF,true),
                       EpsExp::exp(-log(zbar*zl),trunc),
                       trunc
                       ),
                 trunc
                 )/zl;
}

/// \fn    f1_1overz
EpsExp CounterForge::f1_1overz(const double& z, const size_t trunc)
{
    return times(
                 cGamma(),
                 times(
                       EpsExp(-2,-2.,true),
                       _2F1(1.,1.-z,trunc),
                       trunc
                       ),
                 trunc
                 );
}

/// \fn    f1_1minus1overz
EpsExp CounterForge::f1_1minus1overz(const double& z, const size_t trunc)
{
    return times(
                 times(cGamma(),EpsExp(-2,2.,true),trunc),
                 _2F1(-1.,1.-z,trunc)
                 -times(EpsExp::exp(-log(1.-z),trunc),cotan(),trunc),
                 trunc
                 );
}

/// \fn    f2
EpsExp CounterForge::f2(const size_t trunc)
{
    return times(cGamma(),EpsExp(-2,-1.,true),trunc);
}

/// \fn fastPqq

template<>
EpsExp CounterForge::fastPqq<0>(const double& z)
{
    return Pqq<0>(z);
}

template<>
EpsExp CounterForge::fastPqq<1>(const double& z)
{
    return fastr3(z)*_Pqq<0>(z)+_r4*_Pqq<1>(z);
}

EpsExp CounterForge::fastr3(const double& z)
{
    const double mNc = -static_cast<double>(QCD::Nc);
    const EpsExp f1_1overz_foo = _2F1(1.,1.-z,_acc);
    const EpsExp f1_1minus1overz_foo =
        _2F1(-1.,1.-z,_acc)
        - EpsExp::exp(-log(1.-z),_acc)*cotan();
    return _f2*(mNc*f1_1minus1overz_foo+1./mNc*f1_1overz_foo)-_r4;
}


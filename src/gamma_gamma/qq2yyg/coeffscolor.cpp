/**
 *
 * \file    coeffscolor.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    March 2015
 *
 */

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

#include "coeffscolor.h"
#include "dircolor/coeffLC1.h"
#include "dircolor/coeffLC2.h"
#include "dircolor/coeffLC3.h"
#include "dircolor/coeffLC4.h"
#include "dircolor/coeffLC5.h"
#include "dircolor/coeffLC6.h"
#include "dircolor/coeffLC7.h"
#include "dircolor/coeffLC8.h"
#include "dircolor/coeffLC9.h"
#include "dircolor/coeffLC10.h"
#include "dircolor/coeffLC11.h"
#include "dircolor/coeffLC12.h"
#include "dircolor/coeffLC13.h"
#include "dircolor/coeffLC14.h"
#include "dircolor/coeffLC15.h"
#include "dircolor/coeffLC16.h"
#include "dircolor/coeffLC17.h"
#include "dircolor/coeffLC18.h"
#include "dircolor/coeffSC1.h"
#include "dircolor/coeffSC2.h"
#include "dircolor/coeffSC3.h"
#include "dircolor/coeffSC4.h"
#include "dircolor/coeffSC5.h"
#include "dircolor/coeffSC6.h"
#include "dircolor/coeffSC7.h"
#include "dircolor/coeffSC8.h"
#include "dircolor/coeffSC9.h"
#include "dircolor/coeffSC10.h"
#include "dircolor/coeffSC11.h"
#include "dircolor/coeffSC12.h"
#include "dircolor/coeffSC13.h"
#include "dircolor/coeffSC14.h"
#include "dircolor/coeffSC15.h"
#include "dircolor/coeffSC16.h"
#include "dircolor/coeffSC17.h"
#include "dircolor/coeffSC18.h"
#include "dircolor/coeffSC19.h"
#include "dircolor/coeffSC20.h"
#include "dircolor/coeffSC21.h"
#include "dircolor/coeffSC22.h"
#include "dircolor/coeffSC23.h"
#include "dircolor/coeffSC24.h"
#include "dircolor/coeffSC25.h"
#include "dircolor/coeffSC26.h"
#include "dircolor/coeffSC27.h"
#include "dircolor/coeffSC28.h"
#include "dircolor/coeffSC29.h"
#include "dircolor/coeffSC30.h"
#include "dircolor/coeffSC31.h"

double qq2yygLCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooLCbub(0.);
    fooLCbub += productCoeff(qq2yygLC<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooLCbub += productCoeff(qq2yygLC<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooLCbub += productCoeff(qq2yygLC<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooLCbub += productCoeff(qq2yygLC<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooLCbub += productCoeff(qq2yygLC<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooLCbub += productCoeff(qq2yygLC<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooLCbub += productCoeff(qq2yygLC<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return QCD::CA*fooLCbub;
}

double qq2yygLCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooLCbox(0.);
    fooLCbox += productCoeff(qq2yygLC<8>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14,s24,3),0);
    fooLCbox += productCoeff(qq2yygLC<9>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yygLC<10>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14,s23,3),0);
    fooLCbox += productCoeff(qq2yygLC<11>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yygLC<12>(s12,s13,s14,s23,s24),box(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yygLC<13>(s12,s13,s14,s23,s24),box(s23,-s12-s23-s24,s14,3),0);
    fooLCbox += productCoeff(qq2yygLC<14>(s12,s13,s14,s23,s24),box(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yygLC<15>(s12,s13,s14,s23,s24),box(s24,-s12-s23-s24,s13,3),0);
    fooLCbox += productCoeff(qq2yygLC<16>(s12,s13,s14,s23,s24),box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yygLC<17>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yygLC<18>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    return QCD::CA*fooLCbox;
}

double qq2yygSCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooSCbub(0.);
    fooSCbub += productCoeff(qq2yygSC<1>(s12,s13,s14,s23,s24),bubble(s12,3),0);
    fooSCbub += productCoeff(qq2yygSC<2>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooSCbub += productCoeff(qq2yygSC<3>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooSCbub += productCoeff(qq2yygSC<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooSCbub += productCoeff(qq2yygSC<5>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooSCbub += productCoeff(qq2yygSC<6>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooSCbub += productCoeff(qq2yygSC<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooSCbub += productCoeff(qq2yygSC<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    fooSCbub += productCoeff(qq2yygSC<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0);
    fooSCbub += productCoeff(qq2yygSC<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbub;
}

double qq2yygSCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooSCbox(0.);
    fooSCbox += productCoeff(qq2yygSC<11>(s12,s13,s14,s23,s24),box6(s12,s13,s12+s13+s23,3),0);
    fooSCbox += productCoeff(qq2yygSC<12>(s12,s13,s14,s23,s24),box6(s12,s14,s12+s14+s24,3),0);
    fooSCbox += productCoeff(qq2yygSC<13>(s12,s13,s14,s23,s24),box6(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yygSC<14>(s12,s13,s14,s23,s24),box6(s12,s23,s12+s13+s23,3),0);
    fooSCbox += productCoeff(qq2yygSC<15>(s12,s13,s14,s23,s24),box6(s12,s24,s12+s14+s24,3),0);
    fooSCbox += productCoeff(qq2yygSC<16>(s12,s13,s14,s23,s24),box6(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yygSC<17>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yygSC<18>(s12,s13,s14,s23,s24),box6(s13,s12+s14+s24,s24,3),0);
    fooSCbox += productCoeff(qq2yygSC<19>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yygSC<20>(s12,s13,s14,s23,s24),box6(s14,s12+s13+s23,s23,3),0);
    fooSCbox += productCoeff(qq2yygSC<21>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s14+s24,s24,3),0);
    fooSCbox += productCoeff(qq2yygSC<22>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s13+s23,s23,3),0);
    fooSCbox += productCoeff(qq2yygSC<23>(s12,s13,s14,s23,s24),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooSCbox += productCoeff(qq2yygSC<24>(s12,s13,s14,s23,s24),box6(s23,s12+s14+s24,s14,3),0);
    fooSCbox += productCoeff(qq2yygSC<25>(s12,s13,s14,s23,s24),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooSCbox += productCoeff(qq2yygSC<26>(s12,s13,s14,s23,s24),box6(s24,s12+s13+s23,s13,3),0);
    fooSCbox += productCoeff(qq2yygSC<27>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s14+s24,s14,3),0);
    fooSCbox += productCoeff(qq2yygSC<28>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s13+s23,s13,3),0);
    fooSCbox += productCoeff(qq2yygSC<29>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
    fooSCbox += productCoeff(qq2yygSC<30>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
    fooSCbox += productCoeff(qq2yygSC<31>(s12,s13,s14,s23,s24),box6(s12+s14+s24,s12+s13+s23,s12,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbox;
}

double qq2yygcol(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (
            qq2yygLCbub(s12,s13,s14,s23,s24)+qq2yygLCbox(s12,s13,s14,s23,s24)+
            qq2yygSCbub(s12,s13,s14,s23,s24)+qq2yygSCbox(s12,s13,s14,s23,s24)
            );
}

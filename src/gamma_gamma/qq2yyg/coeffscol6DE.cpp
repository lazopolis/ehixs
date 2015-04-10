/**
 *
 * \file    coeffscol6DE.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    March 2015
 *
 */

#include "coeffscol6DE.h"

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

double qq2yyg6ELCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -s12-s13-s14;
    const double s25 = -s12-s23-s24;
    double fooLCbub(0.);
    fooLCbub += productCoeff(qq2yyg6ELC<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooLCbub += productCoeff(qq2yyg6ELC<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooLCbub += productCoeff(qq2yyg6ELC<3>(s12,s13,s14,s23,s24),bubble(s15,3),0);
    fooLCbub += productCoeff(qq2yyg6ELC<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooLCbub += productCoeff(qq2yyg6ELC<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooLCbub += productCoeff(qq2yyg6ELC<6>(s12,s13,s14,s23,s24),bubble(s25,3),0);
    fooLCbub += productCoeff(qq2yyg6ELC<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return QCD::CA*fooLCbub;
}

double qq2yyg6ELCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooLCbox(0.);
    fooLCbox += productCoeff(qq2yyg6ELC<8 >(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14,s24,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<9 >(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<10>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14,s23,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<11>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<12>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<13>(s12,s13,s14,s23,s24),box6(s23,-s12-s23-s24,s14,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<14>(s12,s13,s14,s23,s24),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<15>(s12,s13,s14,s23,s24),box6(s24,-s12-s23-s24,s13,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<16>(s12,s13,s14,s23,s24),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<17>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yyg6ELC<18>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    return QCD::CA*fooLCbox;
}

//double qq2yyg6ESCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
//{
//    double fooSCbub(0.);
//    fooSCbub += productCoeff(qq2yyg6ESC<1>(s12,s13,s14,s23,s24),bubble(s12,3),0);
//    fooSCbub += productCoeff(qq2yyg6ESC<2>(s12,s13,s14,s23,s24),bubble(s13,3),0);
//    fooSCbub += productCoeff(qq2yyg6ESC<3>(s12,s13,s14,s23,s24),bubble(s14,3),0);
//    fooSCbub += productCoeff(qq2yyg6ESC<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
//    fooSCbub += productCoeff(qq2yyg6ESC<5>(s12,s13,s14,s23,s24),bubble(s23,3),0);
//    fooSCbub += productCoeff(qq2yyg6ESC<6>(s12,s13,s14,s23,s24),bubble(s24,3),0);
//    fooSCbub += productCoeff(qq2yyg6ESC<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
//    fooSCbub += productCoeff(qq2yyg6ESC<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
//    fooSCbub += productCoeff(qq2yyg6ESC<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0);
//    fooSCbub += productCoeff(qq2yyg6ESC<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0);
//    return (QCD::CA-2.*QCD::CF)*fooSCbub;
//}
//
//double qq2yyg6ESCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
//{
//    double fooSCbox(0.);
//    fooSCbox += productCoeff(qq2yyg6ESC<11>(s12,s13,s14,s23,s24),box6(s12,s13,s12+s13+s23,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<12>(s12,s13,s14,s23,s24),box6(s12,s14,s12+s14+s24,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<13>(s12,s13,s14,s23,s24),box6(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<14>(s12,s13,s14,s23,s24),box6(s12,s23,s12+s13+s23,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<15>(s12,s13,s14,s23,s24),box6(s12,s24,s12+s14+s24,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<16>(s12,s13,s14,s23,s24),box6(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<17>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<18>(s12,s13,s14,s23,s24),box6(s13,s12+s14+s24,s24,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<19>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<20>(s12,s13,s14,s23,s24),box6(s14,s12+s13+s23,s23,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<21>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s14+s24,s24,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<22>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s13+s23,s23,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<23>(s12,s13,s14,s23,s24),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<24>(s12,s13,s14,s23,s24),box6(s23,s12+s14+s24,s14,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<25>(s12,s13,s14,s23,s24),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<26>(s12,s13,s14,s23,s24),box6(s24,s12+s13+s23,s13,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<27>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s14+s24,s14,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<28>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s13+s23,s13,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<29>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<30>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
//    fooSCbox += productCoeff(qq2yyg6ESC<31>(s12,s13,s14,s23,s24),box6(s12+s14+s24,s12+s13+s23,s12,3),0);
//    return (QCD::CA-2.*QCD::CF)*fooSCbox;
//}
//
//double qq2yyg6Ecol(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
//{
//    return (
//            qq2yyg6ELCbub(s12,s13,s14,s23,s24)+qq2yyg6ELCbox(s12,s13,s14,s23,s24)+
//            qq2yyg6ESCbub(s12,s13,s14,s23,s24)+qq2yyg6ESCbox(s12,s13,s14,s23,s24)
//            );
//}

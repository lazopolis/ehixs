/**
 *
 * \file    coeffs6D.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    February 2015
 *
 */

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

#include "coeffs6D.h"
#include "dir6D/coeff6CA1.h"
#include "dir6D/coeff6CA10.h"
#include "dir6D/coeff6CA11.h"
#include "dir6D/coeff6CA12.h"
#include "dir6D/coeff6CA13.h"
#include "dir6D/coeff6CA14.h"
#include "dir6D/coeff6CA15.h"
#include "dir6D/coeff6CA16.h"
#include "dir6D/coeff6CA2.h"
#include "dir6D/coeff6CA3.h"
#include "dir6D/coeff6CA4.h"
#include "dir6D/coeff6CA5.h"
#include "dir6D/coeff6CA6.h"
#include "dir6D/coeff6CA7.h"
#include "dir6D/coeff6CA8.h"
#include "dir6D/coeff6CA9.h"
#include "dir6D/coeff6CAm2CF1.h"
#include "dir6D/coeff6CAm2CF10.h"
#include "dir6D/coeff6CAm2CF11.h"
#include "dir6D/coeff6CAm2CF12.h"
#include "dir6D/coeff6CAm2CF13.h"
#include "dir6D/coeff6CAm2CF14.h"
#include "dir6D/coeff6CAm2CF15.h"
#include "dir6D/coeff6CAm2CF16.h"
#include "dir6D/coeff6CAm2CF17.h"
#include "dir6D/coeff6CAm2CF18.h"
#include "dir6D/coeff6CAm2CF19.h"
#include "dir6D/coeff6CAm2CF2.h"
#include "dir6D/coeff6CAm2CF20.h"
#include "dir6D/coeff6CAm2CF21.h"
#include "dir6D/coeff6CAm2CF22.h"
#include "dir6D/coeff6CAm2CF23.h"
#include "dir6D/coeff6CAm2CF24.h"
#include "dir6D/coeff6CAm2CF25.h"
#include "dir6D/coeff6CAm2CF26.h"
#include "dir6D/coeff6CAm2CF27.h"
#include "dir6D/coeff6CAm2CF28.h"
#include "dir6D/coeff6CAm2CF29.h"
#include "dir6D/coeff6CAm2CF3.h"
#include "dir6D/coeff6CAm2CF30.h"
#include "dir6D/coeff6CAm2CF31.h"
#include "dir6D/coeff6CAm2CF4.h"
#include "dir6D/coeff6CAm2CF5.h"
#include "dir6D/coeff6CAm2CF6.h"
#include "dir6D/coeff6CAm2CF7.h"
#include "dir6D/coeff6CAm2CF8.h"
#include "dir6D/coeff6CAm2CF9.h"
#include "dir6D/coeff6CF1.h"
#include "dir6D/coeff6CF10.h"
#include "dir6D/coeff6CF11.h"
#include "dir6D/coeff6CF2.h"
#include "dir6D/coeff6CF3.h"
#include "dir6D/coeff6CF4.h"
#include "dir6D/coeff6CF5.h"
#include "dir6D/coeff6CF6.h"
#include "dir6D/coeff6CF7.h"
#include "dir6D/coeff6CF8.h"
#include "dir6D/coeff6CF9.h"

double qq2yyg6(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double CAm2CF = QCD::CA-2.*QCD::CF;
    double foo = 0.;
    foo += QCD::CA*productCoeff(qq2yyg6CA<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<8>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14,s24,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<9>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<10>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14,s23,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<11>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<12>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<13>(s12,s13,s14,s23,s24),box6(s23,-s12-s23-s24,s14,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<14>(s12,s13,s14,s23,s24),box6(s24,-s12-s23-s24,s13,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<15>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    foo += QCD::CA*productCoeff(qq2yyg6CA<16>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<8>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<9>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<10>(s12,s13,s14,s23,s24),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    foo += QCD::CF*productCoeff(qq2yyg6CF<11>(s12,s13,s14,s23,s24),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<1>(s12,s13,s14,s23,s24),bubble(s12,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<2>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<3>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<5>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<6>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<11>(s12,s13,s14,s23,s24),box6(s12,s13,s12+s13+s23,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<12>(s12,s13,s14,s23,s24),box6(s12,s14,s12+s14+s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<13>(s12,s13,s14,s23,s24),box6(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<14>(s12,s13,s14,s23,s24),box6(s12,s23,s12+s13+s23,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<15>(s12,s13,s14,s23,s24),box6(s12,s24,s12+s14+s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<16>(s12,s13,s14,s23,s24),box6(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<17>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<18>(s12,s13,s14,s23,s24),box6(s13,s12+s14+s24,s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<19>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<20>(s12,s13,s14,s23,s24),box6(s14,s12+s13+s23,s23,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<21>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s14+s24,s24,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<22>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s13+s23,s23,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<23>(s12,s13,s14,s23,s24),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<24>(s12,s13,s14,s23,s24),box6(s23,s12+s14+s24,s14,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<25>(s12,s13,s14,s23,s24),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<26>(s12,s13,s14,s23,s24),box6(s24,s12+s13+s23,s13,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<27>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s14+s24,s14,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<28>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s13+s23,s13,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<29>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<30>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
    foo += CAm2CF*productCoeff(qq2yyg6CAm2CF<31>(s12,s13,s14,s23,s24),box6(s12+s14+s24,s12+s13+s23,s12,3),0);
    return foo;
}

double qq2yyg6CAbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCAbub(0.);
    fooCAbub += QCD::CA*productCoeff(qq2yyg6CA<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yyg6CA<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yyg6CA<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yyg6CA<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yyg6CA<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yyg6CA<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yyg6CA<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return fooCAbub;
}

double qq2yyg6CAbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCAbox(0.);
    fooCAbox += QCD::CA*productCoeff(qq2yyg6CA<8>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14,s24,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yyg6CA<9>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yyg6CA<10>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14,s23,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yyg6CA<11>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yyg6CA<12>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yyg6CA<13>(s12,s13,s14,s23,s24),box6(s23,-s12-s23-s24,s14,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yyg6CA<14>(s12,s13,s14,s23,s24),box6(s24,-s12-s23-s24,s13,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yyg6CA<15>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yyg6CA<16>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    return fooCAbox;
}

double qq2yyg6CFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCFbub(0.);
    fooCFbub += QCD::CF*productCoeff(qq2yyg6CF<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yyg6CF<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yyg6CF<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yyg6CF<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yyg6CF<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yyg6CF<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yyg6CF<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return fooCFbub;
}

double qq2yyg6CFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCFbox(0.);
    fooCFbox += QCD::CF*productCoeff(qq2yyg6CF<8>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCFbox += QCD::CF*productCoeff(qq2yyg6CF<9>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCFbox += QCD::CF*productCoeff(qq2yyg6CF<10>(s12,s13,s14,s23,s24),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooCFbox += QCD::CF*productCoeff(qq2yyg6CF<11>(s12,s13,s14,s23,s24),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    return fooCFbox;
}

double qq2yyg6AFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double CAm2CF = QCD::CA-2.*QCD::CF;
    double fooAFbub(0.);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<1>(s12,s13,s14,s23,s24),bubble(s12,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<2>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<3>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<5>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<6>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yyg6CAm2CF<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0);
    return fooAFbub;
}

double qq2yyg6AFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double CAm2CF = QCD::CA-2.*QCD::CF;
    double fooAFbox(0.);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<11>(s12,s13,s14,s23,s24),box6(s12,s13,s12+s13+s23,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<12>(s12,s13,s14,s23,s24),box6(s12,s14,s12+s14+s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<13>(s12,s13,s14,s23,s24),box6(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<14>(s12,s13,s14,s23,s24),box6(s12,s23,s12+s13+s23,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<15>(s12,s13,s14,s23,s24),box6(s12,s24,s12+s14+s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<16>(s12,s13,s14,s23,s24),box6(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<17>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<18>(s12,s13,s14,s23,s24),box6(s13,s12+s14+s24,s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<19>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<20>(s12,s13,s14,s23,s24),box6(s14,s12+s13+s23,s23,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<21>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s14+s24,s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<22>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s13+s23,s23,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<23>(s12,s13,s14,s23,s24),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<24>(s12,s13,s14,s23,s24),box6(s23,s12+s14+s24,s14,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<25>(s12,s13,s14,s23,s24),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<26>(s12,s13,s14,s23,s24),box6(s24,s12+s13+s23,s13,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<27>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s14+s24,s14,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<28>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s13+s23,s13,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<29>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<30>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yyg6CAm2CF<31>(s12,s13,s14,s23,s24),box6(s12+s14+s24,s12+s13+s23,s12,3),0);
    return fooAFbox;
}

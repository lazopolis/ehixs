/**
 *
 * \file    coeffs4D.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

#include "coeffs4D.h"
#include "dir4D/coeff4CA1.h"
#include "dir4D/coeff4CA10.h"
#include "dir4D/coeff4CA11.h"
#include "dir4D/coeff4CA12.h"
#include "dir4D/coeff4CA13.h"
#include "dir4D/coeff4CA14.h"
#include "dir4D/coeff4CA15.h"
#include "dir4D/coeff4CA16.h"
#include "dir4D/coeff4CA2.h"
#include "dir4D/coeff4CA3.h"
#include "dir4D/coeff4CA4.h"
#include "dir4D/coeff4CA5.h"
#include "dir4D/coeff4CA6.h"
#include "dir4D/coeff4CA7.h"
#include "dir4D/coeff4CA8.h"
#include "dir4D/coeff4CA9.h"
#include "dir4D/coeff4CAm2CF1.h"
#include "dir4D/coeff4CAm2CF10.h"
#include "dir4D/coeff4CAm2CF11.h"
#include "dir4D/coeff4CAm2CF12.h"
#include "dir4D/coeff4CAm2CF13.h"
#include "dir4D/coeff4CAm2CF14.h"
#include "dir4D/coeff4CAm2CF15.h"
#include "dir4D/coeff4CAm2CF16.h"
#include "dir4D/coeff4CAm2CF17.h"
#include "dir4D/coeff4CAm2CF18.h"
#include "dir4D/coeff4CAm2CF19.h"
#include "dir4D/coeff4CAm2CF2.h"
#include "dir4D/coeff4CAm2CF20.h"
#include "dir4D/coeff4CAm2CF21.h"
#include "dir4D/coeff4CAm2CF22.h"
#include "dir4D/coeff4CAm2CF23.h"
#include "dir4D/coeff4CAm2CF24.h"
#include "dir4D/coeff4CAm2CF25.h"
#include "dir4D/coeff4CAm2CF26.h"
#include "dir4D/coeff4CAm2CF27.h"
#include "dir4D/coeff4CAm2CF28.h"
#include "dir4D/coeff4CAm2CF29.h"
#include "dir4D/coeff4CAm2CF3.h"
#include "dir4D/coeff4CAm2CF30.h"
#include "dir4D/coeff4CAm2CF31.h"
#include "dir4D/coeff4CAm2CF4.h"
#include "dir4D/coeff4CAm2CF5.h"
#include "dir4D/coeff4CAm2CF6.h"
#include "dir4D/coeff4CAm2CF7.h"
#include "dir4D/coeff4CAm2CF8.h"
#include "dir4D/coeff4CAm2CF9.h"
#include "dir4D/coeff4CF1.h"
#include "dir4D/coeff4CF10.h"
#include "dir4D/coeff4CF11.h"
#include "dir4D/coeff4CF2.h"
#include "dir4D/coeff4CF3.h"
#include "dir4D/coeff4CF4.h"
#include "dir4D/coeff4CF5.h"
#include "dir4D/coeff4CF6.h"
#include "dir4D/coeff4CF7.h"
#include "dir4D/coeff4CF8.h"
#include "dir4D/coeff4CF9.h"

double qq2yyg4CAbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCAbub(0.);
    fooCAbub += productCoeff(qq2yyg4CA<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooCAbub += productCoeff(qq2yyg4CA<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooCAbub += productCoeff(qq2yyg4CA<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooCAbub += productCoeff(qq2yyg4CA<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooCAbub += productCoeff(qq2yyg4CA<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooCAbub += productCoeff(qq2yyg4CA<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooCAbub += productCoeff(qq2yyg4CA<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return QCD::CA*fooCAbub;
}

double qq2yyg4CAbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCAbox(0.);
    fooCAbox += productCoeff(qq2yyg4CA<8>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14,s24,3),0);
    fooCAbox += productCoeff(qq2yyg4CA<9>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCAbox += productCoeff(qq2yyg4CA<10>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14,s23,3),0);
    fooCAbox += productCoeff(qq2yyg4CA<11>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCAbox += productCoeff(qq2yyg4CA<12>(s12,s13,s14,s23,s24),box(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooCAbox += productCoeff(qq2yyg4CA<13>(s12,s13,s14,s23,s24),box(s23,-s12-s23-s24,s14,3),0);
    fooCAbox += productCoeff(qq2yyg4CA<14>(s12,s13,s14,s23,s24),box(s24,-s12-s23-s24,s13,3),0);
    fooCAbox += productCoeff(qq2yyg4CA<15>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    fooCAbox += productCoeff(qq2yyg4CA<16>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    return QCD::CA*fooCAbox;
}

double qq2yyg4CFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCFbub(0.);
    fooCFbub += productCoeff(qq2yyg4CF<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooCFbub += productCoeff(qq2yyg4CF<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooCFbub += productCoeff(qq2yyg4CF<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooCFbub += productCoeff(qq2yyg4CF<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooCFbub += productCoeff(qq2yyg4CF<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooCFbub += productCoeff(qq2yyg4CF<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooCFbub += productCoeff(qq2yyg4CF<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return QCD::CF*fooCFbub;
}

double qq2yyg4CFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCFbox(0.);
    fooCFbox += productCoeff(qq2yyg4CF<8>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCFbox += productCoeff(qq2yyg4CF<9>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCFbox += productCoeff(qq2yyg4CF<10>(s12,s13,s14,s23,s24),box(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooCFbox += productCoeff(qq2yyg4CF<11>(s12,s13,s14,s23,s24),box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    return QCD::CF*fooCFbox;
}

double qq2yyg4AFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooAFbub(0.);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<1>(s12,s13,s14,s23,s24),bubble(s12,3),0);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<2>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<3>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<5>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<6>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0);
    fooAFbub += productCoeff(qq2yyg4CAm2CF<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0);
    return (QCD::CA-2.*QCD::CF)*fooAFbub;
}

double qq2yyg4AFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooAFbox(0.);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<11>(s12,s13,s14,s23,s24),box(s12,s13,s12+s13+s23,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<12>(s12,s13,s14,s23,s24),box(s12,s14,s12+s14+s24,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<13>(s12,s13,s14,s23,s24),box(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<14>(s12,s13,s14,s23,s24),box(s12,s23,s12+s13+s23,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<15>(s12,s13,s14,s23,s24),box(s12,s24,s12+s14+s24,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<16>(s12,s13,s14,s23,s24),box(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<17>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<18>(s12,s13,s14,s23,s24),box(s13,s12+s14+s24,s24,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<19>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<20>(s12,s13,s14,s23,s24),box(s14,s12+s13+s23,s23,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<21>(s12,s13,s14,s23,s24),box(-s12-s13-s14,s12+s14+s24,s24,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<22>(s12,s13,s14,s23,s24),box(-s12-s13-s14,s12+s13+s23,s23,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<23>(s12,s13,s14,s23,s24),box(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<24>(s12,s13,s14,s23,s24),box(s23,s12+s14+s24,s14,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<25>(s12,s13,s14,s23,s24),box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<26>(s12,s13,s14,s23,s24),box(s24,s12+s13+s23,s13,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<27>(s12,s13,s14,s23,s24),box(-s12-s23-s24,s12+s14+s24,s14,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<28>(s12,s13,s14,s23,s24),box(-s12-s23-s24,s12+s13+s23,s13,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<29>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<30>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
    fooAFbox += productCoeff(qq2yyg4CAm2CF<31>(s12,s13,s14,s23,s24),box(s12+s14+s24,s12+s13+s23,s12,3),0);
    return (QCD::CA-2.*QCD::CF)*fooAFbox;
}

double qq2yyg4(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (
            qq2yyg4CAbub(s12,s13,s14,s23,s24)+qq2yyg4CAbox(s12,s13,s14,s23,s24)+
            qq2yyg4CFbub(s12,s13,s14,s23,s24)+qq2yyg4CFbox(s12,s13,s14,s23,s24)+
            qq2yyg4AFbub(s12,s13,s14,s23,s24)+qq2yyg4AFbox(s12,s13,s14,s23,s24)
            );
}

/**
 *
 * \file    coeffsz6D.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

#include "coeffsz6D.h"
#include "dirz6D/coeffz6CA1.h"
#include "dirz6D/coeffz6CA10.h"
#include "dirz6D/coeffz6CA11.h"
#include "dirz6D/coeffz6CA12.h"
#include "dirz6D/coeffz6CA13.h"
#include "dirz6D/coeffz6CA14.h"
#include "dirz6D/coeffz6CA15.h"
#include "dirz6D/coeffz6CA16.h"
#include "dirz6D/coeffz6CA2.h"
#include "dirz6D/coeffz6CA3.h"
#include "dirz6D/coeffz6CA4.h"
#include "dirz6D/coeffz6CA5.h"
#include "dirz6D/coeffz6CA6.h"
#include "dirz6D/coeffz6CA7.h"
#include "dirz6D/coeffz6CA8.h"
#include "dirz6D/coeffz6CA9.h"
#include "dirz6D/coeffz6CAm2CF1.h"
#include "dirz6D/coeffz6CAm2CF2.h"
#include "dirz6D/coeffz6CAm2CF3.h"
#include "dirz6D/coeffz6CAm2CF4.h"
#include "dirz6D/coeffz6CAm2CF5.h"
#include "dirz6D/coeffz6CAm2CF6.h"
#include "dirz6D/coeffz6CAm2CF7.h"
#include "dirz6D/coeffz6CAm2CF8.h"
#include "dirz6D/coeffz6CAm2CF9.h"
#include "dirz6D/coeffz6CAm2CF10.h"
#include "dirz6D/coeffz6CAm2CF11.h"
#include "dirz6D/coeffz6CAm2CF12.h"
#include "dirz6D/coeffz6CAm2CF13.h"
#include "dirz6D/coeffz6CAm2CF14.h"
#include "dirz6D/coeffz6CAm2CF15.h"
#include "dirz6D/coeffz6CAm2CF16.h"
#include "dirz6D/coeffz6CAm2CF17.h"
#include "dirz6D/coeffz6CAm2CF18.h"
#include "dirz6D/coeffz6CAm2CF19.h"
#include "dirz6D/coeffz6CAm2CF20.h"
#include "dirz6D/coeffz6CAm2CF21.h"
#include "dirz6D/coeffz6CAm2CF22.h"
#include "dirz6D/coeffz6CAm2CF23.h"
#include "dirz6D/coeffz6CAm2CF24.h"
#include "dirz6D/coeffz6CAm2CF25.h"
#include "dirz6D/coeffz6CAm2CF26.h"
#include "dirz6D/coeffz6CAm2CF27.h"
#include "dirz6D/coeffz6CAm2CF28.h"
#include "dirz6D/coeffz6CAm2CF29.h"
#include "dirz6D/coeffz6CAm2CF30.h"
#include "dirz6D/coeffz6CAm2CF31.h"
#include "dirz6D/coeffz6CF1.h"
#include "dirz6D/coeffz6CF2.h"
#include "dirz6D/coeffz6CF3.h"
#include "dirz6D/coeffz6CF4.h"
#include "dirz6D/coeffz6CF5.h"
#include "dirz6D/coeffz6CF6.h"
#include "dirz6D/coeffz6CF7.h"
#include "dirz6D/coeffz6CF8.h"
#include "dirz6D/coeffz6CF9.h"
#include "dirz6D/coeffz6CF10.h"
#include "dirz6D/coeffz6CF11.h"

double qq2yygz6CAbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -s12-s13-s14;
    const double s25 = -s12-s23-s24;
    const double zb = -(s15+s25)/s12;
    const double s15n = s15/zb;
    const double s25n = s25/zb;
    const double s35n = (s12+s14+s24)/zb;
    double fooCAbub(0.);
    fooCAbub += productCoeff(qq2yygz6CA<1>(s12,s13,s15n,s25n,s35n,zb),bubble(s13,3),0);
    fooCAbub += productCoeff(qq2yygz6CA<2>(s12,s13,s15n,s25n,s35n,zb),bubble(s14,3),0);
    fooCAbub += productCoeff(qq2yygz6CA<3>(s12,s13,s15n,s25n,s35n,zb),bubble(-s12-s13-s14,3),0);
    fooCAbub += productCoeff(qq2yygz6CA<4>(s12,s13,s15n,s25n,s35n,zb),bubble(s23,3),0);
    fooCAbub += productCoeff(qq2yygz6CA<5>(s12,s13,s15n,s25n,s35n,zb),bubble(s24,3),0);
    fooCAbub += productCoeff(qq2yygz6CA<6>(s12,s13,s15n,s25n,s35n,zb),bubble(-s12-s23-s24,3),0);
    fooCAbub += productCoeff(qq2yygz6CA<7>(s12,s13,s15n,s25n,s35n,zb),bubble(-s12-s13-s14-s23-s24,3),0);
    return QCD::CA*fooCAbub;
}

double qq2yygz6CAbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -s12-s13-s14;
    const double s25 = -s12-s23-s24;
    const double zb = -(s15+s25)/s12;
    const double s15n = s15/zb;
    const double s25n = s25/zb;
    const double s35n = (s12+s14+s24)/zb;
    double fooCAbox(0.);
    fooCAbox += productCoeff(qq2yygz6CA<8>(s12,s13,s15n,s25n,s35n,zb),box6(s13,-s12-s13-s14,s24,3),0);
    fooCAbox += productCoeff(qq2yygz6CA<9>(s12,s13,s15n,s25n,s35n,zb),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCAbox += productCoeff(qq2yygz6CA<10>(s12,s13,s15n,s25n,s35n,zb),box6(s14,-s12-s13-s14,s23,3),0);
    fooCAbox += productCoeff(qq2yygz6CA<11>(s12,s13,s15n,s25n,s35n,zb),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCAbox += productCoeff(qq2yygz6CA<12>(s12,s13,s15n,s25n,s35n,zb),box6(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooCAbox += productCoeff(qq2yygz6CA<13>(s12,s13,s15n,s25n,s35n,zb),box6(s23,-s12-s23-s24,s14,3),0);
    fooCAbox += productCoeff(qq2yygz6CA<14>(s12,s13,s15n,s25n,s35n,zb),box6(s24,-s12-s23-s24,s13,3),0);
    fooCAbox += productCoeff(qq2yygz6CA<15>(s12,s13,s15n,s25n,s35n,zb),box6(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    fooCAbox += productCoeff(qq2yygz6CA<16>(s12,s13,s15n,s25n,s35n,zb),box6(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    return QCD::CA*fooCAbox;
}

double qq2yygz6CFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -s12-s13-s14;
    const double s25 = -s12-s23-s24;
    const double zb = -(s15+s25)/s12;
    const double s15n = s15/zb;
    const double s25n = s25/zb;
    const double s35n = (s12+s14+s24)/zb;
    double fooCFbub(0.);
    fooCFbub += productCoeff(qq2yygz6CF<1>(s12,s13,s15n,s25n,s35n,zb),bubble(s13,3),0);
    fooCFbub += productCoeff(qq2yygz6CF<2>(s12,s13,s15n,s25n,s35n,zb),bubble(s14,3),0);
    fooCFbub += productCoeff(qq2yygz6CF<3>(s12,s13,s15n,s25n,s35n,zb),bubble(-s12-s13-s14,3),0);
    fooCFbub += productCoeff(qq2yygz6CF<4>(s12,s13,s15n,s25n,s35n,zb),bubble(s23,3),0);
    fooCFbub += productCoeff(qq2yygz6CF<5>(s12,s13,s15n,s25n,s35n,zb),bubble(s24,3),0);
    fooCFbub += productCoeff(qq2yygz6CF<6>(s12,s13,s15n,s25n,s35n,zb),bubble(-s12-s23-s24,3),0);
    fooCFbub += productCoeff(qq2yygz6CF<7>(s12,s13,s15n,s25n,s35n,zb),bubble(-s12-s13-s14-s23-s24,3),0);
    return QCD::CF*fooCFbub;
}

double qq2yygz6CFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -s12-s13-s14;
    const double s25 = -s12-s23-s24;
    const double zb = -(s15+s25)/s12;
    const double s15n = s15/zb;
    const double s25n = s25/zb;
    const double s35n = (s12+s14+s24)/zb;
    double fooCFbox(0.);
    fooCFbox += productCoeff(qq2yygz6CF<8>(s12,s13,s15n,s25n,s35n,zb),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCFbox += productCoeff(qq2yygz6CF<9>(s12,s13,s15n,s25n,s35n,zb),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCFbox += productCoeff(qq2yygz6CF<10>(s12,s13,s15n,s25n,s35n,zb),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooCFbox += productCoeff(qq2yygz6CF<11>(s12,s13,s15n,s25n,s35n,zb),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    return QCD::CF*fooCFbox;
}

double qq2yygz6AFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -s12-s13-s14;
    const double s25 = -s12-s23-s24;
    const double zb = -(s15+s25)/s12;
    const double s15n = s15/zb;
    const double s25n = s25/zb;
    const double s35n = (s12+s14+s24)/zb;
    double fooAFbub(0.);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<1>(s12,s13,s15n,s25n,s35n,zb),bubble(s12,3),0);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<2>(s12,s13,s15n,s25n,s35n,zb),bubble(s13,3),0);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<3>(s12,s13,s15n,s25n,s35n,zb),bubble(s14,3),0);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<4>(s12,s13,s15n,s25n,s35n,zb),bubble(-s12-s13-s14,3),0);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<5>(s12,s13,s15n,s25n,s35n,zb),bubble(s23,3),0);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<6>(s12,s13,s15n,s25n,s35n,zb),bubble(s24,3),0);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<7>(s12,s13,s15n,s25n,s35n,zb),bubble(-s12-s23-s24,3),0);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<8>(s12,s13,s15n,s25n,s35n,zb),bubble(-s12-s13-s14-s23-s24,3),0);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<9>(s12,s13,s15n,s25n,s35n,zb),bubble(s12+s14+s24,3),0);
    fooAFbub += productCoeff(qq2yygz6CAm2CF<10>(s12,s13,s15n,s25n,s35n,zb),bubble(s12+s13+s23,3),0);
    return (QCD::CA-2.*QCD::CF)*fooAFbub;
}

double qq2yygz6AFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -s12-s13-s14;
    const double s25 = -s12-s23-s24;
    const double zb = -(s15+s25)/s12;
    const double s15n = s15/zb;
    const double s25n = s25/zb;
    const double s35n = (s12+s14+s24)/zb;
    double fooAFbox(0.);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<11>(s12,s13,s15n,s25n,s35n,zb),box6(s12,s13,s12+s13+s23,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<12>(s12,s13,s15n,s25n,s35n,zb),box6(s12,s14,s12+s14+s24,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<13>(s12,s13,s15n,s25n,s35n,zb),box6(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<14>(s12,s13,s15n,s25n,s35n,zb),box6(s12,s23,s12+s13+s23,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<15>(s12,s13,s15n,s25n,s35n,zb),box6(s12,s24,s12+s14+s24,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<16>(s12,s13,s15n,s25n,s35n,zb),box6(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<17>(s12,s13,s15n,s25n,s35n,zb),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<18>(s12,s13,s15n,s25n,s35n,zb),box6(s13,s12+s14+s24,s24,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<19>(s12,s13,s15n,s25n,s35n,zb),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<20>(s12,s13,s15n,s25n,s35n,zb),box6(s14,s12+s13+s23,s23,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<21>(s12,s13,s15n,s25n,s35n,zb),box6(-s12-s13-s14,s12+s14+s24,s24,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<22>(s12,s13,s15n,s25n,s35n,zb),box6(-s12-s13-s14,s12+s13+s23,s23,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<23>(s12,s13,s15n,s25n,s35n,zb),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<24>(s12,s13,s15n,s25n,s35n,zb),box6(s23,s12+s14+s24,s14,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<25>(s12,s13,s15n,s25n,s35n,zb),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<26>(s12,s13,s15n,s25n,s35n,zb),box6(s24,s12+s13+s23,s13,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<27>(s12,s13,s15n,s25n,s35n,zb),box6(-s12-s23-s24,s12+s14+s24,s14,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<28>(s12,s13,s15n,s25n,s35n,zb),box6(-s12-s23-s24,s12+s13+s23,s13,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<29>(s12,s13,s15n,s25n,s35n,zb),box6(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<30>(s12,s13,s15n,s25n,s35n,zb),box6(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
    fooAFbox += productCoeff(qq2yygz6CAm2CF<31>(s12,s13,s15n,s25n,s35n,zb),box6(s12+s14+s24,s12+s13+s23,s12,3),0);
    return (QCD::CA-2.*QCD::CF)*fooAFbox;
}

double qq2yygz6(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (
            qq2yygz6CAbub(s12,s13,s14,s23,s24)+qq2yygz6CAbox(s12,s13,s14,s23,s24)+
            qq2yygz6CFbub(s12,s13,s14,s23,s24)+qq2yygz6CFbox(s12,s13,s14,s23,s24)+
            qq2yygz6AFbub(s12,s13,s14,s23,s24)+qq2yygz6AFbox(s12,s13,s14,s23,s24)
            );
}

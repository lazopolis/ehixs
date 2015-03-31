/**
 *
 * \file    coeffscol4D.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    March 2015
 *
 */

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

#include "coeffscol4D.h"
#include "dircol4D/coeff4LC1.h"
#include "dircol4D/coeff4LC2.h"
#include "dircol4D/coeff4LC3.h"
#include "dircol4D/coeff4LC4.h"
#include "dircol4D/coeff4LC5.h"
#include "dircol4D/coeff4LC6.h"
#include "dircol4D/coeff4LC7.h"
#include "dircol4D/coeff4LC8.h"
#include "dircol4D/coeff4LC9.h"
#include "dircol4D/coeff4LC10.h"
#include "dircol4D/coeff4LC11.h"
#include "dircol4D/coeff4LC12.h"
#include "dircol4D/coeff4LC13.h"
#include "dircol4D/coeff4LC14.h"
#include "dircol4D/coeff4LC15.h"
#include "dircol4D/coeff4LC16.h"
#include "dircol4D/coeff4LC17.h"
#include "dircol4D/coeff4LC18.h"
#include "dircol4D/coeff4SC1.h"
#include "dircol4D/coeff4SC2.h"
#include "dircol4D/coeff4SC3.h"
#include "dircol4D/coeff4SC4.h"
#include "dircol4D/coeff4SC5.h"
#include "dircol4D/coeff4SC6.h"
#include "dircol4D/coeff4SC7.h"
#include "dircol4D/coeff4SC8.h"
#include "dircol4D/coeff4SC9.h"
#include "dircol4D/coeff4SC10.h"
#include "dircol4D/coeff4SC11.h"
#include "dircol4D/coeff4SC12.h"
#include "dircol4D/coeff4SC13.h"
#include "dircol4D/coeff4SC14.h"
#include "dircol4D/coeff4SC15.h"
#include "dircol4D/coeff4SC16.h"
#include "dircol4D/coeff4SC17.h"
#include "dircol4D/coeff4SC18.h"
#include "dircol4D/coeff4SC19.h"
#include "dircol4D/coeff4SC20.h"
#include "dircol4D/coeff4SC21.h"
#include "dircol4D/coeff4SC22.h"
#include "dircol4D/coeff4SC23.h"
#include "dircol4D/coeff4SC24.h"
#include "dircol4D/coeff4SC25.h"
#include "dircol4D/coeff4SC26.h"
#include "dircol4D/coeff4SC27.h"
#include "dircol4D/coeff4SC28.h"
#include "dircol4D/coeff4SC29.h"
#include "dircol4D/coeff4SC30.h"
#include "dircol4D/coeff4SC31.h"

double qq2yyg4LCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooLCbub(0.);
    fooLCbub += productCoeff(qq2yyg4LC<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooLCbub += productCoeff(qq2yyg4LC<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooLCbub += productCoeff(qq2yyg4LC<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooLCbub += productCoeff(qq2yyg4LC<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooLCbub += productCoeff(qq2yyg4LC<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooLCbub += productCoeff(qq2yyg4LC<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooLCbub += productCoeff(qq2yyg4LC<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return QCD::CA*fooLCbub;
}

double qq2yyg4LCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooLCbox(0.);
    fooLCbox += productCoeff(qq2yyg4LC<8>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14,s24,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<9>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<10>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14,s23,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<11>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<12>(s12,s13,s14,s23,s24),box(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<13>(s12,s13,s14,s23,s24),box(s23,-s12-s23-s24,s14,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<14>(s12,s13,s14,s23,s24),box(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<15>(s12,s13,s14,s23,s24),box(s24,-s12-s23-s24,s13,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<16>(s12,s13,s14,s23,s24),box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<17>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yyg4LC<18>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    return QCD::CA*fooLCbox;
}

double qq2yyg4SCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooSCbub(0.);
    fooSCbub += productCoeff(qq2yyg4SC<1>(s12,s13,s14,s23,s24),bubble(s12,3),0);
    fooSCbub += productCoeff(qq2yyg4SC<2>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooSCbub += productCoeff(qq2yyg4SC<3>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooSCbub += productCoeff(qq2yyg4SC<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooSCbub += productCoeff(qq2yyg4SC<5>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooSCbub += productCoeff(qq2yyg4SC<6>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooSCbub += productCoeff(qq2yyg4SC<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooSCbub += productCoeff(qq2yyg4SC<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    fooSCbub += productCoeff(qq2yyg4SC<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0);
    fooSCbub += productCoeff(qq2yyg4SC<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbub;
}

double qq2yyg4SCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooSCbox(0.);
    fooSCbox += productCoeff(qq2yyg4SC<11>(s12,s13,s14,s23,s24),box(s12,s13,s12+s13+s23,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<12>(s12,s13,s14,s23,s24),box(s12,s14,s12+s14+s24,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<13>(s12,s13,s14,s23,s24),box(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<14>(s12,s13,s14,s23,s24),box(s12,s23,s12+s13+s23,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<15>(s12,s13,s14,s23,s24),box(s12,s24,s12+s14+s24,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<16>(s12,s13,s14,s23,s24),box(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<17>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<18>(s12,s13,s14,s23,s24),box(s13,s12+s14+s24,s24,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<19>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<20>(s12,s13,s14,s23,s24),box(s14,s12+s13+s23,s23,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<21>(s12,s13,s14,s23,s24),box(-s12-s13-s14,s12+s14+s24,s24,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<22>(s12,s13,s14,s23,s24),box(-s12-s13-s14,s12+s13+s23,s23,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<23>(s12,s13,s14,s23,s24),box(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<24>(s12,s13,s14,s23,s24),box(s23,s12+s14+s24,s14,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<25>(s12,s13,s14,s23,s24),box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<26>(s12,s13,s14,s23,s24),box(s24,s12+s13+s23,s13,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<27>(s12,s13,s14,s23,s24),box(-s12-s23-s24,s12+s14+s24,s14,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<28>(s12,s13,s14,s23,s24),box(-s12-s23-s24,s12+s13+s23,s13,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<29>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<30>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
    fooSCbox += productCoeff(qq2yyg4SC<31>(s12,s13,s14,s23,s24),box(s12+s14+s24,s12+s13+s23,s12,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbox;
}

double qq2yyg4col(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (
            qq2yyg4LCbub(s12,s13,s14,s23,s24)+qq2yyg4LCbox(s12,s13,s14,s23,s24)+
            qq2yyg4SCbub(s12,s13,s14,s23,s24)+qq2yyg4SCbox(s12,s13,s14,s23,s24)
            );
}

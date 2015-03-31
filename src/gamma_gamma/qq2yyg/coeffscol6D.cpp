/**
 *
 * \file    coeffscol6D.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    March 2015
 *
 */

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

#include "coeffscol6D.h"
#include "dircol6D/coeff6LC1.h"
#include "dircol6D/coeff6LC2.h"
#include "dircol6D/coeff6LC3.h"
#include "dircol6D/coeff6LC4.h"
#include "dircol6D/coeff6LC5.h"
#include "dircol6D/coeff6LC6.h"
#include "dircol6D/coeff6LC7.h"
#include "dircol6D/coeff6LC8.h"
#include "dircol6D/coeff6LC9.h"
#include "dircol6D/coeff6LC10.h"
#include "dircol6D/coeff6LC11.h"
#include "dircol6D/coeff6LC12.h"
#include "dircol6D/coeff6LC13.h"
#include "dircol6D/coeff6LC14.h"
#include "dircol6D/coeff6LC15.h"
#include "dircol6D/coeff6LC16.h"
#include "dircol6D/coeff6LC17.h"
#include "dircol6D/coeff6LC18.h"
#include "dircol6D/coeff6SC1.h"
#include "dircol6D/coeff6SC2.h"
#include "dircol6D/coeff6SC3.h"
#include "dircol6D/coeff6SC4.h"
#include "dircol6D/coeff6SC5.h"
#include "dircol6D/coeff6SC6.h"
#include "dircol6D/coeff6SC7.h"
#include "dircol6D/coeff6SC8.h"
#include "dircol6D/coeff6SC9.h"
#include "dircol6D/coeff6SC10.h"
#include "dircol6D/coeff6SC11.h"
#include "dircol6D/coeff6SC12.h"
#include "dircol6D/coeff6SC13.h"
#include "dircol6D/coeff6SC14.h"
#include "dircol6D/coeff6SC15.h"
#include "dircol6D/coeff6SC16.h"
#include "dircol6D/coeff6SC17.h"
#include "dircol6D/coeff6SC18.h"
#include "dircol6D/coeff6SC19.h"
#include "dircol6D/coeff6SC20.h"
#include "dircol6D/coeff6SC21.h"
#include "dircol6D/coeff6SC22.h"
#include "dircol6D/coeff6SC23.h"
#include "dircol6D/coeff6SC24.h"
#include "dircol6D/coeff6SC25.h"
#include "dircol6D/coeff6SC26.h"
#include "dircol6D/coeff6SC27.h"
#include "dircol6D/coeff6SC28.h"
#include "dircol6D/coeff6SC29.h"
#include "dircol6D/coeff6SC30.h"
#include "dircol6D/coeff6SC31.h"

double qq2yyg6LCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooLCbub(0.);
    fooLCbub += productCoeff(qq2yyg6LC<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooLCbub += productCoeff(qq2yyg6LC<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooLCbub += productCoeff(qq2yyg6LC<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooLCbub += productCoeff(qq2yyg6LC<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooLCbub += productCoeff(qq2yyg6LC<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooLCbub += productCoeff(qq2yyg6LC<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooLCbub += productCoeff(qq2yyg6LC<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return QCD::CA*fooLCbub;
}

double qq2yyg6LCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooLCbox(0.);
    fooLCbox += productCoeff(qq2yyg6LC<8>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14,s24,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<9>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<10>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14,s23,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<11>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<12>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<13>(s12,s13,s14,s23,s24),box6(s23,-s12-s23-s24,s14,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<14>(s12,s13,s14,s23,s24),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<15>(s12,s13,s14,s23,s24),box6(s24,-s12-s23-s24,s13,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<16>(s12,s13,s14,s23,s24),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<17>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    fooLCbox += productCoeff(qq2yyg6LC<18>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    return QCD::CA*fooLCbox;
}

double qq2yyg6SCbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooSCbub(0.);
    fooSCbub += productCoeff(qq2yyg6SC<1>(s12,s13,s14,s23,s24),bubble(s12,3),0);
    fooSCbub += productCoeff(qq2yyg6SC<2>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooSCbub += productCoeff(qq2yyg6SC<3>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooSCbub += productCoeff(qq2yyg6SC<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooSCbub += productCoeff(qq2yyg6SC<5>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooSCbub += productCoeff(qq2yyg6SC<6>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooSCbub += productCoeff(qq2yyg6SC<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooSCbub += productCoeff(qq2yyg6SC<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    fooSCbub += productCoeff(qq2yyg6SC<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0);
    fooSCbub += productCoeff(qq2yyg6SC<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbub;
}

double qq2yyg6SCbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooSCbox(0.);
    fooSCbox += productCoeff(qq2yyg6SC<11>(s12,s13,s14,s23,s24),box6(s12,s13,s12+s13+s23,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<12>(s12,s13,s14,s23,s24),box6(s12,s14,s12+s14+s24,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<13>(s12,s13,s14,s23,s24),box6(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<14>(s12,s13,s14,s23,s24),box6(s12,s23,s12+s13+s23,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<15>(s12,s13,s14,s23,s24),box6(s12,s24,s12+s14+s24,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<16>(s12,s13,s14,s23,s24),box6(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<17>(s12,s13,s14,s23,s24),box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<18>(s12,s13,s14,s23,s24),box6(s13,s12+s14+s24,s24,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<19>(s12,s13,s14,s23,s24),box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<20>(s12,s13,s14,s23,s24),box6(s14,s12+s13+s23,s23,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<21>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s14+s24,s24,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<22>(s12,s13,s14,s23,s24),box6(-s12-s13-s14,s12+s13+s23,s23,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<23>(s12,s13,s14,s23,s24),box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<24>(s12,s13,s14,s23,s24),box6(s23,s12+s14+s24,s14,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<25>(s12,s13,s14,s23,s24),box6(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<26>(s12,s13,s14,s23,s24),box6(s24,s12+s13+s23,s13,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<27>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s14+s24,s14,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<28>(s12,s13,s14,s23,s24),box6(-s12-s23-s24,s12+s13+s23,s13,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<29>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<30>(s12,s13,s14,s23,s24),box6(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
    fooSCbox += productCoeff(qq2yyg6SC<31>(s12,s13,s14,s23,s24),box6(s12+s14+s24,s12+s13+s23,s12,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbox;
}

double qq2yyg6col(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (
            qq2yyg6LCbub(s12,s13,s14,s23,s24)+qq2yyg6LCbox(s12,s13,s14,s23,s24)+
            qq2yyg6SCbub(s12,s13,s14,s23,s24)+qq2yyg6SCbox(s12,s13,s14,s23,s24)
            );
}

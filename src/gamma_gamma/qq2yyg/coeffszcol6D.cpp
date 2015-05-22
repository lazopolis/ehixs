/**
 *
 * \file    coeffszcol6D.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

#include "coeffszcol6D.h"
#include "dirzcol6D/coeffz6LC1.h"
#include "dirzcol6D/coeffz6LC2.h"
#include "dirzcol6D/coeffz6LC3.h"
#include "dirzcol6D/coeffz6LC4.h"
#include "dirzcol6D/coeffz6LC5.h"
#include "dirzcol6D/coeffz6LC6.h"
#include "dirzcol6D/coeffz6LC7.h"
#include "dirzcol6D/coeffz6LC8.h"
#include "dirzcol6D/coeffz6LC9.h"
#include "dirzcol6D/coeffz6LC10.h"
#include "dirzcol6D/coeffz6LC11.h"
#include "dirzcol6D/coeffz6LC12.h"
#include "dirzcol6D/coeffz6LC13.h"
#include "dirzcol6D/coeffz6LC14.h"
#include "dirzcol6D/coeffz6LC15.h"
#include "dirzcol6D/coeffz6LC16.h"
#include "dirzcol6D/coeffz6SC1.h"
#include "dirzcol6D/coeffz6SC2.h"
#include "dirzcol6D/coeffz6SC3.h"
#include "dirzcol6D/coeffz6SC4.h"
#include "dirzcol6D/coeffz6SC5.h"
#include "dirzcol6D/coeffz6SC6.h"
#include "dirzcol6D/coeffz6SC7.h"
#include "dirzcol6D/coeffz6SC8.h"
#include "dirzcol6D/coeffz6SC9.h"
#include "dirzcol6D/coeffz6SC10.h"
#include "dirzcol6D/coeffz6SC11.h"
#include "dirzcol6D/coeffz6SC12.h"
#include "dirzcol6D/coeffz6SC13.h"
#include "dirzcol6D/coeffz6SC14.h"
#include "dirzcol6D/coeffz6SC15.h"
#include "dirzcol6D/coeffz6SC16.h"
#include "dirzcol6D/coeffz6SC17.h"
#include "dirzcol6D/coeffz6SC18.h"
#include "dirzcol6D/coeffz6SC19.h"
#include "dirzcol6D/coeffz6SC20.h"
#include "dirzcol6D/coeffz6SC21.h"
#include "dirzcol6D/coeffz6SC22.h"
#include "dirzcol6D/coeffz6SC23.h"
#include "dirzcol6D/coeffz6SC24.h"
#include "dirzcol6D/coeffz6SC25.h"
#include "dirzcol6D/coeffz6SC26.h"
#include "dirzcol6D/coeffz6SC27.h"
#include "dirzcol6D/coeffz6SC28.h"
#include "dirzcol6D/coeffz6SC29.h"
#include "dirzcol6D/coeffz6SC30.h"
#include "dirzcol6D/coeffz6SC31.h"

double qq2yygz6LCbub(const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -1-s13-s14;
    const double s25 = -1-s23-s24;
    const double s34 = -1-s13-s14-s23-s24;
    const double zb = -s15-s25;
    const double lam = -s15/zb;
    const double s35n = (1+s14+s24)/zb;
    double fooLCbub(0.);
    fooLCbub += productCoeff(qq2yygz6LC<1>(s13,s35n,lam,zb),bubble(s13,3),0);
    fooLCbub += productCoeff(qq2yygz6LC<2>(s13,s35n,lam,zb),bubble(s14,3),0);
    fooLCbub += productCoeff(qq2yygz6LC<3>(s13,s35n,lam,zb),bubble(s15,3),0);
    fooLCbub += productCoeff(qq2yygz6LC<4>(s13,s35n,lam,zb),bubble(s23,3),0);
    fooLCbub += productCoeff(qq2yygz6LC<5>(s13,s35n,lam,zb),bubble(s24,3),0);
    fooLCbub += productCoeff(qq2yygz6LC<6>(s13,s35n,lam,zb),bubble(s25,3),0);
    fooLCbub += productCoeff(qq2yygz6LC<7>(s13,s35n,lam,zb),bubble(s34,3),0);
    return QCD::CA*fooLCbub;
}

double qq2yygz6LCbox(const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -1-s13-s14;
    const double s25 = -1-s23-s24;
    const double s34 = -1-s13-s14-s23-s24;
    const double zb = -s15-s25;
    const double lam = -s15/zb;
    const double s35n = (1+s14+s24)/zb;
    double fooLCbox(0.);
    fooLCbox += productCoeff(qq2yygz6LC<8>(s13,s35n,lam,zb),box6(s13,s15,s24,3),0);
    fooLCbox += productCoeff(qq2yygz6LC<9>(s13,s35n,lam,zb),box6(s13,s34,s25,3),0);
    fooLCbox += productCoeff(qq2yygz6LC<10>(s13,s35n,lam,zb),box6(s14,s15,s23,3),0);
    fooLCbox += productCoeff(qq2yygz6LC<11>(s13,s35n,lam,zb),box6(s14,s34,s25,3),0);
    fooLCbox += productCoeff(qq2yygz6LC<12>(s13,s35n,lam,zb),box6(s15,s25,s34,3),0);
    fooLCbox += productCoeff(qq2yygz6LC<13>(s13,s35n,lam,zb),box6(s23,s25,s14,3),0);
    fooLCbox += productCoeff(qq2yygz6LC<14>(s13,s35n,lam,zb),box6(s23,s34,s15,3),0);
    fooLCbox += productCoeff(qq2yygz6LC<15>(s13,s35n,lam,zb),box6(s24,s25,s13,3),0);
    fooLCbox += productCoeff(qq2yygz6LC<16>(s13,s35n,lam,zb),box6(s24,s34,s15,3),0);
    return QCD::CA*fooLCbox;
}

double qq2yygz6SCbub(const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -1-s13-s14;
    const double s25 = -1-s23-s24;
    const double s34 = -1-s13-s14-s23-s24;
    const double s35 = 1+s14+s24;
    const double s45 = 1+s13+s23;
    const double zb = -s15-s25;
    const double lam = -s15/zb;
    const double s35n = s35/zb;
    double fooSCbub(0.);
    fooSCbub += productCoeff(qq2yygz6SC<1>(s13,s35n,lam,zb),bubble(1,3),0);
    fooSCbub += productCoeff(qq2yygz6SC<2>(s13,s35n,lam,zb),bubble(s13,3),0);
    fooSCbub += productCoeff(qq2yygz6SC<3>(s13,s35n,lam,zb),bubble(s14,3),0);
    fooSCbub += productCoeff(qq2yygz6SC<4>(s13,s35n,lam,zb),bubble(s15,3),0);
    fooSCbub += productCoeff(qq2yygz6SC<5>(s13,s35n,lam,zb),bubble(s23,3),0);
    fooSCbub += productCoeff(qq2yygz6SC<6>(s13,s35n,lam,zb),bubble(s24,3),0);
    fooSCbub += productCoeff(qq2yygz6SC<7>(s13,s35n,lam,zb),bubble(s25,3),0);
    fooSCbub += productCoeff(qq2yygz6SC<8>(s13,s35n,lam,zb),bubble(s34,3),0);
    fooSCbub += productCoeff(qq2yygz6SC<9>(s13,s35n,lam,zb),bubble(s35,3),0);
    fooSCbub += productCoeff(qq2yygz6SC<10>(s13,s35n,lam,zb),bubble(s45,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbub;
}

double qq2yygz6SCbox(const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -1-s13-s14;
    const double s25 = -1-s23-s24;
    const double s34 = -1-s13-s14-s23-s24;
    const double s35 = 1+s14+s24;
    const double s45 = 1+s13+s23;
    const double zb = -s15-s25;
    const double lam = -s15/zb;
    const double s35n = s35/zb;
    double fooSCbox(0.);
    fooSCbox += productCoeff(qq2yygz6SC<11>(s13,s35n,lam,zb),box6(1,s13,s45,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<12>(s13,s35n,lam,zb),box6(1,s14,s35,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<13>(s13,s35n,lam,zb),box6(1,s15,s34,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<14>(s13,s35n,lam,zb),box6(1,s23,s45,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<15>(s13,s35n,lam,zb),box6(1,s24,s35,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<16>(s13,s35n,lam,zb),box6(1,s25,s34,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<17>(s13,s35n,lam,zb),box6(s13,s34,s25,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<18>(s13,s35n,lam,zb),box6(s13,s35,s24,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<19>(s13,s35n,lam,zb),box6(s14,s34,s25,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<20>(s13,s35n,lam,zb),box6(s14,s45,s23,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<21>(s13,s35n,lam,zb),box6(s15,s35,s24,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<22>(s13,s35n,lam,zb),box6(s15,s45,s23,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<23>(s13,s35n,lam,zb),box6(s23,s34,s15,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<24>(s13,s35n,lam,zb),box6(s23,s35,s14,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<25>(s13,s35n,lam,zb),box6(s24,s34,s15,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<26>(s13,s35n,lam,zb),box6(s24,s45,s13,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<27>(s13,s35n,lam,zb),box6(s25,s35,s14,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<28>(s13,s35n,lam,zb),box6(s25,s45,s13,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<29>(s13,s35n,lam,zb),box6(s34,s35,1,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<30>(s13,s35n,lam,zb),box6(s34,s45,1,3),0);
    fooSCbox += productCoeff(qq2yygz6SC<31>(s13,s35n,lam,zb),box6(s35,s45,1,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbox;
}

double qq2yygz6col(const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (
            qq2yygz6LCbub(s13,s14,s23,s24)+qq2yygz6LCbox(s13,s14,s23,s24)+
            qq2yygz6SCbub(s13,s14,s23,s24)+qq2yygz6SCbox(s13,s14,s23,s24)
            );
}

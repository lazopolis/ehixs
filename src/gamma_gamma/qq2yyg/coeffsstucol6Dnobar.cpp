/**
 *
 * \file    coeffsstucol6Dnobar.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

#include "coeffsstucol6Dnobar.h"

double qq2yygstu6LCnobarbub(const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -1-s13-s14;
    const double s25 = -1-s23-s24;
    const double s34 = -1-s13-s14-s23-s24;
    const double s35 = 1+s14+s24;
    const double s45 = 1+s13+s23;
    const double zb = -s15-s25;
    const double t12 = (s15-s25)/zb;
    const double t34 = (s35-s45)/zb;
    const double u = s13-s14-s23+s24;
    const double z=1.-zb;
    double fooLCbub(0.);
    fooLCbub += productCoeff(qq2yygstu6LCnobar<1>(z,t12,t34,u),bubble(s13,3),0);
    fooLCbub += productCoeff(qq2yygstu6LCnobar<2>(z,t12,t34,u),bubble(s14,3),0);
    fooLCbub += productCoeff(qq2yygstu6LCnobar<3>(z,t12,t34,u),bubble(s15,3),0);
    fooLCbub += productCoeff(qq2yygstu6LCnobar<4>(z,t12,t34,u),bubble(s23,3),0);
    fooLCbub += productCoeff(qq2yygstu6LCnobar<5>(z,t12,t34,u),bubble(s24,3),0);
    fooLCbub += productCoeff(qq2yygstu6LCnobar<6>(z,t12,t34,u),bubble(s25,3),0);
    fooLCbub += productCoeff(qq2yygstu6LCnobar<7>(z,t12,t34,u),bubble(s34,3),0);
    return QCD::CA*fooLCbub;
}

double qq2yygstu6LCnobarbox(const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -1-s13-s14;
    const double s25 = -1-s23-s24;
    const double s34 = -1-s13-s14-s23-s24;
    const double s35 = 1+s14+s24;
    const double s45 = 1+s13+s23;
    const double zb = -s15-s25;
    const double t12 = (s15-s25)/zb;
    const double t34 = (s35-s45)/zb;
    const double u = s13-s14-s23+s24;
    const double z=1.-zb;
    double fooLCbox(0.);
    fooLCbox += productCoeff(qq2yygstu6LCnobar<8>(z,t12,t34,u),box6(s13,s15,s24,3),0);
    fooLCbox += productCoeff(qq2yygstu6LCnobar<9>(z,t12,t34,u),box6(s13,s34,s25,3),0);
    fooLCbox += productCoeff(qq2yygstu6LCnobar<10>(z,t12,t34,u),box6(s14,s15,s23,3),0);
    fooLCbox += productCoeff(qq2yygstu6LCnobar<11>(z,t12,t34,u),box6(s14,s34,s25,3),0);
    fooLCbox += productCoeff(qq2yygstu6LCnobar<12>(z,t12,t34,u),box6(s15,s25,s34,3),0);
    fooLCbox += productCoeff(qq2yygstu6LCnobar<13>(z,t12,t34,u),box6(s23,s25,s14,3),0);
    fooLCbox += productCoeff(qq2yygstu6LCnobar<14>(z,t12,t34,u),box6(s23,s34,s15,3),0);
    fooLCbox += productCoeff(qq2yygstu6LCnobar<15>(z,t12,t34,u),box6(s24,s25,s13,3),0);
    fooLCbox += productCoeff(qq2yygstu6LCnobar<16>(z,t12,t34,u),box6(s24,s34,s15,3),0);
    return QCD::CA*fooLCbox;
}

double qq2yygstu6SCnobarbub(const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -1-s13-s14;
    const double s25 = -1-s23-s24;
    const double s34 = -1-s13-s14-s23-s24;
    const double s35 = 1+s14+s24;
    const double s45 = 1+s13+s23;
    const double zb = -s15-s25;
    const double t12 = (s15-s25)/zb;
    const double t34 = (s35-s45)/zb;
    const double u = s13-s14-s23+s24;
    const double z=1.-zb;
    double fooSCbub(0.);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<1>(z,t12,t34,u),bubble(1,3),0);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<2>(z,t12,t34,u),bubble(s13,3),0);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<3>(z,t12,t34,u),bubble(s14,3),0);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<4>(z,t12,t34,u),bubble(s15,3),0);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<5>(z,t12,t34,u),bubble(s23,3),0);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<6>(z,t12,t34,u),bubble(s24,3),0);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<7>(z,t12,t34,u),bubble(s25,3),0);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<8>(z,t12,t34,u),bubble(s34,3),0);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<9>(z,t12,t34,u),bubble(s35,3),0);
    fooSCbub += productCoeff(qq2yygstu6SCnobar<10>(z,t12,t34,u),bubble(s45,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbub;
}

double qq2yygstu6SCnobarbox(const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double s15 = -1-s13-s14;
    const double s25 = -1-s23-s24;
    const double s34 = -1-s13-s14-s23-s24;
    const double s35 = 1+s14+s24;
    const double s45 = 1+s13+s23;
    const double zb = -s15-s25;
    const double t12 = (s15-s25)/zb;
    const double t34 = (s35-s45)/zb;
    const double u = s13-s14-s23+s24;
    const double z=1.-zb;
    double fooSCbox(0.);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<11>(z,t12,t34,u),box6(1,s13,s45,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<12>(z,t12,t34,u),box6(1,s14,s35,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<13>(z,t12,t34,u),box6(1,s15,s34,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<14>(z,t12,t34,u),box6(1,s23,s45,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<15>(z,t12,t34,u),box6(1,s24,s35,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<16>(z,t12,t34,u),box6(1,s25,s34,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<17>(z,t12,t34,u),box6(s13,s34,s25,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<18>(z,t12,t34,u),box6(s13,s35,s24,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<19>(z,t12,t34,u),box6(s14,s34,s25,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<20>(z,t12,t34,u),box6(s14,s45,s23,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<21>(z,t12,t34,u),box6(s15,s35,s24,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<22>(z,t12,t34,u),box6(s15,s45,s23,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<23>(z,t12,t34,u),box6(s23,s34,s15,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<24>(z,t12,t34,u),box6(s23,s35,s14,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<25>(z,t12,t34,u),box6(s24,s34,s15,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<26>(z,t12,t34,u),box6(s24,s45,s13,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<27>(z,t12,t34,u),box6(s25,s35,s14,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<28>(z,t12,t34,u),box6(s25,s45,s13,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<29>(z,t12,t34,u),box6(s34,s35,1,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<30>(z,t12,t34,u),box6(s34,s45,1,3),0);
    fooSCbox += productCoeff(qq2yygstu6SCnobar<31>(z,t12,t34,u),box6(s35,s45,1,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbox;
}

double qq2yygstu6colnobar(const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (
            qq2yygstu6LCnobarbub(s13,s14,s23,s24)+qq2yygstu6LCnobarbox(s13,s14,s23,s24)+
            qq2yygstu6SCnobarbub(s13,s14,s23,s24)+qq2yygstu6SCnobarbox(s13,s14,s23,s24)
            );
}

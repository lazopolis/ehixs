/**
 *
 * \file    coeffsstucol6D.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#ifndef my_float
#error  coeffsstucol6D.cpp included without defining my_float!
#else

#include "constants.h" // QCD::CF, etc...

#include "coeffsstucol6D.h"

/**
 * \par   patchDelta
 * \brief Relative distance of two Mandelstams for the Taylor-expansion patch to kick in
 * \todo  Find a way to move this to a header file
 */

const double patchDelta = 0.001;

double qq2yygstu6LCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
{
    const my_float s12 = static_cast<my_float>(1);
    const my_float s15 = -s12-s13-s14;
    const my_float s25 = -s12-s23-s24;
    const my_float s34 = -s12-s13-s14-s23-s24;
    const my_float s35 = s12+s14+s24;
    const my_float s45 = s12+s13+s23;
    const my_float zb = -s15-s25;
    const my_float t12 = (s15-s25)/zb;
    const my_float t34 = (s35-s45)/zb;
    const my_float u = s13-s14-s23+s24;
    bool bubSwitch[6] = {true,true,true,true,true,true};
    size_t nPatch = 0;
    double fooLCbub(0.);
    if (todouble<my_float>(fabs<my_float>(s13-s25)/(fabs<my_float>(s13)+fabs<my_float>(s25)))<patchDelta) {
        bubSwitch[1-1]=false;
        bubSwitch[6-1]=false;
        ++nPatch;
        fooLCbub += qq2yygstu6LCbub1325(zb,t12,t34);
    }
    if (todouble<my_float>(fabs<my_float>(s14-s25)/(fabs<my_float>(s14)+fabs<my_float>(s25)))<patchDelta) {
        bubSwitch[2-1]=false;
        bubSwitch[6-1]=false;
        ++nPatch;
        fooLCbub += qq2yygstu6LCbub1325(zb,t12,-t34);
    }
    if (todouble<my_float>(fabs<my_float>(s23-s15)/(fabs<my_float>(s23)+fabs<my_float>(s15)))<patchDelta) {
        bubSwitch[3-1]=false;
        bubSwitch[5-1]=false;
        ++nPatch;
        fooLCbub += qq2yygstu6LCbub1325(zb,-t12,t34);
    }
    if (todouble<my_float>(fabs<my_float>(s24-s15)/(fabs<my_float>(s24)+fabs<my_float>(s15)))<patchDelta) {
        bubSwitch[4-1]=false;
        bubSwitch[5-1]=false;
        ++nPatch;
        fooLCbub += qq2yygstu6LCbub1325(zb,-t12,-t34);
    }
    if (todouble<my_float>(fabs<my_float>(s13-s24)/(fabs<my_float>(s13)+fabs<my_float>(s24)))<patchDelta*todouble(zb)) {
        bubSwitch[1-1]=false;
        bubSwitch[4-1]=false;
        ++nPatch;
        fooLCbub += qq2yygstu6LCbub1324(zb,t12,u);
    }
    if (todouble<my_float>(fabs<my_float>(s14-s23)/(fabs<my_float>(s14)+fabs<my_float>(s23)))<patchDelta*todouble(zb)) {
        bubSwitch[2-1]=false;
        bubSwitch[3-1]=false;
        ++nPatch;
        fooLCbub += qq2yygstu6LCbub1324(zb,-t12,-u);
    }
    if (nPatch>1) {
        std::cerr << "Error in qq2yyg: too many large cancellations!" << std::endl;
        return 0.;
    }
    fooLCbub += bubSwitch[1-1]*productCoeff(qq2yygstu6LC<1>(zb,t12,t34,u),  bubble(s13,3),0);
    fooLCbub += bubSwitch[2-1]*productCoeff(qq2yygstu6LC<1>(zb,t12,-t34,-u),bubble(s14,3),0);
    fooLCbub += bubSwitch[3-1]*productCoeff(qq2yygstu6LC<1>(zb,-t12,t34,-u),bubble(s23,3),0);
    fooLCbub += bubSwitch[4-1]*productCoeff(qq2yygstu6LC<1>(zb,-t12,-t34,u),bubble(s24,3),0);
    fooLCbub += bubSwitch[5-1]*productCoeff(qq2yygstu6LC<2>(zb,t12,t34,u),  bubble(s15,3),0);
    fooLCbub += bubSwitch[6-1]*productCoeff(qq2yygstu6LC<2>(zb,-t12,t34,-u),bubble(s25,3),0);
    fooLCbub +=                productCoeff(qq2yygstu6LC<3>(zb,t12,t34,u),  bubble(s34,3),0);
    return QCD::CA*fooLCbub;
}

double qq2yygstu6LCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
{
    const my_float s12 = static_cast<my_float>(1);
    const my_float s15 = -s12-s13-s14;
    const my_float s25 = -s12-s23-s24;
    const my_float s34 = -s12-s13-s14-s23-s24;
    const my_float s35 = s12+s14+s24;
    const my_float s45 = s12+s13+s23;
    const my_float zb = -s15-s25;
    const my_float t12 = (s15-s25)/zb;
    const my_float t34 = (s35-s45)/zb;
    const my_float u = s13-s14-s23+s24;
    double fooLCbox(0.);
    fooLCbox += productCoeff(qq2yygstu6LC<4>(zb,t12,t34,u),  box6(s13,s15,s24,3),0);
    fooLCbox += productCoeff(qq2yygstu6LC<4>(zb,t12,-t34,-u),box6(s14,s15,s23,3),0);
    fooLCbox += productCoeff(qq2yygstu6LC<4>(zb,-t12,t34,-u),box6(s23,s25,s14,3),0);
    fooLCbox += productCoeff(qq2yygstu6LC<4>(zb,-t12,-t34,u),box6(s24,s25,s13,3),0);
    fooLCbox += productCoeff(qq2yygstu6LC<5>(zb,t12,t34,u),  box6(s13,s34,s25,3),0);
    fooLCbox += productCoeff(qq2yygstu6LC<5>(zb,t12,-t34,-u),box6(s14,s34,s25,3),0);
    fooLCbox += productCoeff(qq2yygstu6LC<5>(zb,-t12,t34,-u),box6(s23,s34,s15,3),0);
    fooLCbox += productCoeff(qq2yygstu6LC<5>(zb,-t12,-t34,u),box6(s24,s34,s15,3),0);
    fooLCbox += productCoeff(qq2yygstu6LC<6>(zb,t12,t34,u),  box6(s15,s25,s34,3),0);
    return QCD::CA*fooLCbox;
}

double qq2yygstu6SCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
{
    const my_float s12 = static_cast<my_float>(1);
    const my_float s15 = -s12-s13-s14;
    const my_float s25 = -s12-s23-s24;
    const my_float s34 = -s12-s13-s14-s23-s24;
    const my_float s35 = s12+s14+s24;
    const my_float s45 = s12+s13+s23;
    const my_float zb = -s15-s25;
    const my_float t12 = (s15-s25)/zb;
    const my_float t34 = (s35-s45)/zb;
    const my_float u = s13-s14-s23+s24;
    bool bubSwitch[6] = {true,true,true,true,true,true};
    size_t nPatch = 0;
    double fooSCbub(0.);
    if (todouble<my_float>(fabs<my_float>(s13-s25)/(fabs<my_float>(s13)+fabs<my_float>(s25)))<patchDelta) {
        bubSwitch[2-2]=false;
        bubSwitch[7-2]=false;
        ++nPatch;
        fooSCbub += qq2yygstu6SCbub1325(zb,t12,t34);
    }
    if (todouble<my_float>(fabs<my_float>(s14-s25)/(fabs<my_float>(s14)+fabs<my_float>(s25)))<patchDelta) {
        bubSwitch[3-2]=false;
        bubSwitch[7-2]=false;
        ++nPatch;
        fooSCbub += qq2yygstu6SCbub1325(zb,t12,-t34);
    }
    if (todouble<my_float>(fabs<my_float>(s23-s15)/(fabs<my_float>(s23)+fabs<my_float>(s15)))<patchDelta) {
        bubSwitch[4-2]=false;
        bubSwitch[6-2]=false;
        ++nPatch;
        fooSCbub += qq2yygstu6SCbub1325(zb,-t12,t34);
    }
    if (todouble<my_float>(fabs<my_float>(s24-s15)/(fabs<my_float>(s24)+fabs<my_float>(s15)))<patchDelta) {
        bubSwitch[5-2]=false;
        bubSwitch[6-2]=false;
        ++nPatch;
        fooSCbub += qq2yygstu6SCbub1325(zb,-t12,-t34);
    }
    if (todouble<my_float>(fabs<my_float>(s13-s24)/(fabs<my_float>(s13)+fabs<my_float>(s24)))<patchDelta*todouble(zb)) {
        bubSwitch[2-2]=false;
        bubSwitch[5-2]=false;
        ++nPatch;
        fooSCbub += qq2yygstu6SCbub1324(zb,t12,u);
    }
    if (todouble<my_float>(fabs<my_float>(s14-s23)/(fabs<my_float>(s14)+fabs<my_float>(s23)))<patchDelta*todouble(zb)) {
        bubSwitch[3-2]=false;
        bubSwitch[4-2]=false;
        ++nPatch;
        fooSCbub += qq2yygstu6SCbub1324(zb,-t12,-u);
    }
    if (nPatch>1) {
        std::cerr << "Error in qq2yyg: too many large cancellations!" << std::endl;
        return 0.;
    }
    fooSCbub +=                productCoeff(qq2yygstu6SC<1>(zb,t12,t34,u),  bubble(s12,3),0);
    fooSCbub += bubSwitch[2-2]*productCoeff(qq2yygstu6SC<2>(zb,t12,t34,u),  bubble(s13,3),0);
    fooSCbub += bubSwitch[3-2]*productCoeff(qq2yygstu6SC<2>(zb,t12,-t34,-u),bubble(s14,3),0);
    fooSCbub += bubSwitch[4-2]*productCoeff(qq2yygstu6SC<2>(zb,-t12,t34,-u),bubble(s23,3),0);
    fooSCbub += bubSwitch[5-2]*productCoeff(qq2yygstu6SC<2>(zb,-t12,-t34,u),bubble(s24,3),0);
    fooSCbub += bubSwitch[6-2]*productCoeff(qq2yygstu6SC<3>(zb,t12,t34,u),  bubble(s15,3),0);
    fooSCbub += bubSwitch[7-2]*productCoeff(qq2yygstu6SC<3>(zb,-t12,t34,-u),bubble(s25,3),0);
    fooSCbub +=                productCoeff(qq2yygstu6SC<4>(zb,t12,t34,u),  bubble(s34,3),0);
    fooSCbub +=                productCoeff(qq2yygstu6SC<5>(zb,t12,t34,u),  bubble(s35,3),0);
    fooSCbub +=                productCoeff(qq2yygstu6SC<5>(zb,t12,-t34,-u),bubble(s45,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbub;
}

double qq2yygstu6SCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
{
    const my_float s12 = static_cast<my_float>(1);
    const my_float s15 = -s12-s13-s14;
    const my_float s25 = -s12-s23-s24;
    const my_float s34 = -s12-s13-s14-s23-s24;
    const my_float s35 = s12+s14+s24;
    const my_float s45 = s12+s13+s23;
    const my_float zb = -s15-s25;
    const my_float t12 = (s15-s25)/zb;
    const my_float t34 = (s35-s45)/zb;
    const my_float u = s13-s14-s23+s24;
    double fooSCbox(0.);
    fooSCbox += productCoeff(qq2yygstu6SC<6> (zb,t12,t34,u),  box6(s12,s13,s45,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<6> (zb,t12,-t34,-u),box6(s12,s14,s35,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<6> (zb,-t12,t34,-u),box6(s12,s23,s45,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<6> (zb,-t12,-t34,u),box6(s12,s24,s35,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<7> (zb,t12,t34,u),  box6(s12,s15,s34,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<7> (zb,-t12,t34,-u),box6(s12,s25,s34,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<8> (zb,t12,t34,u),  box6(s13,s34,s25,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<8> (zb,t12,-t34,-u),box6(s14,s34,s25,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<8> (zb,-t12,t34,-u),box6(s23,s34,s15,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<8> (zb,-t12,-t34,u),box6(s24,s34,s15,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<9> (zb,t12,t34,u),  box6(s13,s35,s24,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<9> (zb,t12,-t34,-u),box6(s14,s45,s23,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<9> (zb,-t12,t34,-u),box6(s23,s35,s14,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<9> (zb,-t12,-t34,u),box6(s24,s45,s13,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<10>(zb,t12,t34,u),  box6(s15,s35,s24,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<10>(zb,t12,-t34,-u),box6(s15,s45,s23,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<10>(zb,-t12,t34,-u),box6(s25,s35,s14,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<10>(zb,-t12,-t34,u),box6(s25,s45,s13,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<11>(zb,t12,t34,u),  box6(s34,s35,s12,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<11>(zb,t12,-t34,-u),box6(s34,s45,s12,3),0);
    fooSCbox += productCoeff(qq2yygstu6SC<12>(zb,t12,t34,u),  box6(s35,s45,s12,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbox;
}

double qq2yygstu6col(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
{
    return (
            qq2yygstu6LCbub(s13,s14,s23,s24)+qq2yygstu6LCbox(s13,s14,s23,s24)+
            qq2yygstu6SCbub(s13,s14,s23,s24)+qq2yygstu6SCbox(s13,s14,s23,s24)
            );
}

#endif

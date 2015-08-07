/**
 *
 * \file    qq2yygz0.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#ifndef my_float
#error  qq2yygz0.cpp included without defining my_float!
#else

#include "constants.h" // QCD::CF, etc...

#include "qq2yygz0.h"

/**
 * \par   patchDelta
 * \brief Relative distance of two Mandelstams for the Taylor-expansion patch to kick in
 * \todo  Find a way to move this to a header file
 */

const double patchDelta = 0.001;

double qq2yygz0LCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
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
    const my_float z = 1-zb;
    bool bubSwitch[6] = {true,true,true,true,true,true};
    size_t nPatch = 0;
    double fooLCbub(0.);
    /// \todo Maybe I can get rid of some explicit template specifications?
    if (todouble<my_float>(fabs<my_float>(s13-s25)/(fabs<my_float>(s13)+fabs<my_float>(s25)))<patchDelta) {
        bubSwitch[1-1]=false;
        bubSwitch[6-1]=false;
        ++nPatch;
        fooLCbub += qq2yygz0LCbub1325(z,t12,t34);
    }
    if (todouble<my_float>(fabs<my_float>(s14-s25)/(fabs<my_float>(s14)+fabs<my_float>(s25)))<patchDelta) {
        bubSwitch[2-1]=false;
        bubSwitch[6-1]=false;
        ++nPatch;
        fooLCbub += qq2yygz0LCbub1325(z,t12,-t34);
    }
    if (todouble<my_float>(fabs<my_float>(s23-s15)/(fabs<my_float>(s23)+fabs<my_float>(s15)))<patchDelta) {
        bubSwitch[3-1]=false;
        bubSwitch[5-1]=false;
        ++nPatch;
        fooLCbub += qq2yygz0LCbub1325(z,-t12,t34);
    }
    if (todouble<my_float>(fabs<my_float>(s24-s15)/(fabs<my_float>(s24)+fabs<my_float>(s15)))<patchDelta) {
        bubSwitch[4-1]=false;
        bubSwitch[5-1]=false;
        ++nPatch;
        fooLCbub += qq2yygz0LCbub1325(z,-t12,-t34);
    }
    if (todouble<my_float>(fabs<my_float>(s13-s24)/(fabs<my_float>(s13)+fabs<my_float>(s24)))<patchDelta*todouble(zb)) {
        bubSwitch[1-1]=false;
        bubSwitch[4-1]=false;
        ++nPatch;
        fooLCbub += qq2yygz0LCbub1324(z,t12,u);
    }
    if (todouble<my_float>(fabs<my_float>(s14-s23)/(fabs<my_float>(s14)+fabs<my_float>(s23)))<patchDelta*todouble(zb)) {
        bubSwitch[2-1]=false;
        bubSwitch[3-1]=false;
        ++nPatch;
        fooLCbub += qq2yygz0LCbub1324(z,-t12,-u);
    }
    if (nPatch>1) {
        std::cerr << "Error in qq2yyg: too many large cancellations!" << std::endl;
        return 0.;
    }
    fooLCbub += bubSwitch[1-1]*productCoeff(qq2yygz0LC<1>(z,t12,t34,u),  bubble(s13,3),0);
    fooLCbub += bubSwitch[2-1]*productCoeff(qq2yygz0LC<1>(z,t12,-t34,-u),bubble(s14,3),0);
    fooLCbub += bubSwitch[3-1]*productCoeff(qq2yygz0LC<1>(z,-t12,t34,-u),bubble(s23,3),0);
    fooLCbub += bubSwitch[4-1]*productCoeff(qq2yygz0LC<1>(z,-t12,-t34,u),bubble(s24,3),0);
    fooLCbub += bubSwitch[5-1]*productCoeff(qq2yygz0LC<2>(z,t12,t34,u),  bubble(s15,3),0);
    fooLCbub += bubSwitch[6-1]*productCoeff(qq2yygz0LC<2>(z,-t12,t34,-u),bubble(s25,3),0);
    fooLCbub +=                productCoeff(qq2yygz0LC<3>(z,t12,t34,u),  bubble(s34,3),0);
    return QCD::CA*fooLCbub;
}

double qq2yygz0LCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
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
    const my_float z = 1-zb;
    double fooLCbox(0.);
    fooLCbox += productCoeff(qq2yygz0LC<4>(z,t12,t34,u),  box6(s13,s15,s24,3),0);
    fooLCbox += productCoeff(qq2yygz0LC<4>(z,t12,-t34,-u),box6(s14,s15,s23,3),0);
    fooLCbox += productCoeff(qq2yygz0LC<4>(z,-t12,t34,-u),box6(s23,s25,s14,3),0);
    fooLCbox += productCoeff(qq2yygz0LC<4>(z,-t12,-t34,u),box6(s24,s25,s13,3),0);
    fooLCbox += productCoeff(qq2yygz0LC<5>(z,t12,t34,u),  box6(s13,s34,s25,3),0);
    fooLCbox += productCoeff(qq2yygz0LC<5>(z,t12,-t34,-u),box6(s14,s34,s25,3),0);
    fooLCbox += productCoeff(qq2yygz0LC<5>(z,-t12,t34,-u),box6(s23,s34,s15,3),0);
    fooLCbox += productCoeff(qq2yygz0LC<5>(z,-t12,-t34,u),box6(s24,s34,s15,3),0);
    fooLCbox += productCoeff(qq2yygz0LC<6>(z,t12,t34,u),  box6(s15,s25,s34,3),0);
    return QCD::CA*fooLCbox;
}

double qq2yygz0SCbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
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
    const my_float z = 1-zb;
    bool bubSwitch[6] = {true,true,true,true,true,true};
    size_t nPatch = 0;
    double fooSCbub(0.);
    if (todouble<my_float>(fabs<my_float>(s13-s25)/(fabs<my_float>(s13)+fabs<my_float>(s25)))<patchDelta) {
        bubSwitch[2-2]=false;
        bubSwitch[7-2]=false;
        ++nPatch;
        fooSCbub += qq2yygz0SCbub1325(z,t12,t34);
    }
    if (todouble<my_float>(fabs<my_float>(s14-s25)/(fabs<my_float>(s14)+fabs<my_float>(s25)))<patchDelta) {
        bubSwitch[3-2]=false;
        bubSwitch[7-2]=false;
        ++nPatch;
        fooSCbub += qq2yygz0SCbub1325(z,t12,-t34);
    }
    if (todouble<my_float>(fabs<my_float>(s23-s15)/(fabs<my_float>(s23)+fabs<my_float>(s15)))<patchDelta) {
        bubSwitch[4-2]=false;
        bubSwitch[6-2]=false;
        ++nPatch;
        fooSCbub += qq2yygz0SCbub1325(z,-t12,t34);
    }
    if (todouble<my_float>(fabs<my_float>(s24-s15)/(fabs<my_float>(s24)+fabs<my_float>(s15)))<patchDelta) {
        bubSwitch[5-2]=false;
        bubSwitch[6-2]=false;
        ++nPatch;
        fooSCbub += qq2yygz0SCbub1325(z,-t12,-t34);
    }
    if (todouble<my_float>(fabs<my_float>(s13-s24)/(fabs<my_float>(s13)+fabs<my_float>(s24)))<patchDelta*todouble(zb)) {
        bubSwitch[2-2]=false;
        bubSwitch[5-2]=false;
        ++nPatch;
        fooSCbub += qq2yygz0SCbub1324(z,t12,u);
    }
    if (todouble<my_float>(fabs<my_float>(s14-s23)/(fabs<my_float>(s14)+fabs<my_float>(s23)))<patchDelta*todouble(zb)) {
        bubSwitch[3-2]=false;
        bubSwitch[4-2]=false;
        ++nPatch;
        fooSCbub += qq2yygz0SCbub1324(z,-t12,-u);
    }
    if (nPatch>1) {
        std::cerr << "Error in qq2yyg: too many large cancellations!" << std::endl;
        return 0.;
    }
    fooSCbub +=                productCoeff(qq2yygz0SC<1>(z,t12,t34,u),  bubble(s12,3),0);
    fooSCbub += bubSwitch[2-2]*productCoeff(qq2yygz0SC<2>(z,t12,t34,u),  bubble(s13,3),0);
    fooSCbub += bubSwitch[3-2]*productCoeff(qq2yygz0SC<2>(z,t12,-t34,-u),bubble(s14,3),0);
    fooSCbub += bubSwitch[4-2]*productCoeff(qq2yygz0SC<2>(z,-t12,t34,-u),bubble(s23,3),0);
    fooSCbub += bubSwitch[5-2]*productCoeff(qq2yygz0SC<2>(z,-t12,-t34,u),bubble(s24,3),0);
    fooSCbub += bubSwitch[6-2]*productCoeff(qq2yygz0SC<3>(z,t12,t34,u),  bubble(s15,3),0);
    fooSCbub += bubSwitch[7-2]*productCoeff(qq2yygz0SC<3>(z,-t12,t34,-u),bubble(s25,3),0);
    fooSCbub +=                productCoeff(qq2yygz0SC<4>(z,t12,t34,u),  bubble(s34,3),0);
    fooSCbub +=                productCoeff(qq2yygz0SC<5>(z,t12,t34,u),  bubble(s35,3),0);
    fooSCbub +=                productCoeff(qq2yygz0SC<5>(z,t12,-t34,-u),bubble(s45,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbub;
}

double qq2yygz0SCbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
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
    const my_float z = 1-zb;
    double fooSCbox(0.);
    fooSCbox += productCoeff(qq2yygz0SC<6> (z,t12,t34,u),  box6(s12,s13,s45,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<6> (z,t12,-t34,-u),box6(s12,s14,s35,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<6> (z,-t12,t34,-u),box6(s12,s23,s45,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<6> (z,-t12,-t34,u),box6(s12,s24,s35,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<7> (z,t12,t34,u),  box6(s12,s15,s34,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<7> (z,-t12,t34,-u),box6(s12,s25,s34,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<8> (z,t12,t34,u),  box6(s13,s34,s25,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<8> (z,t12,-t34,-u),box6(s14,s34,s25,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<8> (z,-t12,t34,-u),box6(s23,s34,s15,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<8> (z,-t12,-t34,u),box6(s24,s34,s15,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<9> (z,t12,t34,u),  box6(s13,s35,s24,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<9> (z,t12,-t34,-u),box6(s14,s45,s23,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<9> (z,-t12,t34,-u),box6(s23,s35,s14,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<9> (z,-t12,-t34,u),box6(s24,s45,s13,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<10>(z,t12,t34,u),  box6(s15,s35,s24,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<10>(z,t12,-t34,-u),box6(s15,s45,s23,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<10>(z,-t12,t34,-u),box6(s25,s35,s14,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<10>(z,-t12,-t34,u),box6(s25,s45,s13,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<11>(z,t12,t34,u),  box6(s34,s35,s12,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<11>(z,t12,-t34,-u),box6(s34,s45,s12,3),0);
    fooSCbox += productCoeff(qq2yygz0SC<12>(z,t12,t34,u),  box6(s35,s45,s12,3),0);
    return (QCD::CA-2.*QCD::CF)*fooSCbox;
}

double qq2yygz0Nfbub(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
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
    const my_float z = 1-zb;
    double fooNfbub(0.);
    fooNfbub += productCoeff(qq2yygz0Nf<3>(z,t12,t34,u),  bubble(s12,3),0);
    fooNfbub += productCoeff(qq2yygz0Nf<4>(z,t12,t34,u),  bubble(s34,3),0);
    fooNfbub += productCoeff(qq2yygz0Nf<5>(z,t12,t34,u),  bubble(s35,3),0);
    fooNfbub += productCoeff(qq2yygz0Nf<5>(z,t12,-t34,-u),bubble(s45,3),0);
    // CHECK THIS FACTOR
    return -fooNfbub/512.;
}

double qq2yygz0Nfbox(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
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
    const my_float z = 1-zb;
    double fooNfbox(0.);
    fooNfbox += productCoeff(qq2yygz0Nf<1> (z,t12,t34,u),  box6(s34,s35,s12,3),0);
    fooNfbox += productCoeff(qq2yygz0Nf<1> (z,t12,-t34,-u),box6(s34,s45,s12,3),0);
    fooNfbox += productCoeff(qq2yygz0Nf<2> (z,t12,t34,u),  box6(s35,s45,s12,3),0);
    // CHECK THIS FACTOR
    return -fooNfbox/512.;
}

double qq2yygz0col(const my_float& s13, const my_float& s14, const my_float& s23, const my_float& s24)
{
    return (
            qq2yygz0LCbub(s13,s14,s23,s24)+qq2yygz0LCbox(s13,s14,s23,s24)+
            qq2yygz0SCbub(s13,s14,s23,s24)+qq2yygz0SCbox(s13,s14,s23,s24)+
            QCD::sumQ2*(qq2yygz0Nfbub(s13,s14,s23,s24)+qq2yygz0Nfbox(s13,s14,s23,s24))
            );
}

Expansion<Parameter::epsilon, double> qq2yygz0LCbubexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       )
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
    const my_float z = 1-zb;
    size_t nPatch = 0;
    Expansion<Parameter::epsilon, double> fooLCbub(-2,{0.,0.,0.});
    fooLCbub += times(qq2yygz0LC<1>(z,t12,t34,u),  bubble(s13,3),3);
    fooLCbub += times(qq2yygz0LC<1>(z,t12,-t34,-u),bubble(s14,3),3);
    fooLCbub += times(qq2yygz0LC<1>(z,-t12,t34,-u),bubble(s23,3),3);
    fooLCbub += times(qq2yygz0LC<1>(z,-t12,-t34,u),bubble(s24,3),3);
    fooLCbub += times(qq2yygz0LC<2>(z,t12,t34,u),  bubble(s15,3),3);
    fooLCbub += times(qq2yygz0LC<2>(z,-t12,t34,-u),bubble(s25,3),3);
    fooLCbub += times(qq2yygz0LC<3>(z,t12,t34,u),  bubble(s34,3),3);
    return QCD::CA*fooLCbub;
}

Expansion<Parameter::epsilon, double> qq2yygz0LCboxexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       )
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
    const my_float z = 1-zb;
    Expansion<Parameter::epsilon, double> fooLCbox(-2,{0.,0.,0.});
    fooLCbox += times(qq2yygz0LC<4>(z,t12,t34,u),  box6(s13,s15,s24,3),3);
    fooLCbox += times(qq2yygz0LC<4>(z,t12,-t34,-u),box6(s14,s15,s23,3),3);
    fooLCbox += times(qq2yygz0LC<4>(z,-t12,t34,-u),box6(s23,s25,s14,3),3);
    fooLCbox += times(qq2yygz0LC<4>(z,-t12,-t34,u),box6(s24,s25,s13,3),3);
    fooLCbox += times(qq2yygz0LC<5>(z,t12,t34,u),  box6(s13,s34,s25,3),3);
    fooLCbox += times(qq2yygz0LC<5>(z,t12,-t34,-u),box6(s14,s34,s25,3),3);
    fooLCbox += times(qq2yygz0LC<5>(z,-t12,t34,-u),box6(s23,s34,s15,3),3);
    fooLCbox += times(qq2yygz0LC<5>(z,-t12,-t34,u),box6(s24,s34,s15,3),3);
    fooLCbox += times(qq2yygz0LC<6>(z,t12,t34,u),  box6(s15,s25,s34,3),3);
    return QCD::CA*fooLCbox;
}

Expansion<Parameter::epsilon, double> qq2yygz0SCbubexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       )
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
    const my_float z = 1-zb;
    Expansion<Parameter::epsilon, double> fooSCbub(-2,{0.,0.,0.});
    fooSCbub += times(qq2yygz0SC<1>(z,t12,t34,u),  bubble(s12,3),3);
    fooSCbub += times(qq2yygz0SC<2>(z,t12,t34,u),  bubble(s13,3),3);
    fooSCbub += times(qq2yygz0SC<2>(z,t12,-t34,-u),bubble(s14,3),3);
    fooSCbub += times(qq2yygz0SC<2>(z,-t12,t34,-u),bubble(s23,3),3);
    fooSCbub += times(qq2yygz0SC<2>(z,-t12,-t34,u),bubble(s24,3),3);
    fooSCbub += times(qq2yygz0SC<3>(z,t12,t34,u),  bubble(s15,3),3);
    fooSCbub += times(qq2yygz0SC<3>(z,-t12,t34,-u),bubble(s25,3),3);
    fooSCbub += times(qq2yygz0SC<4>(z,t12,t34,u),  bubble(s34,3),3);
    fooSCbub += times(qq2yygz0SC<5>(z,t12,t34,u),  bubble(s35,3),3);
    fooSCbub += times(qq2yygz0SC<5>(z,t12,-t34,-u),bubble(s45,3),3);
    return (QCD::CA-2.*QCD::CF)*fooSCbub;
}

Expansion<Parameter::epsilon, double> qq2yygz0SCboxexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       )
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
    const my_float z = 1-zb;
    Expansion<Parameter::epsilon, double> fooSCbox(-2,{0.,0.,0.});
    fooSCbox += times(qq2yygz0SC<6> (z,t12,t34,u),  box6(s12,s13,s45,3),3);
    fooSCbox += times(qq2yygz0SC<6> (z,t12,-t34,-u),box6(s12,s14,s35,3),3);
    fooSCbox += times(qq2yygz0SC<6> (z,-t12,t34,-u),box6(s12,s23,s45,3),3);
    fooSCbox += times(qq2yygz0SC<6> (z,-t12,-t34,u),box6(s12,s24,s35,3),3);
    fooSCbox += times(qq2yygz0SC<7> (z,t12,t34,u),  box6(s12,s15,s34,3),3);
    fooSCbox += times(qq2yygz0SC<7> (z,-t12,t34,-u),box6(s12,s25,s34,3),3);
    fooSCbox += times(qq2yygz0SC<8> (z,t12,t34,u),  box6(s13,s34,s25,3),3);
    fooSCbox += times(qq2yygz0SC<8> (z,t12,-t34,-u),box6(s14,s34,s25,3),3);
    fooSCbox += times(qq2yygz0SC<8> (z,-t12,t34,-u),box6(s23,s34,s15,3),3);
    fooSCbox += times(qq2yygz0SC<8> (z,-t12,-t34,u),box6(s24,s34,s15,3),3);
    fooSCbox += times(qq2yygz0SC<9> (z,t12,t34,u),  box6(s13,s35,s24,3),3);
    fooSCbox += times(qq2yygz0SC<9> (z,t12,-t34,-u),box6(s14,s45,s23,3),3);
    fooSCbox += times(qq2yygz0SC<9> (z,-t12,t34,-u),box6(s23,s35,s14,3),3);
    fooSCbox += times(qq2yygz0SC<9> (z,-t12,-t34,u),box6(s24,s45,s13,3),3);
    fooSCbox += times(qq2yygz0SC<10>(z,t12,t34,u),  box6(s15,s35,s24,3),3);
    fooSCbox += times(qq2yygz0SC<10>(z,t12,-t34,-u),box6(s15,s45,s23,3),3);
    fooSCbox += times(qq2yygz0SC<10>(z,-t12,t34,-u),box6(s25,s35,s14,3),3);
    fooSCbox += times(qq2yygz0SC<10>(z,-t12,-t34,u),box6(s25,s45,s13,3),3);
    fooSCbox += times(qq2yygz0SC<11>(z,t12,t34,u),  box6(s34,s35,s12,3),3);
    fooSCbox += times(qq2yygz0SC<11>(z,t12,-t34,-u),box6(s34,s45,s12,3),3);
    fooSCbox += times(qq2yygz0SC<12>(z,t12,t34,u),  box6(s35,s45,s12,3),3);
    return (QCD::CA-2.*QCD::CF)*fooSCbox;
}

Expansion<Parameter::epsilon, double> qq2yygz0Nfbubexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       )
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
    const my_float z = 1-zb;
    Expansion<Parameter::epsilon, double> fooNfbub(-2,{0.,0.,0.});
    fooNfbub += times(qq2yygz0Nf<3>(z,t12,t34,u),  bubble(s12,3),2);
    fooNfbub += times(qq2yygz0Nf<4>(z,t12,t34,u),  bubble(s34,3),2);
    fooNfbub += times(qq2yygz0Nf<5>(z,t12,t34,u),  bubble(s35,3),2);
    fooNfbub += times(qq2yygz0Nf<5>(z,t12,-t34,-u),bubble(s45,3),2);
    // CHECK THIS FACTOR
    return -1./512.*fooNfbub;
}

Expansion<Parameter::epsilon, double> qq2yygz0Nfboxexp(
                                                       const my_float& s13,
                                                       const my_float& s14,
                                                       const my_float& s23,
                                                       const my_float& s24
                                                       )
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
    const my_float z = 1-zb;
    Expansion<Parameter::epsilon, double> fooNfbox(-2,{0.,0.,0.});
    fooNfbox += times(qq2yygz0Nf<1> (z,t12,t34,u),  box6(s34,s35,s12,3),1);
    fooNfbox += times(qq2yygz0Nf<1> (z,t12,-t34,-u),box6(s34,s45,s12,3),1);
    fooNfbox += times(qq2yygz0Nf<2> (z,t12,t34,u),  box6(s35,s45,s12,3),1);
    // CHECK THIS FACTOR
    return -1./512.*fooNfbox;
}

Expansion<Parameter::epsilon, double> qq2yygz0colexp(
                                                     const my_float& s13,
                                                     const my_float& s14,
                                                     const my_float& s23,
                                                     const my_float& s24
                                                     )
{
    return (
            qq2yygz0LCbubexp(s13,s14,s23,s24)+qq2yygz0LCboxexp(s13,s14,s23,s24)+
            qq2yygz0SCbubexp(s13,s14,s23,s24)+qq2yygz0SCboxexp(s13,s14,s23,s24)+
            QCD::sumQ2*(qq2yygz0Nfbubexp(s13,s14,s23,s24)+qq2yygz0Nfboxexp(s13,s14,s23,s24))
            );
}

#endif

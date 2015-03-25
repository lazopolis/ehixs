/**
 *
 * \file    coeffscolor.cpp
 * \ingroup new_b_bbar
 * \author  Simone Lionetti
 * \date    March 2015
 *
 */

#include "boxmaster.h" // box, bubble
#include "constants.h" // QCD::CF, etc...

#include "coeffscolor.h"
#include "coeffLC1.h"
#include "coeffLC2.h"
#include "coeffLC3.h"
#include "coeffLC4.h"
#include "coeffSC1.h"
#include "coeffSC2.h"
#include "coeffSC3.h"
#include "coeffSC4.h"
#include "coeffSC5.h"
#include "coeffSC6.h"

double bb2HgLcol(const double& s12, const double& s14, const double& s24)
{
    double fooLC(0.);
    fooLC += productCoeff(bb2HgLC<1>(s12,s14,s24),box(s14,s24,s12+s14+s24,3),0);
    fooLC += productCoeff(bb2HgLC<2>(s12,s14,s24),bubble(s14,3),0);
    fooLC += productCoeff(bb2HgLC<3>(s12,s14,s24),bubble(s24,3),0);
    fooLC += productCoeff(bb2HgLC<4>(s12,s14,s24),bubble(s12+s14+s24,3),0);
    return QCD::CA*fooLC;
}

double bb2HgScol(const double& s12, const double& s14, const double& s24)
{
    double fooSC(0.);
    fooSC += productCoeff(bb2HgSC<1>(s12,s14,s24),box(s12,s14,s12+s14+s24,3),0);
    fooSC += productCoeff(bb2HgSC<2>(s12,s14,s24),box(s12,s24,s12+s14+s24,3),0);
    fooSC += productCoeff(bb2HgSC<3>(s12,s14,s24),bubble(s12,3),0);
    fooSC += productCoeff(bb2HgSC<4>(s12,s14,s24),bubble(s14,3),0);
    fooSC += productCoeff(bb2HgSC<5>(s12,s14,s24),bubble(s24,3),0);
    fooSC += productCoeff(bb2HgSC<6>(s12,s14,s24),bubble(s12+s14+s24,3),0);
    return fooSC/QCD::CA;
}

double bb2Hgcol(const double& s12, const double& s14, const double& s24)
{
    return (
            bb2HgScol(s12,s14,s24)+bb2HgLcol(s12,s14,s24)
            );
}

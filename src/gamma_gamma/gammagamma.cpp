/**
 *
 * \file    gammagamma.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#include <iostream>
using namespace std;
#include "gammagamma.h"
#include "gammagamma_cuts.h"

void GammaGamma::ConfigureCuts()
{
    // The file includes the explicit declarations of cut objects.
    // The cuts declared there are available, not actually active.
    // They can be activated in runcard. The parsing of runcard for cuts
    // is done in Production, after the call to this function.
    #include "gammagamma_cut_initialization.h"
}

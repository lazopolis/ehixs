/**
 *
 * \file    gammagamma.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 * \brief   Production P P -> gamma gamma + X
 *
 */

#ifndef GAMMAGAMMA_H
#define GAMMAGAMMA_H

#include "production.h"
#include "gammagamma_me.h"

class GammaGamma : public Production
{

public:

    GammaGamma()
    : Production()
    {
        sectors.push_back(new Factory<Sector,GammaGamma_qq_LO>("LO","leading order"));
        sectors.push_back(new Factory<Sector,GammaGamma_qq_NLO_real>("R","real emission correction"));
        sectors.push_back(new Factory<Sector,GammaGamma_qq_NNLO_RV>("RV","real-virtual correction"));
        return;
    }

};

static Factory<Production,GammaGamma> gammagammafactory("gammagamma","production of two on-shell photons");

#endif

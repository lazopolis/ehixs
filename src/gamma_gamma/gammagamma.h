/**
 *
 * \file    gammagamma.h
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#ifndef GAMMAGAMMA_H
#define GAMMAGAMMA_H

// Implementing the process P P -> gamma gamma + X

#include "production.h"
#include "xsection.h"
#include "gammagamma_me.h"

class GammaGamma : public Production
{

public:

    GammaGamma()
    {
        sectors.push_back(new XSectionMaker<GammaGamma_qq_LO>());
        sectors.push_back(new XSectionMaker<GammaGamma_qq_NLO_real>());
        return;
    }

    void ConfigureCuts();

};

#endif

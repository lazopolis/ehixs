#ifndef GAMMA_STAR_GAMMA_STAR_H
#define GAMMA_STAR_GAMMA_STAR_H

#include "production.h"
//#include "convolutions.h" // for FF




class GammaStarGammaStar : public Production
{
public:
    ~GammaStarGammaStar();
    //: virtual obligations from production
    void SetNumberOfParticles();
    void SetDecayParticleIdInEventBox();
    void create_matrix_elements();
    void ConfigureCuts();
    void SetProcessSpecificParameters(const UserInterface& UI);

};

#endif
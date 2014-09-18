#ifndef BOTTOM_FUSION_H
#define BOTTOM_FUSION_H

// implementing the process P P -> higgs + X @ NNLO via bottom fusion
// we can have up to 2 extra emission particles.
// The particles are numbered
// f1(p1) + f2(p2) -> h(p3) + f3(p4) + f4(p5)
//
// we agree that at NLO the extra particle has momenta p4, so p5 is not used at NLO

// including base class
#include "production.h"
#include "bottom_fusion_me.h"

class BottomFusion : public Production
{
public:
    //: virtual obligations from production
    void SetNumberOfParticles();
    void SetDecayParticleIdInEventBox();
    void create_matrix_elements();
    void ConfigureCuts();
    void SetProcessSpecificParameters(const UserInterface& UI);
};






#endif
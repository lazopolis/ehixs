#ifndef BOTTOM_FUSION_H
#define BOTTOM_FUSION_H

// implementing the process P P -> Higgs + X via bottom fusion
// we can have up to 2 extra emission particles.

// including base class
#include "production.h"
#include "xsection.h"
#include "bottom_fusion_me.h"

class BottomFusion : public Production
{

public:


    BottomFusion()
    {
        sectors.push_back(new XSectionMaker<BottomFusion_bb_LO>());
        sectors.push_back(new XSectionMaker<BottomFusion_bb_NLO_real>());
        sectors.push_back(new XSectionMaker<BottomFusion_bb_NLO_soft>());
        return;
    }

    void ConfigureCuts();

};






#endif
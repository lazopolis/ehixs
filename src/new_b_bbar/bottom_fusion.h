/**
 *
 * \file    bottom_fusion.h
 * \ingroup new_b_bbar
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    August 2014
 *
 */

#ifndef BOTTOM_FUSION_H
#define BOTTOM_FUSION_H

// Implementing the process P P -> Higgs + X via bottom fusion

#include "production.h"
#include "xsection.h"
#include "bottom_fusion_me.h"

class BottomFusion : public Production
{

public:

    BottomFusion()
    {
        sectors.push_back(new XSectionMaker<BottomFusion_bb_LO>());
        sectors.push_back(new XSectionMaker<BottomFusion_bb_NLO_hard>());
        sectors.push_back(new XSectionMaker<BottomFusion_bb_NNLO_RV>());
        return;
    }

    void ConfigureCuts();

};

#endif

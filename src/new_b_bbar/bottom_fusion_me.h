#ifndef BOTTOM_FUSION_ME_H
#define BOTTOM_FUSION_ME_H

#include <stdlib.h>
#include "xsection.h"
#include "variables.h"

/// \todo Move this to either Constants, UserInterface, Model or whatever
constexpr double yukawa_bottom = 1.0;

/**
 *
 * \class BottomFusion_bb_Delta
 * Mother class for subprocesses with bbar initial state and delta-like kinematics
 *
 */

class BottomFusion_bb_Delta : public XSection
{

public:

    BottomFusion_bb_Delta(const UserInterface& UI, const SectorInfo& info);

};

/**
 *
 * \class BottomFusion_bb_LO
 * LO matrix element for bbar->H
 *
 */

class BottomFusion_bb_LO : public BottomFusion_bb_Delta
{

public:

    BottomFusion_bb_LO(const UserInterface& UI) :
    BottomFusion_bb_Delta(UI, XSectionMaker<BottomFusion_bb_LO>::_info)
    {}

    double matrixElement(const KinematicInvariants& invariants) const;

};

#endif

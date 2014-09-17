#ifndef BOTTOM_FUSION_ME_H
#define BOTTOM_FUSION_ME_H

#include "xsection.h"
#include "variables.h"

/**
 *
 * \class BottomFusionXSection
 * \brief Mother class for any subprocess contributing to Higgs production via bottom fusion
 *
 */

class BottomFusionXSection : public XSection
{

public:

    BottomFusionXSection(const UserInterface& UI, const SectorInfo& info) :
    XSection(UI, info)
    {
        // Set process-dependent parameters
        _mH = UI.m_higgs;
        return;
    }

protected:
    
    /// \name Data members
    /// @{
    
    double _mH;                       /// < Higgs mass
    const double yukawa_bottom = 1.0; /// < Bottom-Higgs Yukawa coupling

    /// @}
    
};

/**
 *
 * \class BottomFusion_bb_Delta
 * Mother class for subprocesses with bbar initial state and delta-like kinematics
 *
 */

class BottomFusion_bb_Delta : public BottomFusionXSection
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

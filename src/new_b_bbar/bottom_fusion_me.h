#ifndef BOTTOM_FUSION_ME_H
#define BOTTOM_FUSION_ME_H

#include "xsection.h"
#include "kinematicvariables.h"

/**
 *
 * \class BottomFusionCrossSection
 * \brief Mother class for any subprocess contributing to Higgs production via bottom fusion
 *
 */

class BottomFusionCrossSection : public XSection
{

protected:
    
    /// \name Data members
    /// @{
    
    double mh_;             ///< Higgs mass
    
    /// @}
    
};

/**
 *
 * \class BottomFusion_bb
 * \brief Mother class for subprocesses with bbar initial state
 *
 */

class BottomFusion_bb : public BottomFusionCrossSection
{

public:

    /// \name Constructor
    /// @{
    
    //BottomFusion_bb();

    /// @}
    
    /// \name Member functions
    /// @{
    
    NewLuminosity* AllocateLuminosity(const UserInterface&);

    /// @}
    
protected:
    
    /// \name Data members
    /// @{
    
    double smin;                ///< Minimum s
    double prefactor_;          ///< Constant prefactor of the cross section
    
    /// @}
    
};

/**
 *
 * \class BottomFusion_bb_Delta
 * Mother class for subprocesses with bbar initial state and delta-like kinematics
 *
 */

class BottomFusion_bb_Delta : public BottomFusion_bb
{

public:

    /// \name Input functions
    /// @{

    void Evaluate(double* xx_vegas);
    void Configure();

    /// @}

    /// \name Output functions
    /// @{

    virtual double eval_me(const KinematicInvariants&)=0;

    /// @}
    
protected:

    /// \name Data members
    /// @{
    
    KinematicVariables<OneXGenerator,DeltaPG> kk_;
    
    /// @}

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

//    BottomFusion_bb_LO();
    double eval_me(const KinematicInvariants&);

};

/**
 *
 * \class BottomFusion_bb_NLO_Soft
 * LO matrix element for bbar->H
 *
 */

class BottomFusion_bb_NLO_Soft : public BottomFusion_bb_Delta
{

public:

    BottomFusion_bb_NLO_Soft();  ///< Standard constructor
    double eval_me(const KinematicInvariants&); ///< Actual matrix element computation

};

#endif

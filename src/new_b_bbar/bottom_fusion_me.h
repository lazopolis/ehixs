#ifndef BOTTOM_FUSION_ME_H
#define BOTTOM_FUSION_ME_H

#include "cross_section.h"
#include "bottom_fusion_kinematics.h"

/**
 *
 * \class BottomFusionCrossSection
 * \brief Mother class for any subprocess contributing to Higgs production via bottom fusion
 *
 */

class BottomFusionCrossSection : public CrossSection
{

public:

    /// \name Input functions
    /// @{
    
    // Set the Higgs mass
    void SetHiggsMass(const double& mh){mh_ = mh;}
    
    /// @}

protected:
    
    /// \name Data members
    /// @{
    
    double mh_;             ///< Higgs mass
    
    /// @}
    
    /// \name Measurement function
    /// @{
    
    // Contructs the event with weight w and kinematic variables kv
    void JF(const double& w,const vector<FourVector>& kv);
    // Fills the event-box with an event with weight 0
    void JF();
    
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
    
    BottomFusion_bb();

    /// @}
    
    /// \name Member functions
    /// @{
    
    void AllocateLuminosity(const UserInterface&);

    /// @}
    
protected:
    
    /// \name Data members
    /// @{
    
    int number_of_particles_;   ///< Number of particles
    double smin;                ///< Minimum s
    double prefactor_;          ///< Constant prefactor of the cross section
    
    /// @}
    
    /// \name Luminosity
    /// @{

    double LL(const double& x1,const double& x2);

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
    
    size_t dimension() const
    {
        return 1;
    }
    
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
    
    BottomFusionKinematics<0> kk_;
    
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

    BottomFusion_bb_LO();
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

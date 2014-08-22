#ifndef BOTTOM_FUSION_ME_H
#define BOTTOM_FUSION_ME_H

#include "cross_section.h"
#include "bottom_fusion_kinematics.h"


class BottomFusionCrossSection : public CrossSection
{
public:
    
protected:

    void JF(const double&,const BottomFusionKinematics& kv);
    void JF();
};



class BottomFusion_bb: public BottomFusionCrossSection
{
public:
    BottomFusion_bb();
    void AllocateLuminosity(const UserInterface&);
    
protected:
    NewLuminosity* lumi;
protected:
    double LL(const double& x1,const double& x2);
    
    
protected:
    int number_of_particles_;
    double smin;
    double prefactor_;
    
};

class BottomFusion_bb_Delta: public BottomFusion_bb
{
public:
    void Configure();
    void Evaluate(double* xx_vegas);
    virtual double eval_me(const KinematicInvariants&)=0;
protected:
    BottomFusionKinematicsLO kk_;
};


class BottomFusion_bb_LO : public BottomFusion_bb_Delta
{
public:
    BottomFusion_bb_LO();
    double eval_me(const KinematicInvariants&);
};


#endif

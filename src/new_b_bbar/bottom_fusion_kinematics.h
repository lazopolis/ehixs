#ifndef BOTTOM_FUSION_KINEMATICS_H
#define BOTTOM_FUSION_KINEMATICS_H

#include "fmomentum.h"
#include "kinematic_invariants.h"
#include <sstream>      // std::stringstream
using namespace std;

// base class
// manages four momenta and s_ij for the current phase space point
// inheritance determines how many particles are there in the final state
class BottomFusionKinematics{
public:
    void SetNumberOfParticles(int num_of_particles);
    // can be 3,4, or 5
    
    // getting the mh_sq and the collider S from outside
    void SetBoundaries(const double& mh_sq,const double& S);
    
    // returning the bjorken x's
    double x1() const {return x1_;}
    double x2() const {return x2_;}
    // returning s_ij
    double s(int i,int j)const {return kin_inv_.s(i,j);}
    // s_i is defined to be p_i^2
    double s(int i) const {return kin_inv_.s(i);}
    // q_ij dimensionless invariants: good for complicated matrix elements
    // q_ij = s_ij / s_12
    double q(int i,int j) const {return kin_inv_.q(i,j);}
    // q_i = s_i / s_12
    double q(int i) const {return kin_inv_.q(i);}
    
    // four momenta for the five particles
    FMomentum p1;
    FMomentum p2;
    FMomentum p3;
    FMomentum p4;
    FMomentum p5;
    
public:
    //
    virtual void generate_kinematics(double* xx_vegas)=0;
    
    KinematicInvariants invariants()const {return kin_inv_;}
    
    
    double jacobian;
    friend ostream& operator<<(ostream&, const BottomFusionKinematics&);
protected:
    int num_of_particles_;
    double x1_,x2_;//bjorken x's
    KinematicInvariants kin_inv_;
    double tau_,S_,mh_sq_;
};

// LO kinematics: p1+p2 -> p3=ph and nothing else
class BottomFusionKinematicsLO: public BottomFusionKinematics
{
public:
    void generate_kinematics(double* xx_vegas);
private:
    void set_up_momenta_at_lab();
    void generate_bjorken_xs(const double&);
    void compute_invariants();
};

// NLO kinematics: p1+p2->p3+p4
/*
class BottomFusionKinematicsNLO: public BottomFusionKinematics
{
public:
    void generate_kinematics(double* xx_vegas);
    double z;
    double lambda;
private:
    
    
    double phi;
    double phi_g;
};
*/
#endif



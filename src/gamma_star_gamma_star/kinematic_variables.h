#ifndef KINEMATIC_VARIABLES
#define KINEMATIC_VARIABLES

#include <string>       // std::string
#include <iostream>     // std::cout
#include <sstream>      // std::stringstream
#include <vector>
using namespace std;
#include "constants.h"

#include "fmomentum.h"
#include "kinematic_invariants.h"
#include "bjorken_xs.h"

class Massive2ParticlePhaseSpace{
public:
    void generate(const double& Qsq,const double& s3,const double& s4,const double& v2,const double& v3);
    double Kaellen(const double& a, const double& b,const double& c);
    double jacobian(){return jacobian_;}
    FMomentum p3com() const {return p3com_;}
    FMomentum p4com() const {return p4com_;}
    void SetP3(const FMomentum& p){p3com_=p;}
    void SetP4(const FMomentum& p){p4com_=p;}

private:
    FMomentum p3com_;
    FMomentum p4com_;
    double jacobian_;
};




class GStar2Kinematics{
public:
    void SetNumberOfParticles(int);
    void SetBoundaries(const double& ,const double& );
    void SetMassesSquared(const double& s3, const double& s4);
    
    //void SetS5(const double& x){kin_inv_.Set(5,x);}
    
    double x1() const {return bjorken_.x1();}
    double x2() const {return bjorken_.x2();}
    
    double s(int i,int j)const {return kin_inv_.s(i,j);}
    double s(int i) const {return kin_inv_.s(i);}
    //double q(int i,int j) const {return kin_inv_.q(i,j);}
    //double q(int i) const {return kin_inv_.q(i);}
    
    KinematicInvariants Invariants()const {return kin_inv_;}
    double Jacobian(){return jacobian_;}
    int NumberOfParticles()const {return p_.size();}
    // for Mueller's parametrization
    //double tbar;

    FMomentum P(int i) const {return p_[i];}
    //FMomentum p7;
    virtual void GenerateKinematics(double* xx_vegas)=0;
//
//    void compute_nlo_invariants();
    
//    
//    
//    double nnlo_jacobian;
    friend ostream& operator<<(ostream&, const GStar2Kinematics&);
protected:
    int num_of_particles_;
    BjorkenXs bjorken_;
    Massive2ParticlePhaseSpace born_kins_;
    KinematicInvariants kin_inv_;
    double tau_,smax_,smin_;
protected:
    void generate_bjorken_xs(double* xx_vegas);
    void boost_to_lab();
    void boost_along_z_axis(const double& bb);
    void compute_born_invariants();
    void check_momentum_conservation() const;
    void set_p1_p2_at_lab();
    
    vector<FMomentum> p_;
    double jacobian_;

};

class GStar2KinematicsLO: public GStar2Kinematics
{
public:
    void GenerateKinematics(double* xx_vegas);
};
/*
class NLOKinematics: public KinematicVariables
{
public:
    NLOKinematics(int num_of_particles): KinematicVariables(num_of_particles){};
    void generate_kinematics(double* xx_vegas);
    double z;
    double lambda;
private:
    
    
    double phi;
    double phi_g;
};

class NNLOKinematics: public KinematicVariables
{
public:
    NNLOKinematics(int num_of_particles): KinematicVariables(num_of_particles){};
    void generate_kinematics(double* xx_vegas);
    double z;
    double lambda;
    double rho;
private:
    
    
    double phi;
    double phi_g;
    
};

class NNLOExclusiveKinematics: public KinematicVariables
{
public:
    NNLOExclusiveKinematics(int num_of_particles): KinematicVariables(num_of_particles){};
    void generate_kinematics(double* xx_vegas);
    double z;
    double lambda;
    double rho;
    double x3;
    double x4;
private:
    
    
    double phi;
    double phi_g;
    
};


class LOKinematicsShifted: public KinematicVariables
{
public:
    LOKinematicsShifted(int num_of_particles): KinematicVariables(num_of_particles){}
    void generate_kinematics(double* xx_vegas);
    void SetZ(const double& z){z_=z;}
    virtual void shift_initial_state_particle()=0;
protected:
    double z_;
};

class LOKinematicsShiftedLeft: public LOKinematicsShifted
{
public:
    LOKinematicsShiftedLeft(int num_of_particles): LOKinematicsShifted(num_of_particles){};
    void shift_initial_state_particle();
};

class LOKinematicsShiftedRight: public LOKinematicsShifted
{
public:
    LOKinematicsShiftedRight(int num_of_particles): LOKinematicsShifted(num_of_particles){};
    void shift_initial_state_particle();
};

//------------------- Mueller kinematics

class NNLOKinematicsMueller: public KinematicVariables
{
public:
    NNLOKinematicsMueller(int num_of_particles): KinematicVariables(num_of_particles){};
    void generate_kinematics(double* xx_vegas);
    double z;
    double lambda;
    double u;
    double jacobianFull();
    double jacobianAtLimits();
    double SplittingKernel();
private:
    
    double phi;
    double phi_g;
private:
    void set_tbar();
    
};

*/

#endif

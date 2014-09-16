#ifndef KINEMATIC_VARIABLES
#define KINEMATIC_VARIABLES

#include <string>       // std::string
#include <iostream>     // std::cout
#include <sstream>      // std::stringstream
#include "constants.h"
#include "kinematics.h"
using namespace std;

/// \deprecated You should use kinematicvariables.h instead

class KinematicVariables : public KinematicInvariants, vector<FourMomentum>
{
public:

    KinematicVariables(int num_of_particles){kin_inv_.SetMaxMomentumID(num_of_particles);
        num_of_particles_ = num_of_particles;}
    void SetBoundaries(const double& smin,const double& smax){smax_=smax;smin_=smin;tau_=smin/smax;}
    void SetMassesSquared(const double& s3, const double& s4){kin_inv_.Set(3,s3);kin_inv_.Set(4,s4);}
    void SetS5(const double& x){kin_inv_.Set(5,x);}
    double x1() const {return bjorken_.x1();}
    double x2() const {return bjorken_.x2();}
    
    // for Mueller's parametrization
    double tbar;

    FMomentum p1;
    FMomentum p2;
    FMomentum p3;
    FMomentum p4;
    FMomentum p5;
    FMomentum p6;
    FMomentum p7;
public:
    virtual void generate_kinematics(double* xx_vegas)=0;

    KinematicInvariants invariants()const {return kin_inv_;}
    

    void compute_born_invariants();
    void compute_nlo_invariants();
    void generate_bjorken_xs(double* xx_vegas);
    
    double jacobian;
    double nnlo_jacobian;
    friend ostream& operator<<(ostream&, const KinematicVariables&);
protected:
    int num_of_particles_;
    BjorkenXs bjorken_;
    Massive2ParticlePhaseSpace born_kins_;
    KinematicInvariants kin_inv_;
    double tau_,smax_,smin_;
protected:
    void boost_to_lab();
    void boost_along_z_axis(const double& bb);
    void check_momentum_conservation() const;
    void set_p1_p2_at_lab();

};

class LOKinematics: public KinematicVariables
{
public:
    LOKinematics(int num_of_particles): KinematicVariables(num_of_particles){};
    void generate_kinematics(double* xx_vegas);
};

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



#endif

#ifndef KINEMATIC_VARIABLES
#define KINEMATIC_VARIABLES

#include <string>       // std::string
#include <iostream>     // std::cout
#include <sstream>      // std::stringstream
using namespace std;
#include "constants.h"

#include "fmomentum.h"
#include "kinematic_invariants.h"

/*


class GluonStarKinematics{
public:
    void generate_massive(const double smin, const double& smax, const double& s12,const double& v4, const double& v5, const double& v6, const double& v7);
    void generate_massless(const double smin,const double& smax, const double& s12, const double& v4, const double& v5, const double& v6);
    FMomentum p5(){return p5_;}
    double z(){return z;}
    double rho(){return rho;}
    double lambda(){return lambda;}
    double s5(){return s5_;}
    double s15(){return s15_;}
    double s25(){return s25_;}
    double jacobian(){return jacobian_;}
private:
    FMomentum p5_;
    double z_;
    double rho_;
    double phi_g_;
    double lambda_;
    double s5_,s15_,s25_;
    double jacobian_;
};

class KinematicsHub{
public:
    void SetLOKinematics(const double& z,double* xx_vegas);
    void SetNLOKinematics(const double& z,double* xx_vegas);
    void SetNLOStarKinematics(const double& z,double* xx_vegas);

private:
    FMomentum p_[10];
    BornKinematics born_;
    GluonStarKinematics gluon_;
    BjorkenXs bjorken_;
    KinematicInvariants ss_;
private:
    void boost_p345_to_lab();
    void compute_invariants_at_COM();
    void generate_born_and_boost_to_COM(const double& v2, const double& v3);
    void generate_bjorkens_set_s12_and_p1_p2_at_lab(const double& v0, const double& v1);
};
*/

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




class KinematicVariables{
public:
    KinematicVariables(int num_of_particles){kin_inv_.SetMaxMomentumID(num_of_particles);
        num_of_particles_ = num_of_particles;}
    void SetBoundaries(const double& smin,const double& smax){smax_=smax;smin_=smin;tau_=smin/smax;}
    void SetMassesSquared(const double& s3, const double& s4){kin_inv_.Set(3,s3);kin_inv_.Set(4,s4);}
    void SetS5(const double& x){kin_inv_.Set(5,x);}
    double x1() const {return bjorken_.x1();}
    double x2() const {return bjorken_.x2();}
    double s(int i,int j)const {return kin_inv_.s(i,j);}
    double s(int i) const {return kin_inv_.s(i);}
    double q(int i,int j) const {return kin_inv_.q(i,j);}
    double q(int i) const {return kin_inv_.q(i);}
    
    
    
    
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

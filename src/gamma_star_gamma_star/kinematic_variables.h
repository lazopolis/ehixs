#ifndef KINEMATIC_VARIABLES
#define KINEMATIC_VARIABLES

#include <string>       // std::string
#include <iostream>     // std::cout
#include <sstream>      // std::stringstream
using namespace std;
#include "constants.h"

class FMomentum{
public:
    FMomentum(){for (int i=0;i<4;i++) p[i]=0.0;}
    double p[4];
    void Set(const double& E,const double& px,const double& py, const double& pz){p[0]=E;p[1]=px;p[2]=py;p[3]=pz;}
    void equal(const FMomentum& k)
    {p[0]=k[0];p[1]=k[1];p[2]=k[2];p[3]=k[3];}
    double operator[](int i) const {return p[i];}
    void zboost(const double& b);
    void boost(const double& bx,const double& by,const double& bz);
    double operator*(const FMomentum& Q){return p[0]*Q[0]-p[1]*Q[1]-p[2]*Q[2]-p[3]*Q[3];}
    friend ostream& operator<<(ostream&, const FMomentum&);
};

class KinematicInvariants{
public:
    void SetMaxMomentumID(int max){max_=max;}
    void Set(int i,int j,double x)
        {
        s_ij_[index(min(i,j))][index(max(i,j))] = x;
        }
    void Set(int i,double x)
    {
        s_i_[index(i)]=x;
    }
    double s(int i,int j)const {return s_ij_[index(min(i,j))][index(max(i,j))];}
    double q(int i,int j)const {return q_ij_[index(min(i,j))][index(max(i,j))];}
    double s(int i)const {return s_i_[index(i)];}
    double q(int i)const {return q_i_[index(i)];}
    void compute_dimensionless_invariants();
    
    friend ostream& operator<<(ostream& stream, const KinematicInvariants& kk);
    

private:
    double s_ij_[10][10];//s_ij = (p_i-p_j)^2
    double q_ij_[10][10];//q_ij = s_ij/s_12
    double s_i_[10];// s_i = p_i^2
    double q_i_[10];// q_i = s_i/s_12
    int max_;
private:    
    void check_size(int k) const ;
    int max(int i,int j) const ;
    int min(int i,int j) const;
    int index(int) const;
};

class BjorkenXs{
public:
    void generate(const double& tau,const double& v0, const double& v1);
    double jacobian(){return jacobian_;}
    double x1() const {return x1_;}
    double x2() const {return x2_;}
    double com_rapidity_ratio(){return (x2_-x1_)/(x2_+x1_);}
private:
    double x1_,x2_;
    double jacobian_;
};


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

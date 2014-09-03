#ifndef BOTTOM_FUSION_KINEMATICS_H
#define BOTTOM_FUSION_KINEMATICS_H

#include "fmomentum.h"
#include "kinematic_invariants.h"
#include <sstream>      // std::stringstream
using namespace std;

/**
 *
 * \class BottomFusionKinematics
 *
 * manages four momenta and s_ij for the current phase space point
 * inheritance determines how many particles are there in the final state
 *
 */

class BottomFusionKinematics{

protected:

    ///\name Data members
    //@{

    int num_of_particles_;          //< Number of particles
    double x1_, x2_;                //< Bjorken xvariables
    KinematicInvariants kin_inv_;   //< Kinematic invariants
    double tau_;
    double S_;                      //< Collider center of mass energy (squared)
    double mh_sq_;                  //< Squared Higgs mass

    //@}

public:

    ///\name Data members
    //@{

    double jacobian;                //< Jacobian
    FMomentum p1, p2, p3, p4, p5;   //< Four-momenta for the five particles

    //@}

    ///\name Constructors and destructor
    //@{

    ///Default constructor
    BottomFusionKinematics():
        num_of_particles_(3), x1_(1.), x2_(1.), kin_inv_(), tau_(0.), S_(0.), mh_sq_(0.)
    {}

    //@}

    ///\name Input functions
    //@{

    ///Sets the number of particles, which has to be between 3 and 5
    void SetNumberOfParticles(const int num_of_particles);
    
    ///Sets the mh_sq and the collider S from outside
    void SetBoundaries(const double& mh_sq,const double& S);

    //@}

    ///\name Output functions
    //@{

    ///Return the first Bjorken x
    double x1() const {return x1_;}
    ///Return the second Bjorken x
    double x2() const {return x2_;}

    ///\name Dimensionful invariants s_ij
    //@{
    ///Return s_ij
    double s(const int i,const int j) const {return kin_inv_.s(i,j);}
    ///Return s_i, which is defined to be p_i^2
    double s(const int i) const {return kin_inv_.s(i);}
    //@}

    ///\name Dimensionless invariants q_ij
    ///\note Good for complicated matrix elements
    //@{
    ///Return q_ij = s_ij / s_12
    double q(const int i,const int j) const {return kin_inv_.q(i,j);}
    ///Return q_i = s_i / s_12
    double q(const int i) const {return kin_inv_.q(i);}
    //@}

    ///Return all kinematic invariants
    KinematicInvariants invariants() const {return kin_inv_;}

    ///Stream operator
    friend ostream& operator<<(ostream&, const BottomFusionKinematics&);

    //@}

    ///\name Pure virtual functions
    //@{

    virtual void generate_kinematics(double* xx_vegas)=0;

    //@}

};

///\note Maybe the way to organize this is with templatized BottomFusionKinematics<unsigned int nExtraParticles>
///      and template argument resolution of methods...

/**
 *
 * \class BottomFusionKinematicsLO
 *
 * Class for subprocesses with kinematics p1+p2 -> p3=ph and nothing else
 *
 */

class BottomFusionKinematicsLO : public BottomFusionKinematics
{

public:

    ///\name Initialization function
    //@{

    void generate_kinematics(double* xx_vegas);

    //@}

private:

    ///\name Auxiliary functions
    //@{

    void set_up_momenta_at_lab();
    void generate_bjorken_xs(const double&);
    void compute_invariants();

    //@}

};

/**
 *
 * \class BottomFusionKinematicsNLO
 *
 * Class for subprocesses with kinematics p1+p2 -> p3+p4
 *
 */

class BottomFusionKinematicsNLO : public BottomFusionKinematics
{

public:

    ///\name Constructors
    //@{

    ///Default constructor
    BottomFusionKinematicsNLO() :
    z_(1.),lambda_(0.),phi_(0.)
    {}

    ///Copy constructor
    BottomFusionKinematicsNLO(const BottomFusionKinematicsNLO& that) :
    z_(that.z_),lambda_(that.lambda_),phi_(that.phi_)
    {}

    ///Destructor
    ~BottomFusionKinematicsNLO() {}

    //@}

    ///\name Initialization function
    //@{

    void generate_kinematics(double* xx_vegas);

    //@}

private:

    ///\name Data members
    //@{

    double z_;
    double lambda_;
    double phi_;
    ///\note Achilleas had phi_g also. Unnecessary?

    //@}

    ///\name Auxiliary functions
    //@{

    void set_up_momenta_at_lab();
    void generate_bjorken_xs(const double&);
    void compute_invariants();
    
    //@}
    
};

#endif

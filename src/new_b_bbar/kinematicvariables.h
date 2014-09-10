/**
 * \file    kinematicvariables.h
 * \author  Simone Lionetti
 * \date    September 2014
 */

#ifndef KINEMATIC_VARIABLES_H
#define KINEMATIC_VARIABLES_H

#include "kinematics.h"
using namespace std;

/**
 *
 * \struct Bjorken
 * \brief  Container for Bjorken x data
 *
 */
struct Bjorken
{
    double x1;
    double x2;
    double jacobian;
    //bool isGood?;

    Bjorken() :
    x1(), x2(), jacobian()
    {}

    Bjorken(const double& x1in, const double& x2in, const double& jacin) :
    x1(x1in), x2(x2in), jacobian(jacin)
    {}

};

typedef Bjorken (*xGenerator)(const double&, const double* const);
typedef Bjorken (*pGenerator)(const double&, const double* const);

// Dumb example generator of Bjorken x's from two randoms
Bjorken twoXGenerator(const double& x1x2min, const double* const randoms)
{
    const double x1x2 = x1x2min + (1.-x1x2min) * randoms[0];
    const double x1 = x1x2 + (1.-x1x2) * randoms[1];
    return Bjorken(x1,x1x2/x1,1./x1);
}

// Dumb example generator of Bjorken x's from one random, for delta function
Bjorken oneXGenerator(const double& x1x2min, const double* const randoms)
{
    const double x1x2 = x1x2min;
    const double x1 = x1x2 + (1.-x1x2) * randoms[0];
    return Bjorken(x1,x1x2/x1,(1.-x1x2)/x1);
}

// This is a terrible idea... maybe return to classes??
vector<FourVector> zlambdaPGenerator(const double& S, const double* const randoms)
{
    vector<FourVector> buffer(4);
    // with one extra particle we set
    // p4 = (1-z) [lambdabar p1 + lambda p2 + sqrt(lambda*lambdabar) s12 eperp]
    // (this is covariant and holds in any reference frame if p1, p2 and eperp are transformed)
    // we still define z = mH^2 / s12 with z in (0,1)
    const double E = sqrt(S)/2.;
    const double phi = 2.*consts::Pi*randoms[0];
    const double lambda = randoms[1];
    const double z = _tau / (x1*x2);
    const double sllbar = sqrt(lambda*(1.-lambda))*x1*x2*E;
    p(1) = x1 * E * FourVector(1., 0., 0.,  1.);
    p(2) = x2 * E * FourVector(1., 0., 0., -1.);
    p(4) = (1.-z)*(
                   (1.-lambda) * p(1) +
                   lambda * p(2) +
                   sllbar * FourVector(0.,cos(phi),sin(phi),0.)
                   );
    p(3) = p(1) + p(2) - p(4);
    return;
}

/**
 *
 * \class KinematicVariables
 * \brief Manages generic collider kinematic variables
 * \note  Should the name be changed to ColliderKinematics?
 * \todo  Decide on structure of inheritance, i.e. intermediate layer to compute xs
 * This class contains four momenta, invariants and Bjorken xs for the current phase space point
 * Classes implementing process-specific kinematics should inherit from KinematicVariables
 */

class KinematicVariables : public KinematicInvariants
{

public:

    enum Frame {
        com, /// < Center of mass
        lab  /// < Laboratory
    };

    /// \name Data members
    /// @{

    /// Return the first Bjorken x
    const double& x1 = _x.x1;
    /// Return the second Bjorken x
    const double& x2 = _x.x2;

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    KinematicVariables(xGenerator myXgen = twoXGenerator,pGenerator myPgen = zlambdaPGenerator) :
    KinematicInvariants(), _Xgenerator(myXgen), _Pgenerator(myPgen)
    {}

    /// Copy constructor
    //KinematicVariables(const KinematicVariables& that) :
    //KinematicInvariants(that)
    //{}
    
    /// Destructor
    ~KinematicVariables()
    {}
    
    /// @}

    /// \name Input/output functions
    /// @{

    /// Minimum number of final state particles
    virtual const size_t minFSparticles(void) const = 0;

    /// Number of extra final state partons from real emission
    virtual const size_t realEmissions(void) const = 0;

    /// Total number of final state particles
    const size_t nFS(void) const
    {
        return minFSparticles()+realEmissions();
    }

    /// Number of degrees of freedom
    const size_t nDOF(void) const
    {
        return 2 + 3 * nFS() - 4;
    }

    /// Four-momenta, conventions are:
    /// - p[1] = parton from hadron 1
    /// - p[2] = parton from hadron 2
    /// - p[i>2] = particles in the final state
    FourVector& p(const size_t& i)
    {
        return _p[i-1];
    }

    /// Read-only version of Four-momenta
    const FourVector& p(const size_t& i) const
    {
        return _p[i-1];
    }

    /// Generate kinematics according to random variables
    virtual void generate(const double* const randoms);

    /// @}

protected:
    
    /// \name Data members
    /// @{
    
    Bjorken _x;             // < Bjorken x variables
    double _S;              // < Center of mass collider energy squared
    double _smin_S;         // < Minimum value of the ratio smin/S = x1*x2
    vector<FourVector> _p;  // < Momenta of particles
    xGenerator _Xgenerator;
    pGenerator _Pgenerator;

    /// @}

    /// \name Auxiliary functions
    /// @{
    
    ///@}
    
};

inline void KinematicVariables::generate(const double* const randoms)
{
    _x = _Xgenerator(_smin_S,randoms);
    _p = _Pgenerator(&randoms[2]);
    set(_p);
    return;
}


/**
 *
 * \class DeltaKinematics
 * \brief Collider kinematic variables for delta-like processes
 *
 * Manages kinematics for processes with one and only one final-state particle
 * This information is sufficient to completely determine the momenta
 *
 */

class DeltaKinematics : public KinematicVariables
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    DeltaKinematics() :
    KinematicVariables()
    {}

    /// Copy constructor
    DeltaKinematics(const DeltaKinematics& that) :
    KinematicVariables(that)
    {}

    /// Destructor
    ~DeltaKinematics()
    {}

    /// @}

    /// \name Input/output functions
    /// @{

    /// Minimum number of final state particles
    virtual const size_t minFSparticles(void) const
    {
        return 3;
    }

    /// Number of extra final state partons from real emission
    virtual const size_t realEmissions(void) const
    {
        return 0;
    }

    /// Generate kinematics according to random variables
    void generate(const double* const randoms);

    /// @}

protected:

    /// \name Auxiliary functions
    /// @{

    /// Generate Bjorken x's
    Bjorken generateX(const double* const randoms);
    /// Generate momenta
    vector<FourVector> generateP(const double* const randoms);

    ///@}

};

inline void DeltaKinematics::generate(const double* const randoms)
{
    _x = generateX(randoms);
    _p = generateP(NULL);
    set(_p);
    return;
}

#endif

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

    typedef pair<double, double> Bjorken;

    /// \name Data members
    /// @{

    /// Return the first Bjorken x
    const double& x1 = _x.first;
    /// Return the second Bjorken x
    const double& x2 = _x.second;

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    KinematicVariables() :
    KinematicInvariants()
    {}

    /// Copy constructor
    KinematicVariables(const KinematicVariables& that) :
    KinematicInvariants(that)
    {}
    
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
    void generate(const double* const randoms);

    /// @}

protected:
    
    /// \name Data members
    /// @{
    
    Bjorken _x;             // < Bjorken x variables
    double _S;              // < Center of mass collider energy squared
    double _smin_S;         // < Minimum value of the ratio smin/S = x1*x2
    vector<FourVector> _p;  // < Momenta of particles

    /// @}

    /// \name Auxiliary functions
    /// @{
    
    /// Generate Bjorken x's
    virtual Bjorken generateX(const double* const randoms) = 0;
    /// Generate momenta
    virtual vector<FourVector> generateP(const double* const randoms) = 0;
    
    ///@}
    
};

inline void KinematicVariables::generate(const double* const randoms)
{
    if (nFS() > 1) {
        _x = generateX(randoms);
        _p = generateP(&randoms[2]);
    } else {
        _x = generateX(randoms);
        _p = generateP(NULL);
    }
    set(_p);
    return;
}

#endif

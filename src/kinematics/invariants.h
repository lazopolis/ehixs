/**
 *
 * \file    invariants.h
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef KINEMATICS_INVARIANTS_H
#define KINEMATICS_INVARIANTS_H

#include "fourvector.h"
#include <vector>
using namespace std;

/**
 *
 * \class KinematicInvariants
 * \brief Container for kinematic invariants
 *
 */

class KinematicInvariants
{

public:

    /// \name Constructors
    /// @{

    /// Default constructor
    KinematicInvariants(const size_t n = 0);

    /// Copy constructor
    KinematicInvariants(const KinematicInvariants& that) :
    _n(that._n), _s(that._s), _q(that._q)
    {}

    /// Constructor from momentum set
    KinematicInvariants(const Momenta& p);

    /// Destructor
    ~KinematicInvariants()
    {} // vector should be able to destroy all of its elements
    
    /// @}
    
    /// \name Input functions
    /// @{

    /// Computes the kinematic invariants based on a set of momenta
    void set(const Momenta& p);

    /// @}
    
    /// \name Output functions
    /// @{

    /// Return the dimensionful kinematic invariant corresponding to a pair
    double s(const size_t i, const size_t j) const;

    /// Return the dimensionless kinematic invariant corresponding to a pair
    double q(const size_t i, const size_t j) const;

    /// Print the whole matrix of kinematic invariants
    friend ostream& operator<<(ostream& stream, const KinematicInvariants& kk);
    
    /// @}
    
private:

    /// \name Data members
    /// @{
    
    size_t _n;                      ///< Number of particles
    vector< vector<double> > _s;    ///< Dimensionful kinematic invariants matrix
    vector< vector<double> > _q;    ///< Dimensionless kinematic invariants matrix
    
    /// @}
    
    /// \name Auxiliary functions
    /// @{

    /// Compute the kinematic invariant associated with p1 and p2, i.e. (p1+p2)^2
    double s(const FourVector& p1, const FourVector& p2) const
    {
        return square(p1+p2);
    }

    /// Changes the size of the arrays
    void resize(const size_t n);

    /// @}
    
};

#endif

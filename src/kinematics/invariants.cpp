/**
 *
 * \file    invariants.h
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef INVARIANTS_H
#define INVARIANTS_H

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
    KinematicInvariants(const size_t n = 0) :
    _n(n), _s(n), _q(n)
    {
        for (size_t i = 0; i < _n; ++i)
        {
            _s[i] = vector<double>(_n-i,0.);
            _q[i] = vector<double>(_n-i,0.);
        }
        return;
    }

    /// Copy constructor
    KinematicInvariants(const KinematicInvariants& that) :
    _n(that._n), _s(that._s), _q(that._q)
    {}

    /// Constructor from momentum set
    KinematicInvariants(const Momenta& p) :
    _n(p.size()), _s(p.size()), _q(p.size())
    {
        const double s12 = s(p[1],p[2]);
        for (size_t i = 0; i < _n; ++i)
            for (size_t j = 0; j < _n-i; ++j)
            {
                _s[i].push_back(s(p[i+1],p[j+1]));
                _q[i].push_back(_s[i][j]/s12);
            }
        return;
    }

    /// Destructor
    ~KinematicInvariants()
    {} // vector should be able to destroy all of its elements
    
    /// @}
    
    /// \name Input functions
    /// @{

    /// Computes the kinematic invariants based on a set of momenta
    void set(const Momenta& p)
    {
        // Checks whether it is necessary to resize matrices
        if ( _n != p.size() ) resize(p.size());
        // Fills matrices with new values
        const double s12 = s(p[1],p[2]);
        for (size_t i = 0; i < _n; ++i)
            for (size_t j = 0; j < _n-i; ++j)
            {
                _s[i][j]=s(p[i+1],p[j+1]);
                _q[i][j]=_s[i][j]/s12;
            }
        return;
    }

    /// @}
    
    /// \name Output functions
    /// @{

    /// Return the dimensionful kinematic invariant corresponding to a pair
    double s(const size_t i, const size_t j) const
    {
        if ( i>0 && j>0 && i<=_n && j<=_n )
            return _s[min(i,j)-1][max(i,j)-1];
        cerr << "Trying to read out of the Kinematic Invariants matrix" << endl;
        exit(1);
    }
    
    /// Return the dimensionless kinematic invariant corresponding to a pair
    double q(const size_t i, const size_t j) const
    {
        if ( i>0 && j>0 && i<=_n && j<=_n )
            return _q[min(i,j)-1][max(i,j)-1];
        cerr << "Trying to read out of the Kinematic Invariants matrix" << endl;
        exit(1);
    }

    /// Print the whole matrix of kinematic invariants
    friend ostream& operator<<(ostream& stream, const KinematicInvariants& kk)
    {
        for (int i = 1; i <= kk._n; ++i)
            for (int j = 1; j <= kk._n; ++j)
                stream << "\n s" << i << j << " = " << kk.s(i,j) << ",";
        return stream << endl;
    }
    
    
    /// @}
    
private:

    /// \name Data members
    /// @{
    
    size_t _n;                      /// < Number of particles
    vector< vector<double> > _s;    /// < Dimensionful kinematic invariants matrix
    vector< vector<double> > _q;    /// < Dimensionless kinematic invariants matrix
    
    /// @}
    
    /// \name Auxiliary functions
    /// @{

    /// Compute the kinematic invariant associated with p1 and p2, i.e. (p1+p2)^2
    double s(const FourVector& p1, const FourVector& p2) const
    {
        return square(p1+p2);
    }

    /// Changes the size of the arrays
    void resize(const size_t n)
    {
        _n = n;
        _s.resize(_n);
        _q.resize(_n);
        for (size_t i = 0; i < _n; ++i)
        {
            _s[i].resize(_n-i,0.);
            _q[i].resize(_n-i,0.);
        }
        return;
    }

    /// @}
    
};

#endif

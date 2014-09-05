/**
 *
 * \file   kinematics.h
 * \author Simone Lionetti
 * \date   September 2014
 *
 */

#ifndef KINEMATICS_H
#define KINEMATICS_H

#include "fourmomentum.h"
#include <vector>
//#include <iostream>
//#include <iomanip>  // setprecison()
//#include <stdlib.h> // exit, EXIT_FAILURE
//#include "math.h"   // sqrt
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
    _n(n), _s(), _q()
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
    KinematicInvariants(const vector<FMomentum>& p) :
    _n(p.size()), _s(p.size()), _q(p.size())
    {
        const double s12 = s(p[1],p[2]);
        for (size_t i = 0; i < _n; ++i)
            for (size_t j = 0; j < _n-i; ++j)
            {
                _s[i].push_back(s(p[i],p[j]));
                _q[i].push_back(s(p[i],p[j])/s12);
            }
        return;
    }
    
    ~KinematicInvariants()
    {} // vector should be able to destroy all of its elements
    
    /// @}
    
    /// \name Input functions
    /// @{
    
    void set(const vector<FMomentum>& p)
    {
        if ( _n != 0 ) clear();
        _n = p.size();
        _s = vector< vector<double> >(_n);
        _q = vector< vector<double> >(_n);

        const double s12 = s(p[1],p[2]);
        for (size_t i = 0; i < _n; ++i)
            for (size_t j = 0; j < _n-i; ++j)
            {
                _s[i].push_back(s(p[i],p[j]));
                _q[i].push_back(s(p[i],p[j])/s12);
            }
        return;
    }

    /// @}
    
    /// \name Output functions
    /// @{
    
    double s(const size_t i, const size_t j) const
    {
        if ( i>0 && j>0 && i<=_n && j<=_n )
            return _s[min(i,j)-1][max(i,j)-1];
        cerr << "Trying to read out of the Kinematic Invariants matrix" << endl;
        exit(1);
    }
    
    double q(const size_t i, const size_t j) const
    {
        if ( i>0 && j>0 && i<=_n && j<=_n )
            return _q[min(i,j)-1][max(i,j)-1];
        cerr << "Trying to read out of the Kinematic Invariants matrix" << endl;
        exit(1);
    }
    
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
    double s(const FMomentum& p1, const FMomentum& p2)
    {
        return p1*p1+2.*p1*p2+p2*p2;
    }

    /// Clear the whole object
    void clear()
    {
        if (_n==0) return;
        for (size_t i = 0; i < _n; ++i)
        {
            _s.pop_back();
            _q.pop_back();
        }
        _n=0;
        return;
    }

    /// @}
    
};

#endif

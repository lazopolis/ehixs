/**
 *
 * \file    invariants.cpp
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "invariants.h"

/// \class KinematicInvariants

/// \name Constructors

/// Default constructor
KinematicInvariants::KinematicInvariants(const size_t n) :
_n(n), _s(n), _q(n)
{
    for (size_t i = 0; i < _n; ++i)
    {
        _s[i] = vector<double>(_n-i,0.);
        _q[i] = vector<double>(_n-i,0.);
    }
    return;
}

/// Constructor from momentum set
KinematicInvariants::KinematicInvariants(const Momenta& p) :
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

/// \name Input functions

/// Computes the kinematic invariants based on a set of momenta
void KinematicInvariants::set(const Momenta& p)
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

/// \name Output functions

/// Return the dimensionful kinematic invariant corresponding to a pair
double KinematicInvariants::s(const size_t i, const size_t j) const
{
    if ( i>0 && j>0 && i<=_n && j<=_n )
        return _s[min(i,j)-1][max(i,j)-1];
    cerr << "Trying to read out of the Kinematic Invariants matrix" << endl;
    exit(1);
}

/// Return the dimensionless kinematic invariant corresponding to a pair
double KinematicInvariants::q(const size_t i, const size_t j) const
{
    if ( i>0 && j>0 && i<=_n && j<=_n )
        return _q[min(i,j)-1][max(i,j)-1];
    cerr << "Trying to read out of the Kinematic Invariants matrix" << endl;
    exit(1);
}

/// Print the whole matrix of kinematic invariants
ostream& operator<<(ostream& stream, const KinematicInvariants& kk)
{
    for (int i = 1; i <= kk._n; ++i)
        for (int j = 1; j <= kk._n; ++j)
            stream << "\n s" << i << j << " = " << kk.s(i,j) << ",";
    return stream << endl;
}

/// \name Auxiliary functions

/// Changes the size of the arrays
void KinematicInvariants::resize(const size_t n)
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

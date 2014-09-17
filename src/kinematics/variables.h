/**
 *
 * \file    kinematicvariables.h
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef KINEMATICS_VARIABLES_H
#define KINEMATICS_VARIABLES_H

#include <stdlib.h>
#include "invariants.h"
#include "bjorken.h"
#include "parametrizations.h"
using namespace std;

/**
 *
 * \class KinematicVariables
 * \brief Manages generic collider kinematic variables
 *
 * This class contains four momenta, invariants and Bjorken xs for the current phase space point
 * along with the information needed to generate them from Vegas
 *
 */

template<class xGenerator, class pGenerator>
class KinematicVariables : public KinematicInvariants
{

public:

    /// \name Data members
    /// @{

    const double& x1 = _x.x1;   /// < Alias for the 1st Bjorken x
    const double& x2 = _x.x2;   /// < Alias for the 2nd Bjorken x

    /// Four-momenta, conventions are:
    /// - p[1] = parton from hadron 1
    /// - p[2] = parton from hadron 2
    /// - p[i>2] = particles in the final state
    const Momenta& p = _p;

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    KinematicVariables() :
    KinematicInvariants(), _xGen(_x, _jacobian), _pGen(_p, _jacobian, _x),
    _S(), _m(), _x(), _p(), _jacobian()
    {
        // Check consistency of generators passed by template
        if ( _xGen.Nran() + _pGen.Nran() != Ndof() )
        {
            cerr << "Generators are not compatible with the kinematic structure." << endl;
            exit(1);
        }
        // Prepare array of four-momenta of the correct length
        _p.resize(Ntot());
        return;
    }

    /// Copy constructor
    KinematicVariables(const KinematicVariables& that) :
    KinematicInvariants(that), _xGen(_x, _jacobian), _pGen(_p, _jacobian, _x),
    _S(that._S), _m(that._m), _x(that._x), _p(that._p), _jacobian(that._jacobian)
    {}

    /// Destructor
    ~KinematicVariables()
    {}
    
    /// @}

    /// \name Input/output functions
    /// @{

    /// Total number of final state particles
    const size_t Ntot(void) const
    {
        return _pGen.Nfs()+2;
    }

    /// Number of degrees of freedom
    const size_t Ndof(void) const
    {
        return 2 + 3 * _pGen.Nfs() - 4;
    }

    /// Generate kinematics according to random variables
    virtual void generate(const double* const randoms);

    /// Global jacobian
    const double& jacobian = _jacobian;

    /// Setting parameters
    virtual void setParameters(const double& S, const vector<double>& m)
    {
        cout << "KinematicVariables configured with S = " << S << endl;
        _S = S;
        _m = m;
        double sumOfMasses = 0.;
        for (vector<double>::const_iterator it = _m.begin(); it < _m.end(); ++it)
            sumOfMasses += (*it)*(*it);
        _xGen.setParameters(sumOfMasses/_S);
        _pGen.setParameters(_S,_m);
        return;
    }

    /// @}

protected:
    
    /// \name Data members
    /// @{
    
    xGenerator _xGen;   /// < Generator for the Bjorken x variables
    pGenerator _pGen;   /// < Generator for the set of four-momenta

    double _S;          /// < Center of mass collider energy squared
    vector<double> _m;  /// < Minimum value of the ratio smin/S = x1*x2
    Bjorken _x;         /// < Bjorken x variables
    Momenta _p;         /// < Momenta of particles
    double _jacobian;   /// < Jacobian

    /// @}

};

/// Generate kinematics according to random variables
template <class xGenerator, class pGenerator>
inline void KinematicVariables<xGenerator,pGenerator>::generate(const double* const randoms)
{
    _jacobian = 1.;
    _xGen(randoms);
    _pGen(&randoms[_xGen.Nran()]);
    set(_p);
    return;
}

#endif

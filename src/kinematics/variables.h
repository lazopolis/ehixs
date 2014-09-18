/**
 *
 * \file    variables.h
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
 * \class IKinematicVariables
 * \brief Interface to collider kinematic variables
 *
 */

class IKinematicVariables : public KinematicInvariants
{

public:

    /// \name Read-only data
    /// @{

    const double& x1 = _x.x1;   ///< Alias for the 1st Bjorken x
    const double& x2 = _x.x2;   ///< Alias for the 2nd Bjorken x

    /// Four-momenta, conventions are:
    /// - p[1] = parton from hadron 1
    /// - p[2] = parton from hadron 2
    /// - p[i>2] = particles in the final state
    const Momenta& p = _p;

    /// Global jacobian
    const double& jacobian = _jacobian;

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    IKinematicVariables(const double& S, const vector<double>& m) :
    KinematicInvariants(),
    _S(S), _m(m), _x(), _p(), _jacobian()
    {}

    /// Copy constructor
    IKinematicVariables(const IKinematicVariables& that) :
    KinematicInvariants(that), _S(that._S), _m(that._m), _x(that._x), _p(that._p), _jacobian(that._jacobian)
    {}

    /// Destructor
    virtual ~IKinematicVariables()
    {}

    /// @}

    /// \name Input/output functions
    /// @{

    /// Total number of final state particles
    virtual const size_t Ntot(void) const = 0;

    /// Number of degrees of freedom
    virtual const size_t Ndof(void) const = 0;

    /// Generate kinematics according to random variables
    virtual void generate(const double* const randoms) = 0;

    /// @}

protected:

    /// \name Data members
    /// @{

    double _S;          ///< Center of mass collider energy squared
    vector<double> _m;  ///< Minimum value of the ratio smin/S = x1*x2
    Bjorken _x;         ///< Bjorken x variables
    Momenta _p;         ///< Momenta of particles
    double _jacobian;   ///< Jacobian

    /// @}

};

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
class KinematicVariables : public IKinematicVariables
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    KinematicVariables(const double& S, const vector<double>& m, const size_t expectedDOF) :
    IKinematicVariables(S, m), _xGen(_x, _jacobian), _pGen(_p, _jacobian, _x)
    {
        // Check consistency of generators passed by template
        if ( _xGen.Nran() + _pGen.Nran() != Ndof() )
        {
            cerr << "[KinematicVariables] Error: generators are not compatible with the kinematic structure." << endl;
            exit(1);
        }
        // Check if expected number of degrees of freedom matches
        if ( expectedDOF != Ndof() )
        {
            cerr << "[KinematicVariables] Error: declared number of DOF does not match this kinematic." << endl;
            exit(1);
        }
        // Prepare array of four-momenta of the correct length
        _p.resize(Ntot());
        // Set parameters in generators
        double sumOfMasses2 = 0.;
        for (vector<double>::const_iterator it = _m.begin(); it < _m.end(); ++it)
            sumOfMasses2 += (*it)*(*it);
        _xGen.setParameters(sumOfMasses2/_S);
        _pGen.setParameters(_S,_m);
        return;
    }

    /// Copy constructor
    KinematicVariables(const KinematicVariables& that) :
    IKinematicVariables(that), _xGen(_x, _jacobian), _pGen(_p, _jacobian, _x)
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

    /// @}

protected:
    
    /// \name Data members
    /// @{
    
    xGenerator _xGen;   ///< Generator for the Bjorken x variables
    pGenerator _pGen;   ///< Generator for the set of four-momenta

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

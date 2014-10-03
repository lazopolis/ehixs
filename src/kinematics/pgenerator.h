/**
 *
 * \file    pgenerator.h
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef KINEMATICS_PGENERATOR_H
#define KINEMATICS_PGENERATOR_H

#include "lightcone.h"
#include <vector>
#include "bjorken.h"
using namespace std;

/**
 *
 * \class PGenerator
 * \brief Base class for generators of four-momenta according to some parametrization
 * \todo  Include generation of momenta in the center of mass frame, now lab only
 *
 */

class PGenerator
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    /// It is mandatory to pass the needed references where variables will be generated
    PGenerator(Momenta& ps, const Bjorken& xs) :
    _p(ps), _xs(xs), _E(), _m()
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Sets the parameters needed for generation
    virtual void setParameters(const double& S, const vector<double>& masses);

    /// Generates the first two momenta that are common for all processes,
    /// then passes the job on to generateFSMomenta, returns jacobian
    virtual double operator()(vector<double>& randoms) const;

    /// @}

    /// \name Pure virtual functions
    /// @{
    
    /// Sets the constants in child class
    virtual void computeConstants() = 0;

    /// Generates the momenta of particles in the final state, returns the jacobian
    virtual double generateFSMomenta(vector<double>& randoms) const = 0;

    /// @}

protected:

    /// \name Data members
    /// @{
    
    Momenta& _p;               ///< Reference to target momenta to be generated

    double _E;                 ///< COM energy of the incoming hadrons
    const Bjorken& _xs;        ///< Where to read Bjorken xs
    vector<double> _m;         ///< Masses (beware: counting from 0)

    const double& x1 = _xs.x1; ///< Alias for the 1st Bjorken x
    const double& x2 = _xs.x2; ///< Alias for the 2nd Bjorken x

    /// @}

};

#endif

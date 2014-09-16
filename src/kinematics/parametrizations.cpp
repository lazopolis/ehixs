/**
 *
 * \file   parametrizations.h
 * \author Simone Lionetti
 * \date   September 2014
 *
 */

#ifndef PARAMETRIZATIONS_H
#define PARAMETRIZATIONS_H

#include "fourvector.h"
#include <vector>
#include "bjorken.h"
using namespace std;

/**
 *
 * \namespace LightCone
 * \brief     Contains functions and definitions for light-cone coordinates
 * \todo      Complete with functions that return the plus, minus and perp components
 *
 */

namespace LightCone
{
    const FourVector n    = FourVector(1., 0., 0.,  1.);
    const FourVector nbar = FourVector(1., 0., 0., -1.);

    inline FourVector eperp(const double& phi)
    {
        return FourVector(0.,cos(phi),sin(phi),0.);
    }
};

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
    PGenerator(Momenta& ps, double& jacobian, const Bjorken& xs) :
    _p(ps), _jacobian(jacobian), _xs(xs), _E(), _m()
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Sets the parameters needed for generation
    virtual void setParameters(const double& S, const vector<double>& masses)
    {
        _E = sqrt(S)/2.;
        _m = masses;
        computeConstants();
        return;
    }

    /// Generates the first two momenta that are common for all processes,
    /// then passes the job on to generateFSMomenta
    virtual void operator()(const double* const randoms) const
    {
        _p[1] = x1 * _E * LightCone::n;
        _p[2] = x2 * _E * LightCone::nbar;
        generateFSMomenta(randoms);
        return;
    };

    /// @}

    /// \name Pure virtual functions
    /// @{
    
    /// Sets the constants in child class
    virtual void computeConstants() = 0;

    /// Generates the momenta of particles in the final state
    virtual void generateFSMomenta(const double* const randoms) const = 0;

    /// Returns the number of final-state particles
    virtual size_t Nfs() const = 0;

    /// Returns the number of random numbers needed for correct generation
    virtual size_t Nran() const = 0;

    /// @}

protected:

    /// \name Data members
    /// @{
    
    Momenta& _p;               /// < Reference to target momenta to be generated
    double& _jacobian;         /// < Reference to target jacobian

    double _E;                 /// < COM energy of the incoming hadrons
    const Bjorken& _xs;        /// < Where to read Bjorken xs
    vector<double> _m;         /// < Masses (beware: counting from 0)

    const double& x1 = _xs.x1; /// < Alias for the 1st Bjorken x
    const double& x2 = _xs.x2; /// < Alias for the 2nd Bjorken x

    /// @}

};

/**
 *
 * \class DeltaPG
 * \brief Generates partonic momenta for 2->1 (i.e. delta-like) processes
 *
 */

class DeltaPG : public PGenerator
{
public:

    /// \name Constructors and destructor
    /// @{

    /// Default Constructor
    /// It is mandatory to pass the needed references where variables will be generated
    DeltaPG(Momenta& ps, double& jacobian, const Bjorken& xs) :
    PGenerator(ps, jacobian, xs)
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Implementation of mandatory computeConstants method; nothing to do
    void computeConstants()
    {}

    /// Set the only final-state momentum via conservation
    void generateFSMomenta(const double* const randoms) const
    {
        _p[3] = _p[1] + _p[2];
        return;
    }

    /// Returns the number of final-state particles
    size_t Nfs() const {return 1;}

    /// Returns the number of random numbers needed for correct generation
    size_t Nran() const {return 0;}

    /// @}

};

/**
 *
 * \class ZlambdaPG
 * \brief Generates partonic momenta for 2->1+(1 real emission) processes
 *
 * The momentum of the extra parton is parametrized with the two variables z and lambda.
 *
 * Namely we set:
 * p4 = (1-z) [lambdabar p1 + lambda p2 + sqrt(lambda*lambdabar) s12 eperp]
 * (this is covariant and holds in any reference frame if p1, p2 and eperp are transformed)
 *
 * The relevant limits are:
 * - particle 4 becoming soft as z->1;
 * - particle 4 becoming 1-collinear as lambda->0;
 * - particle 4 becoming 2-collinear as lambda->1.
 *
 */

class ZlambdaPG : public PGenerator
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default Constructor
    /// It is mandatory to pass the needed references where variables will be generated
    ZlambdaPG(Momenta& ps, double& jacobian, const Bjorken& xs) :
    PGenerator(ps, jacobian, xs)
    {}

    /// @}

    /// \name Member functions
    /// @{
    
    /// Sets the intermediate variable _tau for computational speed gain
    void computeConstants()
    {
        // Check number of masses passed to PGenerator::setParameters
        if (_m.size() != 1)
        {
            cerr << "[ZlambdaPG] Error: a wrong number of masses arrived at the momenta generator.\n";
            exit(1);
        }
        _tau = _m[0]*_m[0]/(4.*_E*_E);
        return;
    }

    /// Implements the parametrization
    void generateFSMomenta(const double* const randoms) const
    {
        const double phi = 2.*consts::Pi*randoms[0];
        const double lambda = randoms[1];
        const double z = _tau / (x1*x2);
        const double sllbar = sqrt(lambda*(1.-lambda))*x1*x2*_E;
        _p[4] = (1.-z)*(
                        (1.-lambda) * _p[1] +
                        lambda * _p[2] +
                        sllbar * LightCone::eperp(phi)
                       );
        _p[3] = _p[1] + _p[2] - _p[4];
        // nothing to be done for jacobian = 1
        return;
    }

    /// Returns the number of final-state particles
    size_t Nfs() const {return 2;}

    /// Returns the number of random numbers needed for correct generation
    size_t Nran() const {return 2;}

    /// @}

private:

    /// \name Data members
    /// @{

    double _tau; /// < Alias for M^2/S

    /// @}

};

#endif

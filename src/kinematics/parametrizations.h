/**
 *
 * \file    parametrizations.h
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef KINEMATICS_PARAMETRIZATIONS_H
#define KINEMATICS_PARAMETRIZATIONS_H

#include "pgenerator.h"
using namespace std;

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
    DeltaPG(Momenta& ps, const Bjorken& xs) :
    PGenerator(ps, xs)
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Implementation of mandatory computeConstants method; nothing to do
    void computeConstants()
    {}

    /// Set the only final-state momentum via conservation
    double generateFSMomenta(vector<double>& randoms) const
    {
        _p[3] = _p[1] + _p[2];
        return 1.;
    }

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
    ZlambdaPG(Momenta& ps, const Bjorken& xs) :
    PGenerator(ps, xs)
    {}

    /// @}

    /// \name Member functions
    /// @{
    
    /// Sets the intermediate variable _tau for computational speed gain
    void computeConstants();

    /// Implements the parametrization
    /// The last element of the "randoms" vector is interpreted as phi
    /// The next-to-last element of the "randoms" vector is interpreted as lambda
    double generateFSMomenta(vector<double>& randoms) const;

    /// @}

private:

    /// \name Data members
    /// @{

    double _tau; ///< Alias for M^2/S

    /// @}

};

#endif

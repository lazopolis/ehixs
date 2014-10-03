/**
 *
 * \file    xgenerator.h
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef KINEMATICS_XGENERATOR_H
#define KINEMATICS_XGENERATOR_H

#include "bjorken.h"
#include "fourvector.h"
#include <vector>
using namespace std;

/**
 *
 * \class XGenerator
 * \brief Base class for generators of Bjorken momentum fractions
 *
 */

class XGenerator
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    /// It is mandatory to pass the needed references where variables will be generated
    XGenerator(Bjorken& x) :
    _x(x), _x1x2min(0.)
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Sets the parameter needed for generation, i.e. the minimum product of x1 and x2
    /// \remarks This might need to be moved to child classes
    virtual void setParameters(const double& x1x2min)
    {
        _x1x2min = x1x2min;
        return;
    }

    /// @}

    /// \name Pure virtual functions
    /// @{

    /// Generates the Bjorken variables into the target according to some distribution
    /// Children should eliminate used randoms from vector
    virtual double operator()(vector<double>& randoms) = 0;

    /// @}

protected:

    /// \name Data members
    /// @{

    Bjorken& _x;       ///< Reference to target Bjorken variables to be generated

    double _x1x2min;   ///< Auxiliary variable that specifies the minimum value of the product x1*x2

    /// @}

};

/**
 *
 * \class TwoXGenerator
 * \brief Generator of Bjorken momentum fractions x1 and x2, without rejection
 *
 * This x-generator produces two valid values of x1 and x2 according to a rather dumb distribution.
 *
 */

class TwoXGenerator : public XGenerator
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    /// It is mandatory to pass the needed references where variables will be generated
    TwoXGenerator(Bjorken& x) :
    XGenerator(x)
    {}

    /// @}

    /// \name Member functions
    /// @{
    
    /// Generates the product x1*x2 in the range [_x1x2min, 1] and x1 in [x1*x2, 1]
    /// Eliminates used randoms from vector
    double operator()(vector<double>& randoms);

    /// @}

};

/**
 *
 * \class FlatXGenerator
 * \brief Generator of Bjorken momentum fractions x1 and x2, flat and with rejection
 *
 * This x-generator produces two valid values of x1 and x2 according to the flat distribution.
 * It is inefficient (not all generated values are used) but very simple (good for tests).
 *
 */

class FlatXGenerator : public XGenerator
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    /// It is mandatory to pass the needed references where variables will be generated
    FlatXGenerator(Bjorken& x) :
    XGenerator(x)
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Generates x1 and x2 between 0 and 1, returning 0. jacobian if their product is too small
    /// Eliminates used randoms from vector
    double operator()(vector<double>& randoms);
    
    /// @}
    
};

/**
 *
 * \class OneXGenerator
 * \brief Generator of Bjorken momentum fractions x1 and x2 for overconstrained event
 *
 * This x-generator produces x1 and x2 for those events where the product x1*x2 is fixed by a delta function.
 *
 */

class OneXGenerator : public XGenerator
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    /// It is mandatory to pass the needed references where variables will be generated
    OneXGenerator(Bjorken& x) :
    XGenerator(x)
    {}

    /// @}

    /// \name Member functions
    /// @{
    
    /// Sets the product x1*x2 to _x1x2min (using the delta), then generates x1 in the range [x1*x2, 1]
    /// Eliminates used randoms from vector
    double operator()(vector<double>& randoms);

    /// @}

};

#endif

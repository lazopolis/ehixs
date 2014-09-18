/**
 *
 * \file    bjorken.h
 * \ingroup kinematics
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef KINEMATICS_BJORKEN_H
#define KINEMATICS_BJORKEN_H

#include "fourvector.h"
#include <vector>
using namespace std;

/**
 *
 * \struct Bjorken
 * \brief  Container for Bjorken x data
 * \todo   Introduce "bool isGood" to be able to reject events
 *
 */

struct Bjorken
{

    /// \name Data members
    /// @{

    double x1; ///< Bjorken x of the 1st (left) parton
    double x2; ///< Bjorken x of the 2nd (right) parton

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    Bjorken() :
    x1(), x2()
    {}

    /// Constructor with data
    Bjorken(const double& x1in, const double& x2in) :
    x1(x1in), x2(x2in)
    {}

    /// Copy Constructor
    Bjorken(const Bjorken& that) :
    x1(that.x1), x2(that.x2)
    {}

    /// Destructor
    ~Bjorken()
    {}

    /// @}

};

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
    XGenerator(Bjorken& x, double& jacobian) :
    _x(x), _jacobian(jacobian), _x1x2min(0.)
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
    virtual void operator()(const double* const randoms) = 0;

    /// Returns the number of used random [0,1] variables
    virtual size_t Nran() const = 0;

    /// @}

protected:

    /// \name Data members
    /// @{

    Bjorken& _x;       ///< Reference to target Bjorken variables to be generated
    double& _jacobian; ///< Reference to target jacobian

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
    TwoXGenerator(Bjorken& x, double& jacobian) :
    XGenerator(x,jacobian)
    {}

    /// @}

    /// \name Member functions
    /// @{
    
    /// Generates the product x1*x2 in the range [_x1x2min, 1] and x1 in [x1*x2, 1]
    void operator()(const double* const randoms);

    /// Returns the number of used random [0,1] variables
    size_t Nran() const {return 2;}

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
    OneXGenerator(Bjorken& x, double& jacobian) :
    XGenerator(x,jacobian)
    {}

    /// @}

    /// \name Member functions
    /// @{
    
    /// Sets the product x1*x2 to _x1x2min (using the delta), then generates x1 in the range [x1*x2, 1]
    void operator()(const double* const randoms);

    /// Returns the number of used random [0,1] variables
    size_t Nran() const {return 1;}

    /// @}

};

#endif

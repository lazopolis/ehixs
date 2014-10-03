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

/**
 *
 * \struct Bjorken
 * \brief  Container for Bjorken x data
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

    /// Copy constructor
    Bjorken(const Bjorken& that) :
    x1(that.x1), x2(that.x2)
    {}

    /// Move constructor
    Bjorken(Bjorken&& that) :
    x1(that.x1), x2(that.x2)
    {}

    /// Destructor
    ~Bjorken()
    {}

    /// @}

};

#endif

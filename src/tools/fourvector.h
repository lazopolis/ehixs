/**
 *
 * \file   fourvector.h
 * \author Achilleas, Romain, Simone
 * \date   September 2014
 * Created by Achilleas in Apr 2009
 * Revised by Romain in Jan 2012
 * Revised by Simone in Sep 2014
 *
 */

#ifndef FOURVECTOR_H
#define FOURVECTOR_H

#include <array>        // array<double,4>
#include <cfloat>       // DBL_MIN
#include <iomanip>      // setprecison()
#include <iostream>     // cout
#include <stdlib.h>     // exit, EXIT_FAILURE
#include <cmath>        // sqrt
#include "constants.h"  // Pi
using namespace std;


/**
 *
 * \class FourVector
 * \brief Object that represents a four-vector
 * \todo  Write tests
 *
 */

class FourVector : public array<double,4>
{

public:
    
    /**
     * \enum  Axis
     * \brief Shorthand for calling axes
     */
    enum axis
    {
        x = 1,
        y = 2,
        z = 3
    };

    /// Cycles between axes
    /// \todo Rethink this nasty implementation
    friend axis operator+(const axis& a, const axis& b){
        if (a+b<=3) return static_cast<axis>(static_cast<size_t>(a)+static_cast<size_t>(b));
        else return static_cast<axis>(static_cast<size_t>(static_cast<int>(a)+static_cast<int>(b)-3));
    }

    /// \name Constructors
    /// @{
    
    /// Default constructor
    FourVector() :
    array<double, 4>()
    {}

    /// Constructor with values
    FourVector(const double& p0, const double& p1, const double& p2, const double& p3) :
    array<double, 4>({p0,p1,p2,p3})
    {}

    /// Copy constructor
    FourVector(const FourVector& that) :
    array<double, 4>()
    {}

    /// Destructor
    ~FourVector()
    {}

    /// @}
    
    /// \name Input/output functions
    /// @{

    /// Return the length of the three-vector
    double abs(void) const;

    /// Return the azimuthal angle of the four-vector
    double phi(const axis a = axis::z) const;

    /// Return the rapidity of the four-vector wrt the axis a
    double Y(const axis a = axis::z) const
    {
        return 0.5*log( (operator[](0)+operator[](a)) / (operator[](0)-operator[](a)) );
    }

    /// Return the modulus of the rapidity of the four-vector wrt the axis a
    double absY(const axis a = axis::z) const
    {
        return std::abs(Y(a));
    }

    /// Return the pseudorapidity of the four-vector wrt the axis a
    double eta(const axis a = axis::z) const;

    /// Compute the JADE distance of two FourVectors
    double yJADE(const FourVector& p2) const;

    /// Prints the fourmomentum to an output stream
    friend ostream& operator<<(ostream& stream, const FourVector& x);

    /// @}
    
    /// \name Operations
    /// @{
    
    /// Expicitly set the momentum by component
    FourVector& operator=(const initializer_list<double>& that);

    /// Multiply this momentum for a number
    FourVector operator*(const double& that) const
    {
        return FourVector(that*operator[](0),that*operator[](1),that*operator[](2),that*operator[](3));
    }
    friend FourVector operator*(const double a, const FourVector& b)
    {
        return FourVector(a*b[0],a*b[1],a*b[2],a*b[3]);
    }

    /// Divide this momentum by a number
    FourVector operator/(const double& that) const;

    /// Multiply this momentum for a number
    FourVector& operator*=(const double& that);

    /// Divide this momentum by a number
    FourVector& operator/=(const double& that);

    /// Negative of this momentum
    FourVector operator-(void) const
    {
        return (-1.)*(*this);
    }

    /// Add this momentum to another one
    FourVector operator+(const FourVector& that) const
    {
        return FourVector(operator[](0)+that[0],operator[](1)+that[1],operator[](2)+that[2],operator[](3)+that[3]);
    }

    /// Subtract a momentum from this one
    FourVector operator-(const FourVector& that) const
    {
        return (*this)+(-that);
    }

    /// Add another momentum to this one
    const FourVector& operator+=(const FourVector& that);

    /// Subtract another momentum from this one
    const FourVector& operator-=(const FourVector& that);

    /// Generic boost with velocity (bx,by,bz)
    FourVector& boost(const double& bx,const double& by,const double& bz);

    /// Boost along a specific axis a with velocity b
    FourVector& boost(const double& b, const axis a = axis::z);

    /// Boost along a specific axis a with rapidity eta
    FourVector& rapBoost(const double& eta, const axis a = axis::z);

    /// Rotate around the axis a by an angle theta
    FourVector& rotate(const double& theta, const axis a = axis::z);

    /// Scalar product with another momentum
    double operator*(const FourVector& q) const
    {
        return operator[](0)*q[0]-operator[](1)*q[1]-operator[](2)*q[2]-operator[](3)*q[3];
    }

    /// Square of a momentum
    friend double square(const FourVector& q)
    {
        return q*q;
    }

    /// @}

private:

    /// \name Auxiliary functions
    /// @{

    /// Computes the Lorentz gamma with sanity checks
    static double gamma(const double b2);

    /// @}

};

#endif

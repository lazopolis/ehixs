/**
 *
 * \file    fourvector.h
 * \ingroup kinematics
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \author  Romain Mueller
 * \date    September 2014
 *
 */

#ifndef KINEMATICS_FOURVECTOR_H
#define KINEMATICS_FOURVECTOR_H

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
 * \enum  Axis
 * \brief Shorthand for calling axes
 * \warning Seems to be readable globally: needs fixing!!!
 *
 */

enum Axis : unsigned short
{
    xaxis = 1,
    yaxis = 2,
    zaxis = 3
};

/// Cycles between axes
Axis operator+(const Axis& a, const Axis& b);

/**
 *
 * \class FourVector
 * \brief Object that represents a four-vector
 * \todo  Finish tests
 *
 */

class FourVector : public array<double,4>
{

public:
    
    /// \name Constructors and destructor
    /// @{
    
    /// Default constructor
    FourVector() :
    array<double, 4>()
    {}

    /// Constructor with values
    FourVector(const double& p0, const double& p1, const double& p2, const double& p3) :
    array<double, 4>({p0,p1,p2,p3})
    {}

    /// Constructor from ordinary array
    FourVector(const double p[4]) :
    array<double, 4>({p[0],p[1],p[2],p[3]})
    {}

    /// Constructor from STL array
    FourVector(const array<double,4> p) :
    array<double, 4>(p)
    {}

    /// Copy constructor
    FourVector(const FourVector& that) :
    array<double, 4>(that)
    {}

    /// Move constructor
    FourVector(FourVector&& that) :
    array<double, 4>(that)
    {}

    /// Destructor
    ~FourVector()
    {}

    /// @}
    
    /// \name Input/output functions
    /// @{

    /// Assignment operator
    FourVector& operator=(const FourVector& that)
    {
        array<double,4>::operator=(that);
        return (*this);
    }

    /// Move assignment operator
    FourVector& operator=(FourVector&& that)
    {
        array<double,4>::operator=(that);
        return (*this);
    }
    
    /// Expicitly set the momentum by component
    FourVector& operator=(const initializer_list<double>& that);

    /// Return the length of the three-vector
    double abs(void) const;

    /// Return the azimuthal angle of the four-vector
    double phi(const Axis a = Axis::zaxis) const;

    /// Return the rapidity of the four-vector wrt the Axis a
    double Y(const Axis a = Axis::zaxis) const
    {
        return 0.5*log( (operator[](0)+operator[](a)) / (operator[](0)-operator[](a)) );
    }

    /// Return the modulus of the rapidity of the four-vector wrt the Axis a
    double absY(const Axis a = Axis::zaxis) const
    {
        return std::fabs(Y(a));
    }

    /// Return the pseudorapidity of the four-vector wrt the Axis a
    double eta(const Axis a = Axis::zaxis) const;

    /// Return the transverse component of the four-vector wrt the Axis a
    double T(const Axis a = Axis::zaxis) const
    {
        return sqrt(pow(operator[](a+Axis::xaxis),2.)+pow(operator[](a+Axis::yaxis),2.));
    }

    /// Compute the JADE distance of two FourVectors
    /// Warning: yJADE was not tested yet
    double yJADE(const FourVector& p2) const;

    /// Prints the fourmomentum to an output stream
    friend ostream& operator<<(ostream& stream, const FourVector& x);

    /// @}
    
    /// \name Operations
    /// @{
    
    /// Multiply this momentum for a number
    FourVector operator*(const double& that) const
    {
        return FourVector(that*operator[](0),that*operator[](1),that*operator[](2),that*operator[](3));
    }

    /// Multiply the vector v for a number k
    friend FourVector operator*(const double k, const FourVector& v)
    {
        return FourVector(k*v[0],k*v[1],k*v[2],k*v[3]);
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
    /// beware of the sign ambiguity in the velocity
    FourVector& boost(const double& bx,const double& by,const double& bz);

    /// Boost along a specific Axis a with velocity b
    /// beware of the sign ambiguity in the velocity
    FourVector& boost(const double& b, const Axis a = Axis::zaxis);

    /// Boost along a specific Axis a with rapidity eta
    FourVector& rapBoost(const double& eta, const Axis a = Axis::zaxis);

    /// Rotate around the Axis a by an angle theta
    FourVector& rotate(const double& theta, const Axis a = Axis::zaxis);

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

/**
 *
 * \class Momenta
 * \brief Container for the set of momenta in an event
 *
 * With respect to a typedef, this class has the advantage that it overloads
 * vector<FourVector>'s operator[] so that the numbering of momenta is the
 * usual one starting from 1.
 *
 */

class Momenta : public vector<FourVector>
{

public:

    Momenta(const size_t n = 0)
    : vector<FourVector>(n, FourVector())
    {}

    const FourVector& operator[](const size_t& i) const
    {
        return vector<FourVector>::operator[](i-1);
    }

    FourVector& operator[](const size_t& i)
    {
        return vector<FourVector>::operator[](i-1);
    }
    
};

#endif

/**
 *
 * \file    fourvector.cpp
 * \ingroup kinematics
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \author  Romain Mueller
 * \date    September 2014
 *
 */

#include "fourvector.h"

/// Cycles between axes
Axis operator+(const Axis& a, const Axis& b)
{
    const int foo = (static_cast<int>(a)+static_cast<int>(b))%3;
    if ( foo == 0 ) return Axis::zaxis;
    else return static_cast<Axis>(foo);
}

/// \class FourVector

/// \name Input/output functions

/// Expicitly set the momentum by component
FourVector& FourVector::operator=(const initializer_list<double>& that)
{
    if (that.size()!=size())
    {
        cerr << "Tried to assign more than 4 components to a FourVector object." << endl;
        throw;
    }
    size_t i = 0;
    for (initializer_list<double>::iterator it = that.begin(); it<that.end(); ++it)
        (*this)[i++] = *it;
    return *this;
}

/// Return the length of the three-vector
double FourVector::abs(void) const
{
    return sqrt(
                operator[](1)*operator[](1)+
                operator[](2)*operator[](2)+
                operator[](3)*operator[](3)
                );
}

/// Return the azimuthal angle of the four-vector
double FourVector::phi(const Axis a) const
{
    double res = 0.;
    const Axis x = a+Axis::xaxis;
    const Axis y = a+Axis::yaxis;
    // handle special values
    if(operator[](x)==0.)
    {
        if (operator[](y)==0.)
        {
            std::cout << "\nError in FourVector::phi() : both x and y are 0.0, so phi is undefined.\n";
            exit(1);
        } else {
            if (operator[](y)<0) return 3.*consts::Pi/2.;
            else return consts::Pi/2.;
        }
    }
    else
    {
        res = atan(operator[](y)/operator[](x));
        // res is in the interval [-pi/2,pi/2]
        if (operator[](x)<0) res += consts::Pi;
        else //: x>0
        {
            if (operator[](y)<0) res += 2.*consts::Pi;
        }
    }
    return res;
}

/// Return the pseudorapidity of the four-vector wrt the Axis a
double FourVector::eta(const Axis a) const
{
    const double modulus = abs();
    return 0.5*log( (modulus+operator[](a)) / (modulus-operator[](a)) );
}

/// Compute the JADE distance of two FourVectors
double FourVector::yJADE(const FourVector& p2) const
{
    // norms
    const double norm1 = abs();
    const double norm2 = p2.abs();
    const double EE = operator[](0)*p2[0];
    // special value handling
    if (norm1 == 0. || norm2 == 0.) return 0.;
    // JADE
    return EE *(
                1 - (EE-operator*(p2))/(norm1*norm2)
                )/120./120./2.;
}

/// Prints the fourmomentum to an output stream
ostream& operator<<(ostream& stream, const FourVector& x)
{
    return stream
    <<setprecision(8)
    <<"("<<x[0]<<","<<x[1]<<","<<x[2]<<","<<x[3]<<")";
}

/// \name Operations

/// Divide this momentum by a number
FourVector FourVector::operator/(const double& that) const
{
    const double div = 1. / that;
    return FourVector(div*operator[](0),div*operator[](1),div*operator[](2),div*operator[](3));
}

/// Multiply this momentum for a number
FourVector& FourVector::operator*=(const double& that)
{
    for (FourVector::iterator it = begin(); it<end(); ++it)
        (*it) *= that;
    return *this;
}

/// Divide this momentum by a number
FourVector& FourVector::operator/=(const double& that)
{
    for (FourVector::iterator it = begin(); it<end(); ++it)
        (*it) /= that;
    return *this;
}

/// Add another momentum to this one
const FourVector& FourVector::operator+=(const FourVector& that)
{
    for (size_t i = 0; i<4; ++i)
        operator[](i)+=that[i];
    return *this;
}

/// Subtract another momentum from this one
const FourVector& FourVector::operator-=(const FourVector& that)
{
    for (size_t i = 0; i<4; ++i)
        operator[](i)-=that[i];
    return *this;
}

/// Generic boost with velocity (bx,by,bz)
FourVector& FourVector::boost(const double& bx,const double& by,const double& bz)
{
    // Compute beta^2 and gamma
    const double bsq = bx*bx+by*by+bz*bz;
    if (bsq == 0.) return *this;
    const double g = gamma(bsq);
    // Compute matrix elements
    const double bxx = 1.+(g-1.)*bx*bx/bsq;
    const double byy = 1.+(g-1.)*by*by/bsq;
    const double bzz = 1.+(g-1.)*bz*bz/bsq;
    const double bxy = (g-1.)*bx*by/bsq;
    const double byz = (g-1.)*by*bz/bsq;
    const double bzx = (g-1.)*bz*bx/bsq;
    // Set this vector to its boosted value and return its reference
    return operator=({
        g*operator[](0)    + g*bx*operator[](1) + g*by*operator[](2) + g*bz*operator[](3),
        bx*g*operator[](0) +  bxx*operator[](1) +  bxy*operator[](2) +  bzx*operator[](3),
        by*g*operator[](0) +  bxy*operator[](1) +  byy*operator[](2) +  byz*operator[](3),
        bz*g*operator[](0) +  bzx*operator[](1) +  byz*operator[](2) +  bzz*operator[](3)
    });
}

/// Boost along a specific Axis a with velocity b
FourVector& FourVector::boost(const double& b, const Axis a)
{
    if (b == 0) return *this;
    const double g = gamma(b*b);
    const double E =   g*operator[](0) + b*g*operator[](a);
    const double p = b*g*operator[](0) +   g*operator[](a);
    operator[](0) = E;
    operator[](a) = p;
    return *this;
}

/// Boost along a specific Axis a with rapidity eta
FourVector& FourVector::rapBoost(const double& eta, const Axis a)
{
    const double Sh = sinh(eta);
    const double Ch = cosh(eta);
    const double E = Ch*operator[](0) + Sh*operator[](a);
    const double p = Sh*operator[](0) + Ch*operator[](a);
    operator[](0) = E;
    operator[](a) = p;
    return *this;
}

/// Rotate around the Axis a by an angle theta
FourVector& FourVector::rotate(const double& theta, const Axis a)
{
    // Compute sine and cosine
    const double s = sin(theta);
    const double c = cos(theta);
    // Rotate
    const double pi = c*operator[](a+Axis::xaxis) - s*operator[](a+Axis::yaxis);
    const double pj = s*operator[](a+Axis::xaxis) + c*operator[](a+Axis::yaxis);
    // Set
    operator[](a+Axis::xaxis) = pi;
    operator[](a+Axis::yaxis) = pj;
    return *this;
}

/// \name Auxiliary functions

/// Computes the Lorentz gamma with sanity checks
double FourVector::gamma(const double b2)
{
    if (1.-b2<0.)
    {
        cerr << "Error in velocity: greater than speed of light." << endl;
        exit(1);
    }
    const double _1_gamma = sqrt(1.-b2);
    if (_1_gamma <= DBL_MIN)
    {
        cerr << "Error in velocity: too large for numerical precision." << endl;
        exit(1);
    }
    return 1./_1_gamma;
}

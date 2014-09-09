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

#include <array>
#include <cfloat>   // DBL_MIN
#include <iomanip>  // setprecison()
#include <stdlib.h> // exit, EXIT_FAILURE
#include <math.h>   // sqrt
using namespace std;


/**
 *
 * \class FourVector
 * \brief Object that represents a four-vector
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
    double abs(void) const
    {
        return sqrt(
                    operator[](0)*operator[](0)+
                    operator[](1)*operator[](1)+
                    operator[](2)*operator[](2)+
                    operator[](3)*operator[](3)
                    );
    }

    /// Return the azimuthal angle of the four-vector
    double phi(const axis a = axis::z) const
    {
        double res = 0.;
        const axis x = a+axis::x;
        const axis y = a+axis::y;
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
    double eta(const axis a = axis::z) const
    {
        const double modulus = abs();
        return 0.5*log( (modulus+operator[](a)) / (modulus-operator[](a)) );
    }

    /// Compute the JADE distance of two FourVectors
    double yJADE(const FourVector& p2) const
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
    friend ostream& operator<<(ostream& stream, const FourVector& x)
    {
        return stream
        <<setprecision(8)
        <<"("<<x[0]<<","<<x[1]<<","<<x[2]<<","<<x[3]<<")";
    }
    
    /// @}
    
    /// \name Operations
    /// @{
    
    /// Expicitly set the momentum by component
    FourVector& operator=(const initializer_list<double>& that)
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
    FourVector operator/(const double& that) const
    {
        const double div = 1. / that;
        return FourVector(div*operator[](0),div*operator[](1),div*operator[](2),div*operator[](3));
    }
    
    /// Multiply this momentum for a number
    FourVector& operator*=(const double& that)
    {
        for (FourVector::iterator it = begin(); it<end(); ++it)
            (*it) *= that;
        return *this;
    }
    
    /// Divide this momentum by a number
    FourVector& operator/=(const double& that)
    {
        for (FourVector::iterator it = begin(); it<end(); ++it)
            (*it) /= that;
        return *this;
    }
    
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
    const FourVector& operator+=(const FourVector& that)
    {
        for (size_t i = 0; i<4; ++i)
            operator[](i)+=that[i];
        return *this;
    }
    
    /// Subtract another momentum from this one
    const FourVector& operator-=(const FourVector& that)
    {
        for (size_t i = 0; i<4; ++i)
            operator[](i)-=that[i];
        return *this;
    }

    /// Generic boost with velocity (bx,by,bz)
    FourVector& boost(const double& bx,const double& by,const double& bz)
    {
        // Compute beta^2 and gamma
        const double bsq = bx*bx+by*by+bz*bz;
        const double g = gamma(bsq);
        // Compute matrix elements
        const double bxx = 1+(g-1)*bx*bx/bsq;
        const double byy = 1+(g-1)*by*by/bsq;
        const double bzz = 1+(g-1)*bz*bz/bsq;
        const double bxy = (g-1)*bx*by/bsq;
        const double byz = (g-1)*by*bz/bsq;
        const double bzx = (g-1)*bz*bx/bsq;
        // Set this vector to its boosted value and return its reference
        return operator=({
                g*operator[](0) - g*bx*operator[](1) - g*by*operator[](2) - g*bz*operator[](3),
            -bx*g*operator[](0) +  bxx*operator[](1) +  bxy*operator[](2) +  bzx*operator[](3),
            -by*g*operator[](0) +  bxy*operator[](1) +  byy*operator[](2) +  byz*operator[](3),
            -bz*g*operator[](0) +  bzx*operator[](1) +  byz*operator[](2) +  bzz*operator[](3)
        });
    }

    /// Boost along a specific axis a with velocity b
    FourVector& boost(const double& b, const axis a = axis::z)
    {
        const double g = gamma(b*b);
        const double E =     g*operator[](0) - b*g*operator[](a);
        const double p = - b*g*operator[](0) +   g*operator[](a);
        operator[](0) = E;
        operator[](a) = p;
        return *this;
    }

    /// Boost along a specific axis a with rapidity eta
    FourVector& rapBoost(const double& eta, const axis a = axis::z)
    {
        const double Sh = sinh(eta);
        const double Ch = cosh(eta);
        const double E = Ch*operator[](0) + Sh*operator[](a);
        const double p = Sh*operator[](0) + Ch*operator[](a);
        operator[](0) = E;
        operator[](a) = p;
        return *this;
    }

    /// Rotate around the axis a by an angle theta
    FourVector& rotate(const double& theta, const axis a = axis::z)
    {
        // Compute sine and cosine
        const double s = sin(theta);
        const double c = cos(theta);
        // Rotate
        const double pi =  c*operator[](a+axis::x) + s*operator[](a+axis::y);
        const double pj = -s*operator[](a+axis::x) + c*operator[](a+axis::y);
        // Set
        operator[](a+axis::x) = pi;
        operator[](a+axis::y) = pj;
        return *this;
    }

    /// Scalar product with another momentum
    double operator*(const FourVector& q) const
    {
        return operator[](0)*q[0]-operator[](1)*q[1]-operator[](2)*q[2]-operator[](3)*q[3];
    }
    
    /// @}

private:

    /// \name Auxiliary functions
    /// @{

    /// Computes the Lorentz gamma with sanity checks
    static double gamma(const double b2)
    {
        if (1.-b2<0.)
        {
            cerr << "Error in velocity: greater than speed of light." << endl;
            exit(1);
        }
        const double _1_gamma = sqrt(1.-b2);
        if (_1_gamma <= DBL_MIN) {
            cerr << "Error in velocity: too large for numerical precision." << endl;
            exit(1);
        }
        return 1./_1_gamma;
    }

    /// @}

};

#endif

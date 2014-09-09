/**
 *
 * \file   fourmomentum.h
 * \author Simone Lionetti
 * \date   September 2014
 *
 */

#ifndef FOURMOMENTUM_H
#define FOURMOMENTUM_H

#include <array>
#include <iostream>
#include <cfloat>   // DBL_MIN
#include <iomanip>  // setprecison()
#include <stdlib.h> // exit, EXIT_FAILURE
#include "math.h"   // sqrt
using namespace std;


/**
 *
 * \class FMomentum
 * \brief Object that represents a four-momentum
 *
 */

class FMomentum : public array<double,4>
{
    
public:
    
    /// \name Constructors
    /// @{
    
    /// Default constructor
    FMomentum() :
    array<double, 4>()
    {}
    
    /// Constructor with values
    FMomentum(const double& p0, const double& p1, const double& p2, const double& p3) :
    array<double, 4>({p0,p1,p2,p3})
    {}
    
    /// Copy constructor
    FMomentum(const FMomentum& that) :
    array<double, 4>()
    {}

    /// Destructor
    ~FMomentum()
    {}
    
    /// @}
    
    /// \name Input/output functions
    /// @{
    
    /// Direct handle on components
    //double& operator[](const size_t i)
    //{
    //    return _p[i];
    //}
    
    /// Value of components
    //double operator()(const size_t i) const
    //{
    //    return _p[i];
    //}
    
    /// Prints the fourmomentum to an output stream
    friend ostream& operator<<(ostream& stream, const FMomentum& x)
    {
        return stream
        <<setprecision(8)
        <<"("<<x[0]<<","<<x[1]<<","<<x[2]<<","<<x[3]<<")";
    }
    
    /// @}
    
    /// \name Operations
    /// @{
    
    /// Assignment operator
    //FMomentum& operator=(const FMomentum& that)
    //{
    //    _p = that._p;
    //    return (*this);
    //}
    
    /// Expicitly set the momentum by component
    FMomentum& operator=(initializer_list<double>& that)
    {
        if (that.size()!=size())
        {
            cerr << "Tried to assign more than 4 components to a FMomentum object." << endl;
            throw;
        }
        size_t i = 0;
        for (initializer_list<double>::iterator it = that.begin(); it<that.end(); ++it)
            (*this)[i++] = *it;
        return *this;
    }

    /// Compare two momenta
    //bool operator==(const FMomentum& that) const
    //{
    //    return (_p[0] == that[0]) && (_p[1] == that[1]) && (_p[2] == that(2)) && (_p[3] == that(3));
    //}
    
    /// Compare two momenta
    //bool operator!=(const FMomentum& that) const
    //{
    //    return !(*this == that);
    //}
    
    /// Multiply this momentum for a number
    FMomentum operator*(const double& that) const
    {
        return FMomentum(that*operator[](0),that*operator[](1),that*operator[](2),that*operator[](3));
    }
    
    friend FMomentum operator*(const double a, const FMomentum& b)
    {
        return FMomentum(a*b[0],a*b[1],a*b[2],a*b[3]);
    }
    
    /// Divide this momentum by a number
    FMomentum operator/(const double& that) const
    {
        const double div = 1. / that;
        return FMomentum(div*operator[](0),div*operator[](1),div*operator[](2),div*operator[](3));
    }
    
    /// Multiply this momentum for a number
    FMomentum& operator*=(const double& that)
    {
        for (FMomentum::iterator it = begin(); it<end(); ++it)
            (*it) *= that;
        return *this;
    }
    
    /// Divide this momentum by a number
    FMomentum& operator/=(const double& that)
    {
        for (FMomentum::iterator it = begin(); it<end(); ++it)
            (*it) /= that;
        return *this;
    }
    
    /// Negative of this momentum
    FMomentum operator-(void) const
    {
        return (-1.)*(*this);
    }
    
    /// Add this momentum to another one
    FMomentum operator+(const FMomentum& that) const
    {
        return FMomentum(operator[](0)+that[0],operator[](1)+that[1],operator[](2)+that[2],operator[](3)+that[3]);
    }
    
    /// Subtract a momentum from this one
    FMomentum operator-(const FMomentum& that) const
    {
        return (*this)+(-that);
    }

    /// Add another momentum to this one
    const FMomentum& operator+=(const FMomentum& that)
    {
        for (size_t i = 0; i<4; ++i)
            operator[](i)+=that[i];
        return *this;
    }
    
    /// Subtract another momentum from this one
    const FMomentum& operator-=(const FMomentum& that)
    {
        for (size_t i = 0; i<4; ++i)
            operator[](i)-=that[i];
        return *this;
    }
   
    /// Generic boost
    void boost(const double& bx,const double& by,const double& bz)
    {
        const double bsq = bx*bx+by*by+bz*bz;
        if (1.-bsq<0.)
        {
            cerr << "Error in boost: bsq>1 : " << bx << "," << by << "," << bz << endl;
            exit(1);
        }
        const double g = 1.0/sqrt(1.0-bsq);
        const double d = g*g/(g+1.0);
        const double L[4][4] = {
            {g,     -g*bx,          -g*by,      -g*bz},
            {-g*bx, 1.0+d*bx*bx,    d*bx*by,     d*bx*bz},
            {-g*by, d*by*bx,        1.0+d*by*by, d*by*bz},
            {-g*bz, d*bz*bx,        d*bz*by,     1.0+d*bz*bz}
        };
        double newp[4];
        for (int i=0;i<4;i++)
        {
            newp[i]=0.0;
            for (int j=0;j<4;j++)
                newp[i] += L[i][j]*operator[](j);
        }
        for (int i=0;i<4;i++) operator[](i) = newp[i];
    }


    /// Boost along z
    void zboost(const double& beta)
    {
        const double _1_gamma = sqrt(1.-beta*beta);
        if (_1_gamma <= DBL_MIN) {
            cerr << "Beta in FMomentum boost too small for numerical precision." << endl;
            throw;
        } else {
            const double gamma = 1. / _1_gamma;
            const double E  = + gamma*operator[](0) - beta*gamma*operator[](3);
            const double pz = - beta*gamma*operator[](0) + gamma*operator[](3);
            operator[](0)=E;
            operator[](3)=pz;
        }
    }
    
    /// Scalar product with another momentum
    double operator*(const FMomentum& q) const
    {
        return operator[](0)*q[0]-operator[](1)*q[1]-operator[](2)*q[2]-operator[](3)*q[3];
    }
    
    /// @}
    
};

#endif

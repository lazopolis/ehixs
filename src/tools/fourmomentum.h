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

class FMomentum
{
    
public:
    
    /// \name Constructors
    /// @{
    
    /// Default constructor
    FMomentum() :
    _p()
    {}
    
    /// Constructor with values
    FMomentum(const double& p0, const double& p1, const double& p2, const double& p3) :
    _p({p0,p1,p2,p3})
    {}
    
    /// Copy constructor
    FMomentum(const FMomentum& k) :
    _p(k._p)
    {}
    
    /// Destructor
    ~FMomentum()
    {}
    
    /// @}
    
    /// \name Input/output functions
    /// @{
    
    /// Direct handle on components
    double& operator[](const int i)
    {
        return _p[i];
    }
    
    /// Value of components
    double operator()(const int i) const
    {
        return _p[i];
    }
    
    /// Prints the fourmomentum to an output stream
    friend ostream& operator<<(ostream& stream, const FMomentum& x)
    {
        return stream
        <<setprecision(8)
        <<"("
        <<x._p[0]<<","
        <<x._p[1]<<","
        <<x._p[2]<<","
        <<x._p[3]
        <<")";
    }
    
    /// @}
    
    /// \name Operations
    /// @{
    
    /// Assignment operator
    FMomentum& operator=(const FMomentum& that)
    {
        _p = that._p;
        return *this;
    }
    
    /// Expicitly set the momentum by component
    FMomentum& operator=(initializer_list<double>& that)
    {
        if (that.size()!=_p.size())
        {
            cerr << "Tried to assign more than 4 components to a FMomentum object." << endl;
            throw;
        }
        size_t i = 0;
        for (initializer_list<double>::iterator it = that.begin(); it<that.end(); ++it)
            _p[i++] = *it;
        return *this;
    }
    
    /// Compare two momenta
    bool operator==(const FMomentum& that) const
    {
        return (_p[0] == that(0)) && (_p[1] == that(1)) && (_p[2] == that(2)) && (_p[3] == that(3));
    }
    
    /// Compare two momenta
    bool operator!=(const FMomentum& that) const
    {
        return !(*this == that);
    }
    
    /// Multiply this momentum for a number
    const FMomentum operator*(const double& that)
    {
        return FMomentum(that*_p[0],that*_p[1],that*_p[2],that*_p[3]);
    }
    
    friend const FMomentum operator*(const double a, const FMomentum& b)
    {
        return FMomentum(a*b(0),a*b(1),a*b(2),a*b(3));
    }
    
    /// Divide this momentum by a number
    const FMomentum operator/(const double& that)
    {
        const double div = 1. / that;
        return FMomentum(div*_p[0],div*_p[1],div*_p[2],div*_p[3]);
    }
    
    /// Multiply this momentum for a number
    const FMomentum& operator*=(const double& that)
    {
        _p[0]*=that;
        _p[1]*=that;
        _p[2]*=that;
        _p[3]*=that;
        return *this;
    }
    
    /// Divide this momentum by a number
    const FMomentum& operator/=(const double& that)
    {
        _p[0]/=that;
        _p[1]/=that;
        _p[2]/=that;
        _p[3]/=that;
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
        return FMomentum(_p[0]+that(0),_p[1]+that(1),_p[2]+that(2),_p[3]+that(3));
    }
    
    /// Subtract a momentum from this one
    FMomentum operator-(const FMomentum& that) const
    {
        return FMomentum(_p[0]-that(0),_p[1]-that(1),_p[2]-that(2),_p[3]-that(3));
    }
    
    /// Add another momentum to this one
    const FMomentum& operator+=(const FMomentum& that)
    {
        _p[0]+=that(0);
        _p[1]+=that(1);
        _p[2]+=that(2);
        _p[3]+=that(3);
        return *this;
    }
    
    /// Subtract another momentum from this one
    const FMomentum& operator-=(const FMomentum& that)
    {
        _p[0]-=that(0);
        _p[1]-=that(1);
        _p[2]-=that(2);
        _p[3]-=that(3);
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
                newp[i] += L[i][j]*_p[j];
        }
        for (int i=0;i<4;i++) _p[i] = newp[i];
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
            const double E  = + gamma*_p[0] - beta*gamma*_p[3];
            const double pz = - beta*gamma*_p[0] + gamma*_p[3];
            _p[0]=E;
            _p[3]=pz;
        }
    }
    
    /// Scalar product with another momentum
    double operator*(const FMomentum& q) const
    {
        return _p[0]*q(0)-_p[1]*q(1)-_p[2]*q(2)-_p[3]*q(3);
    }
    
    /// @}
    
private:
    
    /// \name Data members
    /// @{
    
    array<double,4> _p;
    
    /// @}

};

#endif

/**
 *
 * \file    boxmaster.cpp
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#include "boxmaster.h"

/// \fn    polyLog

double polyLog(const size_t n, const double& z)
{
    switch (n) {
        case 1:
            return log(1.-z);
            break;
        case 2:
            return HPL(0,1,z).real();
            break;
        case 3:
            return HPL(0,0,1,z).real();
            break;
        case 4:
            return HPL(0,0,0,1,z).real();
            break;
        default:
            cerr << "Weight of polyLog not valid." << endl;
            return NAN;
            break;
    }
}

/// \fn    continuedExp

Expansion<Parameter::epsilon, double> continuedExp(const double& z, const double& a, const size_t n)
{
    if (z>0) return Expansion<Parameter::epsilon, double>::exp(a*log(z),n);
    else return CounterForge::cosec*Expansion<Parameter::epsilon, double>::exp(a*log(-z),n);
}

/// \fn    bubble

Expansion<Parameter::epsilon, double> bubble(const double& s, const size_t n)
{
    return CounterForge::cGamma*
        Expansion<Parameter::epsilon, double>(-1,1.,true)*
        Expansion<Parameter::epsilon, double>::geometric(2.,n)*
        continuedExp(s,-1.,n);
}

/// \fn    twoFone

Expansion<Parameter::epsilon, double> twoFone(const double& z, const size_t n)
{
    if (z==0.) return Expansion<Parameter::epsilon, double>(0,{1.},true);
    else if (z < 1.) {
        vector<double> coeffs;
        coeffs.push_back(1.);
        for (size_t i = 1; i<n; ++i)
            coeffs.push_back(-polyLog(i,z));
        return Expansion<Parameter::epsilon, double>(0,coeffs);
    }
    else if (z > 1.) {
        Expansion<Parameter::epsilon, double> tmp = Expansion<Parameter::epsilon,double>::exp(-log(z),n);
        tmp = tmp*CounterForge::cotan;
        for (size_t i = 1; i<n; ++i)
            tmp.addCoefficient(static_cast<int>(i),m1n<size_t,double>(i)*polyLog(i,z));
        return tmp; //check
    }
    cerr << "Invalid argument " << z << " for twoFone" << endl;
    return Expansion<Parameter::epsilon, double>(0,{NAN},true);
}

/// \fn    boxF

Expansion<Parameter::epsilon, double> box(const double& s, const double&t, const double& M2,
                                           const size_t n)
{
    const double u = M2-s-t;
    //cout << "Called box " << s << "\t" << t << "\t" << u << endl;
    return CounterForge::cGamma*Expansion<Parameter::epsilon, double>(-2,{-2./(s*t)},true)*
    (
        continuedExp(-s,-1.,n)*twoFone(-u/s,n)+
        continuedExp(-t,-1.,n)*twoFone(-u/t,n)-
        continuedExp(-M2,-1.,n)*twoFone(-(M2*u)/(s*t),n)
    );
}

/**
 *
 * \file    boxmaster.cpp
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#include "boxmaster.h"
#include "chaplin.h"       // HPL
#include "counterforge.h"  // cGamma, cotan, cosec
#include <cmath>           // NAN

/// \fn    polyLog

double polyLog(const size_t n, const double& z)
{
    switch (n) {
        case 1:
            return -log(1.-z);
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
    //cout << "Called continuedExp(" << z << ", " << a << ", " << n << ")" << endl;
    //cout << -z << "^-(" << a << " eps) = " << Expansion<Parameter::epsilon, double>::exp(a*log(-z),n) << endl;
    if (z>0) return Expansion<Parameter::epsilon, double>::exp(a*log(z),n);
    else return times(CounterForge::cos,Expansion<Parameter::epsilon, double>::exp(a*log(-z),n),n);
}

/// \fn    bubble

Expansion<Parameter::epsilon, double> bubble(const double& s, const size_t n)
{
    //cout << "Exponential of " << s << "^-eps = " << continuedExp(s,-1.,n) << endl;
    return times(
                 times(
                 CounterForge::cGamma*Expansion<Parameter::epsilon, double>(-1,1.,true),
                 Expansion<Parameter::epsilon, double>::geometric(2.,n),
                 n
                 ),
                 continuedExp(-s,-1.,n),
                 n);
}

/// \fn    twoFone

Expansion<Parameter::epsilon, double> twoFone(const double& z, const size_t n)
{
    //cout << "Called twoFone(" << z << ", " << n << ")";
    if (z==0.) return Expansion<Parameter::epsilon, double>(0,{1.},true);
    else if (z < 1.) {
        vector<double> coeffs;
        coeffs.push_back(1.);
        for (size_t i = 1; i<n; ++i)
            coeffs.push_back(-polyLog(i,z));
        //cout << " with result " << Expansion<Parameter::epsilon, double>(0,coeffs) << endl;
        return Expansion<Parameter::epsilon, double>(0,coeffs);
    }
    else if (z > 1.) {
        Expansion<Parameter::epsilon, double> tmp = Expansion<Parameter::epsilon,double>::exp(log(z),n);
        tmp = times(tmp,CounterForge::cotan,n);
        //cout << "tmp = " << tmp << endl;
        for (size_t i = 1; i<n; ++i)
            tmp.addCoefficient(static_cast<int>(i),m1n<size_t,double>(i)*polyLog(i,1./z));
        //cout << " with result " << tmp << endl;
        return tmp; //check
    }
    cerr << "Invalid argument " << z << " for twoFone" << endl;
    return Expansion<Parameter::epsilon, double>(0,{NAN},true);
}

/// \fn    box

Expansion<Parameter::epsilon, double> box(const double& s, const double&t, const double& M2,
                                           const size_t n)
{
    const double u = M2-s-t;
//    cout << "Called box " << s << "\t" << t << "\t" << u << endl;
//    cout << "(-t)^-eps = " << continuedExp(-t,-1.,n) << endl;
//    cout << "2F1(-u/s,n) = " << twoFone(-u/s,n) << endl;
//    cout << "(-t)^-eps*2F1(-u/s,n) = " << times(continuedExp(-t,-1.,n),twoFone(-u/s,n),n) << endl;
//    cout << "(-s)^-eps*2F1(-u/t,n) = " << times(continuedExp(-s,-1.,n),twoFone(-u/t,n),n) << endl;
//    cout << "-(-M2)^-eps*2F1(-(M2*u)/(s*t),n) = " << -times(continuedExp(-M2,-1.,n),twoFone(-(M2*u)/(s*t),n),n) << endl;
    return times(
                 CounterForge::cGamma*Expansion<Parameter::epsilon, double>(-2,{2./(s*t)},true),
                 (
                  times(continuedExp(-s,-1.,n),twoFone(-u/t,n),n)+
                  times(continuedExp(-t,-1.,n),twoFone(-u/s,n),n)-
                  times(continuedExp(-M2,-1.,n),twoFone(-(M2*u)/(s*t),n),n)
                  ),
                 n
                 );
}

/// \fn    box6

Expansion<Parameter::epsilon, double> box6(const double& s, const double&t, const double& M2,
                                          const size_t n)
{
    const double u = M2-s-t;
    return times(
                 CounterForge::cGamma*
                 times(
                       Expansion<Parameter::epsilon, double>(-1,1.,true),
                       Expansion<Parameter::epsilon, double>::geometric(2.,n),
                       n
                       ),
                 ( // The setCoefficient(0,0.) represents 2F1(...)-1
                  times(continuedExp(-s,-1.,n),twoFone(-u/t,n+2).setCoefficient(0,0.),n)+
                  times(continuedExp(-t,-1.,n),twoFone(-u/s,n+2).setCoefficient(0,0.),n)-
                  times(continuedExp(-M2,-1.,n),twoFone(-(M2*u)/(s*t),n+2).setCoefficient(0,0.),n)
                  ).setCoefficient(1,0.), // This is analitically zero, num just small
                 n
                 );
}

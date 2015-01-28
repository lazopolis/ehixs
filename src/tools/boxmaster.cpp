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
            tmp.addCoefficient(i,m1n<size_t,double>(i)*polyLog(i,z));
    }
    cerr << "Invalid argument for twoFone" << endl;
    return Expansion<Parameter::epsilon, double>(0,{NAN},true);
}

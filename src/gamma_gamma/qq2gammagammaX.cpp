/**
 *
 * \file    qq2gammagammaX.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */


#include "qq2gammagammaX.h"

const double alphas = 1.;

/// \brief Matrix elements for qq->gammagamma

/// Tree

template<>
double qq2yy<0,0>(const double& s13_s14)
{
    return s13_s14+1./s13_s14;
}

template<>
double qq2yy<0,1>(const double& s13_s14)
{
    return -2.*(s13_s14+1./s13_s14+1.);
}

template<>
double qq2yy<0,2>(const double& s13_s14)
{
    return s13_s14+1./s13_s14+2.;
}

/// End of tree

/// One loop (to be multiplied by c_Gamma)

template<>
double qq2yy<1,-2>(const double& s13os14)
{
    const double s14os13 = 1./s13os14;
    return -2. * (s13os14+s14os13);
}

template<>
double qq2yy<1,-1>(const double& s13os14)
{
    const double s14os13 = 1./s13os14;
    return 4.+s13os14+s14os13;
}

template<>
double qq2yy<1,0>(const double& s13os14)
{
    const double s14os13 = 1./s13os14;
    const double log3 = -log(1.+s14os13);
    const double log4 = -log(1.+s13os14);
    return (
            2 + (consts::pi_square-3)*(s13os14+s14os13) +
            log4*(2 + 3*s13os14) + log3*(2 + 3*s14os13) +
            (2 + 2*s13os14 + s14os13)*log3*log3 +
            (2 + 2*s14os13 + s13os14)*log4*log4
            );
}


/// End of tree

/// \brief Shorthand for full epsilon expansion of qq->gammagamma

template<>
Expansion<Parameter::epsilon, double> qq2yy<0>(const double& s13os14)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yy<0,0>(s13os14),
        qq2yy<0,1>(s13os14),
        qq2yy<0,2>(s13os14)
    },true);
}

template<>
Expansion<Parameter::epsilon, double> qq2yy<1>(const double& s13os14)
{
    return QCD::CF * alphas/(2.*consts::Pi)*
        Expansion<Parameter::epsilon, double>(-2,{
            qq2yy<1,-2>(s13os14),
            qq2yy<1,-1>(s13os14),
            qq2yy<1,0>(s13os14)
        });
}

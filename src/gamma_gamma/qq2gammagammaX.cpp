/**
 *
 * \file    qq2gammagammaX.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */


#include "qq2gammagammaX.h"

/// \brief Matrix elements for qq->gammagamma

/// Tree

template<>
double qq2gammagamma<0,0>(const double& s13_s14)
{
    return s13_s14+1./s13_s14;
}

template<>
double qq2gammagamma<0,1>(const double& s13_s14)
{
    return -2.*(s13_s14+1./s13_s14+1.);
}

template<>
double qq2gammagamma<0,2>(const double& s13_s14)
{
    return s13_s14+1./s13_s14+2.;
}

/// End of tree

/// One loop (to be multiplied by c_Gamma)

template<>
double qq2gammagamma<1,-2>(const double& s13os14)
{
    return QCD::CF * (8 + 11*(s13os14+1./s13os14)/2.);
}

template<>
double qq2gammagamma<1,-1>(const double& s13os14)
{
    const double s14os13 = 1./s13os14;
    return QCD::CF * (
                      - 7 - 31*(s13os14+s14os13)/4.
                      + (2 + s13os14 + 2*s14os13)*log(1+s13os14)
                      + (2 + 2*s13os14 + s14os13)*log(1+s14os13)
                      );
}

template<>
double qq2gammagamma<1,0>(const double& s13os14)
{
    const double s14os13 = 1./s13os14;
    const double log34 = log(1.+s13os14);
    const double log43 = log(1.+s14os13);
    return QCD::CF * (
            50 - (8+5*(s13os14+s14os13))*consts::pi_square + 29*(s13os14+s14os13)
            - (10 + 11*s13os14 + 8*s14os13)*log34
            - (10 + 8*s13os14 + 11*s14os13)*log43
            + (2 + s13os14 + 2*s14os13)*log34*log34
            + (2 + 2*s13os14 + s14os13)*log43*log43
            )*0.25;
}

/// \brief Matrix elements for qq->gammagamma

/// Tree

template<>
double qq2gammagammag<0,0>(
                           const double& s12,
                           const double& s13,
                           const double& s14,
                           const double& s23,
                           const double& s24
                           )
{
    return 2.*QCD::CF*s12*(
             (
              3*s14*(s23+s24)*(s13*s13) +
              s14*(s23+2*s24)*
              (s23*s24 + s14*s14 + s23*s23 + s24*s24) +
              3*(s12*s12)*((s13 + s14 + s23 + s24)*(s13 + s14 + s23 + s24)) +
              4*(s13 + s14 + s23 + s24)*pow(s12,3) + 2*pow(s12,4) +
              (2*s23 + s24)*pow(s13,3) +
              s13*(3*(s23 + s24)*(s14*s14) + 3*s24*(s23*s23) +
                   3*s23*(s24*s24) + 2*pow(s23,3) + pow(s24,3)) +
              s12*pow(s13 + s14 + s23 + s24,3)
              )/
             (s13*s14*(s12 + s13 + s14)*s23*s24*(s12 + s23 + s24)));
}

/// End of tree

/// \brief Shorthand for full epsilon expansion of qq->gammagamma

template<>
Expansion<Parameter::epsilon, double> qq2gammagamma<0>(const double& s13os14)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2gammagamma<0,0>(s13os14),
        qq2gammagamma<0,1>(s13os14),
        qq2gammagamma<0,2>(s13os14)
    },true);
}

template<>
Expansion<Parameter::epsilon, double> qq2gammagamma<1>(const double& s13os14)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2gammagamma<1,-2>(s13os14),
        qq2gammagamma<1,-1>(s13os14),
        qq2gammagamma<1,0>(s13os14)
    });
}

/// \brief Shorthand for full epsilon expansion of qq->gammagammag

template<>
Expansion<Parameter::epsilon, double> qq2gammagammag<0>(
                                                        const double& s12,
                                                        const double& s13,
                                                        const double& s14,
                                                        const double& s23,
                                                        const double& s24
                                                        )
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2gammagammag<0,0>(s12,s13,s14,s23,s24),
    },true);
}

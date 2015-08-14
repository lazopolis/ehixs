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

/// \brief Matrix elements for qq->yyg

/// Tree

template<>
double qq2yyg<0,0>(
                   const double& zb,
                   const double& t12,
                   const double& t34,
                   const double& u
                   )
{
    return -128.*consts::Pi*QCD::CF*alphas*(
                                           -16+32*zb-24*(1+t34*t34)*zb*zb
                                           +6*t12*t12*u*u*zb*zb+pow(u,4)
                                           +8*(1+3*t34*t34)*pow(zb,3)
                                           +(9*pow(t12,4)-(3+t34*t34)*(3+t34*t34))
                                           *pow(zb,4)
                                           )/(
                                              zb*zb*(1-t12)*(1+t12)*
                                              (2+u+(-1+t12-t34)*zb)*(-2-u+(1+t12-t34)*zb)*
                                              (2-u+(-1+t12+t34)*zb)*(-2+u+(1+t12+t34)*zb)
                                              );
}

template<>
double qq2yyg<0,1>(
                   const double& zb,
                   const double& t12,
                   const double& t34,
                   const double& u
                   )
{
    return 128.*consts::Pi*QCD::CF*alphas*(
                                           -48+96*zb-16*t12*t34*u*(-2+zb)*zb*zb
                                           +8*(-17+7*t12*t12-t34*t34)*zb*zb
                                           +2*u*u*(4-4*zb+(7+3*t12*t12+t34*t34)*zb*zb)
                                           +pow(u,4)+8*(11-7*t12*t12+t34*t34)*pow(zb,3)
                                           +(
                                             2*t12*t12*(9+7*t34*t34)
                                             -3*(3+t34*t34)*(3+t34*t34)
                                             +9*pow(t12,4)
                                             )*pow(zb,4)
                                           )/(
                                              zb*zb*(1-t12)*(1+t12)*
                                              (2+u+(-1+t12-t34)*zb)*(-2-u+(1+t12-t34)*zb)*
                                              (2-u+(-1+t12+t34)*zb)*(-2+u+(1+t12+t34)*zb)
                                              );
}

template<>
double qq2yyg<0,2>(
                   const double& zb,
                   const double& t12,
                   const double& t34,
                   const double& u
                   )
{
    return 256.*consts::Pi*QCD::CF*alphas*(
                                           16-32*zb-8*t12*t34*u*(-2+zb)*zb*zb
                                           +4*(8+t12*t12+4*t34*t34)*zb*zb
                                           +u*u*(-4+4*zb-(-1+t34*t34)*zb*zb)
                                           -4*(4+t12*t12+4*t34*t34)*pow(zb,3)
                                           +(t12*t12*(-9+t34*t34)+(3+t34*t34)*(3+t34*t34))*pow(zb,4)
                                           )/(
                                              zb*zb*(1-t12)*(1+t12)*
                                              (2+u+(-1+t12-t34)*zb)*(-2-u+(1+t12-t34)*zb)*
                                              (2-u+(-1+t12+t34)*zb)*(-2+u+(1+t12+t34)*zb)
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

/// \brief Shorthand for full epsilon expansion of qq->gammagammag

template<>
Expansion<Parameter::epsilon, double> qq2yyg<0>(
                                                const double& zb,
                                                const double& t12,
                                                const double& t34,
                                                const double& u
                                                )
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yyg<0,0>(zb,t12,t34,u),
        qq2yyg<0,1>(zb,t12,t34,u),
        qq2yyg<0,2>(zb,t12,t34,u)
    },true);
}

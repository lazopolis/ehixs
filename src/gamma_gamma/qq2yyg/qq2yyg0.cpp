/**
 *
 * \file    qq2yyg0.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#include "qq2yyg0.h"
#include "constants.h"

const double alphas = 1.;

/// \brief Matrix elements for qq->yyg

/// Tree

template<>
double qq2yyg0<0>(
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
double qq2yyg0<1>(
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
double qq2yyg0<2>(
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

/// \brief Shorthand for full epsilon expansion of qq->gammagammag

Expansion<Parameter::epsilon, double> qq2yyg0(
                                              const double& zb,
                                              const double& t12,
                                              const double& t34,
                                              const double& u
                                              )
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yyg0<0>(zb,t12,t34,u),
        qq2yyg0<1>(zb,t12,t34,u),
        qq2yyg0<2>(zb,t12,t34,u)
    },true);
}

/**
 *
 * \file    bb2HX.cpp
 * \author  Simone Lionetti
 * \date    November 2014
 *
 */

#include "bb2HX.h"

/// \brief Matrix elements for bb->H

/// Tree

template<>
double bb2H<0,0>()
{
    return 2.*QCD::Nc*yukawa_bottom*yukawa_bottom;
}

/// End of tree

/// One loop (to be multiplied by c_Gamma)

template<>
double bb2H<1,-2>()
{
    return -2.*QCD::CF * alphastrong * bb2H<0,0>() *
        1.;
}

template<>
double bb2H<1,-1>()
{
    return 0.;
}

template<>
double bb2H<1,0>()
{
    return 2.*QCD::CF * alphastrong * bb2H<0,0>() *
        (0.5*consts::pi_square-1.);
}

/// \brief Matrix elements for bb->Hg

/// Tree

template<>
double bb2Hg<0,0>(const double& z, const double& lambda)
{
    return 2.*static_cast<double>(QCD::CF) * alphastrong * bb2H<0,0>() *
        (1.+z*z)/((1.-z)*lambda*(1.-lambda));
}

template<>
double bb2Hg<0,1>(const double& z, const double& lambda)
{
    return -2.*static_cast<double>(QCD::CF) * alphastrong * bb2H<0,0>() *
        (1.-z)/(lambda*(1.-lambda));
}

/// End of tree

/// One loop (to be multiplied by c_Gamma)

template<>
double bb2Hg<1,0>(const double& z, const double& lambda)
{
    const double pl1 = HPL2(0,1, 1./(1. + (z*pow(1.-z,-2))/((1.-lambda)*lambda)) ).real();
    const double pl2 = HPL2(0,1,1.-lambda).real();
    const double pl3 = HPL2(0,1,(1.-lambda)*(1.-z)).real();
    const double pl4 = HPL2(0,1,lambda).real();
    const double pl5 = HPL2(0,1,lambda*(1.-z)).real();
    const double pl6 = HPL2(0,1,lambda/(z + lambda*(1.-z))).real();
    const double pl7 = HPL2(0,1,(1.-lambda)/(1. - lambda*(1.-z))).real();
    const double l1 = log(1.-z);
    const double l2 = log(z);
    const double l3 = log(1.-lambda);
    const double l4 = log(lambda);
    const double l5 = log(1.- lambda*(1.-z));
    const double l6 = log(z + lambda*(1.-z));
    const double compr = 18.*l3 + 18.*l4 + 18.*l3*l4 - 2.*l2*(17. + 9.*l3 + 9.*l4) - 16.*l3*l5 + 2.*l4*l5
    + 2.*l3*l6 - 16.*l4*l6 + 18.*l5*l6
    - 4.*l1*(-9. + 9.*l2 - 9.*l3 - 9.*l4 + 4.*l5 + 4.*l6)
    + 18.*pl1 + 2.*pl2 + 20.*pl3 + 2.*pl4 + 20.*pl5 - 2.*pl6 - 2.*pl7
    + 36.*l1*l1 + 17.*l2*l2 + 9.*l3*l3 + 9.*l4*l4 + 8.*l5*l5 + 8.*l6*l6 - 14.*consts::pi_square;
    return 16. * (
                  (16.+compr)
                  - 2. * (-5. + 36.*l1 - 34.*l2 + 18.*l3 + 18.*l4)*z
                  + (6.+compr)*z*z
                  ) / (3.*(1.-lambda)*lambda*(1.-z));
}

/// \brief Shorthand for full epsilon expansion of bb->H

template<>
Expansion<Parameter::epsilon, double> bb2H<0>()
{
    return Expansion<Parameter::epsilon, double>(0,{bb2H<0,0>()},true);
}

template<>
Expansion<Parameter::epsilon, double> bb2H<1>()
{
    return Expansion<Parameter::epsilon, double>(-2,{
        bb2H<1,-2>(),
        bb2H<1,-1>(),
        bb2H<1, 0>()
    });
}

/// \brief Shorthand for full epsilon expansion of bb->Hg

template<>
Expansion<Parameter::epsilon, double> bb2Hg<0>(const double& z, const double& lambda)
{
    return Expansion<Parameter::epsilon, double>(0,{bb2Hg<0,0>(z,lambda),bb2Hg<0,1>(z,lambda)},true);
}

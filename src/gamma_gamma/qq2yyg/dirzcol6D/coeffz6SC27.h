/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 27: box6(s25,s35,s14)

// Coefficient order epsilon^-1 of master 27
template<>
double qq2yygz6SC<27,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return -(pow(s13,-1)*pow(-1+zb,-1)*pow(zb,3)*pow(1+s13-lam*zb,-1)*(s13*s13*((-4+(4-5*s35n)*zb)*(lam*lam)+2*s35n*(-1-3*zb+zb*zb)-2*(2-3*zb+zb*zb)+lam*(s35n*(2+9*zb-3*(zb*zb))+3*(2-3*zb+zb*zb))+zb*(1+(-1+s35n)*zb)*pow(lam,3))+(-3-2*lam*(-2+zb)+zb+(-2+zb)*(lam*lam))*pow(s13,3)+s13*((-2+zb)*((-1+zb)*(-1+zb))-2*s35n*zb*(3-4*zb+zb*zb)+lam*(4+zb*(-8+11*s35n+3*(s35n*s35n))+(4-11*s35n+7*(s35n*s35n))*(zb*zb))+2*zb*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))*pow(lam,3)+s35n*s35n*(-1-2*zb-4*(zb*zb)+pow(zb,3))+lam*lam*(-3+(3-7*s35n)*zb+(3+s35n-4*(s35n*s35n))*(zb*zb)-3*((-1+s35n)*(-1+s35n))*pow(zb,3)))+zb*(-(s35n*(-2*s35n*(-1+zb)*zb+(-1+zb)*(-1+zb)+s35n*s35n*(1+zb*zb)))-(2+s35n-2*zb+2*s35n*zb)*(lam*lam)*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))+lam*(1+(-1+s35n)*zb)*((1+s35n-zb+s35n*zb)*(1+s35n-zb+s35n*zb))+pow(lam,3)*pow(1+(-1+s35n)*zb,3)))*pow(1+s13+(-1+s35n)*zb,-3)*pow(s13+(-lam+s35n)*zb,-1))/2.;
}

// Coefficient of master 27 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<27>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<27,-1>(s13,s35n,lam,zb)
    });
}


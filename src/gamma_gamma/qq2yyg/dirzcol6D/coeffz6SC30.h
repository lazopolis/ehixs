/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 30: box6(s34,s45,s12)

// Coefficient order epsilon^-1 of master 30
template<>
double qq2yygz6SC<30,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-1)*pow(s35n,-2)*pow(zb,-1)*(2*(s13*s13)*(2-s35n*(1+2*zb)+(1+zb)*(s35n*s35n))+s35n*s35n*(3-2*zb+(2-2*s35n+s35n*s35n)*(zb*zb))+s13*s35n*(3+2*zb+zb*(2+zb)*(s35n*s35n)-2*s35n*(-1+2*zb+zb*zb))+lam*lam*(4+2*(-4+s35n)*zb+(4+2*s35n-2*(s35n*s35n))*(zb*zb)-2*s35n*(2-3*s35n+s35n*s35n)*pow(zb,3))+lam*(2*s13*(4-4*zb+2*(1-2*zb)*zb*(s35n*s35n)+s35n*(-5+4*(zb*zb))+zb*zb*pow(s35n,3))+s35n*(-5+(7-3*s35n)*zb+(-2-4*s35n+3*(s35n*s35n))*(zb*zb)+s35n*(2-3*s35n+s35n*s35n)*pow(zb,3))))*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 30 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<30>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<30,-1>(s13,s35n,lam,zb)
    });
}


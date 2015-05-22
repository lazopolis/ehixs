/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 25: box6(s24,s34,s15)

// Coefficient order epsilon^-1 of master 25
template<>
double qq2yygz6SC<25,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return -(pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-1)*pow(s35n,-1)*pow(zb,-1)*((1+(-1+s35n)*zb)*(zb*zb)*((1+s13+(-1+s35n)*zb)*(1+s13+(-1+s35n)*zb))*pow(lam,3)+(2-2*zb+2*zb*(-2+3*zb)*(s35n*s35n)+s35n*(-2+8*zb-6*(zb*zb))+zb*zb)*pow(s13,3)+(2-2*zb+s35n*(-1+4*zb))*pow(s13,4)+pow(s13,5)+2*(-1+s35n)*(s13*s13)*(-1+zb-3*s35n*zb-3*(-1+s35n)*s35n*(zb*zb)+s35n*(-1+2*s35n)*pow(zb,3))+s13*((-1+zb)*(-1+zb)+s35n*(-2+6*zb-4*(zb*zb))-2*(zb*zb)*(3-4*zb+zb*zb)*pow(s35n,3)+(-4+zb)*pow(s35n,4)*pow(zb,3)+zb*(s35n*s35n)*(-2+6*zb-4*(zb*zb)+pow(zb,3)))-zb*(lam*lam)*(s13*s13*(4+(-10+11*s35n)*zb+6*((-1+s35n)*(-1+s35n))*(zb*zb))+s35n*zb*(3+2*(-1+s35n)*zb)*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))+(2+(-3+2*s35n)*zb)*pow(s13,3)+s13*(2+(-7+11*s35n)*zb+(8-23*s35n+15*(s35n*s35n))*(zb*zb)+3*(-1+2*s35n)*((-1+s35n)*(-1+s35n))*pow(zb,3)))-s35n*(1-2*zb+(1+s35n*s35n)*(zb*zb)+2*(-1+s35n)*(s35n*s35n)*pow(zb,3)+(-1+s35n)*(-1+s35n)*(s35n*s35n)*pow(zb,4))+lam*(1+(-3+s35n)*zb+(3-2*s35n+3*(s35n*s35n))*(zb*zb)+2*(1+4*(-1+s35n)*zb+(3-6*s35n+2*(s35n*s35n))*(zb*zb))*pow(s13,3)+(1+(-3+s35n)*zb)*pow(s13,4)+(-1+s35n-7*(s35n*s35n)+7*pow(s35n,3))*pow(zb,3)+s13*s13*(2+(-6+8*s35n)*zb+(7-25*s35n+18*(s35n*s35n))*(zb*zb)+3*(-1+5*s35n-6*(s35n*s35n)+2*pow(s35n,3))*pow(zb,3))+5*((-1+s35n)*(-1+s35n))*(s35n*s35n)*pow(zb,4)+s13*(2+2*(-2+s35n)*zb+(2-11*s35n+15*(s35n*s35n))*(zb*zb)+s35n*(13-29*s35n+16*(s35n*s35n))*pow(zb,3)+4*s35n*pow(-1+s35n,3)*pow(zb,4))+s35n*s35n*pow(-1+s35n,3)*pow(zb,5)))*pow(1+s13+(-1+s35n)*zb,-3))/2.;
}

// Coefficient of master 25 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<25>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<25,-1>(s13,s35n,lam,zb)
    });
}


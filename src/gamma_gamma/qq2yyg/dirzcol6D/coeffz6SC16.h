/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 16: box6(s12,s25,s34)

// Coefficient order epsilon^-1 of master 16
template<>
double qq2yygz6SC<16,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-2)*pow(s13,-1)*pow(-1+s35n,-1)*pow(s35n,-1)*pow(zb,-1)*((1+2*s13)*(-1+s35n)*s35n*(s13+s35n)*(s35n*zb*(1+(-1+s35n)*zb)+s13*(1+(-1+2*s35n)*zb)+s13*s13)+(-1+s35n)*(zb*zb)*(4+4*(-1+s35n)*zb+2*(3+(-5+3*s35n)*zb)*(s13*s13)+s35n*(-3+2*s35n)*(zb*zb)+s13*(9+4*(-3+2*s35n)*zb+s35n*(-4+3*s35n)*(zb*zb)))*pow(lam,4)-(-1+s35n)*(3+3*(-1+s35n)*zb+s13*(4+(-5+4*s35n)*zb)+(-1+s35n)*s35n*(zb*zb))*pow(lam,5)*pow(zb,3)+(-1+s35n)*zb*pow(lam,3)*(-2+(2-5*s35n)*zb-3*(-2+s35n)*s35n*(zb*zb)-2*s13*(4+(-6+4*s35n)*zb+(-4+s35n)*s35n*(zb*zb))-3*(s13*s13)*(3+2*(-3+s35n)*zb+(-2+s35n)*s35n*(zb*zb))-2*(2+(-5+2*s35n)*zb)*pow(s13,3)+s35n*((-1+s35n)*(-1+s35n))*pow(zb,3))+lam*(-((-1+s35n)*zb*(s35n*s35n)*(2+(-3+4*s35n)*zb+(-1+s35n)*(zb*zb)))+(-3+s35n*(3-2*zb)+2*(1+zb)*(s35n*s35n))*pow(s13,4)+(-1+2*s35n)*pow(s13,5)+2*pow(s13,3)*(-2+s35n*(1+zb)+(2-3*zb)*(s35n*s35n)+2*zb*pow(s35n,3))+s13*s13*(-2+s35n*(2+2*zb-5*(zb*zb))+s35n*s35n*(2-5*zb+15*(zb*zb))+(3-14*zb)*zb*pow(s35n,3)+4*(zb*zb)*pow(s35n,4))+s13*s35n*(1+zb*(-3+7*s35n-4*(s35n*s35n))+(-2+6*s35n-4*(s35n*s35n))*(zb*zb)+s35n*(-3+2*s35n)*((-1+s35n)*(-1+s35n))*pow(zb,3)))+lam*lam*((-1+zb*(5-7*s35n+2*(s35n*s35n)))*pow(s13,4)+s13*s13*(-4+12*zb-2*(-1+zb)*zb*(s35n*s35n)+s35n*(3-14*zb+5*(zb*zb))-3*(zb*zb)*pow(s35n,3))+pow(s13,3)*(-3+12*zb-7*(s35n*s35n)*(zb*zb)+s35n*(2-12*zb+5*(zb*zb))+2*(zb*zb)*pow(s35n,3))-(-1+s35n)*s35n*zb*(-3-4*(-1+s35n)*zb+(1+s35n-2*(s35n*s35n))*(zb*zb)+s35n*((-1+s35n)*(-1+s35n))*pow(zb,3))+s13*(-2+4*zb+zb*(s35n*s35n)*(4-9*zb-12*(zb*zb))+2*(1+6*zb)*(zb*zb)*pow(s35n,3)-4*pow(s35n,4)*pow(zb,3)+s35n*(1-8*zb+7*(zb*zb)+4*pow(zb,3))))+(-1+s35n)*(1+(-1+s35n)*zb)*pow(lam,6)*pow(zb,4))*pow(-1-s13+lam*zb,-1)*pow(-s13+(lam-s35n)*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 16 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<16>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<16,-1>(s13,s35n,lam,zb)
    });
}


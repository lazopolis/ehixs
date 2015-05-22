/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 15: box6(s12,s24,s35)

// Coefficient order epsilon^-1 of master 15
template<>
double qq2yygz6SC<15,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-1)*pow(-1+s35n,-1)*pow(-1+zb,-1)*pow(zb,-1)*(-((-1+zb)*(-6+7*zb+zb*(s35n*s35n)-zb*zb+s35n*(2-8*zb+zb*zb)+zb*(lam*lam)*(-1+(-1+s35n)*(-1+s35n)*(zb*zb))-2*lam*(-1-s35n*zb*(1+2*zb)+zb*zb+s35n*s35n*(zb*zb)))*pow(s13,3))-(-1+zb)*((-3+s35n)*(2+(-1+s35n)*zb)+zb*(-1+(-1+s35n)*zb)*(lam*lam)+lam*(2-2*(-2+s35n)*zb-(2-3*s35n+s35n*s35n)*(zb*zb)))*pow(s13,4)+(-1+zb)*(4+s35n*(-1+zb)-zb+lam*(-1+(-2+s35n)*zb))*pow(s13,5)+(-1+zb)*pow(s13,6)+(-1+lam)*lam*(-1+s35n)*(2-2*zb+s35n*(-2+3*zb)+(3+(-3+2*s35n)*zb)*(lam*lam)-(-1+zb)*(s35n*s35n)+lam*(-2+zb-zb*(s35n*s35n)+(-1+s35n)*(-1+s35n)*(zb*zb))+zb*(-3+(3-2*s35n)*zb)*pow(lam,3)+(1+(-1+s35n)*zb)*(zb*zb)*pow(lam,4))*pow(zb,4)-s13*s13*(zb*(lam*lam)*(1+(-3+2*s35n)*zb+(3-4*s35n+s35n*s35n)*(zb*zb)-(-1+s35n)*s35n*pow(zb,3))+(-1+zb)*((-5+zb)*((-1+zb)*(-1+zb))+zb*(s35n*s35n)*(1-5*zb+zb*zb)-2*s35n*(-1+6*zb-6*(zb*zb)+pow(zb,3)))-(-1+s35n)*(-2+zb)*pow(lam,3)*pow(zb,4)+(-1+s35n)*(-2+zb)*pow(lam,4)*pow(zb,4)+lam*(-2-6*(-1+s35n)*zb-2*(3-6*s35n+2*(s35n*s35n))*(zb*zb)+(2-6*s35n+4*(s35n*s35n))*pow(zb,3)+(-1+s35n)*pow(zb,4)))+s13*((-1+zb)*(s35n*(-1+5*zb)*((-1+zb)*(-1+zb))-zb*(s35n*s35n)*(1-5*zb+4*(zb*zb))-2*pow(-1+zb,3)+pow(s35n,3)*pow(zb,3))-2*(-1+s35n)*(2+s35n-zb)*pow(lam,3)*pow(zb,4)+(zb*(1+zb)*(s35n*s35n)+2*(-2+zb*zb)-s35n*(-4+zb+3*(zb*zb)))*pow(lam,4)*pow(zb,4)+zb*(lam*lam)*(-1+(4-3*s35n)*zb-3*(2-3*s35n+s35n*s35n)*(zb*zb)-(-2+9*s35n-8*(s35n*s35n)+pow(s35n,3))*pow(zb,3)+pow(-1+s35n,3)*pow(zb,4))-(-1+s35n)*(3+(-2+s35n)*zb)*pow(lam,5)*pow(zb,5)-lam*(-1-3*(-1+s35n)*zb+(-2+6*s35n-3*(s35n*s35n))*(zb*zb)-(2-3*(s35n*s35n)+pow(s35n,3))*pow(zb,3)+(1-4*s35n+3*(s35n*s35n))*pow(zb,4)+s35n*((-1+s35n)*(-1+s35n))*pow(zb,5))))*pow(1+s13-lam*zb,-2)*pow(-s13+(lam-s35n)*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 15 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<15>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<15,-1>(s13,s35n,lam,zb)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 11: box6(s12,s13,s45)

// Coefficient order epsilon^-1 of master 11
template<>
double qq2yygz6SC<11,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-1)*pow(s35n,-1)*pow(-1+zb,-1)*pow(zb,-1)*((1+s13)*(s13+s35n)*(-1+zb)*(s13*s13*(1-3*s35n*(-1+zb)*zb+6*(s35n*s35n)*(zb*zb))+(1+(-1+4*s35n)*zb)*pow(s13,3)+pow(s13,4)+s35n*zb*(1+(-1+s35n)*zb+s35n*s35n*(zb*zb)+(-1+s35n)*(s35n*s35n)*pow(zb,3))+s13*(1+(-1+2*s35n)*zb+3*(s35n*s35n)*(zb*zb)+(-3+4*s35n)*(s35n*s35n)*pow(zb,3)))+(1+s13+(-1+s35n)*zb)*(-1+2*zb+(-1+s35n*s35n)*(zb*zb))*pow(lam,5)*pow(zb,4)+pow(lam,4)*pow(zb,3)*(1-3*zb+(3-6*s35n+s35n*s35n)*(zb*zb)+s13*s13*(4+(-9+2*s35n)*zb+(5-3*s35n)*(zb*zb))+(-1+12*s35n-13*(s35n*s35n)+2*pow(s35n,3))*pow(zb,3)+s13*(5+(-15+7*s35n)*zb+(15-21*s35n+4*(s35n*s35n))*(zb*zb)+(-5+14*s35n-9*(s35n*s35n))*pow(zb,3))-6*s35n*((-1+s35n)*(-1+s35n))*pow(zb,4))+zb*zb*pow(lam,3)*(-1+(3+s35n)*zb-(3-8*s35n+s35n*s35n)*(zb*zb)+2*(-1+zb)*(3+(-5+3*s35n)*zb)*pow(s13,3)+(1-16*s35n+19*(s35n*s35n)-4*pow(s35n,3))*pow(zb,3)+s13*s13*(-9+(29-20*s35n)*zb-3*(10-18*s35n+5*(s35n*s35n))*(zb*zb)+(10-31*s35n+15*(s35n*s35n))*pow(zb,3))+s35n*(4-15*s35n+14*(s35n*s35n)-3*pow(s35n,3))*pow(zb,4)+2*s13*(-2+(6-4*s35n)*zb+(-6+21*s35n-8*(s35n*s35n))*(zb*zb)+(2-23*s35n+25*(s35n*s35n)-6*pow(s35n,3))*pow(zb,3)+6*s35n*((-1+s35n)*(-1+s35n))*pow(zb,4))+3*s35n*(1+s35n)*((-1+s35n)*(-1+s35n))*pow(zb,5))-zb*(lam*lam)*(-1+(3+2*s35n)*zb+(-3-3*s35n+6*(s35n*s35n))*(zb*zb)+(-1+zb)*(7+2*(-10+7*s35n)*zb+(10-32*s35n+13*(s35n*s35n))*(zb*zb))*pow(s13,3)+2*(-1+zb)*(2+(-5+2*s35n)*zb)*pow(s13,4)+(1+6*s35n-6*(s35n*s35n)+3*pow(s35n,3))*pow(zb,3)-2*s35n*(5-3*s35n-2*(s35n*s35n)+pow(s35n,3))*pow(zb,4)+s13*s13*(-5+(17-5*s35n)*zb+(-18+41*s35n-17*(s35n*s35n))*(zb*zb)+(6-47*s35n+53*(s35n*s35n)-15*pow(s35n,3))*pow(zb,3)+3*s35n*(5-12*s35n+5*(s35n*s35n))*pow(zb,4))-s35n*((-2+s35n)*(-2+s35n))*(-1+s35n*s35n)*pow(zb,5)+s13*(-3+(9+5*s35n)*zb+3*(-3+s35n+s35n*s35n)*(zb*zb)+(3-7*s35n+23*(s35n*s35n)-9*pow(s35n,3))*pow(zb,3)-s35n*(3+23*s35n-25*(s35n*s35n)+7*pow(s35n,3))*pow(zb,4)+s35n*(2+7*s35n-16*(s35n*s35n)+7*pow(s35n,3))*pow(zb,5))+s35n*((-1+s35n)*(-1+s35n))*(1+s35n*s35n)*pow(zb,6))+lam*((-1+zb)*(2+2*(-5+s35n)*zb+(5-18*s35n+4*(s35n*s35n))*(zb*zb))*pow(s13,4)+(-1+zb)*(1+(-5+s35n)*zb)*pow(s13,5)+2*(-1+zb)*pow(s13,3)*(1-(4+s35n)*zb+(2-12*s35n)*(zb*zb)+3*s35n*(2-4*s35n+s35n*s35n)*pow(zb,3))+s35n*(zb*zb)*((-1+zb)*(-1+zb)*(1+2*(zb*zb))-zb*zb*(-2+zb+zb*zb)*pow(s35n,3)+zb*(s35n*s35n)*(5-5*zb+2*pow(zb,3))+s35n*(4-7*zb+3*(zb*zb)+3*pow(zb,3)-3*pow(zb,4)))+s13*s13*(-2+(8+6*s35n)*zb+(-9+4*s35n+11*(s35n*s35n))*(zb*zb)+(3-19*s35n+9*(s35n*s35n)+2*pow(s35n,3))*pow(zb,3)+s35n*(12-29*s35n+12*(s35n*s35n)-4*pow(s35n,3))*pow(zb,4)+s35n*(-1+9*s35n-14*(s35n*s35n)+4*pow(s35n,3))*pow(zb,5))+s13*(-1+(4+3*s35n)*zb+(-5-2*s35n+13*(s35n*s35n))*(zb*zb)+(2-3*s35n-12*(s35n*s35n)+9*pow(s35n,3))*pow(zb,3)+s35n*(6-5*s35n-2*(s35n*s35n)+pow(s35n,3))*pow(zb,4)-s35n*(5-9*s35n+9*(s35n*s35n)-2*pow(s35n,3)+pow(s35n,4))*pow(zb,5)+s35n*(1-s35n+2*(s35n*s35n)-3*pow(s35n,3)+pow(s35n,4))*pow(zb,6))))*pow(-1-s13+lam*zb,-1)*pow(-s13+(lam-s35n)*zb,-1)*pow(1+s13+(-1+s35n)*zb,-2))/2.;
}

// Coefficient of master 11 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<11>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<11,-1>(s13,s35n,lam,zb)
    });
}


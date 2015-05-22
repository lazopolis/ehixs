/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 14: box6(s12,s23,s45)

// Coefficient order epsilon^-1 of master 14
template<>
double qq2yygz6SC<14,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-2)*pow(s35n,-1)*pow(-1+zb,-1)*pow(zb,-1)*(-((1+s13)*(-1+zb)*(-(s13*zb*(s35n*s35n)*(2-5*zb+zb*zb))-s35n*(s13*s13)*(2+2*(-3+s35n)*zb+zb*zb)+(2+2*s35n*(-1+zb)-zb*(s35n*s35n))*pow(s13,3)+(2+s35n*(-1+zb))*pow(s13,4)+pow(s13,5)+pow(s35n,3)*pow(zb,3)))+pow(lam,4)*pow(zb,3)*(-2*s35n*(-1+zb)*zb*(1+(-1+s35n)*zb)+s13*s13*(-4+(9-2*s35n)*zb+(-5+3*s35n)*(zb*zb))+s13*(-3+(6-5*s35n)*zb+(-3+7*s35n)*(zb*zb)+2*(-1+s35n)*s35n*pow(zb,3)))+zb*zb*pow(lam,3)*(s35n*(-1+zb)*zb*(1+zb+5*s35n*zb+2*(-1+s35n*s35n)*(zb*zb))-2*(-1+zb)*(3+(-5+3*s35n)*zb)*pow(s13,3)+s13*s13*(9+(-21+16*s35n)*zb+(12-22*s35n+3*(s35n*s35n))*(zb*zb)+(5-3*s35n)*s35n*pow(zb,3))-2*s13*(-2-2*(-2+s35n)*zb+(-2+s35n)*(zb*zb)+s35n*(1+s35n)*pow(zb,3)))+s13*(1-2*zb-(-1+s35n*s35n)*(zb*zb))*pow(lam,5)*pow(zb,4)+lam*(-2*(-1+zb)*(2+6*(-1+s35n)*zb+s35n*(-4+3*s35n)*(zb*zb))*pow(s13,3)-(-1+zb)*(3+4*(-3+2*s35n)*zb+(-4+s35n)*s35n*(zb*zb))*pow(s13,4)-(-1+zb)*(1+(-5+s35n)*zb)*pow(s13,5)+(-1+zb)*(2+s35n-zb+3*s35n*zb)*(s35n*s35n)*pow(zb,3)+s13*s13*(2+2*(-3+5*s35n)*zb+(4-26*s35n+5*(s35n*s35n))*(zb*zb)+(19-5*s35n)*s35n*pow(zb,3)-4*s35n*pow(zb,4))+s13*s35n*zb*(4+(-11+s35n)*zb-(-9+8*s35n+s35n*s35n)*(zb*zb)+(-2+9*s35n)*pow(zb,3)+(-3+s35n)*s35n*pow(zb,4)))+zb*(lam*lam)*(-(s35n*(-1+zb)*(1-zb+zb*(3+2*zb)*(s35n*s35n)+s35n*(2+4*zb-2*(zb*zb)))*(zb*zb))+3*(-1+zb)*(3+6*(-1+s35n)*zb+(-2+s35n)*s35n*(zb*zb))*pow(s13,3)+2*(-1+zb)*(2+(-5+2*s35n)*zb)*pow(s13,4)+s13*s13*(-8+(20-14*s35n)*zb-4*(3-5*s35n+2*(s35n*s35n))*(zb*zb)+s35n*(-5+8*s35n)*pow(zb,3))+s13*(-2+(4-5*s35n)*zb+(-2+13*s35n+s35n*s35n)*(zb*zb)+s35n*(-13+s35n+s35n*s35n)*pow(zb,3)-s35n*(-5+s35n*s35n)*pow(zb,4))))*pow(-1-s13+lam*zb,-1)*pow(-s13+(lam-s35n)*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 14 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<14>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<14,-1>(s13,s35n,lam,zb)
    });
}


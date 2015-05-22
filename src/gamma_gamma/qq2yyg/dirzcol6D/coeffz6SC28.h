/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 28: box6(s25,s45,s13)

// Coefficient order epsilon^-1 of master 28
template<>
double qq2yygz6SC<28,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(s13,-1)*pow(-1+zb,-1)*pow(zb,3)*(s13*s13*(3*(-1+zb)+2*s35n*(-1-3*zb+zb*zb))-zb*zb*(2+3*s35n*zb+s13*(4+(-3+2*s35n)*zb)-2*(zb*zb)+2*(s35n*s35n)*(zb*zb))*pow(lam,4)+(-3+zb)*pow(s13,3)+(1+(-1+s35n)*zb)*pow(lam,5)*pow(zb,3)-s35n*s35n*(1+(-1+s35n)*zb+2*(zb*zb)+(-2+s35n)*pow(zb,3))+zb*pow(lam,3)*(1+(3+5*s35n)*zb+(5+(-3+s35n)*zb)*(s13*s13)+(-3+2*s35n+3*(s35n*s35n))*(zb*zb)+2*s13*(2+(2+4*s35n)*zb+(-3+s35n*s35n)*(zb*zb))+(-1-3*s35n+3*(s35n*s35n)+pow(s35n,3))*pow(zb,3))+s13*s35n*(-2-2*zb+4*(zb*zb)+s35n*(-1-2*zb-4*(zb*zb)+pow(zb,3)))+lam*lam*(-1+zb-3*s35n*zb-s13*s13*(2+(8+5*s35n)*zb-6*(zb*zb))-(3+4*s35n+5*(s35n*s35n))*(zb*zb)+(-2+zb)*pow(s13,3)-(-3-3*s35n+3*(s35n*s35n)+pow(s35n,3))*pow(zb,3)-s13*(1+7*(1+s35n)*zb+(-3+11*s35n+4*(s35n*s35n))*(zb*zb)+3*(-1-2*s35n+s35n*s35n)*pow(zb,3))-2*s35n*(-1+s35n*s35n)*pow(zb,4))+lam*(s13*s13*(4+3*zb+s35n*(2+9*zb-3*(zb*zb))-3*(zb*zb))-2*(-2+zb)*pow(s13,3)+s13*(-6*(-1+zb)*zb+zb*(3+7*zb)*(s35n*s35n)+s35n*(4+5*zb+3*(zb*zb)-4*pow(zb,3)))+s35n*(2+(-2+3*s35n)*zb+(4+3*s35n+s35n*s35n)*(zb*zb)+(-4-s35n+2*(s35n*s35n))*pow(zb,3)+(-1+s35n)*s35n*pow(zb,4))))*pow(1+s13-lam*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1)*pow(s13+(-lam+s35n)*zb,-3))/2.;
}

// Coefficient of master 28 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<28>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<28,-1>(s13,s35n,lam,zb)
    });
}


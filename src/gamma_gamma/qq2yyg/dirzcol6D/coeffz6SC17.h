/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 17: box6(s13,s34,s25)

// Coefficient order epsilon^-1 of master 17
template<>
double qq2yygz6SC<17,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-1)*pow(-1+s35n,-1)*pow(zb,-1)*(-(zb*zb*((1+zb-s35n*zb)*(s13*s13)+(1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))*pow(lam,3))+zb*(lam*lam)*((2-(-2+s35n)*zb)*(s13*s13)+s13*(2+(-4+5*s35n)*zb+(2-3*s35n+s35n*s35n)*(zb*zb))+(2+s35n*zb)*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))+(2+(3-2*s35n)*zb)*pow(s13,3))+(s13+s35n)*(2*s13*(1+(-1+s35n)*zb)+2*(s13*s13)+(1+(-1+s35n)*zb)*(1+(-1+s35n)*zb)+2*pow(s13,3)+pow(s13,4))+lam*(-1+zb-3*s35n*zb+s13*s13*(-2+s35n*(-4+zb)*zb-zb*zb)+(1+4*s35n-5*(s35n*s35n))*(zb*zb)-2*(1+2*zb)*pow(s13,3)+(-1+(-3+s35n)*zb)*pow(s13,4)-(1+s35n-4*(s35n*s35n)+2*pow(s35n,3))*pow(zb,3)+s13*(-2+(2-6*s35n)*zb-5*(-1+s35n)*s35n*(zb*zb)+(-1+s35n)*s35n*pow(zb,3))))*pow(-1-s13+lam*zb,-3)*pow(-s13+(lam-s35n)*zb,-1))/2.;
}

// Coefficient of master 17 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<17>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<17,-1>(s13,s35n,lam,zb)
    });
}


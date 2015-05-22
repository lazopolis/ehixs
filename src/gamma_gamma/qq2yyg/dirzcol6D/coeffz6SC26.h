/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 26: box6(s24,s45,s13)

// Coefficient order epsilon^-1 of master 26
template<>
double qq2yygz6SC<26,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-3)*pow(lam,-1)*pow(s13,-1)*pow(s35n,-1)*pow(zb,-1)*((s13+s35n)*(1-2*s35n+2*s13*s35n*zb+s13*s13+s35n*s35n*(1+zb*zb))-zb*(s13*(2+(-3+2*s35n)*zb)+zb*(2+s35n-2*zb+2*zb*(s35n*s35n)))*pow(lam,4)+(1+(-1+s35n)*zb)*(zb*zb)*pow(lam,5)-lam*(-1+(-1+s35n+3*zb+3*s35n*zb)*(s13*s13)+(1+2*s35n)*(s35n*s35n)*(zb*zb)+s13*(2+3*(1+zb)*(s35n*s35n)+s35n*(-7+zb+4*(zb*zb)))+2*pow(s13,3)+zb*(1+s35n-4*(s35n*s35n)+2*pow(s35n,3))-(-1+s35n)*(s35n*s35n)*pow(zb,3))+pow(lam,3)*(1+(-1+s35n)*zb+(1+(-3+s35n)*zb)*(s13*s13)+2*s13*zb*(2+zb*(-3+s35n*s35n))+(1+2*s35n-s35n*s35n)*(zb*zb)+(-1-3*s35n+3*(s35n*s35n)+pow(s35n,3))*pow(zb,3))+lam*lam*((-2+s35n+6*zb)*(s13*s13)+s13*(1-2*zb-3*(-1+zb)*zb*(s35n*s35n)+3*(zb*zb)+s35n*(-3-zb+6*(zb*zb)))+pow(s13,3)+2*(-1+zb-s35n*zb+zb*zb*pow(s35n,3)+(s35n-pow(s35n,3))*pow(zb,3))))*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 26 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<26>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<26,-1>(s13,s35n,lam,zb)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 19: box6(s14,s34,s25)

// Coefficient order epsilon^-1 of master 19
template<>
double qq2yygz6SC<19,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-3)*((1+(-1+s35n)*zb)*(s13*s13)*(zb*zb)*pow(lam,3)+zb*(lam*lam)*(s13*s35n*zb*(1+(-1+s35n)*zb)-(2+(-2+s35n)*zb)*(s13*s13)+s35n*zb*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))+(-2+(3-2*s35n)*zb)*pow(s13,3))+(s13+s35n)*(2*s13*s35n*zb+2*(s13*s13)+s35n*s35n*(zb*zb)+2*pow(s13,3)+pow(s13,4))+lam*(2*(-1+zb-s35n*zb)*(s35n*s35n)*(zb*zb)+s13*s35n*zb*(-2+(3-5*s35n)*zb+(-1+s35n)*(zb*zb))+s13*s13*(2-2*(1+2*s35n)*zb+s35n*(zb*zb))+(2-4*zb)*pow(s13,3)+(1+(-3+s35n)*zb)*pow(s13,4)))*pow(s35n,-1)*pow(zb,-1)*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 19 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<19>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<19,-1>(s13,s35n,lam,zb)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 31: box6(s35,s45,s12)

// Coefficient order epsilon^-1 of master 31
template<>
double qq2yygz6SC<31,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(s13,-1)*pow(-1+zb,-2)*pow(zb,3)*(2-2*s35n-4*zb+7*s35n*zb+4*(s13*s13)+2*(s35n*s35n)-2*zb*(s35n*s35n)+2*(zb*zb)-5*s35n*(zb*zb)+4*(s35n*s35n)*(zb*zb)+s13*(4-5*zb+10*s35n*zb+zb*zb-2*s35n*(zb*zb))-lam*(2+(-3-2*s13+6*s35n+12*s13*s35n)*zb-(2*s13-3*s35n)*(-1+2*s35n)*(zb*zb)+(1-3*s35n+2*(s35n*s35n))*pow(zb,3))+2*(lam*lam)*(1-zb+(-1+3*s35n)*(zb*zb)+(1-3*s35n+2*(s35n*s35n))*pow(zb,3)))*pow(1+s13-lam*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1)*pow(s13+(-lam+s35n)*zb,-1))/2.;
}

// Coefficient of master 31 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<31>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<31,-1>(s13,s35n,lam,zb)
    });
}


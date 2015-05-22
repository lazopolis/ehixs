/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 21: box6(s15,s35,s24)

// Coefficient order epsilon^-1 of master 21
template<>
double qq2yygz6SC<21,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(s13,-3)*((-1+zb-s35n*zb)*(s13*s13)+(-1+zb)*(s35n*s35n)+s13*(-1+zb)*(s35n*s35n)+lam*(-2*s35n*(-1+zb)*(1+(-1+s35n)*zb)+(-2+2*s35n+2*zb-s35n*zb)*(s13*s13)-s13*(-1+zb)*(-2+4*s35n+2*zb-3*s35n*zb+zb*(s35n*s35n)))+zb*(1+(-1+s35n)*zb)*(s13*s13)*pow(lam,3)-pow(s13,3)+lam*lam*(s13*(-1+zb)*(1+(-1+s35n)*zb)-(2+(-2+s35n)*zb)*(s13*s13)+(-1+zb)*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))+(-2+zb)*pow(s13,3)))*pow(-1+zb,-1)*pow(zb,3)*pow(1+s13-lam*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1)*pow(s13+(-lam+s35n)*zb,-1))/2.;
}

// Coefficient of master 21 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<21>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<21,-1>(s13,s35n,lam,zb)
    });
}


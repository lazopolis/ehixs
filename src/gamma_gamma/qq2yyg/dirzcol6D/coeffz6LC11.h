/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 11: box6(s14,s34,s25)

// Coefficient order epsilon^-1 of master 11
template<>
double qq2yygz6LC<11,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return -(pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-3)*(s13*s13*(4-2*(1+2*lam)*zb+(1+2*(lam*lam))*(zb*zb))+2*s13*zb*(s35n+lam*(-1-2*(-1+s35n)*zb+(-1+s35n)*(zb*zb)))+zb*zb*((s35n+lam*(-1+zb-s35n*zb))*(s35n+lam*(-1+zb-s35n*zb)))-4*(-1+lam*zb)*pow(s13,3)+2*pow(s13,4))*pow(zb,-1)*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 11 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6LC<11>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6LC<11,-1>(s13,s35n,lam,zb)
    });
}


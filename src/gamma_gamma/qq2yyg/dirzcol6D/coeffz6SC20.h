/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 20: box6(s14,s45,s23)

// Coefficient order epsilon^-1 of master 20
template<>
double qq2yygz6SC<20,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-3)*pow(s13,-1)*(-(lam*s35n*(s13*(1-s35n*(-3+zb)+zb)+s35n*(3+(-1+s35n)*zb)+s13*s13))+s35n*((s13+s35n)*(s13+s35n))-(2-2*zb+4*s35n*zb+s13*(2+4*(-1+s35n)*zb)+(1+(-3+s35n)*zb)*(s13*s13))*pow(lam,3)+zb*(2-2*zb+3*s35n*zb+s13*(2-3*zb+2*s35n*zb))*pow(lam,4)-(1+(-1+s35n)*zb)*(zb*zb)*pow(lam,5)+lam*lam*(2*s35n*(2+(-1+s35n)*zb)+(-2+s35n)*(s13*s13)+s13*(-2+s35n*(5+zb)-zb*(s35n*s35n))-pow(s13,3)))*pow(s35n,-1)*pow(zb,-1)*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 20 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<20>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<20,-1>(s13,s35n,lam,zb)
    });
}


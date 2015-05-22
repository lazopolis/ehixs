/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 18: box6(s13,s35,s24)

// Coefficient order epsilon^-1 of master 18
template<>
double qq2yygz6SC<18,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-3)*(-(lam*(-1+s35n)*(s13*(-2+3*s35n+2*zb-s35n*zb)+s13*s13+zb*(s35n*s35n)))+(-1+s35n)*((s13+s35n)*(s13+s35n))+((1+zb-s35n*zb)*(s13*s13)+(1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))*pow(lam,3)-lam*lam*(-((-2+s35n)*(s13*s13))+s13*(-2+3*s35n+2*zb-3*s35n*zb+zb*(s35n*s35n))+(1+(-1+s35n)*zb)*(1+(-1+s35n)*zb)+pow(s13,3)))*pow(-1+s35n,-1)*pow(zb,-1)*pow(-1-s13+lam*zb,-1)*pow(-s13+(lam-s35n)*zb,-1))/2.;
}

// Coefficient of master 18 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<18>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<18,-1>(s13,s35n,lam,zb)
    });
}


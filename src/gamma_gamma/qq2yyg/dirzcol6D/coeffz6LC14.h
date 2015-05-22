/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 14: box6(s23,s34,s15)

// Coefficient order epsilon^-1 of master 14
template<>
double qq2yygz6LC<14,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return -(pow(-1+lam,-1)*pow(lam,-1)*(-(s13*s13*(4-2*(3+4*lam-6*s35n)*zb+(lam*(8-12*s35n)+2*(lam*lam)+3*((1-2*s35n)*(1-2*s35n)))*(zb*zb)))-2*s13*zb*(2*zb*(1+(-1+s35n)*zb)*(lam*lam)+s35n*(3+(-4+6*s35n)*zb+(2-6*s35n+4*(s35n*s35n))*(zb*zb))-lam*(3+2*(-3+5*s35n)*zb+(3-9*s35n+6*(s35n*s35n))*(zb*zb)))-zb*zb*(s35n*s35n*(3+4*(-1+s35n)*zb+2*((-1+s35n)*(-1+s35n))*(zb*zb))-2*lam*s35n*(3+5*(-1+s35n)*zb+2*((-1+s35n)*(-1+s35n))*(zb*zb))+3*(lam*lam)*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb)))+4*(-1+(1+lam-2*s35n)*zb)*pow(s13,3)-2*pow(s13,4))*pow(zb,-1)*pow(-1-s13+lam*zb,-1)*pow(s13+(-lam+s35n)*zb,-3))/2.;
}

// Coefficient of master 14 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6LC<14>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6LC<14,-1>(s13,s35n,lam,zb)
    });
}


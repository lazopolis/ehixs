/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 9: box6(s13,s34,s25)

// Coefficient order epsilon^-1 of master 9
template<>
double qq2yygz6LC<9,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return -(pow(-1+lam,-1)*pow(lam,-1)*pow(zb,-1)*(2-2*(2+lam-s35n)*zb+(2+4*lam-2*s35n-4*lam*s35n+lam*lam+s35n*s35n)*(zb*zb)+s13*s13*(4-2*(1+2*lam)*zb+(1+2*(lam*lam))*(zb*zb))+(4-4*lam*zb)*pow(s13,3)+2*pow(s13,4)+2*lam*(1+lam-s35n)*(-1+s35n)*pow(zb,3)+2*s13*(2+(-3-lam+s35n)*zb+(1+2*lam-2*lam*s35n)*(zb*zb)+lam*(-1+s35n)*pow(zb,3))+lam*lam*((-1+s35n)*(-1+s35n))*pow(zb,4))*pow(-1-s13+lam*zb,-3)*pow(-s13+(lam-s35n)*zb,-1))/2.;
}

// Coefficient of master 9 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6LC<9>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6LC<9,-1>(s13,s35n,lam,zb)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 12: box6(s15,s25,s34)

// Coefficient order epsilon^-1 of master 12
template<>
double qq2yygz6LC<12,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-1)*pow(zb,-1)*(-((lam-s35n)*zb*(-1+lam*zb)*(1-2*lam*zb*(1+s35n*zb)+2*(lam*lam)*(zb*zb)+s35n*s35n*(zb*zb)))-3*(s13*s13)*(2+(-2-4*lam+4*s35n)*zb+(1-2*s35n-4*lam*s35n+4*(lam*lam)+2*(s35n*s35n))*(zb*zb))+4*(-2+zb+2*lam*zb-2*s35n*zb)*pow(s13,3)-4*pow(s13,4)+s13*(-2+(3+6*lam-6*s35n)*zb-3*(1-2*s35n-4*lam*s35n+4*(lam*lam)+2*(s35n*s35n))*(zb*zb)+(1-3*s35n-12*s35n*(lam*lam)+3*(s35n*s35n)+6*lam*(s35n*s35n)+8*pow(lam,3)-2*pow(s35n,3))*pow(zb,3)))*pow(-1-s13+lam*zb,-1)*pow(-s13+(lam-s35n)*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1);
}

// Coefficient of master 12 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6LC<12>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6LC<12,-1>(s13,s35n,lam,zb)
    });
}


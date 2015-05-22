/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 24: box6(s23,s35,s14)

// Coefficient order epsilon^-1 of master 24
template<>
double qq2yygz6SC<24,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-3)*pow(lam,-1)*pow(-1+s35n,-1)*pow(zb,-1)*((s13+s35n)*(-2*s35n*(-1+zb)*zb+2*s13*(1+(-1+s35n)*zb)+s13*s13+(-1+zb)*(-1+zb)+s35n*s35n*(1+zb*zb))+((-1+(-1+s35n)*zb)*(s13*s13)+2*s13*(-1+(-1+s35n)*(-1+s35n)*(zb*zb))+(-2+(-1+s35n)*zb)*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb)))*pow(lam,3)-lam*((6+s35n-3*zb+3*s35n*zb)*(s13*s13)+s13*(4-4*zb+s35n*(3+zb)+3*(1+zb)*(s35n*s35n))+(1+zb)*((-1+zb)*(-1+zb))+2*pow(s13,3)+zb*(2+2*zb-zb*zb)*pow(s35n,3)+s35n*(2-3*zb+4*(zb*zb)-3*pow(zb,3))+s35n*s35n*(3+2*zb-5*(zb*zb)+3*pow(zb,3)))+lam*lam*((4+s35n)*(s13*s13)+s13*(3-3*(-1+zb)*zb*(s35n*s35n)-3*(zb*zb)+s35n*(5-3*zb+6*(zb*zb)))+pow(s13,3)-2*(-((1+zb)*((-1+zb)*(-1+zb)))-3*zb*(s35n*s35n)*(1-zb+zb*zb)+(-1+zb)*(zb*zb)*pow(s35n,3)+s35n*(-2+2*zb-3*(zb*zb)+3*pow(zb,3)))))*pow(-1-s13+lam*zb,-1)*pow(-s13+(lam-s35n)*zb,-1))/2.;
}

// Coefficient of master 24 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<24>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<24,-1>(s13,s35n,lam,zb)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 22: box6(s15,s45,s23)

// Coefficient order epsilon^-1 of master 22
template<>
double qq2yygz6SC<22,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return -(pow(s13,-1)*(s13*s13*(-2-s35n*zb+lam*(-(s35n*(-2+zb))+2*zb)-(4+(-2+s35n)*zb)*(lam*lam)+zb*(5+(-3+s35n)*zb)*pow(lam,3))+zb*(-s35n+lam*(1+(-1+s35n)*zb))*(1-2*lam+2*(lam*lam)-2*zb*pow(lam,3)+zb*zb*pow(lam,4))+s13*(-2+s35n*(2-4*zb)+zb+(-1+zb)*(s35n*s35n)+lam*(2+s35n*zb*(3+zb)-(-1+zb)*zb*(s35n*s35n))+lam*lam*(-3+(2-5*s35n)*zb+(-1+s35n)*(zb*zb))+2*zb*(3+2*(-1+s35n)*zb)*pow(lam,3)+(-4+(3-2*s35n)*zb)*(zb*zb)*pow(lam,4))+(-1+(-2+zb)*(lam*lam))*pow(s13,3))*pow(-1+zb,-1)*pow(zb,3)*pow(1+s13-lam*zb,-3)*pow(1+s13+(-1+s35n)*zb,-1)*pow(s13+(-lam+s35n)*zb,-1))/2.;
}

// Coefficient of master 22 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<22>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<22,-1>(s13,s35n,lam,zb)
    });
}


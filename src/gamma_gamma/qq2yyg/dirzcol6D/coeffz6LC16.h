/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 16: box6(s24,s34,s15)

// Coefficient order epsilon^-1 of master 16
template<>
double qq2yygz6LC<16,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return -(pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-1)*pow(zb,-1)*(2-2*(2+lam-s35n)*zb+(2+lam*(4-8*s35n)-2*s35n+3*(lam*lam)+3*(s35n*s35n))*(zb*zb)+s13*s13*(4-2*(3+4*lam-6*s35n)*zb+(lam*(8-12*s35n)+2*(lam*lam)+3*((1-2*s35n)*(1-2*s35n)))*(zb*zb))-4*(-1+(1+lam-2*s35n)*zb)*pow(s13,3)+2*pow(s13,4)+2*(-1+s35n)*(lam-5*lam*s35n+3*(lam*lam)+2*(s35n*s35n))*pow(zb,3)+2*s13*(2-3*(1+lam-s35n)*zb+(1+6*lam-4*s35n-10*lam*s35n+2*(lam*lam)+6*(s35n*s35n))*(zb*zb)+(-1+s35n)*(3*lam-2*s35n-6*lam*s35n+2*(lam*lam)+4*(s35n*s35n))*pow(zb,3))+(-1+s35n)*(-1+s35n)*(-4*lam*s35n+3*(lam*lam)+2*(s35n*s35n))*pow(zb,4))*pow(1+s13+(-1+s35n)*zb,-3))/2.;
}

// Coefficient of master 16 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6LC<16>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6LC<16,-1>(s13,s35n,lam,zb)
    });
}


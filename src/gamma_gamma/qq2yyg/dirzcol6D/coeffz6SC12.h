/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 12: box6(s12,s14,s35)

// Coefficient order epsilon^-1 of master 12
template<>
double qq2yygz6SC<12,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return -(pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-1)*pow(-1+s35n,-1)*pow(-1+zb,-1)*pow(zb,-1)*(s13*(s13+s35n)*(-1+zb)*(s13*s13*(4+(-6+9*s35n)*zb+(3-9*s35n+6*(s35n*s35n))*(zb*zb))+(3+(-3+4*s35n)*zb)*pow(s13,3)+pow(s13,4)+s13*(2+(-4+8*s35n)*zb+3*(1-4*s35n+3*(s35n*s35n))*(zb*zb)+(-1+4*s35n)*((-1+s35n)*(-1+s35n))*pow(zb,3))+s35n*zb*(2+4*(-1+s35n)*zb+3*((-1+s35n)*(-1+s35n))*(zb*zb)+pow(-1+s35n,3)*pow(zb,3)))-(-1+s35n)*pow(lam,3)*(1-zb+2*s13*(s35n+4*s35n*zb+(5-3*zb)*zb)-3*(-2+zb)*(s13*s13)+zb*zb+zb*(s35n*s35n)*(1+3*zb+3*(zb*zb))+s35n*(2+6*(zb*zb)-2*pow(zb,3))-pow(zb,3))*pow(zb,4)+(-1+s35n)*pow(lam,4)*(1+(-1+s35n)*zb+s13*zb*(9-6*zb+s35n*(3+2*zb))-(-2+zb)*(s13*s13)+(3+6*s35n+s35n*s35n)*(zb*zb)+3*(-1+s35n*s35n)*pow(zb,3))*pow(zb,4)-(-1+s35n)*(s13*(3+(-2+s35n)*zb)+zb*(3-3*zb+2*s35n*(1+zb)+zb*(s35n*s35n)))*pow(lam,5)*pow(zb,5)+zb*(lam*lam)*(-3*(-1+zb)*(-1+(-1+s35n)*(-1+s35n)*(zb*zb))*pow(s13,3)+(-1-s35n*(-1+zb)*zb+zb*zb)*pow(s13,4)+s35n*pow(zb,3)*(-2+zb-2*(zb*zb)+pow(zb,3)+s35n*s35n*(1+zb+3*(zb*zb)+pow(zb,3))-s35n*(-1+2*zb+zb*zb+2*pow(zb,3)))+s13*s13*(-4+(9-5*s35n)*zb+(-3+s35n+2*(s35n*s35n))*(zb*zb)+(-12+20*s35n-11*(s35n*s35n)+3*pow(s35n,3))*pow(zb,3)-3*(-2+4*s35n-3*(s35n*s35n)+pow(s35n,3))*pow(zb,4))+s13*(-2+(7-5*s35n)*zb+(-8+11*s35n-3*(s35n*s35n))*(zb*zb)+(2-7*s35n+4*(s35n*s35n)+pow(s35n,3))*pow(zb,3)+(-2-11*s35n+17*(s35n*s35n)-5*pow(s35n,3)+pow(s35n,4))*pow(zb,4)+(1+4*s35n-8*(s35n*s35n)+4*pow(s35n,3)-pow(s35n,4))*pow(zb,5)))+lam*((-1+zb)*(-3-(3+2*s35n)*zb+2*(3-5*s35n+2*(s35n*s35n))*(zb*zb))*pow(s13,4)+(-1+zb)*(-1+(-2+s35n)*zb)*pow(s13,5)+2*(-1+zb)*pow(s13,3)*(-2+zb-6*s35n*zb-4*(-1+s35n)*(zb*zb)+3*pow(-1+s35n,3)*pow(zb,3))-(-1+s35n)*(s35n*s35n)*(1+zb*zb)*pow(zb,4)+s13*s35n*zb*(4+(-13+9*s35n)*zb+(14-19*s35n+5*(s35n*s35n))*(zb*zb)-(2-5*s35n+2*(s35n*s35n)+pow(s35n,3))*pow(zb,3)+(1+4*s35n-9*(s35n*s35n)+5*pow(s35n,3)-pow(s35n,4))*pow(zb,4)+s35n*(-3+6*s35n-4*(s35n*s35n)+pow(s35n,3))*pow(zb,5))+s13*s13*(2+2*(-3+7*s35n)*zb+2*(2-14*s35n+7*(s35n*s35n))*(zb*zb)-2*(-2-2*s35n+3*(s35n*s35n)+pow(s35n,3))*pow(zb,3)+(-3+17*s35n-26*(s35n*s35n)+16*pow(s35n,3)-4*pow(s35n,4))*pow(zb,4)+(1-9*s35n+18*(s35n*s35n)-14*pow(s35n,3)+4*pow(s35n,4))*pow(zb,5)))+(-1+s35n)*(1+(-1+s35n)*zb)*pow(lam,6)*pow(zb,6))*pow(-1-s13+lam*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1)*pow(s13+(-lam+s35n)*zb,-2))/2.;
}

// Coefficient of master 12 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<12>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<12,-1>(s13,s35n,lam,zb)
    });
}


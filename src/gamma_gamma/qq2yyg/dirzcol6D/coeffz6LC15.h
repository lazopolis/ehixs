/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 15: box6(s24,s25,s13)

// Coefficient order epsilon^-1 of master 15
template<>
double qq2yygz6LC<15,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-1)*pow(-1+s35n,-3)*pow(-1+zb,-1)*pow(zb,-1)*((-1+zb)*(2-2*s35n+s35n*s35n)*pow(s13,5)+(-1+zb)*pow(s13,4)*(7-4*zb+s35n*(-7+6*zb)+(5-3*zb)*(s35n*s35n)+(-1+zb)*pow(s35n,3)+lam*(-1-2*zb+s35n*(-1+4*zb)-(1+3*zb)*(s35n*s35n)+zb*pow(s35n,3)))+(-1+zb)*pow(s13,3)*(8-11*zb+s35n*(3+(-1+s35n)*zb)*(lam*lam)+s35n*(-5+16*zb-6*(zb*zb))+3*(zb*zb)+s35n*s35n*(4-9*zb+3*(zb*zb))+(-1+5*zb)*pow(s35n,3)-zb*pow(s35n,4)+lam*(-1-3*zb+s35n*(-10+13*zb-10*(zb*zb))+4*(zb*zb)+s35n*s35n*(2-10*zb+9*(zb*zb))-4*(zb*zb)*pow(s35n,3)+zb*zb*pow(s35n,4)))+(-1+zb)*(s13*s13)*(-((-5+zb)*((-1+zb)*(-1+zb)))-(1+(-1+s35n)*zb)*(1+(-1+s35n)*zb)*pow(lam,3)+lam*lam*(zb*(4+zb)*(s35n*s35n)+3*((-1+zb)*(-1+zb))+s35n*(3+2*zb-5*(zb*zb))+zb*zb*pow(s35n,3))+3*s35n*(-1+5*zb-5*(zb*zb)+pow(zb,3))-3*(s35n*s35n)*(-1+2*zb-3*(zb*zb)+pow(zb,3))+pow(s35n,3)*(-1+2*zb-zb*zb+pow(zb,3))+lam*(-3*(1+zb)*((-1+zb)*(-1+zb))+zb*(4-9*zb+3*(zb*zb))*pow(s35n,3)+s35n*s35n*(3-22*zb+21*(zb*zb)-9*pow(zb,3))+3*s35n*(-3+5*zb-5*(zb*zb)+3*pow(zb,3))))-lam*(-2+(2+lam-s35n)*zb+lam*(-1+s35n)*(zb*zb))*(-1+3*lam-4*(lam*lam)+2*pow(lam,3))*pow(-1+s35n,3)*pow(zb,4)+s13*(-((-1+zb)*(-(s35n*(-3+7*zb)*((-1+zb)*(-1+zb)))+zb*((-1+zb)*(-1+zb))*pow(s35n,4)+2*pow(-1+zb,3)+pow(s35n,3)*(1-6*zb+9*(zb*zb)-5*pow(zb,3))+3*(s35n*s35n)*(-1+4*zb-6*(zb*zb)+3*pow(zb,3))))-2*(-2+zb)*pow(lam,4)*pow(-1+s35n,3)*pow(zb,4)-3*(lam*lam)*(1+(-4+3*s35n)*zb+3*(2-3*s35n+s35n*s35n)*(zb*zb)+(-4+s35n)*((-1+s35n)*(-1+s35n))*pow(zb,3)-3*pow(-1+s35n,3)*pow(zb,4)+pow(-1+s35n,3)*pow(zb,5))+pow(lam,3)*(1+(-4+3*s35n)*zb+3*(2-3*s35n+s35n*s35n)*(zb*zb)+(-4+s35n)*((-1+s35n)*(-1+s35n))*pow(zb,3)-9*pow(-1+s35n,3)*pow(zb,4)+4*pow(-1+s35n,3)*pow(zb,5))+lam*(3-zb*(11-6*s35n-3*(s35n*s35n)+pow(s35n,3))-zb*zb*(-14+14*s35n+6*(s35n*s35n)-7*pow(s35n,3)+pow(s35n,4))+3*((-1+s35n)*(-1+s35n))*(-2-2*s35n+s35n*s35n)*pow(zb,3)-(1+3*s35n)*pow(-1+s35n,3)*pow(zb,4)+s35n*pow(-1+s35n,3)*pow(zb,5))))*pow(-1-s13+lam*zb,-1)*pow(-s13+(lam-s35n)*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 15 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6LC<15>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6LC<15,-1>(s13,s35n,lam,zb)
    });
}


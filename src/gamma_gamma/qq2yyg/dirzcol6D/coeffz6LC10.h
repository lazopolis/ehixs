/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 10: box6(s14,s15,s23)

// Coefficient order epsilon^-1 of master 10
template<>
double qq2yygz6LC<10,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return (pow(-1+lam,-1)*pow(lam,-1)*pow(s13,-1)*pow(-1+s35n,-3)*pow(-1+zb,-1)*pow(zb,-1)*(-(s13*(s13+s35n)*(-1+zb)*(s35n*(-2+5*zb)*((-1+zb)*(-1+zb))+(2-2*s35n+s35n*s35n)*pow(s13,3)+2*zb*(2-6*zb+5*(zb*zb))*pow(s35n,3)+s13*s13*(4-4*zb+2*s35n*(-2+5*zb)+(3-9*zb)*(s35n*s35n)+3*zb*pow(s35n,3))+(3-5*zb)*(zb*zb)*pow(s35n,4)+s13*(3*((-1+zb)*(-1+zb))-4*s35n*(1-4*zb+3*(zb*zb))+2*(s35n*s35n)*(2-8*zb+9*(zb*zb))+6*(1-2*zb)*zb*pow(s35n,3)+3*(zb*zb)*pow(s35n,4))-pow(-1+zb,3)+pow(s35n,5)*pow(zb,3)-2*(s35n*s35n)*(-1+5*zb-9*(zb*zb)+5*pow(zb,3))))-2*(-(s13*(-2+zb))+zb*(2+s35n-2*zb+2*s35n*zb))*pow(lam,4)*pow(-1+s35n,3)*pow(zb,4)+2*(1+(-1+s35n)*zb)*pow(lam,5)*pow(-1+s35n,3)*pow(zb,5)+pow(lam,3)*((-1+zb)*(s13*s13)*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))+(3-3*zb+s35n*(4+3*zb))*pow(-1+s35n,3)*pow(zb,5)-s13*(1+(-4+3*s35n)*zb+3*(2-3*s35n+s35n*s35n)*(zb*zb)+(-4+s35n)*((-1+s35n)*(-1+s35n))*pow(zb,3)-9*pow(-1+s35n,3)*pow(zb,4)+4*pow(-1+s35n,3)*pow(zb,5)))+lam*lam*(-2*s35n*(-1+zb)*(s13*s13)*(3+4*(-1+s35n)*zb+(-1+s35n)*(-1+s35n)*(zb*zb))-s35n*(-1+zb)*(3+(-1+s35n)*zb)*pow(s13,3)-(1-zb+s35n*(3+zb))*pow(-1+s35n,3)*pow(zb,5)+s13*(-((-1+zb)*pow(s35n,4)*pow(zb,3))+zb*zb*pow(s35n,3)*(5-8*zb-3*(zb*zb)+3*pow(zb,3))+zb*(s35n*s35n)*(7-17*zb+13*(zb*zb)+15*pow(zb,3)-9*pow(zb,4))-3*(-2+zb)*pow(zb,4)+s35n*(3-10*zb+12*(zb*zb)-6*pow(zb,3)-17*pow(zb,4)+9*pow(zb,5))))-lam*(pow(s13,4)*(1+zb+s35n*s35n*(1+2*zb-3*(zb*zb))-2*(zb*zb)+s35n*(1-5*zb+4*(zb*zb))+(-1+zb)*zb*pow(s35n,3))+pow(s13,3)*((3+4*zb)*((-1+zb)*(-1+zb))+s35n*zb*(-5+19*zb-14*(zb*zb))-12*(-1+zb)*(zb*zb)*pow(s35n,3)+3*(-1+zb)*(zb*zb)*pow(s35n,4)+s35n*s35n*(6-2*zb-23*(zb*zb)+19*pow(zb,3)))+s13*s13*(s35n*((-1+zb)*(-1+zb))*(-3+2*zb+15*(zb*zb))-3*(zb*zb)*(1-6*zb+5*(zb*zb))*pow(s35n,4)-3*(1+zb)*pow(-1+zb,3)+3*(-1+zb)*pow(s35n,5)*pow(zb,3)+zb*pow(s35n,3)*(8+5*zb-43*(zb*zb)+30*pow(zb,3))+s35n*s35n*(9-19*zb-10*(zb*zb)+50*pow(zb,3)-30*pow(zb,4)))-s35n*pow(-1+s35n,3)*pow(zb,5)+s13*(1-3*zb+2*(zb*zb)+2*pow(zb,3)-2*(1-4*zb+3*(zb*zb))*pow(s35n,5)*pow(zb,3)+zb*zb*pow(s35n,4)*(2+8*zb-25*(zb*zb)+15*pow(zb,3))+zb*pow(s35n,3)*(7-11*zb-16*(zb*zb)+38*pow(zb,3)-19*pow(zb,4))-pow(zb,4)+(-1+zb)*pow(s35n,6)*pow(zb,4)-s35n*(2-8*zb+6*(zb*zb)+10*pow(zb,3)-10*pow(zb,4)+3*pow(zb,5))+s35n*s35n*(4-15*zb+13*(zb*zb)+18*pow(zb,3)-29*pow(zb,4)+12*pow(zb,5)))))*pow(-1-s13+lam*zb,-1)*pow(-s13+(lam-s35n)*zb,-1)*pow(1+s13+(-1+s35n)*zb,-1))/2.;
}

// Coefficient of master 10 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6LC<10>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6LC<10,-1>(s13,s35n,lam,zb)
    });
}


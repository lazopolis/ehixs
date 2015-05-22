/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 23: box6(s23,s34,s15)

// Coefficient order epsilon^-1 of master 23
template<>
double qq2yygz6SC<23,-1>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return -(pow(-1+lam,-1)*pow(lam,-1)*pow(-1+s35n,-1)*pow(zb,-1)*((-6+6*zb+zb*(-2+(-3+2*s35n)*zb)*(lam*lam)+2*(2-3*zb)*zb*(s35n*s35n)-zb*zb+2*s35n*(1-8*zb+3*(zb*zb))+lam*(2+8*zb-2*(3-6*s35n+2*(s35n*s35n))*(zb*zb)))*pow(s13,3)+(-4+lam+s35n+2*zb+3*lam*zb-4*s35n*zb-lam*s35n*zb)*pow(s13,4)-pow(s13,5)+s13*s13*(-4+6*zb-3*(zb*zb)+6*zb*(s35n*s35n)*(1-4*zb+zb*zb)+zb*(lam*lam)*(-4-(4+s35n)*zb+6*((-1+s35n)*(-1+s35n))*(zb*zb))+(1+zb-s35n*zb)*(zb*zb)*pow(lam,3)+2*(3-2*zb)*(zb*zb)*pow(s35n,3)-2*s35n*(-1+10*zb-9*(zb*zb)+pow(zb,3))+lam*(2+4*(2+s35n)*zb+(-12+25*s35n-6*(s35n*s35n))*(zb*zb)-3*(-1+5*s35n-6*(s35n*s35n)+2*pow(s35n,3))*pow(zb,3)))+zb*zb*(-((-2+(-1+s35n)*zb)*((1+(-1+s35n)*zb)*(1+(-1+s35n)*zb))*pow(lam,3))+s35n*s35n*(-3+4*zb+2*(1-2*zb)*zb*(s35n*s35n)-2*(zb*zb)+s35n*(1-6*zb+5*(zb*zb))+zb*zb*pow(s35n,3))+lam*s35n*(6+zb*(-10+13*s35n-3*(s35n*s35n))-(-4+3*s35n)*((-1+s35n)*(-1+s35n))*(zb*zb)-s35n*pow(-1+s35n,3)*pow(zb,3))+lam*lam*(-3*((-1+zb)*(-1+zb))+zb*(s35n*s35n)*(-2-9*zb+6*(zb*zb))+3*(1-2*zb)*(zb*zb)*pow(s35n,3)+2*pow(s35n,4)*pow(zb,3)-s35n*(3+4*zb-9*(zb*zb)+2*pow(zb,3))))+s13*zb*(-2*zb*(-1+(-1+s35n)*(-1+s35n)*(zb*zb))*pow(lam,3)+lam*lam*(-4+zb-9*s35n*zb+3*(2-3*s35n+s35n*s35n)*(zb*zb)+3*(-1+2*s35n)*((-1+s35n)*(-1+s35n))*pow(zb,3))-s35n*(6-8*zb+4*(zb*zb)-2*zb*(s35n*s35n)*(3-8*zb+zb*zb)+(-4+zb)*(zb*zb)*pow(s35n,3)+s35n*(-2+18*zb-16*(zb*zb)+pow(zb,3)))+lam*(6*((-1+zb)*(-1+zb))+zb*(s35n*s35n)*(-3+29*zb-12*(zb*zb))+4*(-2+3*zb)*(zb*zb)*pow(s35n,3)-4*pow(s35n,4)*pow(zb,3)+s35n*(2+25*zb-27*(zb*zb)+4*pow(zb,3)))))*pow(-1-s13+lam*zb,-1)*pow(s13+(-lam+s35n)*zb,-3))/2.;
}

// Coefficient of master 23 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6SC<23>(const double& s13, const double& s35n, const double& lam, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6SC<23,-1>(s13,s35n,lam,zb)
    });
}


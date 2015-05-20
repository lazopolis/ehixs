/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 20: box(s14,s12+s13+s23,s23)

// Coefficient order epsilon^-1 of master 20
template<>
double qq2yygz6CAm2CF<20,-1>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return ((s15n+s25n)*pow(s12,-1)*pow(s13,-1)*pow(s15n,-3)*pow(s25n,-1)*pow(s35n,-1)*pow(s15n+s25n+s35n,-1)*pow(zb,-1)*(zb*zb*(8*s12*s13+(9*s12+7*s13)*s35n*zb+s25n*zb*(5*s12+6*s13+4*s35n*zb)+4*(s12*s12)+3*(s13*s13)+s25n*s25n*(zb*zb)+3*(s35n*s35n)*(zb*zb))*pow(s15n,6)+s15n*s35n*(-(s13*s25n)+s12*s35n)*(zb*(s13*(-s12+4*s13)+(s12+4*s13)*s35n*zb)*(s25n*s25n)+s12*s35n*(2*s13*(2*s12+s13)+(5*s12+3*s13)*s35n*zb+s35n*s35n*(zb*zb))+s25n*(-2*s12*(s13*s13)+s35n*zb*(-(s12*s13)+4*(s12*s12)+2*(s13*s13))+2*(s12+s13)*(s35n*s35n)*(zb*zb))+2*s13*(zb*zb)*pow(s25n,3))+s35n*(s13+s25n*zb+s35n*zb)*pow(-(s13*s25n)+s12*s35n,3)+(2*s25n*zb+3*(s12+s13+s35n*zb))*pow(s15n,7)*pow(zb,3)+zb*pow(s15n,5)*(8*s13*(s12*s12)+7*s12*(s13*s13)+s35n*zb*(20*s12*s13+14*(s12*s12)+5*(s13*s13))+(2*s12+3*s13+s35n*zb)*(s25n*s25n)*(zb*zb)+(9*s12+5*s13)*(s35n*s35n)*(zb*zb)+s25n*zb*(14*s12*s13+2*(5*s12+4*s13)*s35n*zb+6*(s12*s12)+7*(s13*s13)+2*(s35n*s35n)*(zb*zb))+2*pow(s12,3)+pow(s13,3)+pow(s35n,3)*pow(zb,3))+pow(s15n,4)*(2*s12*s13*((s12+s13)*(s12+s13))+(6*s12*s13+2*s12*s35n*zb-s13*s35n*zb+2*(s12*s12)+4*(s13*s13))*(s25n*s25n)*(zb*zb)+(17*s12*s13+19*(s12*s12)+2*(s13*s13))*(s35n*s35n)*(zb*zb)+s35n*zb*(23*s13*(s12*s12)+14*s12*(s13*s13)+8*pow(s12,3)+pow(s13,3))+s25n*zb*(s35n*zb*(23*s12*s13+16*(s12*s12)+7*(s13*s13))+5*s12*(s35n*s35n)*(zb*zb)+2*(5*s13*(s12*s12)+6*s12*(s13*s13)+pow(s12,3)+2*pow(s13,3)))+(3*s12+s13)*pow(s35n,3)*pow(zb,3))+pow(s15n,3)*(zb*(s25n*s25n)*(s35n*zb*(6*s12*s13+4*(s12*s12)-3*(s13*s13))+s13*(4*s12*s13+2*(s12*s12)+3*(s13*s13))-6*s13*(s35n*s35n)*(zb*zb))+s12*s35n*(3*s13*(3*s12*s13+2*(s12*s12)+s13*s13)+s35n*zb*(25*s12*s13+13*(s12*s12)+9*(s13*s13))+(13*s12+6*s13)*(s35n*s35n)*(zb*zb))-3*s13*s35n*pow(s25n,3)*pow(zb,3)+s25n*(s13*s13*(3*s12*s13+2*(s12*s12)+s13*s13)+4*s12*(4*s12+3*s13)*(s35n*s35n)*(zb*zb)+2*s35n*zb*(7*s13*(s12*s12)+6*s12*(s13*s13)+3*pow(s12,3)+3*pow(s13,3))-3*s13*pow(s35n,3)*pow(zb,3)))+s15n*s15n*(s12*(s35n*s35n)*(s13*(7*s12*s13+7*(s12*s12)+s13*s13)+s35n*zb*(13*s12*s13+11*(s12*s12)+2*(s13*s13))+(5*s12+s13)*(s35n*s35n)*(zb*zb))+s13*s35n*(s12-6*s13-3*s35n*zb)*(zb*zb)*pow(s25n,3)-s13*s35n*pow(s25n,4)*pow(zb,3)+s25n*s25n*(-(s13*s35n*zb*(-(s12*s13)+2*(s12*s12)+s13*s13))+(4*s12*s13+3*(s12*s12)-7*(s13*s13))*(s35n*s35n)*(zb*zb)+pow(s13,4)-3*s13*pow(s35n,3)*pow(zb,3))+s25n*s35n*(-3*(s12*s12)*(s13*s13)+(4*s12*s13+8*(s12*s12)-s13*s13)*(s35n*s35n)*(zb*zb)+s35n*zb*(s13*(s12*s12)+s12*(s13*s13)+7*pow(s12,3)+3*pow(s13,3))+3*pow(s13,4)-s13*pow(s35n,3)*pow(zb,3)))+pow(s15n,8)*pow(zb,4))*pow(s13+s15n*zb+s35n*zb,-1)*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-1))/2.;
}

// Coefficient of master 20 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6CAm2CF<20>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6CAm2CF<20,-1>(s12,s13,s15n,s25n,s35n,zb)
    });
}


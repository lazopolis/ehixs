/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 12: box(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24)

// Coefficient order epsilon^-1 of master 12
template<>
double qq2yygz6CA<12,-1>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return ((s15n+s25n)*pow(s12,-1)*pow(s13,-1)*pow(s15n,-1)*pow(s25n,-1)*pow(zb,-1)*(s12*(s25n*s25n)*(zb*zb)*(-(s35n*zb*(8*s12*s13+s12*s12+4*(s13*s13)))-s13*(17*s12*s13+8*(s12*s12)+8*(s13*s13))+2*(s12-s13)*(s35n*s35n)*(zb*zb))-s13*(5*s12*s13+2*s12*s35n*zb+3*(s12*s12)+2*(s13*s13))*pow(s25n,3)*pow(zb,3)+pow(s15n,3)*pow(zb,3)*(-55*s13*(s12*s12)-41*s12*(s13*s13)-s35n*zb*(32*s12*s13+19*(s12*s12)+2*(s13*s13))+(5*s12+8*s13+8*s35n*zb)*(s25n*s25n)*(zb*zb)+(3*s12+2*s13)*(s35n*s35n)*(zb*zb)+s25n*zb*(-15*s12*s13+3*(3*s12+4*s13)*s35n*zb-6*(s12*s12)+s13*s13+9*(s35n*s35n)*(zb*zb))-20*pow(s12,3)-2*pow(s13,3)+pow(s25n,3)*pow(zb,3)+2*pow(s35n,3)*pow(zb,3))-s12*s25n*zb*((16*s12*s13-s12*s12+12*(s13*s13))*(s35n*s35n)*(zb*zb)+2*s35n*zb*(10*s13*(s12*s12)+17*s12*(s13*s13)+2*pow(s12,3)+8*pow(s13,3))+s13*(26*s13*(s12*s12)+26*s12*(s13*s13)+9*pow(s12,3)+8*pow(s13,3))+2*(s12+2*s13)*pow(s35n,3)*pow(zb,3))-s12*s12*((10*s12*s13-s12*s12+12*(s13*s13))*(s35n*s35n)*(zb*zb)+4*s13*(3*s13*(s12*s12)+4*s12*(s13*s13)+pow(s12,3)+2*pow(s13,3))+s35n*zb*(10*s13*(s12*s12)+22*s12*(s13*s13)+3*pow(s12,3)+16*pow(s13,3))+2*(s12+2*s13)*pow(s35n,3)*pow(zb,3))+s15n*s15n*(zb*zb)*(s25n*s25n*(zb*zb)*((11*s12+12*s13)*s35n*zb+4*(s13*s13)+4*(s35n*s35n)*(zb*zb))+2*(s13+s35n*zb)*pow(s25n,3)*pow(zb,3)-s12*(48*s13*(s12*s12)+67*s12*(s13*s13)+s35n*zb*(62*s12*s13+21*(s12*s12)+40*(s13*s13))+(5*s12+14*s13)*(s35n*s35n)*(zb*zb)+12*pow(s12,3)+28*pow(s13,3)-2*pow(s35n,3)*pow(zb,3))+s25n*zb*(-2*s35n*zb*(13*s12*s13+3*(s12*s12)-2*(s13*s13))-s12*(36*s12*s13+8*(s12*s12)+35*(s13*s13))+(7*s12+6*s13)*(s35n*s35n)*(zb*zb)+2*pow(s35n,3)*pow(zb,3)))+s15n*zb*(s25n*s25n*(zb*zb)*(2*s35n*zb*(s12*s12+2*(s13*s13))-s12*(12*s12*s13+s12*s12+11*(s13*s13))+2*(3*s12+s13)*(s35n*s35n)*(zb*zb))-s12*s25n*zb*(28*s13*(s12*s12)+54*s12*(s13*s13)+s35n*zb*(46*s12*s13+9*(s12*s12)+42*(s13*s13))+(s12+22*s13)*(s35n*s35n)*(zb*zb)+3*pow(s12,3)+28*pow(s13,3))+(-3*s12*s13+2*(s12+s13)*s35n*zb-s12*s12-s13*s13)*pow(s25n,3)*pow(zb,3)-s12*(44*(s12*s12)*(s13*s13)+(26*s12*s13+3*(s12*s12)+12*(s13*s13))*(s35n*s35n)*(zb*zb)+21*s13*pow(s12,3)+3*pow(s12,4)+38*s12*pow(s13,3)+s35n*zb*(40*s13*(s12*s12)+56*s12*(s13*s13)+11*pow(s12,3)+16*pow(s13,3))+8*pow(s13,4)+2*(s12+2*s13)*pow(s35n,3)*pow(zb,3)))+2*(-13*s12*s13-2*s12*s35n*zb+s25n*zb*(s12+2*s13+5*s35n*zb)-8*(s12*s12)-2*(s13*s13)+2*(s25n*s25n)*(zb*zb)+2*(s35n*s35n)*(zb*zb))*pow(s15n,4)*pow(zb,4)+(-5*s12-2*s13+3*s25n*zb+2*s35n*zb)*pow(s15n,5)*pow(zb,5))*pow(s12+s13+s15n*zb,-1)*pow(s12+s15n*zb+s25n*zb,-1)*pow(s13+s15n*zb+s35n*zb,-1)*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-1))/4.;
}

// Coefficient of master 12 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6CA<12>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6CA<12,-1>(s12,s13,s15n,s25n,s35n,zb)
    });
}


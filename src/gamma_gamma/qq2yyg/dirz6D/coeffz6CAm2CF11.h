/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 11: box(s12,s13,s12+s13+s23)

// Coefficient order epsilon^-1 of master 11
template<>
double qq2yygz6CAm2CF<11,-1>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return -((s15n+s25n)*zb*pow(s12,-1)*pow(s13,-1)*((s12+s13+s15n*zb+s25n*zb+s35n*zb)*(s25n*(s13+2*s35n*zb)+s35n*(s12+2*s13+2*s35n*zb)+s15n*(s12+s13+s25n*zb+3*s35n*zb)+zb*(s15n*s15n))*pow(s25n,-1)*pow(s35n,-1)*pow(s15n+s25n+s35n,-1)*pow(zb,-2)-2*(s12+s13+s15n*zb+s25n*zb+s35n*zb)*pow(s25n,-1)*pow(zb,-1)+s12*(s12*s35n+s15n*(s12+s13+3*s25n*zb+s35n*zb)+s25n*(2*s12+s13+2*s35n*zb)+zb*(s15n*s15n)+2*zb*(s25n*s25n))*pow(s25n,-1)*pow(s15n+s25n+s35n,-1)*pow(zb,-1)*pow(s12+s15n*zb+s25n*zb,-1)-2*(s12+s13+s15n*zb+s25n*zb+s35n*zb)*pow(s13+s15n*zb+s35n*zb,-1)+s12*(s25n*(s13+2*s35n*zb)+s35n*(s12+2*s13+2*s35n*zb)+s15n*(s12+s13+s25n*zb+3*s35n*zb)+zb*(s15n*s15n))*pow(s35n,-1)*pow(s15n+s25n+s35n,-1)*pow(zb,-1)*pow(s13+s15n*zb+s35n*zb,-1)+(s12+s13+s15n*zb+s25n*zb+s35n*zb)*(s12*s35n+s15n*(s12+s13+3*s25n*zb+s35n*zb)+s25n*(2*s12+s13+2*s35n*zb)+zb*(s15n*s15n)+2*zb*(s25n*s25n))*pow(s15n+s25n+s35n,-1)*pow(s12+s15n*zb+s25n*zb,-1)*pow(s13+s15n*zb+s35n*zb,-1)-pow(s15n,-1)*pow(zb,-1)*(7*s12*s13*s35n*zb+6*s13*(s12*s12)+5*s35n*zb*(s12*s12)+s25n*zb*(9*s12*s13+2*(3*s12+s13)*s35n*zb+6*(s12*s12)-s13*s13)+2*s12*(s13*s13)-2*s35n*zb*(s13*s13)+(7*s12+s13+4*s25n*zb+4*s35n*zb)*(s15n*s15n)*(zb*zb)+2*(2*s12+s13)*(s25n*s25n)*(zb*zb)+2*s12*(s35n*s35n)*(zb*zb)+s15n*zb*(8*s12*s13+(9*s12+s13)*s35n*zb+s25n*zb*(11*s12+3*s13+4*s35n*zb)+7*(s12*s12)-3*(s13*s13)+2*(s25n*s25n)*(zb*zb)+2*(s35n*s35n)*(zb*zb))+2*pow(s12,3)-2*pow(s13,3)+2*pow(s15n,3)*pow(zb,3))*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-2)+4*s12*(s13+s15n*zb+s25n*zb+s35n*zb)*pow(s15n,-1)*pow(zb,-1)*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-1)+(s12*s35n*zb+4*s13*s35n*zb+s25n*zb*(-2*s12+s13+2*s35n*zb)+s15n*zb*(-s12+3*s13+s25n*zb+3*s35n*zb)-2*(s12*s12)+2*(s13*s13)+s15n*s15n*(zb*zb)+2*(s35n*s35n)*(zb*zb))*pow(s25n,-1)*pow(zb,-1)*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-1)+2*s12*(s12+s15n*zb)*(s13+s15n*zb+s25n*zb+s35n*zb)*(s12*s35n+s15n*(s12+s13+3*s25n*zb+s35n*zb)+s25n*(2*s12+s13+2*s35n*zb)+zb*(s15n*s15n)+2*zb*(s25n*s25n))*pow(s15n,-1)*pow(s25n,-1)*pow(s15n+s25n+s35n,-1)*pow(zb,-2)*pow(s12+s15n*zb+s25n*zb,-1)*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-1)+zb*(-(s12*s35n)+s15n*(s12+s13+3*s25n*zb+s35n*zb)+s25n*(2*s12+3*s13+2*s35n*zb)+zb*(s15n*s15n)+2*zb*(s25n*s25n))*pow(s13+s15n*zb+s35n*zb,-1)*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-1)-(s13+s15n*zb)*(s25n*(s13+2*s35n*zb)+s35n*(s12+2*s13+2*s35n*zb)+s15n*(s12+s13+s25n*zb+3*s35n*zb)+zb*(s15n*s15n))*(2*s12*s13+(s12+s13)*s25n*zb+s12*s35n*zb+s13*s35n*zb+s15n*zb*(s12+s13+s25n*zb+s35n*zb)+s15n*s15n*(zb*zb))*pow(s15n,-1)*pow(s35n,-1)*pow(s15n+s25n+s35n,-1)*pow(zb,-2)*pow(s13+s15n*zb+s35n*zb,-1)*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-1)))/2.;
}

// Coefficient of master 11 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6CAm2CF<11>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6CAm2CF<11,-1>(s12,s13,s15n,s25n,s35n,zb)
    });
}


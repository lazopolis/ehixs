/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 10: box6(s23,-s12-s13-s14-s23-s24,-s12-s13-s14)

// Coefficient order epsilon^-1 of master 10
template<>
double qq2yyg6CF<10,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-3)*(-((3*s24*(s13*s13)+s24*(s24*(s23+s24)+s14*(5*s23+3*s24)+3*(s14*s14))+s13*(2*s23*s24+s24*(6*s14+5*s24)-s23*s23))*pow(s12,4))-s24*(3*s13+3*s14+2*(s23+s24))*pow(s12,5)-s24*pow(s12,6)+pow(s12,3)*(s13*s13*(2*s23*s24-s24*(3*s14+4*s24)+2*(s23*s23))-s24*pow(s13,3)+s24*(-4*s23*(s14*s14)+s14*((s23+s24)*(s23+s24))+(2*s23-s24)*((s23+s24)*(s23+s24))-pow(s14,3))+s13*(-3*s24*(s14*s14)+2*s14*(-(s23*s24)+s23*s23-2*(s24*s24))+2*(3*s24*(s23*s23)+s23*(s24*s24)+pow(s23,3)-pow(s24,3))))-(s23+s24)*(s24*(s13*s13)*(s14*(3*s23*s24+s23*s23+s24*s24)+s24*(5*s23*s24+2*(s23*s23)+s24*s24))+s14*s23*s24*(-(s23*(s14*s14))+s14*(-(s23*s23)+s24*s24)+s24*((s23+s24)*(s23+s24)))+pow(s13,3)*pow(s24,3)-s13*(s23*(s14*s14)*(2*s23*s24+s23*s23-2*(s24*s24))+s14*(s24*s24)*(-5*s23*s24-4*(s23*s23)+s24*s24)+s24*s24*(-5*s24*(s23*s23)-2*s23*(s24*s24)-2*pow(s23,3)+pow(s24,3))))+s12*s12*((2*s23*s24+s23*s23-s24*s24)*pow(s13,3)+s13*s13*(7*s24*(s23*s23)+s14*(3*s23*s24+2*(s23*s23)-s24*s24)+4*s23*(s24*s24)+2*pow(s23,3)-2*pow(s24,3))+s13*(s14*s14*(s23*s23+s24*s24)+s23*(5*s24*(s23*s23)+2*s23*(s24*s24)+pow(s23,3)-4*pow(s24,3))+s14*(12*s24*(s23*s23)+9*s23*(s24*s24)+4*pow(s23,3)+pow(s24,3)))+s24*(s14*s14*(5*s23*s24+2*(s23*s23)+2*(s24*s24))+(-s23+s24)*pow(s14,3)+s14*(8*s24*(s23*s23)+2*s23*(s24*s24)+5*pow(s23,3)-pow(s24,3))+(s23-2*s24)*pow(s23+s24,3)))+s12*(s24*(s23*s24+s23*s23-s24*s24)*pow(s13,3)+s13*s13*(s24*(-(s24*(s23*s23))-6*s23*(s24*s24)+pow(s23,3)-2*pow(s24,3))+s14*(6*s24*(s23*s23)+4*s23*(s24*s24)+2*pow(s23,3)-pow(s24,3)))+s13*(s23*(s14*s14)*(6*s23*s24+2*(s23*s23)+5*(s24*s24))+2*s14*s23*(4*s24*(s23*s23)+2*s23*(s24*s24)+pow(s23,3)-2*pow(s24,3))+s24*s24*(-17*s24*(s23*s23)-9*s23*(s24*s24)-7*pow(s23,3)+pow(s24,3)))-s24*(-2*s14*(-(s23*s24)+s23*s23-s24*s24)*((s23+s24)*(s23+s24))-s23*(s23+2*s24)*pow(s14,3)+s14*s14*(-6*s24*(s23*s23)-s23*(s24*s24)-4*pow(s23,3)+pow(s24,3))+s24*pow(s23+s24,4))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1);
}

// Coefficient of master 10 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6CF<10>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6CF<10,-1>(s12,s13,s14,s23,s24)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 9: box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24)

// Coefficient order epsilon^-1 of master 9
template<>
double qq2yyg6CA<9,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s14,-2)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)*(s14*s23*(s13*s13)*(5*(s23-s24)*(s14*s14)-6*(s23+s24)*(s24*s24)+s14*(-3*s23*s24-2*(s23*s23)+s24*s24))+((-6*s23+8*s24)*(s13*s13)+(-7*s23+2*s24)*(s14*s14)-2*s13*(s14*(6*s23-5*s24)-2*s23*s24+7*(s23*s23)-15*(s24*s24))+s14*(6*s23*s24-18*(s23*s23)+16*(s24*s24))-6*(s23-2*s24)*((s23+s24)*(s23+s24)))*pow(s12,4)+(6*s23*s24+s14*(-6*s23+8*s24)+s13*(-6*s23+10*s24)-6*(s23*s23)+12*(s24*s24))*pow(s12,5)-2*(s23-2*s24)*pow(s12,6)-2*s23*(2*s24*(s14*s14)+(s23+s24)*(s23*s23)+s14*(2*s23*s24+s23*s23+2*(s24*s24)))*pow(s14,3)+s13*s23*(s14*s14)*((3*s23-8*s24)*(s14*s14)+2*s23*(s24*s24)-s14*(5*s23*s24+3*(s23*s23)+4*(s24*s24))-2*pow(s23,3))-s12*s12*(2*(2*s23*s24+s23*s23-3*(s24*s24))*pow(s13,3)+s13*(s14*s14*(13*s23*s24+16*(s23*s23)+6*(s24*s24))+2*(2*s23*s24+s23*s23-5*(s24*s24))*((s23+s24)*(s23+s24))-4*(s23-s24)*pow(s14,3)+2*s14*(16*s24*(s23*s23)+3*s23*(s24*s24)+10*pow(s23,3)-5*pow(s24,3)))+s13*s13*((-5*s23+2*s24)*(s14*s14)+16*s24*(s23*s23)+2*s14*(4*s23*s24+5*(s23*s23)-2*(s24*s24))-6*s23*(s24*s24)+4*pow(s23,3)-24*pow(s24,3))+s14*(s14*s14*(14*s23*s24+13*(s23*s23)+4*(s24*s24))+2*s23*(3*s23-s24)*((s23+s24)*(s23+s24))+(s23+2*s24)*pow(s14,3)+s14*(22*s24*(s23*s23)+5*s23*(s24*s24)+21*pow(s23,3)+2*pow(s24,3))))+s12*(-(s23*(s14*s14)*((3*s23+10*s24)*(s14*s14)+2*s23*(5*s23*s24+3*(s23*s23)+2*(s24*s24))+s14*(13*s23*s24+11*(s23*s23)+6*(s24*s24))))+(2*s14*s23*s24+3*s23*(s14*s14)-2*s24*(s23*s24+2*(s23*s23)-3*(s24*s24)))*pow(s13,3)+s13*s14*(s14*s14*(-11*s23*s24+2*(s23*s23)-2*(s24*s24))+3*s23*pow(s14,3)-4*s23*(3*s24*(s23*s23)+4*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3))-s14*(17*s24*(s23*s23)+8*s23*(s24*s24)+15*pow(s23,3)+2*pow(s24,3)))+2*(s13*s13)*(s14*s14*(2*(s23*s23)-s24*s24)+3*s23*pow(s14,3)+s14*(-5*s24*(s23*s23)-4*s23*(s24*s24)-2*pow(s23,3)+pow(s24,3))+s24*(-3*s24*(s23*s23)+3*s23*(s24*s24)-2*pow(s23,3)+4*pow(s24,3))))+pow(s13,3)*(s23*(2*s23-s24)*(s14*s14)+2*s14*s23*(s24*s24)-2*(s23*s23)*(s24*s24)+2*pow(s24,4))-pow(s12,3)*(2*(s13*s13)*(s14*(3*s23-s24)+3*s23*s24+5*(s23*s23)-12*(s24*s24))+2*(s23-s24)*pow(s13,3)+s13*((5*s23+4*s24)*(s14*s14)+2*s14*(5*s23*s24+14*(s23*s23)-10*(s24*s24))+2*(7*s24*(s23*s23)-13*s23*(s24*s24)+5*pow(s23,3)-15*pow(s24,3)))+2*(s23*(11*s23+4*s24)*(s14*s14)+2*(s23+s24)*pow(s14,3)+s14*(6*s24*(s23*s23)-7*s23*(s24*s24)+9*pow(s23,3)-4*pow(s24,3))+(s23-2*s24)*pow(s23+s24,3))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/4.;
}

// Coefficient of master 9 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6CA<9>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6CA<9,-1>(s12,s13,s14,s23,s24)
    });
}

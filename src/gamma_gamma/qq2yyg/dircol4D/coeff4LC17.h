/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 17: box(-s12-s13-s14-s23-s24,s23,-s12-s13-s14)

// Coefficient order epsilon^0 of master 17
template<>
double qq2yyg4LC<17,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s24,-2)*(-2*s23*(s23+s24)*(s14*s14)*(-(s23*(s14*s14))+s14*(-(s23*s23)+s24*s24)+s24*((s23+s24)*(s23+s24)))-2*s14*(s24*(s23+s24)+s14*(5*s23+3*s24)+s13*(6*s14+2*s23+5*s24)+3*(s13*s13)+3*(s14*s14))*pow(s12,4)-2*s14*(3*s13+3*s14+2*(s23+s24))*pow(s12,5)-2*s14*pow(s12,6)+2*s23*(s24*s24)*pow(s13,4)-pow(s12,3)*(2*(s13*s13)*(-2*s14*(s23-2*s24)+s24*(-s23+s24)+3*(s14*s14))+2*s14*pow(s13,3)+2*s14*(4*s23*(s14*s14)-s14*((s23+s24)*(s23+s24))-(2*s23-s24)*((s23+s24)*(s23+s24))+pow(s14,3))+s13*(2*s23*s24*(s23+2*s24)+4*(s23+2*s24)*(s14*s14)+s14*(-2*s23*s24-10*(s23*s23)+3*(s24*s24))+6*pow(s14,3)))+s13*s13*(-(s14*s23*s24*(11*s23*s24+6*(s23*s23)+9*(s24*s24)))+s14*s14*(-8*s24*(s23*s23)-5*s23*(s24*s24)+4*pow(s23,3)-pow(s24,3))-2*s23*(s23+s24)*pow(s24,3))-pow(s13,3)*((3*s23+s24)*pow(s24,3)+s14*(6*s24*(s23*s23)+pow(s24,3)))+s13*s14*(s14*s14*(-3*s23*(s24*s24)+6*pow(s23,3))-2*s24*(6*(s23*s23)*(s24*s24)+6*s24*pow(s23,3)+2*pow(s23,4)+s23*pow(s24,3)-pow(s24,4))+2*s14*(-8*(s23*s23)*(s24*s24)-3*s24*pow(s23,3)+pow(s23,4)-4*s23*pow(s24,3)+pow(s24,4)))+s12*s12*(s13*s13*((6*s23-2*s24)*(s14*s14)+2*s14*(4*s23*s24+7*(s23*s23)-2*(s24*s24))-(7*s23+3*s24)*(s24*s24))+2*(2*s23-s24)*(s14+s24)*pow(s13,3)+s13*(2*(s14*s14)*(8*s23*s24+11*(s23*s23)+2*(s24*s24))-2*s23*s24*(4*s23*s24+s23*s23+3*(s24*s24))+2*s24*pow(s14,3)+s14*(2*s24*(s23*s23)-11*s23*(s24*s24)+10*pow(s23,3)+pow(s24,3)))+2*s14*(s14*s14*(5*s23*s24+2*(s23*s23)+2*(s24*s24))+(-s23+s24)*pow(s14,3)+s14*(8*s24*(s23*s23)+2*s23*(s24*s24)+5*pow(s23,3)-pow(s24,3))+(s23-2*s24)*pow(s23+s24,3)))+s12*((s24*(s23*s24+2*(s23*s23)-3*(s24*s24))+s14*(4*s23*s24+4*(s23*s23)-s24*s24))*pow(s13,3)+2*s23*s24*pow(s13,4)+s13*s13*(2*s23*(7*s23+4*s24)*(s14*s14)-s24*s24*(10*s23*s24+7*(s23*s23)+s24*s24)+s14*(-2*s24*(s23*s23)-6*s23*(s24*s24)+6*pow(s23,3)-3*pow(s24,3)))+s13*(-2*s23*(s24*s24)*((s23+s24)*(s23+s24))+(10*s23*s24+12*(s23*s23)+s24*s24)*pow(s14,3)+s14*s14*(8*s24*(s23*s23)-4*s23*(s24*s24)+16*pow(s23,3)+pow(s24,3))+2*s14*(-14*(s23*s23)*(s24*s24)-4*s24*pow(s23,3)+pow(s23,4)-9*s23*pow(s24,3)+pow(s24,4)))+2*s14*(2*s14*(-(s23*s24)+s23*s23-s24*s24)*((s23+s24)*(s23+s24))+s23*(s23+2*s24)*pow(s14,3)+s14*s14*(6*s24*(s23*s23)+s23*(s24*s24)+4*pow(s23,3)-pow(s24,3))-s24*pow(s23+s24,4))))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient order epsilon^1 of master 17
template<>
double qq2yyg4LC<17,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s24,-2)*(-((s13*(-3*s24*(6*s23+s24)+2*s14*(s23+4*s24))+2*(-(s23*s24*(9*s23+10*s24))+2*(s23+2*s24)*(s14*s14)+s14*(2*(s23*s23)+7*(s24*s24))))*pow(s12,4))-2*(-3*s23*s24+s14*(s23+2*s24))*pow(s12,5)+s24*(s14*(-2*s23*s24+4*(s23*s23)-3*(s24*s24))+s24*(4*s23*s24+3*(s23*s23)+s24*s24))*pow(s13,3)+(2*s23+s24)*(s24*s24)*pow(s13,4)-2*s23*s24*(s14*s14)*(s14*s24*(2*s23+3*s24)+(s23+s24)*(s14*s14)-s24*(s23*s23)+2*s23*(s24*s24)-pow(s23,3)+2*pow(s24,3))+s13*s13*(2*(3*s23+2*s24)*(s24*s24)*((s23+s24)*(s23+s24))+s14*s14*(19*s24*(s23*s23)+2*s23*(s24*s24)+6*pow(s23,3)-2*pow(s24,3))+s14*s24*(16*s24*(s23*s23)+7*s23*(s24*s24)+13*pow(s23,3)+6*pow(s24,3)))+pow(s12,3)*((2*s14*(s23-2*s24)+s24*(20*s23+3*s24))*(s13*s13)+s13*(-8*s24*(s14*s14)+s14*(17*s23*s24+2*(s23*s23)-16*(s24*s24))+2*s24*(29*s23*s24+19*(s23*s23)+5*(s24*s24)))-2*(s14*s14*(7*s23*s24+4*(s23*s23)+11*(s24*s24))-s23*s24*(20*s23*s24+9*(s23*s23)+11*(s24*s24))+(s23+2*s24)*pow(s14,3)+s14*(-12*s24*(s23*s23)-3*s23*(s24*s24)+pow(s23,3)+10*pow(s24,3))))+s12*s12*(s13*s13*(4*s23*(s14*s14)+2*s14*(15*s23*s24+6*(s23*s23)-5*(s24*s24))+s24*(47*s23*s24+25*(s23*s23)+11*(s24*s24)))+(2*s14*s23+s24*(10*s23+s24))*pow(s13,3)+s13*(s14*s14*(12*s23*s24+10*(s23*s23)-17*(s24*s24))+s14*s23*(51*s23*s24+4*(s23*s23)+40*(s24*s24))+2*s23*pow(s14,3)+s24*(79*s24*(s23*s23)+66*s23*(s24*s24)+26*pow(s23,3)+11*pow(s24,3)))-2*(-(s23*s24*(3*s23+4*s24)*((s23+s24)*(s23+s24)))+(5*s23*s24+2*(s23*s23)+5*(s24*s24))*pow(s14,3)+s14*s24*(-20*s24*(s23*s23)+2*s23*(s24*s24)-14*pow(s23,3)+7*pow(s24,3))+s14*s14*(-(s24*(s23*s23))+12*s23*(s24*s24)+2*pow(s23,3)+12*pow(s24,3))))+s13*s14*(s14*s14*(13*s24*(s23*s23)+4*s23*(s24*s24)+6*pow(s23,3)+2*pow(s24,3))+2*s14*s24*(6*s24*(s23*s23)+3*s23*(s24*s24)+6*pow(s23,3)+5*pow(s24,3))+2*s24*(-4*(s23*s23)*(s24*s24)-5*s24*pow(s23,3)-pow(s23,4)+4*s23*pow(s24,3)+4*pow(s24,4)))+s12*((s14*(13*s23*s24+6*(s23*s23)-4*(s24*s24))+s24*(15*s23*s24+5*(s23*s23)+2*(s24*s24)))*pow(s13,3)+s24*(2*s23+s24)*pow(s13,4)+s13*s13*(s14*s14*(20*s23*s24+14*(s23*s23)-9*(s24*s24))+s14*(38*s24*(s23*s23)+25*s23*(s24*s24)+6*pow(s23,3)-pow(s24,3))+s24*(37*s24*(s23*s23)+42*s23*(s24*s24)+9*pow(s23,3)+12*pow(s24,3)))+s13*(2*s23*(s14*s14)*(20*s23*s24+5*(s23*s23)+6*(s24*s24))+(7*s23*s24+8*(s23*s23)-6*(s24*s24))*pow(s14,3)+s14*s24*(43*s24*(s23*s23)+31*s23*(s24*s24)+26*pow(s23,3)+16*pow(s24,3))+2*s24*(22*(s23*s23)*(s24*s24)+14*s24*pow(s23,3)+3*pow(s23,4)+13*s23*pow(s24,3)+2*pow(s24,4)))-2*s14*(s24*(s23+s24)*pow(s14,3)+s14*s14*(3*s24*(s23*s23)+8*s23*(s24*s24)+pow(s23,3)+4*pow(s24,3))+s24*(-3*(s23*s23)*(s24*s24)-9*s24*pow(s23,3)-4*pow(s23,4)+4*s23*pow(s24,3)+2*pow(s24,4))+s14*(-5*s24*pow(s23,3)+12*s23*pow(s24,3)+5*pow(s24,4)))))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient order epsilon^2 of master 17
template<>
double qq2yyg4LC<17,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s24,-2)*(-2*(3*s13*s14-s14*s24+4*s23*s24+3*(s14*s14))*pow(s12,5)-2*s14*pow(s12,6)+s24*(s24*(-4*s23*s24-9*(s23*s23)+3*(s24*s24))+4*s14*(s23*s24-5*(s23*s23)+4*(s24*s24)))*pow(s13,3)+(-2*s23+s24)*(s24*s24)*pow(s13,4)+pow(s12,4)*(-6*s14*(s13*s13)+s13*(4*s14*(s23+2*s24)+s24*(-23*s23+4*s24)-12*(s14*s14))-2*(12*s23*s24*(s23+s24)-6*s24*(s14*s14)+s14*(7*s23*s24-3*(s23*s23)-5*(s24*s24))+3*pow(s14,3)))+pow(s12,3)*(s13*s13*(2*s14*(4*s23+5*s24)+s24*(-24*s23+7*s24)-6*(s14*s14))-2*s14*pow(s13,3)+s13*(4*(2*s23+7*s24)*(s14*s14)+2*s24*(-25*s23*s24-28*(s23*s23)+6*(s24*s24))+s14*(-25*s23*s24+14*(s23*s23)+61*(s24*s24))-6*pow(s14,3))-2*(-(s14*s14*(4*s23*s24+9*(s23*s23)+12*(s24*s24)))+12*s23*s24*((s23+s24)*(s23+s24))-9*s24*pow(s14,3)+pow(s14,4)+s14*(35*s24*(s23*s23)+25*s23*(s24*s24)-2*pow(s23,3)-2*pow(s24,3))))+2*s23*(s14*s14)*(s14*s14*(s23*s24+2*(s23*s23)-s24*s24)-s14*s24*(5*s23*s24+9*(s23*s23)+2*(s24*s24))-s24*(11*s24*(s23*s23)+4*s23*(s24*s24)+8*pow(s23,3)+pow(s24,3)))+s24*(s13*s13)*(s14*s14*(-3*s23*s24-49*(s23*s23)+21*(s24*s24))+2*s24*(-5*s24*(s23*s23)+s23*(s24*s24)-4*pow(s23,3)+2*pow(s24,3))+s14*(-24*s24*(s23*s23)+38*s23*(s24*s24)-45*pow(s23,3)+25*pow(s24,3)))-s12*(s24*(s14*(5*s23-21*s24)+14*s23*s24+11*(s23*s23)-7*(s24*s24))*pow(s13,3)+(2*s23-s24)*s24*pow(s13,4)+2*s14*(s24*(3*s23*s24+12*(s23*s23)+s24*s24)*((s23+s24)*(s23+s24))+(-5*s23*s24-3*(s23*s23)+s24*s24)*pow(s14,3)+3*s14*s24*(13*s24*(s23*s23)+4*s23*(s24*s24)+12*pow(s23,3)+pow(s24,3))+s14*s14*(6*s24*(s23*s23)+2*s23*(s24*s24)-6*pow(s23,3)+3*pow(s24,3)))-s13*s13*(s14*s14*(14*s23*s24+8*(s23*s23)+47*(s24*s24))+s14*s24*(19*s23*s24-99*(s23*s23)+79*(s24*s24))+s24*(-48*s24*(s23*s23)-14*s23*(s24*s24)-17*pow(s23,3)+15*pow(s24,3)))-s13*(2*s24*(-5*s23*s24-4*(s23*s23)+2*(s24*s24))*((s23+s24)*(s23+s24))+(27*s23*s24+14*(s23*s23)+25*(s24*s24))*pow(s14,3)+s14*s24*(-114*s24*(s23*s23)+44*s23*(s24*s24)-121*pow(s23,3)+47*pow(s24,3))+s14*s14*(-109*s24*(s23*s23)+22*s23*(s24*s24)+8*pow(s23,3)+64*pow(s24,3))))+s13*s14*(s14*s14*(-27*s24*(s23*s23)-11*s23*(s24*s24)+4*pow(s23,3)+6*pow(s24,3))+s14*s24*(-33*s24*(s23*s23)+25*s23*(s24*s24)-64*pow(s23,3)+16*pow(s24,3))+2*s24*(10*(s23*s23)*(s24*s24)-13*s24*pow(s23,3)-12*pow(s23,4)+16*s23*pow(s24,3)+5*pow(s24,4)))+s12*s12*(s13*s13*(8*(s23+2*s24)*(s14*s14)+s24*(-40*s23*s24-43*(s23*s23)+18*(s24*s24))+2*s14*(-8*s23*s24+4*(s23*s23)+35*(s24*s24)))+(4*s14*(s23+s24)+s24*(-11*s23+4*s24))*pow(s13,3)+s13*(s14*s14*(27*s23*s24+28*(s23*s23)+88*(s24*s24))+4*(s23+5*s24)*pow(s14,3)+s24*(-82*s24*(s23*s23)-29*s23*(s24*s24)-41*pow(s23,3)+12*pow(s24,3))+s14*(-146*s24*(s23*s23)-19*s23*(s24*s24)+4*pow(s23,3)+84*pow(s24,3)))+2*(s23*(s14*s14)*(-30*s23*s24+6*(s23*s23)-13*(s24*s24))+3*(4*s23*s24+3*(s23*s23)+2*(s24*s24))*pow(s14,3)+4*s24*pow(s14,4)-s14*s24*(60*s24*(s23*s23)+23*s23*(s24*s24)+39*pow(s23,3)+2*pow(s24,3))-4*s23*s24*pow(s23+s24,3))))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient of master 17 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg4LC<17>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yyg4LC<17,0>(s12,s13,s14,s23,s24),
        qq2yyg4LC<17,1>(s12,s13,s14,s23,s24),
        qq2yyg4LC<17,2>(s12,s13,s14,s23,s24)
    });
}

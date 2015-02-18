/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 16: box(-s12-s13-s14-s23-s24,s24,-s12-s13-s14)

// Coefficient order epsilon^0 of master 16
template<>
double qq2yygCA<16,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*(s14*(s23*s23)*(s14*s23*(s23+5*s24)+2*s24*(s23*s23-s24*s24))-2*(3*s23*s24+6*s13*(s14+s23+s24)+s14*(7*s23+4*s24)+3*(s13*s13)+3*(s14*s14)+3*(s23*s23)+s24*s24)*pow(s12,4)-2*(3*s13+3*s14+3*s23+2*s24)*pow(s12,5)-2*pow(s12,6)+s23*s24*(s23+4*s24)*pow(s13,3)-pow(s12,3)*(6*(s14+s23+2*s24)*(s13*s13)+s13*(4*s23*s24+16*s14*(s23+s24)+6*(s14*s14)+7*(s23*s23)+4*(s24*s24))+2*pow(s13,3)+2*((5*s23+2*s24)*(s14*s14)+s23*(-2*s23*s24+s23*s23-s24*s24)+s14*(3*s23*s24+4*(s23*s23)+s24*s24)+pow(s14,3)))+s12*s12*(-2*(s24*(-3*s23+2*s24)+s14*(s23+4*s24))*(s13*s13)-4*s24*pow(s13,3)+s23*(-2*(s23-2*s24)*(s14*s14)+4*s24*(2*s23*s24+2*(s23*s23)+s24*s24)+s14*(15*s23*s24+s23*s23+4*(s24*s24))-2*pow(s14,3))-s13*(4*(s23+s24)*(s14*s14)-25*s24*(s23*s23)-24*s23*(s24*s24)+2*s14*(-6*s23*s24+s23*s23+s24*s24)+pow(s23,3)-4*pow(s24,3)))+s13*s13*(s14*s23*(3*s23*s24+s23*s23+8*(s24*s24))+2*s24*(7*s24*(s23*s23)+6*s23*(s24*s24)+4*pow(s23,3)+pow(s24,3)))+s13*s23*(s14*s14*(2*s23*s24+s23*s23+4*(s24*s24))+2*s24*(9*s24*(s23*s23)+8*s23*(s24*s24)+4*pow(s23,3)+3*pow(s24,3))+s14*(13*s24*(s23*s23)+9*s23*(s24*s24)+2*pow(s23,3)+6*pow(s24,3)))+s12*((4*s23*s24+s23*s23-2*(s24*s24))*pow(s13,3)+s13*s13*(18*s24*(s23*s23)+2*s14*(7*s23*s24+s23*s23-s24*s24)+24*s23*(s24*s24)+pow(s23,3)+4*pow(s24,3))+s23*(s14*s14*(11*s23*s24+3*(s23*s23)+4*(s24*s24))+4*s24*pow(s14,3)+2*s24*(s24*(s23*s23)+s23*(s24*s24)+pow(s23,3)+pow(s24,3))+s14*(14*s24*(s23*s23)+7*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3)))+s13*(s23*(s23+14*s24)*(s14*s14)+2*s24*(18*s24*(s23*s23)+11*s23*(s24*s24)+12*pow(s23,3)+pow(s24,3))+s14*(28*s24*(s23*s23)+24*s23*(s24*s24)+5*pow(s23,3)+2*pow(s24,3)))))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient order epsilon^1 of master 16
template<>
double qq2yygCA<16,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*((22*s23*s24+6*s14*(s23+2*s24)+2*s13*(5*s23+6*s24)+17*(s23*s23)+8*(s24*s24))*pow(s12,4)+4*(s23+s24)*pow(s12,5)+s24*(9*s23*s24+2*(s23*s23)+4*(s24*s24))*pow(s13,3)+pow(s12,3)*(4*(2*s23+3*s24)*(s13*s13)+12*s24*(s14*s14)+50*s24*(s23*s23)+24*s23*(s24*s24)+s14*(46*s23*s24+29*(s23*s23)+16*(s24*s24))+s13*(45*s23*s24+8*s14*(s23+3*s24)+36*(s23*s23)+22*(s24*s24))+28*pow(s23,3)+4*pow(s24,3))+s14*(s23*s23)*((5*s23+8*s24)*(s14*s14)+22*s24*(s23*s23)+32*s23*(s24*s24)+s14*(20*s23*s24+7*(s23*s23)+19*(s24*s24))+6*pow(s23,3)+16*pow(s24,3))+s12*s12*(s14*s14*(28*s23*s24+15*(s23*s23)+8*(s24*s24))+s13*s13*(34*s23*s24+2*s14*(s23+6*s24)+23*(s23*s23)+22*(s24*s24))+2*(s23+2*s24)*pow(s13,3)-2*(s23-2*s24)*pow(s14,3)+s23*(52*s24*(s23*s23)+37*s23*(s24*s24)+21*pow(s23,3)+2*pow(s24,3))+s14*(89*s24*(s23*s23)+43*s23*(s24*s24)+45*pow(s23,3)+4*pow(s24,3))+s13*(-2*(s23-6*s24)*(s14*s14)+66*s24*(s23*s23)+27*s23*(s24*s24)+s14*(58*s23*s24+38*(s23*s23)+30*(s24*s24))+46*pow(s23,3)+8*pow(s24,3)))+s13*s23*(s14*s14*(6*s23*s24+7*(s23*s23)-4*(s24*s24))+s14*(19*s24*(s23*s23)+16*s23*(s24*s24)+10*pow(s23,3)-5*pow(s24,3))+2*(-7*(s23*s23)*(s24*s24)+s24*pow(s23,3)+2*pow(s23,4)-12*s23*pow(s24,3)-6*pow(s24,4)))+s12*((11*s23*s24+4*(s23*s23)+8*(s24*s24))*pow(s13,3)+s13*s13*(s14*(22*s23*s24+11*(s23*s23)+16*(s24*s24))+2*(14*s24*(s23*s23)+9*s23*(s24*s24)+9*pow(s23,3)+4*pow(s24,3)))+s23*(22*(s23*s23)*(s24*s24)+s14*s14*(47*s23*s24+22*(s23*s23)+15*(s24*s24))+(3*s23+4*s24)*pow(s14,3)+20*s24*pow(s23,3)+6*pow(s23,4)+4*s23*pow(s24,3)+s14*(72*s24*(s23*s23)+61*s23*(s24*s24)+28*pow(s23,3)+7*pow(s24,3))-4*pow(s24,4))+s13*(3*(s23*s23)*(s24*s24)+s14*s14*(15*s23*s24+10*(s23*s23)+8*(s24*s24))+35*s24*pow(s23,3)+24*pow(s23,4)-20*s23*pow(s24,3)+s14*(61*s24*(s23*s23)+20*s23*(s24*s24)+37*pow(s23,3)+6*pow(s24,3))-2*pow(s24,4)))+s13*s13*(-2*(s23*s23)*(s24*s24)+2*s24*pow(s23,3)+4*pow(s23,4)-8*s23*pow(s24,3)+s14*(5*s23*(s24*s24)+2*pow(s23,3)+4*pow(s24,3))-2*pow(s24,4)))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient order epsilon^2 of master 16
template<>
double qq2yygCA<16,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*(-((6*s13*(2*s14+s23)+2*s14*(5*s23-2*s24)-17*s23*s24+6*(s13*s13)+6*(s14*s14)+14*(s23*s23)-6*(s24*s24))*pow(s12,4))-2*(3*s13+3*s14+2*s23)*pow(s12,5)-2*pow(s12,6)-s14*(s23*s23)*((s23-4*s24)*(s14*s14)+s14*(12*s23*s24+11*(s23*s23)-7*(s24*s24))+8*(2*s24*(s23*s23)+pow(s23,3)-pow(s24,3)))+pow(s13,3)*(-5*s24*(s23*s23)+13*s23*(s24*s24)-4*pow(s23,3)+4*pow(s24,3))+pow(s12,3)*(-6*s14*(s13*s13)-8*(s23-s24)*(s14*s14)+s14*(46*s23*s24-25*(s23*s23)+14*(s24*s24))+s13*(-8*s14*(s23-s24)+57*s23*s24-6*(s14*s14)-31*(s23*s23)+18*(s24*s24))-2*pow(s13,3)-2*pow(s14,3)+4*(10*s23*(s24*s24)-8*pow(s23,3)+pow(s24,3)))+s12*s12*(s14*s14*(41*s23*s24-12*(s23*s23)+8*(s24*s24))+s13*s13*(55*s23*s24+2*s14*(s23+2*s24)-26*(s23*s23)+18*(s24*s24))+2*s23*pow(s13,3)-2*(s23-2*s24)*pow(s14,3)+s14*(6*s24*(s23*s23)+65*s23*(s24*s24)-52*pow(s23,3)+4*pow(s24,3))+s13*(-2*(s23-4*s24)*(s14*s14)+23*s24*(s23*s23)-4*s14*(-26*s23*s24+8*(s23*s23)-7*(s24*s24))+122*s23*(s24*s24)-64*pow(s23,3)+12*pow(s24,3))+s23*(-33*s24*(s23*s23)+18*s23*(s24*s24)-28*pow(s23,3)+27*pow(s24,3)))+s13*s13*(s14*(-3*s24*(s23*s23)+27*s23*(s24*s24)-11*pow(s23,3)+4*pow(s24,3))+s23*(-21*s24*(s23*s23)+35*s23*(s24*s24)-12*pow(s23,3)+36*pow(s24,3)))+s12*((15*s23*s24-9*(s23*s23)+6*(s24*s24))*pow(s13,3)+s13*s13*(10*s24*(s23*s23)+91*s23*(s24*s24)+s14*(50*s23*s24-13*(s23*s23)+14*(s24*s24))-38*pow(s23,3)+12*pow(s24,3))+s13*(s14*s14*(47*s23*s24-5*(s23*s23)+8*(s24*s24))+s14*(27*s24*(s23*s23)+121*s23*(s24*s24)-59*pow(s23,3)+8*pow(s24,3))+s23*(-36*s24*(s23*s23)+92*s23*(s24*s24)-41*pow(s23,3)+83*pow(s24,3)))+s23*(s14*s14*(10*s23*s24-21*(s23*s23)+27*(s24*s24))-(s23-12*s24)*pow(s14,3)+s14*(-44*s24*(s23*s23)+22*s23*(s24*s24)-39*pow(s23,3)+23*pow(s24,3))-4*(s23*s23*(s24*s24)+4*s24*pow(s23,3)+2*pow(s23,4)-3*s23*pow(s24,3)-2*pow(s24,4))))+s13*s23*(s14*s14*(6*s23*s24-8*(s23*s23)+14*(s24*s24))+s14*(-22*s24*(s23*s23)+64*s23*(s24*s24)-23*pow(s23,3)+47*pow(s24,3))+4*(4*(s23*s23)*(s24*s24)-5*s24*pow(s23,3)-2*pow(s23,4)+13*s23*pow(s24,3)+6*pow(s24,4))))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient of master 16 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCA<16>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCA<16,0>(s12,s13,s14,s23,s24),
        qq2yygCA<16,1>(s12,s13,s14,s23,s24),
        qq2yygCA<16,2>(s12,s13,s14,s23,s24)
    });
}

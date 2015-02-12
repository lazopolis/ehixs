/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 25: box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14)

// Coefficient order epsilon^0 of master 25
template<>
double qq2yygCAm2CF<25,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (-((s24*(s23+s24)*(s13*s13)+s13*s14*(s23*s23)+s14*s14*(s23*s23)+s12*(2*s14*s23*(s14+2*s23+s24)+(s23+2*s24)*(s13*s13)+s13*(3*s23*s24+s14*(3*s23+2*s24)+s23*s23+2*(s24*s24)))+s12*s12*(2*s14*(2*s23+s24)+s13*(2*s14+3*s23+4*s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24))+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s13,-1)*pow(s23,-2))-s24*((s23+s24)*(s13*(4*s23+s24)+s23*(3*s14+2*(s23+s24)))+(3*s23+s24)*(s12*s12)+s12*(6*s23*s24+s13*(3*s23+s24)+s14*(3*s23+s24)+5*(s23*s23)+s24*s24))*pow(s12+s13+s14,-1)*pow(s23,-2)+s14*(-(s14*s23)+s13*s24+s12*(s13+s14-s23+s24)+s12*s12)*pow(s13,-1)*pow(s12+s14+s24,-1)+s24*(s12+s23+s24)*(-(s14*s23)+s13*s24+s12*(s13+s14-s23+s24)+s12*s12)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s14+s24,-1)+2*s12*(s24*(s23+s24)*(s13*s13)+2*s13*s14*(s23*s23)+s14*(2*s14+s23+s24)*(s23*s23)+s12*s12*(3*s23*s24+s14*(5*s23+2*s24)+2*s13*(s14+2*(s23+s24))+s13*s13+s14*s14+4*(s23*s23)+s24*s24)+s12*(2*s14*s23*(s14+3*s23+s24)+(s23+2*s24)*(s13*s13)+s13*(4*s23*s24+s14*(3*s23+2*s24)+3*(s23*s23)+2*(s24*s24)))+(2*s13+2*s14+3*s23+2*s24)*pow(s12,3)+pow(s12,4))*pow(s13,-1)*pow(s23,-2)*pow(s12+s23+s24,-1)-(s12+s14)*(-(s14*s23)+s13*s24+s12*(s13+s14-s23+s24)+s12*s12)*(s12*(2*s13+3*s14)+s14*(s13+s14+s23+s24)+2*(s12*s12))*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1))/4.;
}

// Coefficient order epsilon^1 of master 25
template<>
double qq2yygCAm2CF<25,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s12,-1)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*(-3*(s13+s14)*s24*(s14+s24)*(-(s14*s23*(s14+s23+s24))+s13*(s14*(-s23+s24)+s24*(s23+s24))+s24*(s13*s13))*(s23*s23)+(16*s23*s24+s13*(11*s23+3*s24)+s14*(13*s23+4*s24)+11*(s23*s23)+3*(s24*s24))*pow(s12,6)+(4*s23+s24)*pow(s12,7)+pow(s12,5)*((10*s23+3*s24)*(s13*s13)+3*(5*s23+2*s24)*(s14*s14)+30*s24*(s23*s23)+24*s23*(s24*s24)+s13*(39*s23*s24+9*s14*(3*s23+s24)+21*(s23*s23)+8*(s24*s24))+s14*(46*s23*s24+35*(s23*s23)+9*(s24*s24))+11*pow(s23,3)+3*pow(s24,3))+pow(s12,4)*(30*(s23*s23)*(s24*s24)+s13*s13*(35*s23*s24+s14*(17*s23+6*s24)+10*(s23*s23)+8*(s24*s24))+s14*s14*(48*s23*s24+41*(s23*s23)+9*(s24*s24))+(3*s23+s24)*pow(s13,3)+(7*s23+4*s24)*pow(s14,3)+19*s24*pow(s23,3)+5*pow(s23,4)+16*s23*pow(s24,3)+s13*(3*(7*s23+3*s24)*(s14*s14)+43*s24*(s23*s23)+45*s23*(s24*s24)+2*s14*(41*s23*s24+26*(s23*s23)+9*(s24*s24))+13*pow(s23,3)+6*pow(s24,3))+s14*(84*s24*(s23*s23)+57*s23*(s24*s24)+33*pow(s23,3)+6*pow(s24,3))+pow(s24,4))+pow(s12,3)*(s13*s13*((7*s23+3*s24)*(s14*s14)+2*s14*(25*s23*s24+10*(s23*s23)+6*(s24*s24))+s24*(39*s23*s24+20*(s23*s23)+6*(s24*s24)))+(s14*(3*s23+s24)+3*s24*(4*s23+s24))*pow(s13,3)+(22*s23*s24+21*(s23*s23)+3*(s24*s24))*pow(s14,3)+(s23+s24)*pow(s14,4)+s14*s14*(86*s24*(s23*s23)+45*s23*(s24*s24)+36*pow(s23,3)+3*pow(s24,3))+s13*(s14*s14*(58*s23*s24+41*(s23*s23)+12*(s24*s24))+(5*s23+3*s24)*pow(s14,3)+s14*(82*s24*(s23*s23)+68*s23*(s24*s24)+30*pow(s23,3)+9*pow(s24,3))+s23*(2*s24*(s23*s23)+13*s23*(s24*s24)+3*pow(s23,3)+13*pow(s24,3)))+s14*(74*(s23*s23)*(s24*s24)+53*s24*pow(s23,3)+13*pow(s23,4)+28*s23*pow(s24,3)+pow(s24,4))+s23*(9*(s23*s23)*(s24*s24)+3*s24*pow(s23,3)+pow(s23,4)+11*s23*pow(s24,3)+4*pow(s24,4)))+s12*(s24*pow(s13,3)*(-(s24*(s23*s23))+6*s23*(s24*s24)+s14*(6*s23*s24+3*(s23*s23)+s24*s24)-2*pow(s23,3)+pow(s24,3))+s14*(s23*s23)*(s14*s14*(21*s23*s24+3*(s23*s23)+16*(s24*s24))+3*(s23+2*s24)*pow(s14,3)+s24*(2*s24*(s23*s23)+6*s23*(s24*s24)-pow(s23,3)+3*pow(s24,3))+s14*(10*s24*(s23*s23)+26*s23*(s24*s24)+pow(s23,3)+13*pow(s24,3)))-s24*(s13*s13)*(2*s14*s23*(5*s23*s24+2*(s23*s23)-2*(s24*s24))+11*(s23*s23)*(s24*s24)-s14*s14*(6*s23*s24+4*(s23*s23)+s24*s24)+11*s24*pow(s23,3)+4*pow(s23,4)+s23*pow(s24,3)+pow(s24,4))-s13*s23*(-(s23*(3*s23+7*s24)*pow(s14,3))+s14*s14*(-16*s24*(s23*s23)-5*s23*(s24*s24)-2*pow(s23,3)+pow(s24,3))+s14*s24*(14*s24*(s23*s23)+20*s23*(s24*s24)+7*pow(s23,3)+5*pow(s24,3))+s24*(19*(s23*s23)*(s24*s24)+10*s24*pow(s23,3)+2*pow(s23,4)+15*s23*pow(s24,3)+4*pow(s24,4))))+s12*s12*(pow(s13,3)*(2*s24*(s23*s23)+15*s23*(s24*s24)+s14*(9*s23*s24+2*(s23*s23)+2*(s24*s24))-2*pow(s23,3)+3*pow(s24,3))+s13*s13*(4*(s14*s14)*(5*s23*s24+2*(s23*s23)+s24*s24)+s14*(21*s24*(s23*s23)+37*s23*(s24*s24)+4*pow(s23,3)+6*pow(s24,3))+s23*(-9*s24*(s23*s23)+2*s23*(s24*s24)-2*pow(s23,3)+13*pow(s24,3)))+s13*((15*s23*s24+10*(s23*s23)+2*(s24*s24))*pow(s14,3)+s14*s14*(48*s24*(s23*s23)+28*s23*(s24*s24)+20*pow(s23,3)+3*pow(s24,3))+s14*s23*(13*s24*(s23*s23)+19*s23*(s24*s24)+5*pow(s23,3)+8*pow(s24,3))-s24*(24*(s23*s23)*(s24*s24)+27*s24*pow(s23,3)+11*pow(s23,4)+8*s23*pow(s24,3)+pow(s24,4)))+s23*(-(s24*(s23*s23)*((s23+s24)*(s23+s24)))+(38*s23*s24+17*(s23*s23)+12*(s24*s24))*pow(s14,3)+4*(s23+s24)*pow(s14,4)+s14*s14*(52*s24*(s23*s23)+60*s23*(s24*s24)+11*pow(s23,3)+12*pow(s24,3))+s14*(30*(s23*s23)*(s24*s24)+10*s24*pow(s23,3)+2*pow(s23,4)+25*s23*pow(s24,3)+4*pow(s24,4)))))*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1))/4.;
}

// Coefficient order epsilon^2 of master 25
template<>
double qq2yygCAm2CF<25,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s12,-1)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*((3*s13*(3*s14+s23)+s14*(7*s23-3*s24)-8*s23*s24+3*(s13*s13)+6*(s14*s14)+8*(s23*s23)-6*(s24*s24))*pow(s12,6)+(3*s13+4*s14+2*s23)*pow(s12,7)+pow(s12,8)+pow(s12,5)*(6*s14*(s13*s13)+9*(s23-s24)*(s14*s14)+4*s24*(s23*s23)+s14*(-35*s23*s24+26*(s23*s23)-21*(s24*s24))+s13*(7*s14*(s23-s24)-30*s23*s24+9*(s14*s14)+19*(s23*s23)-18*(s24*s24))-25*s23*(s24*s24)+pow(s13,3)+4*pow(s14,3)+19*pow(s23,3)-8*pow(s24,3))-s23*(s14+s24)*(-((3*s23-4*s24)*(s14+s23+s24)*(s14*s14)*(s23*s23))+s24*(-7*s23*s24+3*(s23*s23)-3*(s24*s24))*pow(s13,3)-s13*s14*s23*(-7*s24*(s23*s23)-9*s23*(s24*s24)+s14*(-14*s23*s24+6*(s23*s23)+s24*s24)+3*pow(s23,3)+pow(s24,3))-s13*s13*(s24*(4*s24*(s23*s23)+10*s23*(s24*s24)-3*pow(s23,3)+3*pow(s24,3))+s14*(-13*s24*(s23*s23)+8*s23*(s24*s24)+3*pow(s23,3)+3*pow(s24,3))))+s12*(s14*(s23*s23)*(s14*s14*(-4*s23*s24+22*(s23*s23)-21*(s24*s24))+2*(3*s23-5*s24)*pow(s14,3)+s14*(22*s24*(s23*s23)-14*s23*(s24*s24)+11*pow(s23,3)-15*pow(s24,3))+s24*(5*s24*(s23*s23)-5*s23*(s24*s24)+6*pow(s23,3)-4*pow(s24,3)))+pow(s13,3)*((-2*s23*s24+7*(s23*s23)-3*(s24*s24))*(s24*s24)+s14*(-5*s24*(s23*s23)-7*s23*(s24*s24)+5*pow(s23,3)-3*pow(s24,3)))+s13*s23*(2*(-12*s23*s24+9*(s23*s23)-2*(s24*s24))*pow(s14,3)+s14*s14*(-15*s24*(s23*s23)-49*s23*(s24*s24)+35*pow(s23,3)-15*pow(s24,3))+s14*(-27*(s23*s23)*(s24*s24)+19*s24*pow(s23,3)+8*pow(s23,4)-34*s23*pow(s24,3)-16*pow(s24,4))+s24*(-3*(s23*s23)*(s24*s24)+4*s24*pow(s23,3)+3*pow(s23,4)-9*s23*pow(s24,3)-5*pow(s24,4)))+s13*s13*(s24*(s23*s23)*(6*s23*s24+3*(s23*s23)+13*(s24*s24))+s14*s14*(-19*s24*(s23*s23)-11*s23*(s24*s24)+17*pow(s23,3)-3*pow(s24,3))+s14*(-7*(s23*s23)*(s24*s24)-4*s24*pow(s23,3)+13*pow(s23,4)-10*s23*pow(s24,3)-3*pow(s24,4))))+s12*s12*(pow(s13,3)*(6*s24*(s23*s23)+s14*(-11*s23*s24+8*(s23*s23)-5*(s24*s24))-14*s23*(s24*s24)+3*pow(s23,3)-8*pow(s24,3))+s13*((-33*s23*s24+11*(s23*s23)-6*(s24*s24))*pow(s14,3)+s14*s14*(-60*s24*(s23*s23)-89*s23*(s24*s24)+76*pow(s23,3)-12*pow(s24,3))+s23*(6*(s23*s23)*(s24*s24)+22*s24*pow(s23,3)+5*pow(s23,4)-32*s23*pow(s24,3)-24*pow(s24,4))+s14*(-96*(s23*s23)*(s24*s24)+39*s24*pow(s23,3)+55*pow(s23,4)-82*s23*pow(s24,3)-6*pow(s24,4)))+s23*(3*s24*(s23*s23)*((s23+s24)*(s23+s24))+(-33*s23*s24+34*(s23*s23)-21*(s24*s24))*pow(s14,3)+3*(s23-3*s24)*pow(s14,4)+s14*s14*(31*s24*(s23*s23)-50*s23*(s24*s24)+52*pow(s23,3)-15*pow(s24,3))+s14*(10*(s23*s23)*(s24*s24)+40*s24*pow(s23,3)+13*pow(s23,4)-17*s23*pow(s24,3)-3*pow(s24,4)))+s13*s13*(s14*s14*(-35*s23*s24+16*(s23*s23)-11*(s24*s24))-10*(s23*s23)*(s24*s24)+20*s24*pow(s23,3)+8*pow(s23,4)+s14*(-20*s24*(s23*s23)-84*s23*(s24*s24)+45*pow(s23,3)-23*pow(s24,3))-41*s23*pow(s24,3)-9*pow(s24,4)))+pow(s12,4)*(s14*s14*(-55*s23*s24+31*(s23*s23)-24*(s24*s24))+s13*s13*(-32*s23*s24-s14*(s23+5*s24)+3*(s14*s14)+17*(s23*s23)-18*(s24*s24))-6*(s23*s23)*(s24*s24)+(s14-s23)*pow(s13,3)+(5*s23-9*s24)*pow(s14,3)+pow(s14,4)+29*s24*pow(s23,3)+17*pow(s23,4)+s13*((5*s23-14*s24)*(s14*s14)-2*s24*(s23*s23)+s14*(-97*s23*s24+49*(s23*s23)-47*(s24*s24))-83*s23*(s24*s24)+3*pow(s14,3)+40*pow(s23,3)-24*pow(s24,3))+s14*(-6*s24*(s23*s23)-72*s23*(s24*s24)+60*pow(s23,3)-17*pow(s24,3))-18*s23*pow(s24,3)-3*pow(s24,4))-pow(s12,3)*((10*s23*s24+s14*(s23+s24)-6*(s23*s23)+6*(s24*s24))*pow(s13,3)+(37*s23*s24-16*(s23*s23)+9*(s24*s24))*pow(s14,3)-(s23-3*s24)*pow(s14,4)+s14*s14*(33*s24*(s23*s23)+68*s23*(s24*s24)-69*pow(s23,3)+9*pow(s24,3))+s13*s13*((s23+5*s24)*(s14*s14)-4*s24*(s23*s23)+70*s23*(s24*s24)+s14*(69*s23*s24-34*(s23*s23)+31*(s24*s24))-25*pow(s23,3)+24*pow(s24,3))+s23*(-16*(s23*s23)*(s24*s24)-20*s24*pow(s23,3)-5*pow(s23,4)+2*s23*pow(s24,3)+3*pow(s24,4))+s14*(35*(s23*s23)*(s24*s24)-60*s24*pow(s23,3)-50*pow(s23,4)+33*s23*pow(s24,3)+3*pow(s24,4))+s13*(44*(s23*s23)*(s24*s24)+s14*s14*(100*s23*s24-41*(s23*s23)+35*(s24*s24))-(s23-7*s24)*pow(s14,3)-41*s24*pow(s23,3)-26*pow(s23,4)+69*s23*pow(s24,3)+s14*(39*s24*(s23*s23)+170*s23*(s24*s24)-98*pow(s23,3)+37*pow(s24,3))+9*pow(s24,4))))*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1))/4.;
}

// Coefficient of master 25 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCAm2CF<25>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCAm2CF<25,0>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<25,1>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<25,2>(s12,s13,s14,s23,s24)
    });
}


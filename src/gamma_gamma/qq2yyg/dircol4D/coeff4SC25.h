/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 25: box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14)

// Coefficient order epsilon^0 of master 25
template<>
double qq2yyg4SC<25,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-3)*pow(s12+s14+s24,-1)*(s24*(3*s13*(3*s14+3*s23+4*s24)+s14*(13*s23+12*s24)+3*(s13*s13)+6*(s14*s14)+6*((s23+s24)*(s23+s24)))*pow(s12,5)+s24*(3*s13+4*(s14+s23+s24))*pow(s12,6)+s24*pow(s12,7)+(s14+s24)*(s23+s24)*pow(s13,3)*pow(s24,3)+s14*(s14+s23+s24)*pow(s23,3)*pow(s24,3)-s13*(s23*s23)*(s14*s23*s24*(7*s23*s24+3*(s23*s23)+2*(s24*s24))-s24*(s23+s24)*pow(s14,3)+s14*s14*(3*s24*(s23*s23)+pow(s23,3)-2*pow(s24,3))+s24*s24*(4*s24*(s23*s23)+3*s23*(s24*s24)+2*pow(s23,3)+pow(s24,3)))-s23*s24*(s13*s13)*(2*s24*(s23+s24)*(s14*s14)+s24*(s24*(s23*s23)+2*s23*(s24*s24)+pow(s23,3)+pow(s24,3))+s14*(s24*(s23*s23)+3*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3)))+pow(s12,3)*(s24*(s13*s13)*(17*s23*s24+9*s14*(s23+2*s24)+3*(s14*s14)+4*(s23*s23)+18*(s24*s24))+s24*(s14+s23+4*s24)*pow(s13,3)+s13*(3*s24*(5*s23+6*s24)*(s14*s14)+3*s24*pow(s14,3)+s24*(17*s24*(s23*s23)+24*s23*(s24*s24)+3*pow(s23,3)+12*pow(s24,3))+s14*(18*s24*(s23*s23)+39*s23*(s24*s24)-pow(s23,3)+27*pow(s24,3)))+s24*(7*(s23*s23)*(s24*s24)+6*(s14*s14)*(3*s23*s24+2*(s23*s23)+s24*s24)+s14*(7*s23+4*s24)*((s23+s24)*(s23+s24))+(7*s23+4*s24)*pow(s14,3)+pow(s14,4)+4*s24*pow(s23,3)+pow(s23,4)+4*s23*pow(s24,3)+pow(s24,4)))-s12*(-((s14*(2*s23+3*s24)+s24*(3*s23+4*s24))*(s24*s24)*pow(s13,3))-s23*s23*(s14*(3*s23+2*s24)+s14*s14+(s23+s24)*(s23+s24))*pow(s24,3)+s13*s23*(s24*s24*pow(s14,3)+s14*s14*(s24*(s23*s23)-s23*(s24*s24)+2*pow(s23,3)+3*pow(s24,3))+s24*(10*(s23*s23)*(s24*s24)+8*s24*pow(s23,3)+pow(s23,4)+5*s23*pow(s24,3)+pow(s24,4))+s14*(9*(s23*s23)*(s24*s24)+8*s24*pow(s23,3)+pow(s23,4)+3*s23*pow(s24,3)+3*pow(s24,4)))-s13*s13*((s23+3*s24)*(s14*s14)*(s24*s24)+s24*(-2*s24*pow(s23,3)-pow(s23,4)+3*s23*pow(s24,3)+3*pow(s24,4))+s14*(2*(s23*s23)*(s24*s24)-pow(s23,4)+3*s23*pow(s24,3)+6*pow(s24,4))))+s12*s12*(s24*(3*s24*(s23+2*s24)+s14*(s23+3*s24))*pow(s13,3)+s23*s24*(2*s23*(s23+s24)*(s24*s24)+3*(s14*s14)*((s23+s24)*(s23+s24))+3*(s23+s24)*pow(s14,3)+pow(s14,4)+s14*(3*s24*(s23*s23)+5*s23*(s24*s24)+pow(s23,3)+pow(s24,3)))+s13*s13*(3*s24*(s23+2*s24)*(s14*s14)+s24*(6*s24*(s23*s23)+15*s23*(s24*s24)-pow(s23,3)+12*pow(s24,3))+s14*(5*s24*(s23*s23)+15*s23*(s24*s24)-pow(s23,3)+18*pow(s24,3)))+s13*(3*s24*(s23+s24)*pow(s14,3)+s14*s14*(8*s24*(s23*s23)+12*s23*(s24*s24)-pow(s23,3)+9*pow(s24,3))+s24*(3*(s23*s23)*(s24*s24)-4*s24*pow(s23,3)-2*pow(s23,4)+6*s23*pow(s24,3)+3*pow(s24,4))+s14*(15*(s23*s23)*(s24*s24)+s24*pow(s23,3)-2*pow(s23,4)+15*s23*pow(s24,3)+9*pow(s24,4))))+s24*pow(s12,4)*(6*(s14+s23+2*s24)*(s13*s13)+3*(5*s23+4*s24)*(s14*s14)+3*s14*(9*s23*s24+5*(s23*s23)+4*(s24*s24))+s13*(3*s14*(7*s23+9*s24)+9*(s14*s14)+2*(13*s23*s24+5*(s23*s23)+9*(s24*s24)))+pow(s13,3)+4*pow(s14,3)+4*pow(s23+s24,3)))*pow(s12+s23+s24,-1))/4.;
}

// Coefficient order epsilon^1 of master 25
template<>
double qq2yyg4SC<25,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s12,-1)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-3)*(-((3*s23-s24)*(3*s13+4*s14+5*s23+4*s24)*pow(s12,7))+(-3*s23+s24)*pow(s12,8)+6*(s13+s14)*s24*(s14+s24)*(-(s14*s23*(s14+s23+s24))+s13*(s14*(-s23+s24)+s24*(s23+s24))+s24*(s13*s13))*pow(s23,3)+pow(s12,6)*((-9*s23+3*s24)*(s13*s13)+6*(-3*s23+s24)*(s14*s14)-34*s24*(s23*s23)+s13*(9*s14*(-3*s23+s24)-4*(5*s23*s24+9*(s23*s23)-3*(s24*s24)))-2*s14*(10*s23*s24+27*(s23*s23)-6*(s24*s24))+s23*(s24*s24)-25*pow(s23,3)+6*pow(s24,3))-pow(s12,5)*(s13*s13*(6*s14*(3*s23-s24)+19*s23*s24+27*(s23*s23)-12*(s24*s24))+6*(s14*s14)*(3*s23*s24+12*(s23*s23)-2*(s24*s24))+11*(s23*s23)*(s24*s24)+(3*s23-s24)*pow(s13,3)+4*(3*s23-s24)*pow(s14,3)+37*s24*pow(s23,3)+17*pow(s23,4)+s13*(9*(3*s23-s24)*(s14*s14)+83*s24*(s23*s23)+s14*(41*s23*s24+96*(s23*s23)-27*(s24*s24))+2*s23*(s24*s24)+37*pow(s23,3)-18*pow(s24,3))+s14*(97*s24*(s23*s23)-9*s23*(s24*s24)+84*pow(s23,3)-12*pow(s24,3))-15*s23*pow(s24,3)-4*pow(s24,4))+s12*(-(s14*pow(s23,3)*(2*s14*s24*(18*s23*s24+5*(s23*s23)+9*(s24*s24))+s14*s14*(33*s23*s24+4*(s23*s23)+25*(s24*s24))+3*(s23+3*s24)*pow(s14,3)+2*s24*(-2*s24*(s23*s23)+s23*(s24*s24)-2*pow(s23,3)+pow(s24,3))))+s13*(s23*s23)*(-(s24*(s14*s14)*(27*s23*s24+24*(s23*s23)+23*(s24*s24)))-(19*s23*s24+4*(s23*s23)+11*(s24*s24))*pow(s14,3)+2*s14*(11*(s23*s23)*(s24*s24)+12*s24*pow(s23,3)+2*pow(s23,4)+s23*pow(s24,3)-8*pow(s24,4))+2*s24*(13*(s23*s23)*(s24*s24)+13*s24*pow(s23,3)+4*pow(s23,4)+2*s23*pow(s24,3)-2*pow(s24,4)))+s24*pow(s13,3)*(-8*(s23*s23)*(s24*s24)+6*s24*pow(s23,3)+4*pow(s23,4)+3*s23*pow(s24,3)+s14*(-8*s24*(s23*s23)+3*s23*(s24*s24)-2*pow(s23,3)+pow(s24,3))+pow(s24,4))+s23*(s13*s13)*(s24*(s14*s14)*(-19*s23*s24-10*(s23*s23)+s24*s24)+2*s23*s24*(15*s24*(s23*s23)+7*s23*(s24*s24)+6*pow(s23,3)-6*pow(s24,3))+s14*(10*(s23*s23)*(s24*s24)+16*s24*pow(s23,3)+4*pow(s23,4)-29*s23*pow(s24,3)+pow(s24,4))))-pow(s12,4)*((s14*(3*s23-s24)+6*s23*s24+6*(s23*s23)-4*(s24*s24))*pow(s13,3)+(4*s23*s24+42*(s23*s23)-4*(s24*s24))*pow(s14,3)+(3*s23-s24)*pow(s14,4)-8*(s24*s24)*pow(s23,3)+5*s24*pow(s23,4)+4*pow(s23,5)+s13*s13*((9*s23-3*s24)*(s14*s14)+69*s24*(s23*s23)+6*s14*(4*s23*s24+8*(s23*s23)-3*(s24*s24))+3*s23*(s24*s24)+8*pow(s23,3)-18*pow(s24,3))+s14*s14*(95*s24*(s23*s23)-15*s23*(s24*s24)+102*pow(s23,3)-6*pow(s24,3))-24*(s23*s23)*pow(s24,3)+s13*(2*(s14*s14)*(11*s23*s24+42*(s23*s23)-9*(s24*s24))+(9*s23-3*s24)*pow(s14,3)+s14*(167*s24*(s23*s23)-9*s23*(s24*s24)+97*pow(s23,3)-27*pow(s24,3))+2*(31*(s23*s23)*(s24*s24)+23*s24*pow(s23,3)+pow(s23,4)-12*s23*pow(s24,3)-6*pow(s24,4)))+2*s14*(15*(s23*s23)*(s24*s24)+57*s24*pow(s23,3)+27*pow(s23,4)-15*s23*pow(s24,3)-2*pow(s24,4))-14*s23*pow(s24,4)-pow(s24,5))+pow(s12,3)*(pow(s13,3)*(-20*s24*(s23*s23)-3*s14*(s23*s24+2*(s23*s23)-s24*s24)+4*pow(s23,3)+6*pow(s24,3))+s13*s13*(-69*(s23*s23)*(s24*s24)+s14*s14*(-5*s23*s24-21*(s23*s23)+6*(s24*s24))-8*s24*pow(s23,3)+16*pow(s23,4)+15*s23*pow(s24,3)+s14*(-89*s24*(s23*s23)+7*s23*(s24*s24)-22*pow(s23,3)+18*pow(s24,3))+12*pow(s24,4))+s23*((-35*s23*s24-52*(s23*s23)+7*(s24*s24))*pow(s14,3)+(-9*s23+s24)*pow(s14,4)-s14*s14*(122*s24*(s23*s23)+25*s23*(s24*s24)+60*pow(s23,3)-15*pow(s24,3))+2*s24*(17*(s23*s23)*(s24*s24)+14*s24*pow(s23,3)+5*pow(s23,4)+10*s23*pow(s24,3)+2*pow(s24,4))+s14*(-31*(s23*s23)*(s24*s24)-39*s24*pow(s23,3)-12*pow(s23,4)+21*s23*pow(s24,3)+13*pow(s24,4)))+s13*(-((s23*s24+24*(s23*s23)-3*(s24*s24))*pow(s14,3))+s24*s24*pow(s23,3)+48*s24*pow(s23,4)+12*pow(s23,5)-23*(s23*s23)*pow(s24,3)+s14*s14*(-103*s24*(s23*s23)+13*s23*(s24*s24)-79*pow(s23,3)+9*pow(s24,3))+19*s23*pow(s24,4)+s14*(-95*(s23*s23)*(s24*s24)-101*s24*pow(s23,3)-20*pow(s23,4)+33*s23*pow(s24,3)+9*pow(s24,4))+3*pow(s24,5)))+s12*s12*(pow(s13,3)*(-2*s24*(7*s14+11*s24)*(s23*s23)+3*s23*(s14+2*s24)*(s24*s24)+4*s24*pow(s23,3)+4*pow(s23,4)+(3*s14+4*s24)*pow(s24,3))+s23*s23*(2*s24*(3*s23*s24+2*(s23*s23)+2*(s24*s24))*((s23+s24)*(s23+s24))-2*(27*s23*s24+13*(s23*s23)+3*(s24*s24))*pow(s14,3)-3*(3*s23+s24)*pow(s14,4)+s14*s14*(-61*s24*(s23*s23)-60*s23*(s24*s24)-12*pow(s23,3)+pow(s24,3))+s14*(6*s24*pow(s23,3)+4*s23*pow(s24,3)+8*pow(s24,4)))+s13*s13*(8*(s24*s24)*pow(s23,3)+42*s24*pow(s23,4)+8*pow(s23,5)-39*(s23*s23)*pow(s24,3)+s14*s14*(-30*s24*(s23*s23)+5*s23*(s24*s24)-10*pow(s23,3)+3*pow(s24,3))+8*s23*pow(s24,4)+2*s14*(-35*(s23*s23)*(s24*s24)-9*s24*pow(s23,3)+4*pow(s23,4)+7*s23*pow(s24,3)+3*pow(s24,4))+3*pow(s24,5))+s13*s23*((-19*s23*s24-19*(s23*s23)+2*(s24*s24))*pow(s14,3)-2*(s14*s14)*(37*s24*(s23*s23)+25*s23*(s24*s24)+11*pow(s23,3)-4*pow(s24,3))+2*s14*(-16*(s23*s23)*(s24*s24)+12*s24*pow(s23,3)+6*pow(s23,4)-20*s23*pow(s24,3)+5*pow(s24,4))+2*(35*(s24*s24)*pow(s23,3)+23*s24*pow(s23,4)+2*pow(s23,5)+7*(s23*s23)*pow(s24,3)-6*s23*pow(s24,4)+2*pow(s24,5)))))*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1))/8.;
}

// Coefficient order epsilon^2 of master 25
template<>
double qq2yyg4SC<25,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s12,-1)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-3)*((11*s23*s24+3*s13*(s23+s24)+4*s14*(s23+s24)+5*(s23*s23)+4*(s24*s24))*pow(s12,7)+(s23+s24)*pow(s12,8)+pow(s12,6)*(3*(s23+s24)*(s13*s13)+6*(s23+s24)*(s14*s14)+38*s24*(s23*s23)+27*s23*(s24*s24)+s13*(29*s23*s24+9*s14*(s23+s24)+10*(s23*s23)+12*(s24*s24))+s14*(38*s23*s24+17*(s23*s23)+12*(s24*s24))+4*pow(s23,3)+6*pow(s24,3))+(s14+s24)*(s23*s23)*(-((3*s23-4*s24)*(s14+s23+s24)*(s14*s14)*(s23*s23))+s24*(-7*s23*s24+3*(s23*s23)-3*(s24*s24))*pow(s13,3)-s13*s14*s23*(-7*s24*(s23*s23)-9*s23*(s24*s24)+s14*(-14*s23*s24+6*(s23*s23)+s24*s24)+3*pow(s23,3)+pow(s24,3))-s13*s13*(s24*(4*s24*(s23*s23)+10*s23*(s24*s24)-3*pow(s23,3)+3*pow(s24,3))+s14*(-13*s24*(s23*s23)+8*s23*(s24*s24)+3*pow(s23,3)+3*pow(s24,3))))+pow(s12,5)*(74*(s23*s23)*(s24*s24)+3*(s14*s14)*(16*s23*s24+7*(s23*s23)+4*(s24*s24))+s13*s13*(25*s23*s24+6*s14*(s23+s24)+5*(s23*s23)+12*(s24*s24))+(s23+s24)*pow(s13,3)+4*(s23+s24)*pow(s14,3)+39*s24*pow(s23,3)-6*pow(s23,4)+25*s23*pow(s24,3)+s14*(116*s24*(s23*s23)+69*s23*(s24*s24)+10*pow(s23,3)+12*pow(s24,3))+s13*(9*(s23+s24)*(s14*s14)+77*s24*(s23*s23)+68*s23*(s24*s24)+s14*(74*s23*s24+24*(s23*s23)+27*(s24*s24))-3*pow(s23,3)+18*pow(s24,3))+4*pow(s24,4))+pow(s12,4)*((s14*(s23+s24)+s24*(7*s23+4*s24))*pow(s13,3)+(26*s23*s24+11*(s23*s23)+4*(s24*s24))*pow(s14,3)+(s23+s24)*pow(s14,4)+65*(s24*s24)*pow(s23,3)+6*s24*pow(s23,4)-9*pow(s23,5)+58*(s23*s23)*pow(s24,3)+s14*s14*(130*s24*(s23*s23)+57*s23*(s24*s24)+6*pow(s23,3)+6*pow(s24,3))+s13*s13*(3*(s23+s24)*(s14*s14)+51*s24*(s23*s23)+56*s23*(s24*s24)+s14*(42*s23*s24+7*(s23*s23)+18*(s24*s24))-11*pow(s23,3)+18*pow(s24,3))+8*s23*pow(s24,4)+s14*(171*(s23*s23)*(s24*s24)+110*s24*pow(s23,3)-24*pow(s23,4)+40*s23*pow(s24,3)+4*pow(s24,4))+s13*(138*(s23*s23)*(s24*s24)+s14*s14*(61*s23*s24+18*(s23*s23)+18*(s24*s24))+3*(s23+s24)*pow(s14,3)+46*s24*pow(s23,3)-21*pow(s23,4)+60*s23*pow(s24,3)+s14*(173*s24*(s23*s23)+125*s23*(s24*s24)-18*pow(s23,3)+27*pow(s24,3))+12*pow(s24,4))+pow(s24,5))-s12*s12*(s23*s23*(s24*(-2*s23*s24+s23*s23-4*(s24*s24))*((s23+s24)*(s23+s24))+(-57*s23*s24+24*(s23*s23)-34*(s24*s24))*pow(s14,3)+2*(s23-6*s24)*pow(s14,4)+s14*s14*(-17*s24*(s23*s23)-110*s23*(s24*s24)+36*pow(s23,3)-36*pow(s24,3))+s14*(-35*(s23*s23)*(s24*s24)+18*s24*pow(s23,3)+9*pow(s23,4)-63*s23*pow(s24,3)-18*pow(s24,4)))+pow(s13,3)*(-21*(s23*s23)*(s24*s24)+2*s24*pow(s23,3)+pow(s23,4)+s14*(-14*s24*(s23*s23)-9*s23*(s24*s24)+6*pow(s23,3)-3*pow(s24,3))-13*s23*pow(s24,3)-4*pow(s24,4))+s13*s13*(-23*(s24*s24)*pow(s23,3)+2*s24*pow(s23,4)+2*pow(s23,5)+3*(s14*s14)*(-11*s24*(s23*s23)-5*s23*(s24*s24)+5*pow(s23,3)-pow(s24,3))-48*(s23*s23)*pow(s24,3)+s14*(-92*(s23*s23)*(s24*s24)-32*s24*pow(s23,3)+31*pow(s23,4)-30*s23*pow(s24,3)-6*pow(s24,4))-13*s23*pow(s24,4)-3*pow(s24,5))+s13*s23*((-31*s23*s24+11*(s23*s23)-6*(s24*s24))*pow(s14,3)-10*(s24*s24)*pow(s23,3)+2*s24*pow(s23,4)+pow(s23,5)+s14*s14*(-71*s24*(s23*s23)-88*s23*(s24*s24)+61*pow(s23,3)-11*pow(s24,3))-25*(s23*s23)*pow(s24,3)+s14*(-101*(s23*s23)*(s24*s24)+6*s24*pow(s23,3)+35*pow(s23,4)-70*s23*pow(s24,3)-4*pow(s24,4))-12*s23*pow(s24,4)+pow(s24,5)))-s12*(s14*pow(s23,3)*(s14*s14*(-13*s23*s24+18*(s23*s23)-30*(s24*s24))+6*(s23-2*s24)*pow(s14,3)+s14*(14*s24*(s23*s23)-29*s23*(s24*s24)+9*pow(s23,3)-26*pow(s24,3))+s24*(s24*(s23*s23)-11*s23*(s24*s24)+4*pow(s23,3)-8*pow(s24,3)))+s13*(s23*s23)*((-25*s23*s24+18*(s23*s23)-5*(s24*s24))*pow(s14,3)+s14*s14*(-15*s24*(s23*s23)-42*s23*(s24*s24)+29*pow(s23,3)-10*pow(s24,3))+s14*(-25*(s23*s23)*(s24*s24)+7*s24*pow(s23,3)+4*pow(s23,4)-16*s23*pow(s24,3)-4*pow(s24,4))+s24*(-(s23*s23*(s24*s24))-2*s24*pow(s23,3)-pow(s23,4)+s23*pow(s24,3)+pow(s24,4)))+pow(s13,3)*(s14*(-11*(s23*s23)*(s24*s24)-7*s24*pow(s23,3)+3*pow(s23,4)-4*s23*pow(s24,3)-pow(s24,4))-s24*(6*(s23*s23)*(s24*s24)-5*s24*pow(s23,3)+2*pow(s23,4)+4*s23*pow(s24,3)+pow(s24,4)))+s23*(s13*s13)*(-3*s24*pow(s23,4)+s14*s23*(-12*s24*(s23*s23)-9*s23*(s24*s24)+7*pow(s23,3)-11*pow(s24,3))+s14*s14*(-19*s24*(s23*s23)-13*s23*(s24*s24)+15*pow(s23,3)-pow(s24,3))+11*(s23*s23)*pow(s24,3)+s23*pow(s24,4)+pow(s24,5)))+pow(s12,3)*(pow(s13,3)*(3*s23*s24*(2*s14+5*s24)+12*s24*(s23*s23)+3*(s14+2*s24)*(s24*s24)-4*pow(s23,3))+s13*s13*(92*(s23*s23)*(s24*s24)+s14*s14*(17*s23*s24+2*(s23*s23)+6*(s24*s24))+13*s24*pow(s23,3)-13*pow(s23,4)+48*s23*pow(s24,3)+s14*(82*s24*(s23*s23)+66*s23*(s24*s24)-27*pow(s23,3)+18*pow(s24,3))+12*pow(s24,4))+s23*((64*s23*s24-2*(s23*s23)+15*(s24*s24))*pow(s14,3)+(2*s23+5*s24)*pow(s14,4)+s14*s14*(116*s24*(s23*s23)+131*s23*(s24*s24)-36*pow(s23,3)+15*pow(s24,3))+s14*(144*(s23*s23)*(s24*s24)+14*s24*pow(s23,3)-30*pow(s23,4)+90*s23*pow(s24,3)+5*pow(s24,4))+s23*(16*(s23*s23)*(s24*s24)-6*s24*pow(s23,3)-3*pow(s23,4)+40*s23*pow(s24,3)+21*pow(s24,4)))+s13*((16*s23*s24+4*(s23*s23)+3*(s24*s24))*pow(s14,3)+75*(s24*s24)*pow(s23,3)-4*s24*pow(s23,4)-12*pow(s23,5)+84*(s23*s23)*pow(s24,3)+s14*s14*(127*s24*(s23*s23)+63*s23*(s24*s24)-26*pow(s23,3)+9*pow(s24,3))+17*s23*pow(s24,4)+s14*(215*(s23*s23)*(s24*s24)+92*s24*pow(s23,3)-64*pow(s23,4)+64*s23*pow(s24,3)+9*pow(s24,4))+3*pow(s24,5))))*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1))/4.;
}

// Coefficient of master 25 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg4SC<25>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yyg4SC<25,0>(s12,s13,s14,s23,s24),
        qq2yyg4SC<25,1>(s12,s13,s14,s23,s24),
        qq2yyg4SC<25,2>(s12,s13,s14,s23,s24)
    });
}

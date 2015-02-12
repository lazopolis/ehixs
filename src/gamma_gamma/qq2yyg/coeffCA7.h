/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 7: bubble(-s12-s13-s14-s23-s24)

// Coefficient order epsilon^-1 of master 7
template<>
double qq2yygCA<7,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-2)*pow(s14,-2)*pow(s13+s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*pow(s24,-2)*pow(s23+s24,-1)*(-((s23*s24*(s14*s14)*(s23*s24-s23*s23+2*(s24*s24))+s13*s14*(s14*s14*(s23*s24+s23*s23+s24*s24)+s23*s24*(s23*s24-s23*s23+2*(s24*s24)))+s13*s13*(s14*s14*(s23*s24+s23*s23+s24*s24)+s23*s24*(s23*s24-s23*s23+2*(s24*s24))))*pow(s12,6))+s24*(s23+s24)*(3*s23*s24+s14*(s23+2*s24)+2*(s23*s23)+s24*s24)*pow(s14,5)*pow(s23,3)-s23*s24*pow(s13,5)*(s14*s23*(s23+s24)*(s24*s24)-(s23-s24)*(s24*s24)*((s23+s24)*(s23+s24))+s14*s14*(4*s24*(s23*s23)-3*s23*(s24*s24)+4*pow(s23,3)-2*pow(s24,3)))+s13*(s23*s23)*pow(s14,4)*(s24*(-3*s23*s24+2*(s23*s23)-2*(s24*s24))*((s23+s24)*(s23+s24))+s14*s14*(s24*(s23*s23)+s23*(s24*s24)+pow(s23,3)+2*pow(s24,3))+s14*s23*(4*s24*(s23*s23)+8*s23*(s24*s24)+pow(s23,3)+5*pow(s24,3)))-pow(s12,5)*(-(s23*s24*(s23+s24)*(s14*s14)*(s14*(3*s23-5*s24)+3*(-(s23*s24)+s23*s23-2*(s24*s24))))+(3*(s14*s14)*(s23*s24+s23*s23+s24*s24)+s23*s24*(2*s23*s24-3*(s23*s23)+5*(s24*s24)))*pow(s13,3)+s13*s14*(s14*s23*s24*(6*s23*s24-5*(s23*s23)+11*(s24*s24))-3*s23*(s23-2*s24)*s24*((s23+s24)*(s23+s24))+3*(s23*s24+s23*s23+s24*s24)*pow(s14,3)+s14*s14*(4*s24*(s23*s23)+5*s23*(s24*s24)+2*pow(s23,3)+2*pow(s24,3)))+s13*s13*(3*s14*s23*s24*(s23*s24-2*(s23*s23)+3*(s24*s24))-3*s23*(s23-2*s24)*s24*((s23+s24)*(s23+s24))+6*(s23*s24+s23*s23+s24*s24)*pow(s14,3)+s14*s14*(4*s24*(s23*s23)+5*s23*(s24*s24)+2*pow(s23,3)+2*pow(s24,3))))+s14*pow(s13,4)*(s23*(4*s23-s24)*((s23+s24)*(s23+s24))*pow(s24,3)+s23*(s14*s14)*(-10*(s23*s23)*(s24*s24)-9*s24*pow(s23,3)+2*pow(s23,4)+11*s23*pow(s24,3)+6*pow(s24,4))+s14*s24*(2*(s24*s24)*pow(s23,3)-4*s24*pow(s23,4)-3*pow(s23,5)+8*(s23*s23)*pow(s24,3)+6*s23*pow(s24,4)+pow(s24,5)))+s23*(s13*s13)*pow(s14,3)*(s24*((s23+s24)*(s23+s24))*(-5*s24*(s23*s23)+3*s23*(s24*s24)-pow(s23,3)+3*pow(s24,3))+s14*s14*(s24*pow(s23,3)+4*pow(s23,4)+9*s23*pow(s24,3)+2*pow(s24,4))+s14*(s24*pow(s23,4)+2*pow(s23,5)+4*(s23*s23)*pow(s24,3)+6*s23*pow(s24,4)+3*pow(s24,5)))+s14*s14*pow(s13,3)*(-(s23*s24*((s23+s24)*(s23+s24))*(2*s24*(s23*s23)-6*s23*(s24*s24)+pow(s23,3)-2*pow(s24,3)))+s23*(s14*s14)*(-7*(s23*s23)*(s24*s24)-5*s24*pow(s23,3)+5*pow(s23,4)+15*s23*pow(s24,3)+6*pow(s24,4))+s14*(-9*(s24*s24)*pow(s23,4)-5*s24*pow(s23,5)+pow(s23,6)+4*pow(s23,3)*pow(s24,3)+15*(s23*s23)*pow(s24,4)+9*s23*pow(s24,5)+pow(s24,6)))+s12*(s24*(s14*(7*s23+2*s24)+s23*(5*s23+3*s24)+2*(s14*s14))*(s23*s23)*((s23+s24)*(s23+s24))*pow(s14,4)+pow(s13,5)*(-(s14*(s23+s24)*(s23*s23)*(s24*s24))+s23*(s24*s24)*(3*s24*(s23*s23)-2*s23*(s24*s24)+2*pow(s23,3)-3*pow(s24,3))+s14*s14*(-(s23*s23*(s24*s24))+2*pow(s23,4)+s23*pow(s24,3)-pow(s24,4)))+s13*s23*pow(s14,3)*(s23*s24*(s23*s24+6*(s23*s23)-s24*s24)*((s23+s24)*(s23+s24))+pow(s14,3)*(2*s24*(s23*s23)+s23*(s24*s24)+pow(s23,3)+2*pow(s24,3))+2*(s14*s14)*(10*(s23*s23)*(s24*s24)+7*s24*pow(s23,3)+2*pow(s23,4)+7*s23*pow(s24,3)+pow(s24,4))+s14*s23*(33*(s23*s23)*(s24*s24)+18*s24*pow(s23,3)+2*pow(s23,4)+21*s23*pow(s24,3)+4*pow(s24,4)))+pow(s13,4)*(s23*(s23*s24+2*(s23*s23)-4*(s24*s24))*(s24*s24)*((s23+s24)*(s23+s24))+s14*s23*s24*(12*(s23*s23)*(s24*s24)+9*s24*pow(s23,3)+2*pow(s23,4)+s23*pow(s24,3)-4*pow(s24,4))+pow(s14,3)*(5*s24*pow(s23,3)+9*pow(s23,4)+7*s23*pow(s24,3)-2*pow(s24,4))+s14*s14*(-2*(s24*s24)*pow(s23,3)-2*s24*pow(s23,4)+3*pow(s23,5)+16*(s23*s23)*pow(s24,3)+13*s23*pow(s24,4)+2*pow(s24,5)))+s14*pow(s13,3)*(s23*s24*((s23+s24)*(s23+s24))*(6*s24*(s23*s23)+5*s23*(s24*s24)+2*pow(s23,3)-4*pow(s24,3))+pow(s14,3)*(4*(s23*s23)*(s24*s24)+12*s24*pow(s23,3)+13*pow(s23,4)+13*s23*pow(s24,3)-pow(s24,4))+s14*s14*(10*(s24*s24)*pow(s23,3)+7*s24*pow(s23,4)+11*pow(s23,5)+43*(s23*s23)*pow(s24,3)+26*s23*pow(s24,4)+3*pow(s24,5))+s14*(14*(s24*s24)*pow(s23,4)+5*s24*pow(s23,5)+pow(s23,6)+23*pow(s23,3)*pow(s24,3)+17*(s23*s23)*pow(s24,4)+5*s23*pow(s24,5)+pow(s24,6)))+s13*s13*(s14*s14)*(s23*s24*((s23+s24)*(s23+s24))*(2*s24*(s23*s23)-s23*(s24*s24)+4*pow(s23,3)-3*pow(s24,3))+s23*pow(s14,3)*(9*s24*(s23*s23)+4*s23*(s24*s24)+7*pow(s23,3)+9*pow(s24,3))+s14*s14*(27*(s24*s24)*pow(s23,3)+21*s24*pow(s23,4)+12*pow(s23,5)+38*(s23*s23)*pow(s24,3)+15*s23*pow(s24,4)+pow(s24,5))+s14*(26*(s24*s24)*pow(s23,4)+14*s24*pow(s23,5)+3*pow(s23,6)+30*pow(s23,3)*pow(s24,3)+22*(s23*s23)*pow(s24,4)+8*s23*pow(s24,5)+pow(s24,6))))-pow(s12,4)*(-(s23*s24*(s23+s24)*(s14*s14)*(4*(s23-s24)*(s14*s14)+s14*(-6*s23*s24+10*(s23*s23)-11*(s24*s24))+3*(s23-2*s24)*((s23+s24)*(s23+s24))))+(3*(s14*s14)*(s23*s24+s23*s23+s24*s24)+s23*s24*(s23*s24-3*(s23*s23)+4*(s24*s24)))*pow(s13,4)+pow(s13,3)*(9*(s23*s24+s23*s23+s24*s24)*pow(s14,3)+s14*s14*(7*s24*(s23*s23)+12*s23*(s24*s24)+2*pow(s23,3)+6*pow(s24,3))+s23*s24*(-5*s24*(s23*s23)+17*s23*(s24*s24)-7*pow(s23,3)+15*pow(s24,3))+s14*(-9*s24*pow(s23,3)+9*s23*pow(s24,3)))+s13*s13*(9*(s23*s24+s23*s23+s24*s24)*pow(s14,4)+s24*(s14*s14)*(10*s24*(s23*s23)+19*s23*(s24*s24)-9*pow(s23,3)+pow(s24,3))+pow(s14,3)*(15*s24*(s23*s23)+23*s23*(s24*s24)+7*pow(s23,3)+10*pow(s24,3))+s14*s23*s24*(-11*s24*(s23*s23)+28*s23*(s24*s24)-16*pow(s23,3)+23*pow(s24,3))-3*s23*(s23-2*s24)*s24*pow(s23+s24,3))+s13*s14*(3*(s23*s24+s23*s23+s24*s24)*pow(s14,4)+s24*(s14*s14)*(10*s24*(s23*s23)+17*s23*(s24*s24)-7*pow(s23,3)+pow(s24,3))+pow(s14,3)*(8*s24*(s23*s23)+11*s23*(s24*s24)+5*pow(s23,3)+4*pow(s24,3))+s14*s23*s24*(-(s24*(s23*s23))+41*s23*(s24*s24)-15*pow(s23,3)+27*pow(s24,3))-3*s23*(s23-2*s24)*s24*pow(s23+s24,3)))+s12*s12*(pow(s13,5)*(s23*s24*(3*s24*(s23*s23)-s23*(s24*s24)+pow(s23,3)-3*pow(s24,3))+s14*s14*(s24*(s23*s23)-2*s23*(s24*s24)+2*pow(s23,3)-2*pow(s24,3)))+s23*s24*(s23+s24)*pow(s14,3)*(s14*s14*(5*s23*s24+8*(s23*s23)-s24*s24)+(-2*s23*s24+4*(s23*s23)-s24*s24)*((s23+s24)*(s23+s24))+s23*pow(s14,3)+2*s14*(6*s24*(s23*s23)-s23*(s24*s24)+7*pow(s23,3)-pow(s24,3)))+pow(s13,4)*(pow(s14,3)*(3*s24*(s23*s23)-7*s23*(s24*s24)+5*pow(s23,3)-6*pow(s24,3))+s14*s23*s24*(12*s24*(s23*s23)+s23*(s24*s24)+6*pow(s23,3)-5*pow(s24,3))+s23*s24*(5*(s23*s23)*(s24*s24)+10*s24*pow(s23,3)+2*pow(s23,4)-15*s23*pow(s24,3)-12*pow(s24,4))+s14*s14*(6*s24*pow(s23,3)+7*pow(s23,4)+s23*pow(s24,3)-2*pow(s24,4)))+pow(s13,3)*(3*pow(s14,4)*(s24*(s23*s23)-3*s23*(s24*s24)+pow(s23,3)-2*pow(s24,3))+s14*s23*s24*(24*(s23*s23)*(s24*s24)+36*s24*pow(s23,3)+12*pow(s23,4)-17*s23*pow(s24,3)-17*pow(s24,4))+pow(s14,3)*(5*(s23*s23)*(s24*s24)+22*s24*pow(s23,3)+18*pow(s23,4)+7*s23*pow(s24,3)-3*pow(s24,4))+s14*s14*(26*(s24*s24)*pow(s23,3)+19*s24*pow(s23,4)+5*pow(s23,5)+15*(s23*s23)*pow(s24,3)+4*s23*pow(s24,4)+2*pow(s24,5))+s23*s24*(2*s23*s24+s23*s23-5*(s24*s24))*pow(s23+s24,3))+s14*(s13*s13)*(-(pow(s14,4)*(-(s24*(s23*s23))+5*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3)))+pow(s14,3)*(9*(s23*s23)*(s24*s24)+24*s24*pow(s23,3)+13*pow(s23,4)+9*s23*pow(s24,3)-pow(s24,4))+s14*s23*(44*(s24*s24)*pow(s23,3)+24*s24*pow(s23,4)+pow(s23,5)+5*(s23*s23)*pow(s24,3)-35*s23*pow(s24,4)-19*pow(s24,5))+s14*s14*(45*(s24*s24)*pow(s23,3)+37*s24*pow(s23,4)+10*pow(s23,5)+24*(s23*s23)*pow(s24,3)+6*s23*pow(s24,4)+2*pow(s24,5))+s23*s24*(s23*s24+4*(s23*s23)-5*(s24*s24))*pow(s23+s24,3))+s13*s23*(s14*s14)*(-((s23*s23+s24*s24)*pow(s14,4))+pow(s14,3)*(9*s24*(s23*s23)+5*s23*(s24*s24)+2*pow(s23,3)+3*pow(s24,3))+s14*s14*(40*(s23*s23)*(s24*s24)+31*s24*pow(s23,3)+5*pow(s23,4)+13*s23*pow(s24,3)-2*pow(s24,4))+s14*(42*(s24*s24)*pow(s23,3)+26*s24*pow(s23,4)+pow(s23,5)+5*(s23*s23)*pow(s24,3)-21*s23*pow(s24,4)-9*pow(s24,5))+s24*(-2*s23*s24+5*(s23*s23)-5*(s24*s24))*pow(s23+s24,3)))-pow(s12,3)*(pow(s13,5)*(s14*s14*(s23*s24+s23*s23+s24*s24)-s24*pow(s23,3)+s23*pow(s24,3))+pow(s13,4)*(-2*s14*s23*s24*(s23*s24+2*(s23*s23)-s24*s24)+4*(s23*s24+s23*s23+s24*s24)*pow(s14,3)+s14*s14*(2*s24*(s23*s23)+9*s23*(s24*s24)-2*pow(s23,3)+6*pow(s24,3))+s23*s24*(-8*s24*(s23*s23)+9*s23*(s24*s24)-5*pow(s23,3)+12*pow(s24,3)))+pow(s13,3)*(-(s23*s24*(2*s23*s24+5*(s23*s23)-15*(s24*s24))*((s23+s24)*(s23+s24)))+s24*(25*s23*s24+8*(s23*s23)+14*(s24*s24))*pow(s14,3)+6*(s23*s24+s23*s23+s24*s24)*pow(s14,4)+s14*s23*s24*(-27*s24*(s23*s23)+14*s23*(s24*s24)-19*pow(s23,3)+22*pow(s24,3))+s14*s14*(2*(s23*s23)*(s24*s24)-11*s24*pow(s23,3)-5*pow(s23,4)+8*s23*pow(s24,3)+2*pow(s24,4)))-s23*s24*(s23+s24)*(s14*s14)*(s14*s14*(13*(s23*s23)-6*(s24*s24))+(3*s23-s24)*pow(s14,3)+s14*(5*s24*(s23*s23)-13*s23*(s24*s24)+11*pow(s23,3)-7*pow(s24,3))+(s23-2*s24)*pow(s23+s24,3))+s13*s14*(-(s14*s23*s24*(-10*s23*s24+15*(s23*s23)-21*(s24*s24))*((s23+s24)*(s23+s24)))+(s23*s24+s23*s23+s24*s24)*pow(s14,5)+pow(s14,4)*(4*s24*(s23*s23)+7*s23*(s24*s24)+4*pow(s23,3)+2*pow(s24,3))+pow(s14,3)*(s23*s23*(s24*s24)-10*s24*pow(s23,3)-pow(s23,4)+7*s23*pow(s24,3)+pow(s24,4))+s23*(s14*s14)*(-21*(s23*s23)*(s24*s24)-30*s24*pow(s23,3)-2*pow(s23,4)+29*s23*pow(s24,3)+22*pow(s24,4))-s23*(s23-2*s24)*s24*pow(s23+s24,4))+s13*s13*(-(s14*s23*s24*(-(s23*s24)+14*(s23*s23)-19*(s24*s24))*((s23+s24)*(s23+s24)))+4*(s23*s24+s23*s23+s24*s24)*pow(s14,5)+pow(s14,4)*(10*s24*(s23*s23)+23*s23*(s24*s24)+6*pow(s23,3)+10*pow(s24,3))+pow(s14,3)*(7*(s23*s23)*(s24*s24)-15*s24*pow(s23,3)-6*pow(s23,4)+13*s23*pow(s24,3)+3*pow(s24,4))+s23*(s14*s14)*(-26*(s23*s23)*(s24*s24)-32*s24*pow(s23,3)-2*pow(s23,4)+35*s23*pow(s24,3)+31*pow(s24,4))-s23*(s23-2*s24)*s24*pow(s23+s24,4))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/2.;
}

// Coefficient order epsilon^0 of master 7
template<>
double qq2yygCA<7,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s12,-1)*pow(s13,-2)*pow(s14,-2)*pow(s13+s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*pow(s24,-2)*pow(s23+s24,-1)*(2*(2*s23*s24*(s14*s14)*(s23*s24-s23*s23+2*(s24*s24))+s13*s14*(s23*s24*(s23*s24-s23*s23+2*(s24*s24))+s14*s14*(s23*s24+2*(s23*s23)+2*(s24*s24)))+s13*s13*(2*s23*s24*(s23*s24-s23*s23+2*(s24*s24))+s14*s14*(s23*s24+2*(s23*s23)+2*(s24*s24))))*pow(s12,7)+2*pow(s12,6)*(-(s23*s24*(s23+s24)*(s14*s14)*(s14*(7*s23-12*s24)+6*(-(s23*s24)+s23*s23-2*(s24*s24))))+3*(2*s23*s24*(s23*s24-s23*s23+2*(s24*s24))+s14*s14*(s23*s24+2*(s23*s23)+2*(s24*s24)))*pow(s13,3)+s13*s14*(s14*s23*s24*(15*s23*s24-7*(s23*s23)+22*(s24*s24))-3*s23*(s23-2*s24)*s24*((s23+s24)*(s23+s24))+3*(s23*s24+2*(s23*s23)+2*(s24*s24))*pow(s14,3)+s14*s14*(9*s24*(s23*s23)+12*s23*(s24*s24)+5*pow(s23,3)+6*pow(s24,3)))+s13*s13*(2*s14*s23*s24*(3*s23*s24-5*(s23*s23)+8*(s24*s24))-6*s23*(s23-2*s24)*s24*((s23+s24)*(s23+s24))+6*(s23*s24+2*(s23*s23)+2*(s24*s24))*pow(s14,3)+s14*s14*(9*s24*(s23*s23)+12*s23*(s24*s24)+5*pow(s23,3)+6*pow(s24,3))))-2*s13*s23*s24*(s23+s24)*(s14*s14)*(s24*(3*s23*s24+4*(s23*s23)-2*(s24*s24))*pow(s13,4)-(s14+s23+s24)*(5*s23+6*s24)*(s23*s23)*pow(s14,3)+s13*s23*(s14*s14)*(-13*s24*(s23*s23)-2*s14*(5*s23*s24+7*(s23*s23)-4*(s24*s24))+4*s23*(s24*s24)-9*pow(s23,3)+8*pow(s24,3))+pow(s13,3)*(s24*(7*s24*(s23*s23)+s23*(s24*s24)+4*pow(s23,3)-2*pow(s24,3))-2*s14*(-5*s24*(s23*s23)-7*s23*(s24*s24)+2*pow(s23,3)+2*pow(s24,3)))+s14*(s13*s13)*(17*(s23*s23)*(s24*s24)+2*s24*pow(s23,3)-4*pow(s23,4)+s14*(2*s24*(s23*s23)+19*s23*(s24*s24)-13*pow(s23,3)-2*pow(s24,3))+9*s23*pow(s24,3)-2*pow(s24,4)))+s12*(-2*s24*(s23+s24)*(s14*(2*s23+5*s24)+3*(3*s23*s24+2*(s23*s23)+s24*s24))*pow(s14,5)*pow(s23,3)+s24*pow(s13,5)*(-4*s23*(s23-2*s24)*(s24*s24)*((s23+s24)*(s23+s24))+s14*s23*s24*(11*s24*(s23*s23)+19*s23*(s24*s24)-pow(s23,3)+7*pow(s24,3))+2*(s14*s14)*(11*(s23*s23)*(s24*s24)+19*s24*pow(s23,3)+11*pow(s23,4)+8*s23*pow(s24,3)+2*pow(s24,4)))+s13*(s23*s23)*pow(s14,4)*(2*s24*(25*s23*s24+6*(s23*s23)+12*(s24*s24))*((s23+s24)*(s23+s24))+s14*s14*(22*s24*(s23*s23)+38*s23*(s24*s24)-4*pow(s23,3)+6*pow(s24,3))+s14*(101*(s23*s23)*(s24*s24)+39*s24*pow(s23,3)-4*pow(s23,4)+81*s23*pow(s24,3)+23*pow(s24,4)))+s23*(s13*s13)*pow(s14,3)*(2*s24*((s23+s24)*(s23+s24))*(33*s24*(s23*s23)+s23*(s24*s24)+19*pow(s23,3)-5*pow(s24,3))-2*(s14*s14)*(-50*(s23*s23)*(s24*s24)-29*s24*pow(s23,3)+11*pow(s23,4)+5*s23*pow(s24,3)+3*pow(s24,4))+s14*(254*(s24*s24)*pow(s23,3)+107*s24*pow(s23,4)-8*pow(s23,5)+166*(s23*s23)*pow(s24,3)+32*s23*pow(s24,4)+5*pow(s24,5)))+s14*pow(s13,4)*(-2*s23*(10*s23*s24+4*(s23*s23)-5*(s24*s24))*(s24*s24)*((s23+s24)*(s23+s24))+s14*s24*(26*(s24*s24)*pow(s23,3)+20*s24*pow(s23,4)+17*pow(s23,5)+42*(s23*s23)*pow(s24,3)+13*s23*pow(s24,4)-6*pow(s24,5))+s14*s14*(100*(s24*s24)*pow(s23,3)+58*s24*pow(s23,4)-14*pow(s23,5)+22*(s23*s23)*pow(s24,3)+26*s23*pow(s24,4)+8*pow(s24,5)))+s14*s14*pow(s13,3)*(2*s24*(s23*s23)*(7*s23*s24+8*(s23*s23)-10*(s24*s24))*((s23+s24)*(s23+s24))+4*(s14*s14)*(31*(s24*s24)*pow(s23,3)+18*s24*pow(s23,4)-8*pow(s23,5)-4*(s23*s23)*pow(s24,3)+s23*pow(s24,4)+pow(s24,5))+s14*(160*(s24*s24)*pow(s23,4)+81*s24*pow(s23,5)-4*pow(s23,6)+90*pow(s23,3)*pow(s24,3)+32*(s23*s23)*pow(s24,4)+11*s23*pow(s24,5)-6*pow(s24,6))))+s12*s12*(-2*s24*(s23+s24)*(s23*s23)*pow(s14,4)*((4*s23+5*s24)*(s14*s14)+24*s24*(s23*s23)+9*s23*(s24*s24)+s14*(23*s23*s24+19*(s23*s23)+4*(s24*s24))+14*pow(s23,3)-pow(s24,3))+pow(s13,5)*(-4*s23*(s24*s24)*(s24*(s23*s23)-7*s23*(s24*s24)+2*pow(s23,3)-6*pow(s24,3))+s14*s23*s24*(10*s24*(s23*s23)+25*s23*(s24*s24)-pow(s23,3)+14*pow(s24,3))-2*(s14*s14)*(-11*(s23*s23)*(s24*s24)+s24*pow(s23,3)+7*pow(s23,4)-10*s23*pow(s24,3)-6*pow(s24,4)))+pow(s13,4)*(-4*s23*(-(s23*s24)+2*(s23*s23)-6*(s24*s24))*(s24*s24)*((s23+s24)*(s23+s24))+pow(s14,3)*(74*(s23*s23)*(s24*s24)-14*s24*pow(s23,3)-56*pow(s23,4)+48*s23*pow(s24,3)+32*pow(s24,4))+s14*s23*s24*(-21*(s23*s23)*(s24*s24)-47*s24*pow(s23,3)-13*pow(s23,4)+63*s23*pow(s24,3)+50*pow(s24,4))+s23*(s14*s14)*(121*(s23*s23)*(s24*s24)+27*s24*pow(s23,3)-18*pow(s23,4)+129*s23*pow(s24,3)+65*pow(s24,4)))+s13*s23*pow(s14,3)*(-2*pow(s14,3)*(-3*s24*(s23*s23)-9*s23*(s24*s24)+2*pow(s23,3)+pow(s24,3))+2*s24*((s23+s24)*(s23+s24))*(8*s24*(s23*s23)+20*s23*(s24*s24)-8*pow(s23,3)+5*pow(s24,3))+s14*s14*(109*(s23*s23)*(s24*s24)+40*s24*pow(s23,3)-14*pow(s23,4)+66*s23*pow(s24,3)+23*pow(s24,4))+s14*(109*(s24*s24)*pow(s23,3)+4*s24*pow(s23,4)-8*pow(s23,5)+205*(s23*s23)*pow(s24,3)+143*s23*pow(s24,4)+35*pow(s24,5)))+s13*s13*(s14*s14)*(-2*s23*s24*((s23+s24)*(s23+s24))*(-(s24*(s23*s23))-25*s23*(s24*s24)+5*pow(s23,3)-14*pow(s24,3))+pow(s14,3)*(66*(s23*s23)*(s24*s24)+2*s24*pow(s23,3)-36*pow(s23,4)+4*s23*pow(s24,3)+8*pow(s24,4))+s14*s14*(325*(s24*s24)*pow(s23,3)+107*s24*pow(s23,4)-56*pow(s23,5)+193*(s23*s23)*pow(s24,3)+69*s23*pow(s24,4)+2*pow(s24,5))+s14*(275*(s24*s24)*pow(s23,4)+65*s24*pow(s23,5)-12*pow(s23,6)+373*pow(s23,3)*pow(s24,3)+243*(s23*s23)*pow(s24,4)+62*s23*pow(s24,5)-6*pow(s24,6)))+s14*pow(s13,3)*(-2*s23*s24*((s23+s24)*(s23+s24))*(17*s24*(s23*s23)+3*s23*(s24*s24)+6*pow(s23,3)-11*pow(s24,3))+pow(s14,3)*(100*(s23*s23)*(s24*s24)-16*s24*pow(s23,3)-74*pow(s23,4)+34*s23*pow(s24,3)+28*pow(s24,4))+s14*s14*(309*(s24*s24)*pow(s23,3)+87*s24*pow(s23,4)-60*pow(s23,5)+221*(s23*s23)*pow(s24,3)+97*s23*pow(s24,4)+2*pow(s24,5))+s14*(71*(s24*s24)*pow(s23,4)+14*s24*pow(s23,5)-4*pow(s23,6)+157*pow(s23,3)*pow(s24,3)+179*(s23*s23)*pow(s24,4)+69*s23*pow(s24,5)-6*pow(s24,6))))+pow(s12,5)*(-2*s23*s24*(s23+s24)*(s14*s14)*(2*(5*s23-6*s24)*(s14*s14)+s14*(-13*s23*s24+24*(s23*s23)-28*(s24*s24))+6*(s23-2*s24)*((s23+s24)*(s23+s24)))+6*(2*s23*s24*(s23*s24-s23*s23+2*(s24*s24))+s14*s14*(s23*s24+2*(s23*s23)+2*(s24*s24)))*pow(s13,4)+pow(s13,3)*(s14*s23*s24*(16*s23*s24-35*(s23*s23)+51*(s24*s24))+18*(s23*s24+2*(s23*s23)+2*(s24*s24))*pow(s14,3)+4*s23*s24*(-2*s24*(s23*s23)+23*s23*(s24*s24)-7*pow(s23,3)+18*pow(s24,3))+2*(s14*s14)*(15*s24*(s23*s23)+29*s23*(s24*s24)+5*pow(s23,3)+18*pow(s24,3)))+2*(s13*s13)*(-(s14*s23*(29*s23-40*s24)*s24*((s23+s24)*(s23+s24)))+9*(s23*s24+2*(s23*s23)+2*(s24*s24))*pow(s14,4)+pow(s14,3)*(31*s24*(s23*s23)+56*s23*(s24*s24)+17*pow(s23,3)+32*pow(s24,3))+s14*s14*(65*(s23*s23)*(s24*s24)+s24*pow(s23,3)+2*pow(s23,4)+69*s23*pow(s24,3)+6*pow(s24,4))-6*s23*(s23-2*s24)*s24*pow(s23+s24,3))+s13*s14*(6*(s23*s24+2*(s23*s23)+2*(s24*s24))*pow(s14,4)+pow(s14,3)*(32*s24*(s23*s23)+54*s23*(s24*s24)+24*pow(s23,3)+28*pow(s24,3))+2*s14*s23*s24*(9*s24*(s23*s23)+89*s23*(s24*s24)-24*pow(s23,3)+56*pow(s24,3))+s14*s14*(124*(s23*s23)*(s24*s24)+7*s24*pow(s23,3)+4*pow(s23,4)+127*s23*pow(s24,3)+12*pow(s24,4))-6*s23*(s23-2*s24)*s24*pow(s23+s24,3)))+pow(s12,3)*(-2*s23*s24*(s23+s24)*pow(s14,3)*(2*(s14*s14)*(5*s23*s24+10*(s23*s23)-2*(s24*s24))+(-3*s23*s24+10*(s23*s23)-4*(s24*s24))*((s23+s24)*(s23+s24))+2*s23*pow(s14,3)+s14*(34*s24*(s23*s23)-9*s23*(s24*s24)+38*pow(s23,3)-8*pow(s24,3)))+pow(s13,5)*(s14*s23*s24*(6*s23*s24-s23*s23+7*(s24*s24))-4*s23*s24*(2*s24*(s23*s23)-5*s23*(s24*s24)+pow(s23,3)-6*pow(s24,3))-2*(s14*s14)*(3*s24*(s23*s23)-5*s23*(s24*s24)+5*pow(s23,3)-6*pow(s24,3)))+pow(s13,4)*(pow(s14,3)*(-22*s24*(s23*s23)+36*s23*(s24*s24)-26*pow(s23,3)+40*pow(s24,3))+s14*s23*s24*(-26*s24*(s23*s23)+75*s23*(s24*s24)-31*pow(s23,3)+70*pow(s24,3))+4*s23*s24*(5*(s23*s23)*(s24*s24)-8*s24*pow(s23,3)-2*pow(s23,4)+29*s23*pow(s24,3)+18*pow(s24,4))+s14*s14*(94*(s23*s23)*(s24*s24)-12*s24*pow(s23,3)-40*pow(s23,4)+94*s23*pow(s24,3)+30*pow(s24,4)))+s13*(s14*s14)*(pow(s14,4)*(-4*s24*(s23*s23)+6*s23*(s24*s24)+4*pow(s23,3)+4*pow(s24,3))+pow(s14,3)*(76*(s23*s23)*(s24*s24)+15*s24*pow(s23,3)-4*pow(s23,4)+39*s23*pow(s24,3)+8*pow(s24,4))+s14*s14*(151*(s24*s24)*pow(s23,3)-6*s24*pow(s23,4)-16*pow(s23,5)+254*(s23*s23)*pow(s24,3)+123*s23*pow(s24,4)+4*pow(s24,5))+s14*s23*(-36*(s24*s24)*pow(s23,3)-73*s24*pow(s23,4)-4*pow(s23,5)+226*(s23*s23)*pow(s24,3)+290*s23*pow(s24,4)+97*pow(s24,5))-2*s23*s24*(-3*s23*s24+10*(s23*s23)-12*(s24*s24))*pow(s23+s24,3))+pow(s13,3)*(-6*pow(s14,4)*(5*s24*(s23*s23)-8*s23*(s24*s24)+3*pow(s23,3)-8*pow(s24,3))+pow(s14,3)*(244*(s23*s23)*(s24*s24)-12*s24*pow(s23,3)-94*pow(s23,4)+196*s23*pow(s24,3)+64*pow(s24,4))+s14*s23*s24*(-42*(s23*s23)*(s24*s24)-154*s24*pow(s23,3)-59*pow(s23,4)+148*s23*pow(s24,3)+95*pow(s24,4))+s23*(s14*s14)*(157*(s23*s23)*(s24*s24)-18*s24*pow(s23,3)-24*pow(s23,4)+330*s23*pow(s24,3)+185*pow(s24,4))-4*s23*s24*(s23*s24+s23*s23-6*(s24*s24))*pow(s23+s24,3))+s14*(s13*s13)*(2*pow(s14,4)*(-9*s24*(s23*s23)+14*s23*(s24*s24)+pow(s23,3)+12*pow(s24,3))+pow(s14,3)*(236*(s23*s23)*(s24*s24)+18*s24*pow(s23,3)-58*pow(s23,4)+148*s23*pow(s24,3)+42*pow(s24,4))+s14*s14*(374*(s24*s24)*pow(s23,3)+41*s24*pow(s23,4)-40*pow(s23,5)+523*(s23*s23)*pow(s24,3)+246*s23*pow(s24,4)+4*pow(s24,5))+2*s14*s23*(-28*(s24*s24)*pow(s23,3)-35*s24*pow(s23,4)-2*pow(s23,5)+122*(s23*s23)*pow(s24,3)+193*s23*pow(s24,4)+76*pow(s24,5))-2*s23*(9*s23-8*s24)*s24*pow(s23+s24,4)))+2*pow(s12,4)*((2*s23*s24*(s23*s24-s23*s23+2*(s24*s24))+s14*s14*(s23*s24+2*(s23*s23)+2*(s24*s24)))*pow(s13,5)+pow(s13,4)*(3*s14*s23*s24*(2*s23*s24-3*(s23*s23)+5*(s24*s24))+4*(s23*s24+2*(s23*s23)+2*(s24*s24))*pow(s14,3)+2*s23*s24*(-4*s24*(s23*s23)+19*s23*(s24*s24)-5*pow(s23,3)+18*pow(s24,3))+s14*s14*(3*s24*(s23*s23)+22*s23*(s24*s24)-5*pow(s23,3)+18*pow(s24,3)))+pow(s13,3)*(-2*s23*s24*(-(s23*s24)+5*(s23*s23)-18*(s24*s24))*((s23+s24)*(s23+s24))+6*(s23*s24+2*(s23*s23)+2*(s24*s24))*pow(s14,4)+pow(s14,3)*(11*s24*(s23*s23)+62*s23*(s24*s24)-pow(s23,3)+46*pow(s24,3))+s14*s23*s24*(-40*s24*(s23*s23)+63*s23*(s24*s24)-41*pow(s23,3)+62*pow(s24,3))+s14*s14*(84*(s23*s23)*(s24*s24)+s24*pow(s23,3)-11*pow(s23,4)+84*s23*pow(s24,3)+15*pow(s24,4)))-s23*s24*(s23+s24)*(s14*s14)*(s14*s14*(-2*s23*s24+34*(s23*s23)-20*(s24*s24))+(7*s23-4*s24)*pow(s14,3)+s14*(16*s24*(s23*s23)-31*s23*(s24*s24)+27*pow(s23,3)-20*pow(s24,3))+2*(s23-2*s24)*pow(s23+s24,3))+s13*s13*(4*(s23*s24+2*(s23*s23)+2*(s24*s24))*pow(s14,5)+pow(s14,4)*(13*s24*(s23*s23)+58*s23*(s24*s24)+13*pow(s23,3)+38*pow(s24,3))+pow(s14,3)*(160*(s23*s23)*(s24*s24)+29*s24*pow(s23,3)-9*pow(s23,4)+133*s23*pow(s24,3)+25*pow(s24,4))+s14*s14*(46*(s24*s24)*pow(s23,3)-37*s24*pow(s23,4)-3*pow(s23,5)+198*(s23*s23)*pow(s24,3)+120*s23*pow(s24,4)+2*pow(s24,5))-4*s14*s23*(7*s23-8*s24)*s24*pow(s23+s24,3)-2*s23*(s23-2*s24)*s24*pow(s23+s24,4))+s13*s14*(-(s14*s23*s24*(-21*s23*s24+27*(s23*s23)-46*(s24*s24))*((s23+s24)*(s23+s24)))+(s23*s24+2*(s23*s23)+2*(s24*s24))*pow(s14,5)+pow(s14,4)*(5*s24*(s23*s23)+18*s23*(s24*s24)+9*pow(s23,3)+10*pow(s24,3))+pow(s14,3)*(77*(s23*s23)*(s24*s24)+14*s24*pow(s23,3)+2*pow(s23,4)+64*s23*pow(s24,3)+10*pow(s24,4))+s14*s14*(50*(s24*s24)*pow(s23,3)-33*s24*pow(s23,4)-3*pow(s23,5)+174*(s23*s23)*pow(s24,3)+96*s23*pow(s24,4)+2*pow(s24,5))-s23*(s23-2*s24)*s24*pow(s23+s24,4))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/4.;
}

// Coefficient order epsilon^1 of master 7
template<>
double qq2yygCA<7,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s12,-1)*pow(s13,-2)*pow(s14,-2)*pow(s13+s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*pow(s24,-2)*pow(s23+s24,-1)*(2*pow(s12,7)*(s23*s24*(s14*s14)*(s23*s24-s23*s23+2*(s24*s24))+s13*s13*(s14*s14*(s23*s23+s24*s24)+s23*s24*(s23*s24-s23*s23+2*(s24*s24)))+s13*(s23*s23+s24*s24)*pow(s14,3))+2*pow(s12,6)*(-(s23*s24*(s23+s24)*(s14*s14)*(s14*(5*s23-7*s24)+3*(-(s23*s24)+s23*s23-2*(s24*s24))))+(3*(s14*s14)*(s23*s23+s24*s24)+s23*s24*(4*s23*s24-3*(s23*s23)+7*(s24*s24)))*pow(s13,3)+s13*(s14*s14)*(-3*s23*s24*(-2*s23*s24+s23*s23-3*(s24*s24))+3*(s14*s14)*(s23*s23+s24*s24)+2*s14*(s24*(s23*s23)+3*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3)))+s13*s13*(s14*s23*s24*(2*s23*s24-3*(s23*s23)+5*(s24*s24))-3*s23*(s23-2*s24)*s24*((s23+s24)*(s23+s24))+6*(s23*s23+s24*s24)*pow(s14,3)+2*(s14*s14)*(s24*(s23*s23)+3*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3))))+4*s13*s23*s24*(s23+s24)*(s14*s14)*(s24*(-(s23*s24)-2*(s23*s23)+3*(s24*s24))*pow(s13,4)+(s14+s23+s24)*(3*s23+5*s24)*(s23*s23)*pow(s14,3)+s13*s23*(s14*s14)*(8*s24*(s23*s23)+8*s14*(s23*s24+s23*s23-s24*s24)-5*s23*(s24*s24)+5*pow(s23,3)-8*pow(s24,3))+pow(s13,3)*(s24*(-3*s24*(s23*s23)+2*s23*(s24*s24)-2*pow(s23,3)+3*pow(s24,3))+2*s14*(-3*s24*(s23*s23)-5*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3)))+s14*(s13*s13)*(-13*(s23*s23)*(s24*s24)-2*s24*pow(s23,3)+2*pow(s23,4)-6*s23*pow(s24,3)+s14*(-(s24*(s23*s23))-17*s23*(s24*s24)+7*pow(s23,3)+3*pow(s24,3))+3*pow(s24,4)))-s12*s12*(2*s24*(s23+s24)*(s23*s23)*(2*s24*(s14*s14)+s14*(15*s23*s24+13*(s23*s23)+2*(s24*s24))+s23*(21*s23*s24+11*(s23*s23)+10*(s24*s24)))*pow(s14,4)+pow(s13,5)*(s14*s23*s24*(-15*s24*(s23*s23)-44*s23*(s24*s24)+pow(s23,3)-28*pow(s24,3))+2*s23*(s24*s24)*(-(s24*(s23*s23))-12*s23*(s24*s24)+2*pow(s23,3)-9*pow(s24,3))+2*(s14*s14)*(-8*(s23*s23)*(s24*s24)+3*s24*pow(s23,3)+6*pow(s23,4)-12*s23*pow(s24,3)-5*pow(s24,4)))+s13*s23*pow(s14,3)*(2*pow(s14,3)*(-5*s24*(s23*s23)-15*s23*(s24*s24)+3*pow(s23,3)-2*pow(s24,3))-2*s24*((s23+s24)*(s23+s24))*(12*s24*(s23*s23)+18*s23*(s24*s24)+2*pow(s23,3)+7*pow(s24,3))+s14*s14*(-338*(s23*s23)*(s24*s24)-162*s24*pow(s23,3)+8*pow(s23,4)-205*s23*pow(s24,3)-49*pow(s24,4))-s14*s24*(511*(s23*s23)*(s24*s24)+456*s24*pow(s23,3)+150*pow(s23,4)+264*s23*pow(s24,3)+59*pow(s24,4)))+s14*pow(s13,3)*(2*s23*s24*((s23+s24)*(s23+s24))*(16*s24*(s23*s23)+6*s23*(s24*s24)+5*pow(s23,3)-3*pow(s24,3))+pow(s14,3)*(-86*(s23*s23)*(s24*s24)+20*s24*pow(s23,3)+66*pow(s23,4)-48*s23*pow(s24,3)-26*pow(s24,4))+s14*s14*(-647*(s24*s24)*pow(s23,3)-309*s24*pow(s23,4)+40*pow(s23,5)-465*(s23*s23)*pow(s24,3)-211*s23*pow(s24,4)-8*pow(s24,5))-s14*s24*(329*(s24*s24)*pow(s23,3)+270*s24*pow(s23,4)+109*pow(s23,5)+266*(s23*s23)*pow(s24,3)+94*s23*pow(s24,4)-4*pow(s24,5)))+s13*s13*(s14*s14)*(-2*s23*s24*((s23+s24)*(s23+s24))*(2*s24*(s23*s23)+16*s23*(s24*s24)+4*pow(s23,3)+11*pow(s24,3))+2*pow(s14,3)*(-40*(s23*s23)*(s24*s24)-4*s24*pow(s23,3)+18*pow(s23,4)-9*s23*pow(s24,3)-4*pow(s24,4))-s14*s24*(783*(s24*s24)*pow(s23,3)+766*s24*pow(s23,4)+288*pow(s23,5)+420*(s23*s23)*pow(s24,3)+111*s23*pow(s24,4)-4*pow(s24,5))+2*(s14*s14)*(-398*(s24*s24)*pow(s23,3)-192*s24*pow(s23,4)+18*pow(s23,5)-241*(s23*s23)*pow(s24,3)-73*s23*pow(s24,4)-2*pow(s24,5)))+pow(s13,4)*(s14*s23*s24*(10*s23*s24+11*(s23*s23)-40*(s24*s24))*((s23+s24)*(s23+s24))+2*s23*(-3*s23*s24+2*(s23*s23)-8*(s24*s24))*(s24*s24)*((s23+s24)*(s23+s24))+pow(s14,3)*(-52*(s23*s23)*(s24*s24)+24*s24*pow(s23,3)+48*pow(s23,4)-58*s23*pow(s24,3)-28*pow(s24,4))+2*(s14*s14)*(-104*(s24*s24)*pow(s23,3)-43*s24*pow(s23,4)+6*pow(s23,5)-118*(s23*s23)*pow(s24,3)-71*s23*pow(s24,4)-2*pow(s24,5))))+s12*(-2*s24*(s23+s24)*(8*s23*s24+s24*(2*s14+3*s24)+5*(s23*s23))*pow(s14,5)*pow(s23,3)+s13*(s23*s23)*pow(s14,4)*(2*s24*(36*s23*s24+16*(s23*s23)+15*(s24*s24))*((s23+s24)*(s23+s24))+s14*s14*(36*s24*(s23*s23)+66*s23*(s24*s24)-4*pow(s23,3)+20*pow(s24,3))+s14*s24*(245*s24*(s23*s23)+184*s23*(s24*s24)+98*pow(s23,3)+37*pow(s24,3)))+s24*pow(s13,5)*(-2*s23*(s23-3*s24)*(s24*s24)*((s23+s24)*(s23+s24))+s14*s23*s24*(16*s24*(s23*s23)+31*s23*(s24*s24)-pow(s23,3)+14*pow(s24,3))+s14*s14*(38*(s23*s23)*(s24*s24)+38*s24*pow(s23,3)+22*pow(s23,4)+32*s23*pow(s24,3)+4*pow(s24,4)))+s23*(s13*s13)*pow(s14,3)*(2*s24*((s23+s24)*(s23+s24))*(44*s24*(s23*s23)+5*s23*(s24*s24)+35*pow(s23,3)-6*pow(s24,3))-2*(s14*s14)*(-89*(s23*s23)*(s24*s24)-51*s24*pow(s23,3)+10*pow(s23,4)-15*s23*pow(s24,3)+pow(s24,4))+s14*s24*(260*(s23*s23)*(s24*s24)+484*s24*pow(s23,3)+219*pow(s23,4)+5*pow(s24,4)))+2*(s14*s14)*pow(s13,3)*(s23*s24*((s23+s24)*(s23+s24))*(9*s24*(s23*s23)-2*s23*(s24*s24)+16*pow(s23,3)-2*pow(s24,3))+2*s14*s24*(24*(s24*s24)*pow(s23,3)+69*s24*pow(s23,4)+38*pow(s23,5)+6*(s23*s23)*pow(s24,3)+12*s23*pow(s24,4)-pow(s24,5))+s14*s14*(98*(s24*s24)*pow(s23,3)+59*s24*pow(s23,4)-14*pow(s23,5)+19*(s23*s23)*pow(s24,3)+14*s23*pow(s24,4)+2*pow(s24,5)))+s14*pow(s13,4)*(-2*s23*(5*s23*s24+5*(s23*s23)-2*(s24*s24))*(s24*s24)*((s23+s24)*(s23+s24))+s14*s24*(40*(s24*s24)*pow(s23,3)+40*s24*pow(s23,4)+31*pow(s23,5)+92*(s23*s23)*pow(s24,3)+57*s23*pow(s24,4)-4*pow(s24,5))+s14*s14*(122*(s24*s24)*pow(s23,3)+74*s24*pow(s23,4)-12*pow(s23,5)+66*(s23*s23)*pow(s24,3)+62*s23*pow(s24,4)+8*pow(s24,5))))+pow(s12,5)*(-2*s23*s24*(s23+s24)*(s14*s14)*((7*s23-8*s24)*(s14*s14)+s14*(-4*s23*s24+17*(s23*s23)-17*(s24*s24))+3*(s23-2*s24)*((s23+s24)*(s23+s24)))+2*(3*(s14*s14)*(s23*s23+s24*s24)+s23*s24*(5*s23*s24-3*(s23*s23)+8*(s24*s24)))*pow(s13,4)+pow(s13,3)*(s14*s23*s24*(15*s23*s24-13*(s23*s23)+28*(s24*s24))+18*(s23*s23+s24*s24)*pow(s14,3)+2*s23*s24*(s24*(s23*s23)+29*s23*(s24*s24)-7*pow(s23,3)+21*pow(s24,3))+s14*s14*(-6*s24*(s23*s23)+28*s23*(s24*s24)+24*pow(s24,3)))+s13*(s14*s14)*(6*(s23*s23+s24*s24)*pow(s14,3)+s14*s14*(-6*s24*(s23*s23)+22*s23*(s24*s24)+8*pow(s23,3)+20*pow(s24,3))+2*s23*s24*(-4*s24*(s23*s23)+33*s23*(s24*s24)-14*pow(s23,3)+23*pow(s24,3))+s14*(123*(s23*s23)*(s24*s24)+36*s24*pow(s23,3)+2*pow(s23,4)+93*s23*pow(s24,3)+10*pow(s24,4)))+s13*s13*(18*(s23*s23+s24*s24)*pow(s14,4)+2*s14*s23*s24*(-11*s24*(s23*s23)+12*s23*(s24*s24)-12*pow(s23,3)+11*pow(s24,3))+2*pow(s14,3)*(-6*s24*(s23*s23)+25*s23*(s24*s24)+4*pow(s23,3)+22*pow(s24,3))+s14*s14*(132*(s23*s23)*(s24*s24)+39*s24*pow(s23,3)+2*pow(s23,4)+99*s23*pow(s24,3)+10*pow(s24,4))-6*s23*(s23-2*s24)*s24*pow(s23+s24,3)))-pow(s12,3)*(2*s23*s24*(s23+s24)*pow(s14,3)*(s14*s14*(4*s23*s24+11*(s23*s23)-3*(s24*s24))+(7*(s23*s23)-3*(s24*s24))*((s23+s24)*(s23+s24))+s14*(30*s24*(s23*s23)-4*s23*(s24*s24)+29*pow(s23,3)-6*pow(s24,3)))+pow(s13,5)*(s14*s23*s24*(-13*s23*s24+s23*s23-14*(s24*s24))+2*s23*s24*(s24*(s23*s23)-9*s23*(s24*s24)+pow(s23,3)-9*pow(s24,3))+2*(s14*s14)*(7*s24*(s23*s23)-2*s23*(s24*s24)+4*pow(s23,3)-4*pow(s24,3)))+pow(s13,4)*(s14*s23*s24*(-9*s24*(s23*s23)-96*s23*(s24*s24)+19*pow(s23,3)-68*pow(s24,3))+2*pow(s14,3)*(28*s24*(s23*s23)-5*s23*(s24*s24)+12*pow(s23,3)-14*pow(s24,3))+s14*s14*(-118*(s23*s23)*(s24*s24)-7*s24*pow(s23,3)+32*pow(s23,4)-117*s23*pow(s24,3)-26*pow(s24,4))+2*s23*s24*(-15*(s23*s23)*(s24*s24)+6*s24*pow(s23,3)+2*pow(s23,4)-43*s23*pow(s24,3)-24*pow(s24,4)))+pow(s13,3)*(6*pow(s14,4)*(14*s24*(s23*s23)-s23*(s24*s24)+4*pow(s23,3)-6*pow(s24,3))+pow(s14,3)*(-308*(s23*s23)*(s24*s24)-53*s24*pow(s23,3)+80*pow(s23,4)-231*s23*pow(s24,3)-58*pow(s24,4))+s14*s23*s24*(53*(s23*s23)*(s24*s24)+107*s24*pow(s23,3)+39*pow(s23,4)-55*s23*pow(s24,3)-40*pow(s24,4))+s14*s14*(-425*(s24*s24)*pow(s23,3)-149*s24*pow(s23,4)+12*pow(s23,5)-494*(s23*s23)*pow(s24,3)-240*s23*pow(s24,4)-4*pow(s24,5))+2*s23*s24*(s23*s23-7*(s24*s24))*pow(s23+s24,3))+s13*(s14*s14)*(2*s24*(s23*s24+7*(s23*s23)-2*(s24*s24))*pow(s14,4)+pow(s14,3)*(-137*(s23*s23)*(s24*s24)-52*s24*pow(s23,3)+10*pow(s23,4)-57*s23*pow(s24,3)-8*pow(s24,4))-s14*s23*s24*(337*(s23*s23)*(s24*s24)+159*s24*pow(s23,3)+26*pow(s23,4)+299*s23*pow(s24,3)+95*pow(s24,4))+s14*s14*(-536*(s24*s24)*pow(s23,3)-208*s24*pow(s23,4)+4*pow(s23,5)-485*(s23*s23)*pow(s24,3)-171*s23*pow(s24,4)-4*pow(s24,5))+2*s23*s24*(2*s23*s24+8*(s23*s23)-5*(s24*s24))*pow(s23+s24,3))+s14*(s13*s13)*(2*pow(s14,4)*(28*s24*(s23*s23)+s23*(s24*s24)+4*pow(s23,3)-10*pow(s24,3))+pow(s14,3)*(-340*(s23*s23)*(s24*s24)-97*s24*pow(s23,3)+58*pow(s23,4)-185*s23*pow(s24,3)-40*pow(s24,4))-s14*s23*s24*(308*(s23*s23)*(s24*s24)+130*s24*pow(s23,3)+29*pow(s23,4)+328*s23*pow(s24,3)+121*pow(s24,4))+s14*s14*(-962*(s24*s24)*pow(s23,3)-392*s24*pow(s23,4)+16*pow(s23,5)-879*(s23*s23)*pow(s24,3)-345*s23*pow(s24,4)-8*pow(s24,5))+2*s23*s24*(3*s23*s24+6*(s23*s23)-s24*s24)*pow(s23+s24,3)))+2*pow(s12,4)*((s14*s14*(s23*s23+s24*s24)+s23*s24*(2*s23*s24-s23*s23+3*(s24*s24)))*pow(s13,5)+pow(s13,4)*(-4*s14*s23*s24*(-3*s23*s24+s23*s23-4*(s24*s24))+4*(s23*s23+s24*s24)*pow(s14,3)-2*(s14*s14)*(6*s24*(s23*s23)-5*s23*(s24*s24)+3*pow(s23,3)-6*pow(s24,3))+s23*s24*(29*s23*(s24*s24)-5*pow(s23,3)+24*pow(s24,3)))+s13*(s14*s14)*(-(s23*s24*(-2*s23*s24+19*(s23*s23)-19*(s24*s24))*((s23+s24)*(s23+s24)))+(s23*s23+s24*s24)*pow(s14,4)+2*pow(s14,3)*(-6*s24*(s23*s23)+2*s23*(s24*s24)+pow(s23,3)+4*pow(s24,3))+s14*s24*(178*(s23*s23)*(s24*s24)+126*s24*pow(s23,3)+27*pow(s23,4)+81*s23*pow(s24,3)+2*pow(s24,4))+s14*s14*(109*(s23*s23)*(s24*s24)+42*s24*pow(s23,3)-pow(s23,4)+64*s23*pow(s24,3)+9*pow(s24,4)))+pow(s13,3)*(-(s23*s24*(-4*s23*s24+5*(s23*s23)-21*(s24*s24))*((s23+s24)*(s23+s24)))+6*(s23*s23+s24*s24)*pow(s14,4)+s14*s23*s24*(-17*s24*(s23*s23)+35*s23*(s24*s24)-21*pow(s23,3)+31*pow(s24,3))+pow(s14,3)*(-36*s24*(s23*s23)+24*s23*(s24*s24)-10*pow(s23,3)+32*pow(s24,3))+s14*s14*(108*(s23*s23)*(s24*s24)+28*s24*pow(s23,3)-9*pow(s23,4)+85*s23*pow(s24,3)+13*pow(s24,4)))-s23*s24*(s23+s24)*(s14*s14)*(s14*s14*(s23*s24+25*(s23*s23)-14*(s24*s24))+3*(s23-s24)*pow(s14,3)+s14*(17*s24*(s23*s23)-15*s23*(s24*s24)+19*pow(s23,3)-13*pow(s24,3))+(s23-2*s24)*pow(s23+s24,3))+s13*s13*(-(s14*s23*s24*(4*s23*s24+15*(s23*s23)-7*(s24*s24))*((s23+s24)*(s23+s24)))+4*(s23*s23+s24*s24)*pow(s14,5)-2*pow(s14,4)*(18*s24*(s23*s23)-9*s23*(s24*s24)+pow(s23,3)-14*pow(s24,3))+s24*(s14*s14)*(189*(s23*s23)*(s24*s24)+126*s24*pow(s23,3)+28*pow(s23,4)+93*s23*pow(s24,3)+2*pow(s24,4))+pow(s14,3)*(207*(s23*s23)*(s24*s24)+76*s24*pow(s23,3)-10*pow(s23,4)+133*s23*pow(s24,3)+22*pow(s24,4))-s23*(s23-2*s24)*s24*pow(s23+s24,4))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/4.;
}

// Coefficient of master 7 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCA<7>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygCA<7,-1>(s12,s13,s14,s23,s24),
        qq2yygCA<7,0>(s12,s13,s14,s23,s24),
        qq2yygCA<7,1>(s12,s13,s14,s23,s24)
    });
}


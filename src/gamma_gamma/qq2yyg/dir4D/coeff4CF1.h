/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 1: bubble(s13)

// Coefficient order epsilon^-1 of master 1
template<>
double qq2yyg4CF<1,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return pow(s13,-1)*pow(s14,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)*((s13*(5*s24*(s23+s24)+s14*(-4*s23+10*s24))+5*s24*(s13*s13)+s14*(-2*s14*(s23-s24)+5*s23*s24-5*(s23*s23)+10*(s24*s24)))*pow(s12,7)+(-(s14*(s23-2*s24))+s13*s24)*pow(s12,8)+pow(s12,6)*((s14*(-5*s23+19*s24)+s24*(20*s23+23*s24))*(s13*s13)+s14*(-((s23+2*s24)*(s14*s14))+s14*(s23*s24-11*(s23*s23)+8*(s24*s24))-10*(s23-2*s24)*((s23+s24)*(s23+s24)))+s13*((-5*s23+4*s24)*(s14*s14)+s14*(22*s23*s24-17*(s23*s23)+45*(s24*s24))+10*s24*((s23+s24)*(s23+s24)))+10*s24*pow(s13,3))+s24*pow(s13,6)*(s14*(s23-s24)*s24+s23*(s14*s14)+pow(s24,3))-s13*s23*(s23+s24)*pow(s14,3)*(s24*(6*s23+5*s24)*(s14*s14)+2*s24*pow(s23,3)+pow(s23,4)-s23*pow(s24,3)+s14*(6*s24*(s23*s23)+5*s23*(s24*s24)+2*pow(s23,3)+pow(s24,3)))+pow(s13,5)*(s24*(s14*s14)*(5*s23*s24+s23*s23-s24*s24)+s14*(-3*s23*s24+s23*s23-2*(s24*s24))*(s24*s24)+s23*(s23+s24)*pow(s14,3)+2*(s23+s24)*pow(s24,4))+pow(s13,4)*(-(s14*s23*(s24*s24)*(7*s23*s24+s23*s23+6*(s24*s24)))+2*s23*(s23*s24+s23*s23+s24*s24)*pow(s14,3)+s23*(s23-2*s24)*pow(s14,4)-s24*(s14*s14)*(-8*s24*(s23*s23)-5*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3))+(s23+s24)*(s23+s24)*pow(s24,4))-(s23+s24)*(s13*s13)*(s14*s14)*(s24*(s14*s14)*(7*s23*s24+7*(s23*s23)-s24*s24)+6*s23*s24*pow(s14,3)+3*s23*(s23+s24)*pow(s24,3)+s14*(-5*(s23*s23)*(s24*s24)+4*s24*pow(s23,3)+2*pow(s23,4)-6*s23*pow(s24,3)+pow(s24,4)))-s14*pow(s13,3)*((3*s23*s24+s23*s23-s24*s24)*(s24*s24)*((s23+s24)*(s23+s24))+s23*(5*s23*s24-2*(s23*s23)+8*(s24*s24))*pow(s14,3)+2*s23*s24*pow(s14,4)-s24*(s14*s14)*(4*s24*(s23*s23)+7*s23*(s24*s24)-2*pow(s23,3)+pow(s24,3))+s14*s24*(-3*(s23*s23)*(s24*s24)-3*s24*pow(s23,3)+pow(s23,4)+4*s23*pow(s24,3)+3*pow(s24,4)))-s23*(2*s14*s24+s23*s23)*pow(s14,4)*pow(s23+s24,3)+pow(s12,5)*(s13*s13*((s23-4*s24)*(s14*s14)+6*s24*(12*s23*s24+5*(s23*s23)+7*(s24*s24))+s14*(36*s23*s24-18*(s23*s23)+75*(s24*s24)))+2*s24*(8*s14+15*s23+21*s24)*pow(s13,3)+10*s24*pow(s13,4)-s14*(s14*s14*(12*s23*s24+7*(s23*s23)+8*(s24*s24))+2*s24*pow(s14,3)+4*s14*(5*s24*(s23*s23)-4*s23*(s24*s24)+6*pow(s23,3)-3*pow(s24,3))+10*(s23-2*s24)*pow(s23+s24,3))+s13*(s14*s14*(-3*s23*s24-26*(s23*s23)+10*(s24*s24))-9*s24*pow(s14,3)+s14*(108*s23*(s24*s24)-28*pow(s23,3)+80*pow(s24,3))+10*s24*pow(s23+s24,3)))+s12*(-(s23*(6*s24*(s14*s14)+s23*(5*s23*s24+3*(s23*s23)+2*(s24*s24))+s14*(5*s23*s24+4*(s23*s23)+4*(s24*s24)))*((s23+s24)*(s23+s24))*pow(s14,3))+pow(s13,6)*(2*s14*(s23-s24)*s24+s23*(s14*s14)+3*pow(s24,3))+pow(s13,5)*(s14*s14*(10*s23*s24+4*(s23*s23)-5*(s24*s24))-s14*s24*(s23*s24-4*(s23*s23)+6*(s24*s24))+2*s23*pow(s14,3)+(9*s23+11*s24)*pow(s24,3))+pow(s13,4)*(s14*(s24*s24)*(-14*s23*s24-10*(s23*s23)+s24*s24)+(6*s23*s24+10*(s23*s23)-3*(s24*s24))*pow(s14,3)+s23*pow(s14,4)+s14*s14*(17*s24*(s23*s23)+9*s23*(s24*s24)+4*pow(s23,3)-18*pow(s24,3))+(20*s23*s24+9*(s23*s23)+11*(s24*s24))*pow(s24,3))-s14*(s13*s13)*(6*s23*s24*pow(s14,4)+pow(s14,3)*(19*s24*(s23*s23)+19*s23*(s24*s24)-3*pow(s23,3)-4*pow(s24,3))+s14*s14*(3*(s23*s23)*(s24*s24)+14*s24*pow(s23,3)+7*pow(s23,4)+5*s23*pow(s24,3)+9*pow(s24,4))+s14*(33*(s24*s24)*pow(s23,3)+19*s24*pow(s23,4)+5*pow(s23,5)+42*(s23*s23)*pow(s24,3)+30*s23*pow(s24,4)+7*pow(s24,5))+2*s24*(2*s23*s24+s23*s23-2*(s24*s24))*pow(s23+s24,3))-s13*(s14*s14)*(s24*(9*s23*s24+11*(s23*s23)-s24*s24)*pow(s14,3)+s23*(s14*s14)*(25*s24*(s23*s23)+32*s23*(s24*s24)+5*pow(s23,3)+12*pow(s24,3))+s14*(27*(s24*s24)*pow(s23,3)+29*s24*pow(s23,4)+11*pow(s23,5)+16*(s23*s23)*pow(s24,3)+10*s23*pow(s24,4)+3*pow(s24,5))+2*s23*(2*s23*s24+s23*s23+2*(s24*s24))*pow(s23+s24,3))+pow(s13,3)*(s23*(5*s23-9*s24)*pow(s14,4)+pow(s14,3)*(10*s24*(s23*s23)+2*s23*(s24*s24)+9*pow(s23,3)-5*pow(s24,3))+s14*s24*(-26*(s23*s23)*(s24*s24)-21*s24*pow(s23,3)-4*pow(s23,4)+s23*pow(s24,3)+10*pow(s24,4))-s14*s14*(-(s23*s23*(s24*s24))+2*s24*pow(s23,3)+2*pow(s23,4)+24*s23*pow(s24,3)+22*pow(s24,4))+3*pow(s24,3)*pow(s23+s24,3)))+pow(s12,4)*((2*(7*s23-8*s24)*(s14*s14)+3*s24*(32*s23*s24+10*(s23*s23)+23*(s24*s24))+s14*(28*s23*s24-2*(s23*s23)+53*(s24*s24)))*pow(s13,3)+(s14*(5*s23+4*s24)+2*s24*(10*s23+19*s24))*pow(s13,4)+5*s24*pow(s13,5)+s13*s13*(-(s14*s14*(8*s23*s24+8*(s23*s23)+23*(s24*s24)))+2*s24*(10*s23+19*s24)*((s23+s24)*(s23+s24))+(8*s23-17*s24)*pow(s14,3)+s14*(-2*s24*(s23*s23)+131*s23*(s24*s24)-24*pow(s23,3)+115*pow(s24,3)))-s14*(2*s14*(-3*s23*s24+13*(s23*s23)-4*(s24*s24))*((s23+s24)*(s23+s24))+(10*s23*s24+s23*s23+6*(s24*s24))*pow(s14,3)+s14*s14*(35*s24*(s23*s23)+30*s23*(s24*s24)+18*pow(s23,3)+12*pow(s24,3))+5*(s23-2*s24)*pow(s23+s24,4))+s13*(-2*s14*(-6*s23*s24+11*(s23*s23)-35*(s24*s24))*((s23+s24)*(s23+s24))-4*(7*s23*s24+2*(s23*s23)+8*(s24*s24))*pow(s14,3)+(s23-3*s24)*pow(s14,4)-s14*s14*(55*s24*(s23*s23)+s23*(s24*s24)+50*pow(s23,3)-6*pow(s24,3))+5*s24*pow(s23+s24,4)))+s12*s12*(((7*s23-4*s24)*(s14*s14)+3*s14*(2*s23*s24+s23*s23-2*(s24*s24))+3*(4*s23+7*s24)*(s24*s24))*pow(s13,5)+(s14*(s23-s24)+3*(s24*s24))*pow(s13,6)+pow(s13,4)*(s14*s14*(18*s23*s24+19*(s23*s23)-29*(s24*s24))+s24*s24*(54*s23*s24+18*(s23*s23)+37*(s24*s24))+(9*s23-5*s24)*pow(s14,3)+2*s14*(3*s24*(s23*s23)+pow(s23,3)+3*pow(s24,3)))+pow(s13,3)*(3*(4*s23+7*s24)*(s24*s24)*((s23+s24)*(s23+s24))+(s23*s24+23*(s23*s23)-21*(s24*s24))*pow(s14,3)+(3*s23-2*s24)*pow(s14,4)+2*(s14*s14)*(8*s24*(s23*s23)-15*s23*(s24*s24)+3*pow(s23,3)-26*pow(s24,3))+s14*(-20*(s23*s23)*(s24*s24)-16*s24*pow(s23,3)-2*pow(s23,4)+29*s23*pow(s24,3)+38*pow(s24,4)))-(s23+s24)*(s14*s14)*(6*s23*s24*pow(s14,3)+s14*s14*(17*s24*(s23*s23)+16*s23*(s24*s24)+6*pow(s23,3)+2*pow(s24,3))+s14*(18*(s23*s23)*(s24*s24)+26*s24*pow(s23,3)+13*pow(s23,4)+7*s23*pow(s24,3)+2*pow(s24,4))+s23*(3*s23-s24)*pow(s23+s24,3))-s13*s14*(2*(2*s23-s24)*s24*pow(s14,4)+s14*((s23+s24)*(s23+s24))*(23*s24*(s23*s23)+14*s23*(s24*s24)+17*pow(s23,3)+2*pow(s24,3))+pow(s14,3)*(29*s24*(s23*s23)+27*s23*(s24*s24)+3*pow(s23,3)+3*pow(s24,3))+s14*s14*(63*(s23*s23)*(s24*s24)+59*s24*pow(s23,3)+27*pow(s23,4)+52*s23*pow(s24,3)+20*pow(s24,4))+(2*s23*s24+s23*s23-5*(s24*s24))*pow(s23+s24,4))+s13*s13*(-(s24*(39*s23*s24+11*(s23*s23)+33*(s24*s24))*pow(s14,3))+3*s23*(2*s23-5*s24)*pow(s14,4)-3*s14*((s23+s24)*(s23+s24))*(5*s24*(s23*s23)+pow(s23,3)-10*pow(s24,3))-s14*s14*(67*(s23*s23)*(s24*s24)+50*s24*pow(s23,3)+20*pow(s23,4)+69*s23*pow(s24,3)+29*pow(s24,4))+3*(s24*s24)*pow(s23+s24,4)))+pow(s12,3)*((2*(8*s23-7*s24)*(s14*s14)+s14*(13*s23*s24+7*(s23*s23)+9*(s24*s24))+s24*(56*s23*s24+10*(s23*s23)+55*(s24*s24)))*pow(s13,4)+(s14*(4*s23-2*s24)+s24*(5*s23+17*s24))*pow(s13,5)+s24*pow(s13,6)+pow(s13,3)*(s14*s14*(5*s23*s24+22*(s23*s23)-49*(s24*s24))+(14*s23-15*s24)*pow(s14,3)+s24*(66*s24*(s23*s23)+111*s23*(s24*s24)+10*pow(s23,3)+55*pow(s24,3))+s14*(55*s23*(s24*s24)-4*pow(s23,3)+66*pow(s24,3)))+s13*s13*((-20*s23*s24+13*(s23*s23)-42*(s24*s24))*pow(s14,3)+3*(s23-s24)*pow(s14,4)-s14*s14*(35*s24*(s23*s23)+51*s23*(s24*s24)+24*pow(s23,3)+41*pow(s24,3))+s14*(33*(s23*s23)*(s24*s24)-38*s24*pow(s23,3)-14*pow(s23,4)+142*s23*pow(s24,3)+85*pow(s24,4))+s24*(5*s23+17*s24)*pow(s23+s24,3))-s14*(2*s23*s24*pow(s14,4)+pow(s14,3)*(21*s24*(s23*s23)+24*s23*(s24*s24)+4*pow(s23,3)+6*pow(s24,3))+s14*s14*(51*(s23*s23)*(s24*s24)+53*s24*pow(s23,3)+22*pow(s23,4)+28*s23*pow(s24,3)+8*pow(s24,4))+2*s14*(-2*s23*s24+7*(s23*s23)-s24*s24)*pow(s23+s24,3)+(s23-2*s24)*pow(s23+s24,5))+s13*((-15*s23*s24+s23*s23-6*(s24*s24))*pow(s14,4)+s24*pow(s14,5)-pow(s14,3)*(52*s24*(s23*s23)+70*s23*(s24*s24)+25*pow(s23,3)+40*pow(s24,3))-s14*s14*(66*(s23*s23)*(s24*s24)+95*s24*pow(s23,3)+44*pow(s23,4)+17*s23*pow(s24,3)+2*pow(s24,4))-2*s14*(s23*s24+4*(s23*s23)-15*(s24*s24))*pow(s23+s24,3)+s24*pow(s23+s24,5))))*pow(s12+s23+s24,-1)*pow(s12+s13+s23+s24,-3);
}

// Coefficient order epsilon^0 of master 1
template<>
double qq2yyg4CF<1,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s13,-1)*pow(s14,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)*((s13*(25*s24*(s23+s24)+s14*(-26*s23+39*s24))+25*s24*(s13*s13)+s14*(-2*s14*(8*s23-7*s24)+5*(s23*s24-6*(s23*s23)+7*(s24*s24))))*pow(s12,7)-(s14*(6*s23-7*s24)-5*s13*s24)*pow(s12,8)+pow(s12,6)*((5*s24*(20*s23+23*s24)+s14*(-40*s23+82*s24))*(s13*s13)+s14*(5*(-4*s23+s24)*(s14*s14)+s14*(-7*s23*s24-82*(s23*s23)+60*(s24*s24))-10*(6*s23-7*s24)*((s23+s24)*(s23+s24)))+s13*((-48*s23+46*s24)*(s14*s14)+s14*(39*s23*s24-110*(s23*s23)+177*(s24*s24))+50*s24*((s23+s24)*(s23+s24)))+50*s24*pow(s13,3))+s24*pow(s13,6)*(4*s14*(s23-s24)*s24+2*(4*s23+s24)*(s14*s14)+5*pow(s24,3))-s23*((s23+s24)*(s23+s24))*pow(s14,4)*(22*s24*(s23*s23)+19*s23*(s24*s24)+s14*(26*s23*s24+7*(s23*s23)+18*(s24*s24))+11*pow(s23,3)+8*pow(s24,3))-s14*pow(s13,3)*((13*s23*s24+6*(s23*s23)-7*(s24*s24))*(s24*s24)*((s23+s24)*(s23+s24))+s23*(6*s23+23*s24)*pow(s14,4)+pow(s14,3)*(83*s24*(s23*s23)+71*s23*(s24*s24)+5*pow(s23,3)-9*pow(s24,3))+s14*s14*(45*(s23*s23)*(s24*s24)+47*s24*pow(s23,3)+6*pow(s23,4)-9*s23*pow(s24,3)-13*pow(s24,4))+s14*s24*(-13*(s23*s23)*(s24*s24)+4*s24*pow(s23,3)+10*pow(s23,4)-14*s23*pow(s24,3)-7*pow(s24,4)))+pow(s13,5)*(s14*(-13*s23*s24+2*(s23*s23)-4*(s24*s24))*(s24*s24)+2*s24*(s14*s14)*(20*s23*s24+7*(s23*s23)+3*(s24*s24))+2*(s23*s24+4*(s23*s23)-s24*s24)*pow(s14,3)+10*(s23+s24)*pow(s24,4))+pow(s13,4)*(s14*s14*(s24*s24)*(52*s23*s24+51*(s23*s23)+7*(s24*s24))+(-22*s23*s24+2*(s23*s23)+3*(s24*s24))*pow(s14,4)+pow(s14,3)*(-(s24*(s23*s23))-7*s23*(s24*s24)+14*pow(s23,3)+pow(s24,3))+s14*(s24*s24)*(-34*s24*(s23*s23)-19*s23*(s24*s24)-8*pow(s23,3)+7*pow(s24,3))+5*((s23+s24)*(s23+s24))*pow(s24,4))-s13*s13*(s14*s14)*(s23*(86*s23*s24+18*(s23*s23)+65*(s24*s24))*pow(s14,3)+s24*((s23+s24)*(s23+s24))*(9*s24*(s23*s23)+11*s23*(s24*s24)+4*pow(s23,3)-6*pow(s24,3))+2*(s14*s14)*(96*(s23*s23)*(s24*s24)+70*s24*pow(s23,3)+14*pow(s23,4)+35*s23*pow(s24,3)-5*pow(s24,4))+s14*(107*(s24*s24)*pow(s23,3)+86*s24*pow(s23,4)+22*pow(s23,5)+33*(s23*s23)*pow(s24,3)-22*s23*pow(s24,4)-12*pow(s24,5)))-s13*(s23+s24)*pow(s14,3)*(s23*(s14*s14)*(81*s23*s24+19*(s23*s23)+54*(s24*s24))+38*(s24*s24)*pow(s23,3)+32*s24*pow(s23,4)+10*pow(s23,5)+17*(s23*s23)*pow(s24,3)+2*s14*(55*(s23*s23)*(s24*s24)+48*s24*pow(s23,3)+16*pow(s23,4)+22*s23*pow(s24,3)-pow(s24,4))-5*s23*pow(s24,4)-6*pow(s24,5))+pow(s12,5)*(s13*s13*((-24*s23+44*s24)*(s14*s14)+30*s24*(12*s23*s24+5*(s23*s23)+7*(s24*s24))+s14*(82*s23*s24-140*(s23*s23)+331*(s24*s24)))+(30*s24*(5*s23+7*s24)+s14*(-20*s23+78*s24))*pow(s13,3)+50*s24*pow(s13,4)+s14*(s14*s14*(-66*s23*s24-103*(s23*s23)+23*(s24*s24))-17*s23*pow(s14,3)-4*s14*(44*s24*(s23*s23)-23*s23*(s24*s24)+42*pow(s23,3)-25*pow(s24,3))-10*(6*s23-7*s24)*pow(s23+s24,3))+s13*(s14*s14*(-17*s23*s24-220*(s23*s23)+171*(s24*s24))+(-53*s23+4*s24)*pow(s14,3)-2*s14*(77*s24*(s23*s23)-172*s23*(s24*s24)+90*pow(s23,3)-159*pow(s24,3))+50*s24*pow(s23+s24,3)))-s12*(-(pow(s13,6)*(8*s14*(s23-s24)*s24+2*(4*s23+s24)*(s14*s14)+15*pow(s24,3)))-pow(s13,5)*(s14*s24*(-8*s23*s24+12*(s23*s23)-13*(s24*s24))+2*(s14*s14)*(43*s23*s24+13*(s23*s23)+s24*s24)+10*s23*pow(s14,3)+5*(9*s23+11*s24)*pow(s24,3))+pow(s13,4)*((-26*s23*s24-47*(s23*s23)+4*(s24*s24))*pow(s14,3)+(4*s23-s24)*pow(s14,4)+s14*s24*(68*s24*(s23*s23)+43*s23*(s24*s24)+12*pow(s23,3)-38*pow(s24,3))-5*(20*s23*s24+9*(s23*s23)+11*(s24*s24))*pow(s24,3)-s14*s14*(139*s24*(s23*s23)+176*s23*(s24*s24)+18*pow(s23,3)+5*pow(s24,3)))+(s23+s24)*pow(s14,3)*((s23+s24)*(s23+s24)*(16*s24*(s23*s23)+s23*(s24*s24)+23*pow(s23,3)-6*pow(s24,3))+s14*s14*(90*s24*(s23*s23)+58*s23*(s24*s24)+28*pow(s23,3)-2*pow(s24,3))+s14*(160*(s23*s23)*(s24*s24)+164*s24*pow(s23,3)+61*pow(s23,4)+49*s23*pow(s24,3)-8*pow(s24,4)))+s14*(s13*s13)*((77*s23*s24+36*(s23*s23)-9*(s24*s24))*pow(s14,4)+pow(s14,3)*(402*s24*(s23*s23)+242*s23*(s24*s24)+111*pow(s23,3)-52*pow(s24,3))+s14*s14*(223*(s23*s23)*(s24*s24)+313*s24*pow(s23,3)+122*pow(s23,4)-17*s23*pow(s24,3)-50*pow(s24,4))+s14*(186*(s24*s24)*pow(s23,3)+141*s24*pow(s23,4)+34*pow(s23,5)+69*(s23*s23)*pow(s24,3)-50*s23*pow(s24,4)-40*pow(s24,5))+s24*(20*s23*s24+12*(s23*s23)-21*(s24*s24))*pow(s23+s24,3))+s13*(s14*s14)*(pow(s14,3)*(190*s24*(s23*s23)+115*s23*(s24*s24)+57*pow(s23,3)-10*pow(s24,3))+3*(s14*s14)*(189*(s23*s23)*(s24*s24)+169*s24*pow(s23,3)+49*pow(s23,4)+59*s23*pow(s24,3)-10*pow(s24,4))+s14*(383*(s24*s24)*pow(s23,3)+339*s24*pow(s23,4)+109*pow(s23,5)+151*(s23*s23)*pow(s24,3)-42*s23*pow(s24,4)-40*pow(s24,5))+(27*s24*(s23*s23)+6*s23*(s24*s24)+12*pow(s23,3)-10*pow(s24,3))*pow(s23+s24,3))+pow(s13,3)*((125*s23*s24+30*(s23*s23)-22*(s24*s24))*pow(s14,4)+(6*s23-3*s24)*pow(s14,5)-pow(s14,3)*(-36*s24*(s23*s23)-3*s23*(s24*s24)+pow(s23,3)+23*pow(s24,3))+s14*s24*(125*(s23*s23)*(s24*s24)+124*s24*pow(s23,3)+28*pow(s23,4)-38*s23*pow(s24,3)-67*pow(s24,4))+s14*s14*(-105*(s23*s23)*(s24*s24)+23*s24*pow(s23,3)+22*pow(s23,4)-118*s23*pow(s24,3)-28*pow(s24,4))-15*pow(s24,3)*pow(s23+s24,3)))+pow(s12,4)*(((64*s23-2*s24)*(s14*s14)+15*s24*(32*s23*s24+10*(s23*s23)+23*(s24*s24))+s14*(70*s23*s24-60*(s23*s23)+273*(s24*s24)))*pow(s13,3)+(10*s24*(10*s23+19*s24)+s14*(10*s23+27*s24))*pow(s13,4)+25*s24*pow(s13,5)+s13*s13*(s14*s14*(47*s23*s24-142*(s23*s23)+142*(s24*s24))+10*s24*(10*s23+19*s24)*((s23+s24)*(s23+s24))-(29*s23+20*s24)*pow(s14,3)+s14*(-198*s24*(s23*s23)+475*s23*(s24*s24)-180*pow(s23,3)+522*pow(s24,3)))-s14*(2*s14*(s23*s24+86*(s23*s23)-40*(s24*s24))*((s23+s24)*(s23+s24))+(93*s23*s24+79*(s23*s23)-8*(s24*s24))*pow(s14,3)+(7*s23-2*s24)*pow(s14,4)+s14*s14*(313*s24*(s23*s23)+70*s23*(s24*s24)+212*pow(s23,3)-45*pow(s24,3))+5*(6*s23-7*s24)*pow(s23+s24,4))+s13*(-2*s14*(13*s23*s24+70*(s23*s23)-141*(s24*s24))*((s23+s24)*(s23+s24))-2*(93*s23*s24+119*(s23*s23)-12*(s24*s24))*pow(s14,3)+(-51*s23+6*s24)*pow(s14,4)+s14*s14*(-438*s24*(s23*s23)+192*s23*(s24*s24)-384*pow(s23,3)+247*pow(s24,3))+25*s24*pow(s23+s24,4)))-s12*s12*(-(((48*s23-4*s24)*(s14*s14)+s14*(19*s23*s24+10*(s23*s23)-14*(s24*s24))+15*(4*s23+7*s24)*(s24*s24))*pow(s13,5))+(-4*s14*(s23-s24)-15*(s24*s24))*pow(s13,6)-pow(s13,4)*(2*(s14*s14)*(104*s23*s24+52*(s23*s23)-11*(s24*s24))+5*(s24*s24)*(54*s23*s24+18*(s23*s23)+37*(s24*s24))+s14*s24*(-(s23*s24)-16*(s23*s23)+82*(s24*s24))+(37*s23-13*s24)*pow(s14,3))+pow(s13,3)*(-15*(4*s23+7*s24)*(s24*s24)*((s23+s24)*(s23+s24))+(12*s23*s24-38*(s23*s23)+21*(s24*s24))*pow(s14,3)+(25*s23-3*s24)*pow(s14,4)+2*(s14*s14)*(-72*s24*(s23*s23)-113*s23*(s24*s24)+6*pow(s23,3)-16*pow(s24,3))+s14*(169*(s23*s23)*(s24*s24)+142*s24*pow(s23,3)+20*pow(s23,4)-169*s23*pow(s24,3)-230*pow(s24,4)))+s13*s14*((80*s23*s24+57*(s23*s23)-20*(s24*s24))*pow(s14,4)+s14*((s23+s24)*(s23+s24))*(168*s24*(s23*s23)-4*s23*(s24*s24)+112*pow(s23,3)-63*pow(s24,3))+pow(s14,3)*(624*s24*(s23*s23)+323*s23*(s24*s24)+249*pow(s23,3)-60*pow(s24,3))+s14*s14*(550*(s23*s23)*(s24*s24)+746*s24*pow(s23,3)+320*pow(s23,4)+49*s23*pow(s24,3)-78*pow(s24,4))+(13*s23*s24+6*(s23*s23)-21*(s24*s24))*pow(s23+s24,4))+s14*s14*(2*s14*((s23+s24)*(s23+s24))*(56*s24*(s23*s23)+8*s23*(s24*s24)+56*pow(s23,3)-13*pow(s24,3))+pow(s14,3)*(114*s24*(s23*s23)+65*s23*(s24*s24)+42*pow(s23,3)-6*pow(s24,3))+s14*s14*(418*(s23*s23)*(s24*s24)+411*s24*pow(s23,3)+134*pow(s23,4)+117*s23*pow(s24,3)-24*pow(s24,4))+(-(s23*s24)+18*(s23*s23)-4*(s24*s24))*pow(s23+s24,4))+s13*s13*(2*(127*s23*s24+69*(s23*s23)-25*(s24*s24))*pow(s14,4)+9*(2*s23-s24)*pow(s14,5)+s14*((s23+s24)*(s23+s24))*(100*s24*(s23*s23)+19*s23*(s24*s24)+20*pow(s23,3)-148*pow(s24,3))+pow(s14,3)*(363*s24*(s23*s23)+136*s23*(s24*s24)+207*pow(s23,3)-40*pow(s24,3))+s14*s14*(157*(s23*s23)*(s24*s24)+383*s24*pow(s23,3)+162*pow(s23,4)-168*s23*pow(s24,3)-116*pow(s24,4))-15*(s24*s24)*pow(s23+s24,4)))+pow(s12,3)*((4*(24*s23-5*s24)*(s14*s14)+5*s24*(56*s23*s24+10*(s23*s23)+55*(s24*s24))+s14*(33*s23*s24+10*(s23*s23)+78*(s24*s24)))*pow(s13,4)+(s14*(14*s23-5*s24)+5*s24*(5*s23+17*s24))*pow(s13,5)+5*s24*pow(s13,6)+pow(s13,3)*(s14*s14*(187*s23*s24+74*(s23*s23)+9*(s24*s24))+(31*s23-32*s24)*pow(s14,3)+5*s24*(66*s24*(s23*s23)+111*s23*(s24*s24)+10*pow(s23,3)+55*pow(s24,3))+s14*(-122*s24*(s23*s23)+222*s23*(s24*s24)-60*pow(s23,3)+365*pow(s24,3)))+s13*s13*(-2*(78*s23*s24+68*(s23*s23)+9*(s24*s24))*pow(s14,3)+(-55*s23+8*s24)*pow(s14,4)+s14*s14*(-243*s24*(s23*s23)+190*s23*(s24*s24)-246*pow(s23,3)+180*pow(s24,3))+s14*(-39*(s23*s23)*(s24*s24)-326*s24*pow(s23,3)-100*pow(s23,4)+587*s23*pow(s24,3)+400*pow(s24,4))+5*s24*(5*s23+17*s24)*pow(s23+s24,3))-s14*((34*s23*s24+28*(s23*s23)-6*(s24*s24))*pow(s14,4)+pow(s14,3)*(323*s24*(s23*s23)+160*s23*(s24*s24)+146*pow(s23,3)-24*pow(s24,3))+s14*s14*(327*(s23*s23)*(s24*s24)+493*s24*pow(s23,3)+218*pow(s23,4)+5*s23*pow(s24,3)-47*pow(s24,4))+2*s14*(-(s23*s24)+44*(s23*s23)-15*(s24*s24))*pow(s23+s24,3)+(6*s23-7*s24)*pow(s23+s24,5))+s13*((-239*s23*s24-185*(s23*s23)+38*(s24*s24))*pow(s14,4)+(-19*s23+10*s24)*pow(s14,5)-pow(s14,3)*(639*s24*(s23*s23)+213*s23*(s24*s24)+406*pow(s23,3)-64*pow(s24,3))+s14*s14*(-300*(s23*s23)*(s24*s24)-704*s24*pow(s23,3)-312*pow(s23,4)+267*s23*pow(s24,3)+175*pow(s24,4))-s14*(39*s23*s24+50*(s23*s23)-123*(s24*s24))*pow(s23+s24,3)+5*s24*pow(s23+s24,5))))*pow(s12+s23+s24,-1)*pow(s12+s13+s23+s24,-3))/2.;
}

// Coefficient order epsilon^1 of master 1
template<>
double qq2yyg4CF<1,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s14,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)*((4*s13*s24+s14*(-4*s23+2*s24))*pow(s12,8)+pow(s12,7)*(20*s13*s24*(s13+s23+s24)+(-14*s23+11*s24)*(s14*s14)+s14*(s13*(-18*s23+19*s24)-10*(s23*s24+2*(s23*s23)-s24*s24))-2*pow(s14,3))+pow(s12,6)*((-6*s14*(5*s23-9*s24)+4*s24*(20*s23+23*s24))*(s13*s13)+40*s24*pow(s13,3)+s13*((-48*s23+69*s24)*(s14*s14)-2*s14*(s23*s24+38*(s23*s23)-45*(s24*s24))+40*s24*((s23+s24)*(s23+s24))-10*pow(s14,3))-s14*((37*s23-12*s24)*(s14*s14)+2*s14*(9*s23*s24+36*(s23*s23)-25*(s24*s24))+20*(2*s23-s24)*((s23+s24)*(s23+s24))+4*pow(s14,3)))+2*s24*pow(s13,6)*(s14*(s23-s24)*s24+(2*s23+5*s24)*(s14*s14)+2*pow(s24,3))-s23*((s23+s24)*(s23+s24))*pow(s14,4)*(52*s24*(s23*s23)+44*s23*(s24*s24)+s14*(39*s23*s24+14*(s23*s23)+22*(s24*s24))+23*pow(s23,3)+15*pow(s24,3))-s14*pow(s13,3)*(2*(2*s23*s24+2*(s23*s23)-5*(s24*s24))*(s24*s24)*((s23+s24)*(s23+s24))+2*s23*(6*s23+13*s24)*pow(s14,4)+pow(s14,3)*(179*s24*(s23*s23)+74*s23*(s24*s24)+71*pow(s23,3)-27*pow(s24,3))+s14*s14*(122*(s23*s23)*(s24*s24)+205*s24*pow(s23,3)+66*pow(s23,4)-100*s23*pow(s24,3)-82*pow(s24,4))+s14*s24*(-85*(s23*s23)*(s24*s24)+34*s24*pow(s23,3)+32*pow(s23,4)-154*s23*pow(s24,3)-67*pow(s24,4)))-s13*(s23+s24)*pow(s14,3)*(s23*(s14*s14)*(117*s23*s24+38*(s23*s23)+67*(s24*s24))+3*((s23+s24)*(s23+s24))*(7*s24*(s23*s23)-s23*(s24*s24)+6*pow(s23,3)-5*pow(s24,3))+s14*(221*(s23*s23)*(s24*s24)+235*s24*pow(s23,3)+87*pow(s23,4)+67*s23*pow(s24,3)-6*pow(s24,4)))+pow(s13,5)*(2*(31*s23+26*s24)*(s14*s14)*(s24*s24)+(-10*s23*s24-4*(s23*s23)+9*(s24*s24))*pow(s14,3)+s14*(-4*s23+5*s24)*pow(s24,3)+8*(s23+s24)*pow(s24,4))+pow(s13,4)*((-29*s23*s24-16*(s23*s23)+10*(s24*s24))*pow(s14,4)+s14*(s24*s24)*(-14*s24*(s23*s23)+9*s23*(s24*s24)-6*pow(s23,3)+17*pow(s24,3))+pow(s14,3)*(-81*s24*(s23*s23)+5*s23*(s24*s24)-30*pow(s23,3)+50*pow(s24,3))+s24*(s14*s14)*(56*s24*(s23*s23)+165*s23*(s24*s24)-24*pow(s23,3)+89*pow(s24,3))+4*((s23+s24)*(s23+s24))*pow(s24,4))-s13*s13*(s14*s14)*(s23*(116*s23*s24+36*(s23*s23)+73*(s24*s24))*pow(s14,3)+2*s24*((s23+s24)*(s23+s24))*(7*s24*(s23*s23)-6*s23*(s24*s24)+6*pow(s23,3)-10*pow(s24,3))+s14*s14*(358*(s23*s23)*(s24*s24)+370*s24*pow(s23,3)+119*pow(s23,4)+86*s23*pow(s24,3)-21*pow(s24,4))+s14*(232*(s24*s24)*pow(s23,3)+209*s24*pow(s23,4)+58*pow(s23,5)+10*(s23*s23)*pow(s24,3)-129*s23*pow(s24,4)-58*pow(s24,5)))+pow(s12,5)*((24*s24*(5*s23+7*s24)+s14*(-20*s23+66*s24))*pow(s13,3)+40*s24*pow(s13,4)+s13*s13*((-48*s23+169*s24)*(s14*s14)+24*s24*(12*s23*s24+5*(s23*s23)+7*(s24*s24))+s14*(52*s23*s24-104*(s23*s23)+233*(s24*s24))-20*pow(s14,3))-s14*(s14*s14*(90*s23*s24+163*(s23*s23)-80*(s24*s24))+(49*s23-3*s24)*pow(s14,3)+2*pow(s14,4)+2*s14*(95*s24*(s23*s23)-24*s23*(s24*s24)+74*pow(s23,3)-45*pow(s24,3))+20*(2*s23-s24)*pow(s23+s24,3))-2*s13*(s14*s14*(-30*s23*s24+112*(s23*s23)-147*(s24*s24))+(69*s23-35*s24)*pow(s14,3)+8*pow(s14,4)+s14*(71*s24*(s23*s23)-76*s23*(s24*s24)+62*pow(s23,3)-85*pow(s24,3))-20*s24*pow(s23+s24,3)))+s12*(2*pow(s13,6)*(2*s14*(s23-s24)*s24+(2*s23+5*s24)*(s14*s14)+6*pow(s24,3))+pow(s13,5)*(2*(s14*s14)*(48*s23*s24+2*(s23*s23)+55*(s24*s24))+(-6*s23+17*s24)*pow(s14,3)+4*(9*s23+11*s24)*pow(s24,3)+s14*(4*s24*(s23*s23)+13*pow(s24,3)))+pow(s13,4)*((11*s23*s24-75*(s23*s23)+152*(s24*s24))*pow(s14,3)+(-24*s23+17*s24)*pow(s14,4)+4*(20*s23*s24+9*(s23*s23)+11*(s24*s24))*pow(s24,3)+s14*s24*(-30*s24*(s23*s23)+46*s23*(s24*s24)-12*pow(s23,3)+83*pow(s24,3))+s14*s14*(108*s24*(s23*s23)+486*s23*(s24*s24)-24*pow(s23,3)+334*pow(s24,3)))-(s23+s24)*pow(s14,3)*((s23+s24)*(s23+s24)*(36*s24*(s23*s23)+2*s23*(s24*s24)+35*pow(s23,3)-15*pow(s24,3))+s14*s14*(143*s24*(s23*s23)+73*s23*(s24*s24)+58*pow(s23,3)-6*pow(s24,3))+s14*(296*(s23*s23)*(s24*s24)+334*s24*pow(s23,3)+125*pow(s23,4)+66*s23*pow(s24,3)-21*pow(s24,4)))-s14*(s13*s13)*((95*s23*s24+78*(s23*s23)-27*(s24*s24))*pow(s14,4)+pow(s14,3)*(841*s24*(s23*s23)+267*s23*(s24*s24)+397*pow(s23,3)-166*pow(s24,3))+s14*s14*(290*(s23*s23)*(s24*s24)+864*s24*pow(s23,3)+374*pow(s23,4)-568*s23*pow(s24,3)-367*pow(s24,4))+s14*(78*(s24*s24)*pow(s23,3)+174*s24*pow(s23,4)+44*pow(s23,5)-375*(s23*s23)*pow(s24,3)-520*s23*pow(s24,4)-197*pow(s24,5))+2*s24*(3*s23*s24+4*(s23*s23)-11*(s24*s24))*pow(s23+s24,3))-s13*(s14*s14)*(s14*((s23+s24)*(s23+s24))*(245*s24*(s23*s23)-44*s23*(s24*s24)+208*pow(s23,3)-149*pow(s24,3))+pow(s14,3)*(301*s24*(s23*s23)+148*s23*(s24*s24)+120*pow(s23,3)-21*pow(s24,3))+s14*s14*(1029*(s23*s23)*(s24*s24)+1120*s24*pow(s23,3)+378*pow(s23,4)+186*s23*pow(s24,3)-101*pow(s24,4))+2*(12*s24*(s23*s23)-9*s23*(s24*s24)+6*pow(s23,3)-13*pow(s24,3))*pow(s23+s24,3))+pow(s13,3)*(-2*(86*s23*s24+85*(s23*s23)-54*(s24*s24))*pow(s14,4)-2*(7*s23-5*s24)*pow(s14,5)+pow(s14,3)*(-352*s24*(s23*s23)+290*s23*(s24*s24)-270*pow(s23,3)+370*pow(s24,3))+s14*s24*(s23*s23*(s24*s24)-64*s24*pow(s23,3)-20*pow(s23,4)+134*s23*pow(s24,3)+89*pow(s24,4))+s14*s14*(388*(s23*s23)*(s24*s24)-92*s24*pow(s23,3)-56*pow(s23,4)+814*s23*pow(s24,3)+400*pow(s24,4))+12*pow(s24,3)*pow(s23+s24,3)))+pow(s12,4)*(2*s24*(17*s14+40*s23+76*s24)*pow(s13,4)+20*s24*pow(s13,5)+pow(s13,3)*((8*s23+213*s24)*(s14*s14)+12*s24*(32*s23*s24+10*(s23*s23)+23*(s24*s24))+s14*(72*s23*s24-56*(s23*s23)+255*(s24*s24))-20*pow(s14,3))+s13*s13*(s14*s14*(351*s23*s24-236*(s23*s23)+643*(s24*s24))+8*s24*(10*s23+19*s24)*((s23+s24)*(s23+s24))-2*(99*s23-80*s24)*pow(s14,3)-24*pow(s14,4)+s14*(-128*s24*(s23*s23)+381*s23*(s24*s24)-132*pow(s23,3)+397*pow(s24,3)))-s14*(8*s14*(6*s23*s24+19*(s23*s23)-10*(s24*s24))*((s23+s24)*(s23+s24))+(156*s23*s24+179*(s23*s23)-54*(s24*s24))*pow(s14,3)+22*s23*pow(s14,4)+s14*s14*(483*s24*(s23*s23)+14*s23*(s24*s24)+322*pow(s23,3)-163*pow(s24,3))+10*(2*s23-s24)*pow(s23+s24,4))+s13*(-32*s14*(s23*s24+3*(s23*s23)-5*(s24*s24))*((s23+s24)*(s23+s24))+(-195*s23*s24-508*(s23*s23)+367*(s24*s24))*pow(s14,3)+(-165*s23+29*s24)*pow(s14,4)-6*pow(s14,5)+s14*s14*(-354*s24*(s23*s23)+540*s23*(s24*s24)-396*pow(s23,3)+494*pow(s24,3))+20*s24*pow(s23+s24,4)))+s12*s12*(2*(s14*(s23-s24)+6*(s24*s24))*pow(s13,6)+pow(s13,5)*((24*s23+58*s24)*(s14*s14)+12*(4*s23+7*s24)*(s24*s24)+s14*(10*s23*s24+4*(s23*s23)+11*(s24*s24))-2*pow(s14,3))+pow(s13,4)*(4*(s24*s24)*(54*s23*s24+18*(s23*s23)+37*(s24*s24))+s14*s14*(357*s23*s24+4*(s23*s23)+395*(s24*s24))-45*(s23-2*s24)*pow(s14,3)-4*pow(s14,4)+s14*(-16*s24*(s23*s23)+71*s23*(s24*s24)-4*pow(s23,3)+149*pow(s24,3)))+pow(s13,3)*(12*(4*s23+7*s24)*(s24*s24)*((s23+s24)*(s23+s24))+(47*s23*s24-320*(s23*s23)+483*(s24*s24))*pow(s14,3)+(-115*s23+64*s24)*pow(s14,4)-2*pow(s14,5)+s14*s14*(221*s24*(s23*s23)+1192*s23*(s24*s24)-148*pow(s23,3)+814*pow(s24,3))+s14*(-9*(s23*s23)*(s24*s24)-92*s24*pow(s23,3)-16*pow(s23,4)+328*s23*pow(s24,3)+271*pow(s24,4)))-s13*s14*((137*s23*s24+132*(s23*s23)-36*(s24*s24))*pow(s14,4)+pow(s14,3)*(1245*s24*(s23*s23)+378*s23*(s24*s24)+628*pow(s23,3)-245*pow(s24,3))+s14*((s23+s24)*(s23+s24))*(155*s24*(s23*s23)-138*s23*(s24*s24)+116*pow(s23,3)-165*pow(s24,3))+s14*s14*(651*(s23*s23)*(s24*s24)+1432*s24*pow(s23,3)+624*pow(s23,4)-607*s23*pow(s24,3)-450*pow(s24,4))+2*(3*s23*s24+2*(s23*s23)-7*(s24*s24))*pow(s23+s24,4))-s14*s14*(s14*((s23+s24)*(s23+s24))*(211*s24*(s23*s23)+13*s23*(s24*s24)+169*pow(s23,3)-77*pow(s24,3))+pow(s14,3)*(201*s24*(s23*s23)+90*s23*(s24*s24)+92*pow(s23,3)-16*pow(s24,3))+s14*s14*(693*(s23*s23)*(s24*s24)+786*s24*pow(s23,3)+274*pow(s23,4)+103*s23*pow(s24,3)-78*pow(s24,4))+2*(3*s23*s24+8*(s23*s23)-3*(s24*s24))*pow(s23+s24,4))+s13*s13*((-398*s23*s24-461*(s23*s23)+242*(s24*s24))*pow(s14,4)+(-48*s23+21*s24)*pow(s14,5)-s14*((s23+s24)*(s23+s24))*(58*s24*(s23*s23)-33*s23*(s24*s24)+14*pow(s23,3)-137*pow(s24,3))+pow(s14,3)*(-894*s24*(s23*s23)+554*s23*(s24*s24)-732*pow(s23,3)+756*pow(s24,3))+s14*s14*(529*(s23*s23)*(s24*s24)-395*s24*pow(s23,3)-228*pow(s23,4)+1329*s23*pow(s24,3)+639*pow(s24,4))+12*(s24*s24)*pow(s23+s24,4)))+pow(s12,3)*((3*s14*(2*s23+s24)+4*s24*(5*s23+17*s24))*pow(s13,5)+4*s24*pow(s13,6)+pow(s13,4)*(6*(7*s23+25*s24)*(s14*s14)+4*s24*(56*s23*s24+10*(s23*s23)+55*(s24*s24))+s14*(34*s23*s24-4*(s23*s23)+117*(s24*s24))-10*pow(s14,3))+pow(s13,3)*(s14*s14*(538*s23*s24-84*(s23*s23)+694*(s24*s24))+(-136*s23+175*s24)*pow(s14,3)-16*pow(s14,4)+4*s24*(66*s24*(s23*s23)+111*s23*(s24*s24)+10*pow(s23,3)+55*pow(s24,3))+s14*(-66*s24*(s23*s23)+302*s23*(s24*s24)-52*pow(s23,3)+381*pow(s24,3)))+s13*s13*((-79*s23*s24-594*(s23*s23)+627*(s24*s24))*pow(s14,3)+(-207*s23+73*s24)*pow(s14,4)-6*pow(s14,5)+s14*s14*(-51*s24*(s23*s23)+1260*s23*(s24*s24)-372*pow(s23,3)+936*pow(s24,3))+s14*(81*(s23*s23)*(s24*s24)-204*s24*pow(s23,3)-72*pow(s23,4)+546*s23*pow(s24,3)+333*pow(s24,4))+4*s24*(5*s23+17*s24)*pow(s23+s24,3))-s14*((67*s23*s24+68*(s23*s23)-12*(s24*s24))*pow(s14,4)+2*pow(s14,3)*(292*s24*(s23*s23)+90*s23*(s24*s24)+153*pow(s23,3)-52*pow(s24,3))+s14*s14*(483*(s23*s23)*(s24*s24)+789*s24*pow(s23,3)+328*pow(s23,4)-137*s23*pow(s24,3)-159*pow(s24,4))+s14*(27*s23*s24+78*(s23*s23)-35*(s24*s24))*pow(s23+s24,3)+2*(2*s23-s24)*pow(s23+s24,5))+s13*((-418*s23*s24-486*(s23*s23)+195*(s24*s24))*pow(s14,4)+(-56*s23+9*s24)*pow(s14,5)+pow(s14,3)*(-1111*s24*(s23*s23)+256*s23*(s24*s24)-814*pow(s23,3)+603*pow(s24,3))-12*(s14*s14)*(-4*(s23*s23)*(s24*s24)+56*s24*pow(s23,3)+27*pow(s23,4)-67*s23*pow(s24,3)-34*pow(s24,4))-s14*(23*s23*s24+34*(s23*s23)-75*(s24*s24))*pow(s23+s24,3)+4*s24*pow(s23+s24,5))))*pow(s12+s23+s24,-1)*pow(s12+s13+s23+s24,-3))/2.;
}

// Coefficient of master 1 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg4CF<1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg4CF<1,-1>(s12,s13,s14,s23,s24),
        qq2yyg4CF<1,0>(s12,s13,s14,s23,s24),
        qq2yyg4CF<1,1>(s12,s13,s14,s23,s24)
    });
}

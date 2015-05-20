/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 5: bubble(s24)

// Coefficient order epsilon^-1 of master 5
template<>
double qq2yygz6CF<5,-1>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return -((s15n+s25n)*pow(s12,-1)*pow(s25n,-1)*(s15n*s15n*(zb*zb)*(8*s13*(s12*s12)+4*s12*(s13*s13)+s35n*zb*(3*s12*s13+5*(s12*s12)+2*(s13*s13))+(2*s12+4*s13-s35n*zb)*(s25n*s25n)*(zb*zb)+s13*(s35n*s35n)*(zb*zb)+s25n*zb*(12*s12*s13+4*s12*s35n*zb+6*s13*s35n*zb+4*(s12*s12)+6*(s13*s13)+s35n*s35n*(zb*zb))+2*pow(s12,3)+pow(s13,3))+(9*s12*s13+(5*s12+4*s13)*s35n*zb+2*s25n*zb*(3*(s12+s13)+s35n*zb)+5*(s12*s12)+3*(s13*s13)+s25n*s25n*(zb*zb)+s35n*s35n*(zb*zb))*pow(s15n,3)*pow(zb,3)+(s13+s35n*zb)*(-(s12*s12*(3*s13*s35n*zb+2*(s13*s13)+s35n*s35n*(zb*zb)))-s12*s25n*zb*((s12+4*s13)*s35n*zb+3*(s13*s13)+s35n*s35n*(zb*zb))+s25n*s25n*(zb*zb)*(s13*(s12+s13)-(s12-3*s13)*s35n*zb+2*(s35n*s35n)*(zb*zb))+s13*pow(s25n,3)*pow(zb,3))+s15n*zb*(4*s13*(s12+s13+s35n*zb)*(s25n*s25n)*(zb*zb)-s12*(s13+s35n*zb)*(-(s12*s13)+(s12+3*s13)*s35n*zb-2*(s12*s12)+s13*s13+s35n*s35n*(zb*zb))+(s13-s35n*zb)*pow(s25n,3)*pow(zb,3)+s25n*zb*(s13*(3*s12*s13+5*(s12*s12)+2*(s13*s13))+s35n*zb*(3*(s12*s12)+4*(s13*s13))+3*(-s12+s13)*(s35n*s35n)*(zb*zb)+pow(s35n,3)*pow(zb,3)))+(4*s12+3*s13+2*s25n*zb+2*s35n*zb)*pow(s15n,4)*pow(zb,4)+pow(s15n,5)*pow(zb,5))*pow(s12+s15n*zb+s25n*zb,-1)*pow(s13+s35n*zb,-3)*pow(s13+s15n*zb+s35n*zb,-1));
}

// Coefficient order epsilon^0 of master 5
template<>
double qq2yygz6CF<5,0>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return ((s15n+s25n)*pow(s12,-1)*pow(s13,-1)*pow(s15n,-1)*pow(s25n,-1)*pow(zb,-1)*(-((s12+s25n*zb)*((s13+s35n*zb)*(s13+s35n*zb))*((s13*(21*s12+19*s13)*s35n*zb+9*(2*s12+s13)*(s13*s13)+2*(3*s12+5*s13)*(s35n*s35n)*(zb*zb))*pow(s25n,3)*pow(zb,3)+s12*(s13+s35n*zb)*((3*s12+4*s13)*s35n*zb*((s12+s13)*(s12+s13))+(4*s12*s13+s12*s12+3*(s13*s13))*(s35n*s35n)*(zb*zb)+2*s13*pow(s12+s13,3)+s13*pow(s35n,3)*pow(zb,3))+s25n*s25n*(zb*zb)*(4*(s13*s13)*(6*s12*s13+5*(s12*s12)+s13*s13)+s13*s35n*zb*(63*s12*s13+29*(s12*s12)+15*(s13*s13))+(49*s12*s13+12*(s12*s12)+17*(s13*s13))*(s35n*s35n)*(zb*zb)+2*(5*s12+3*s13)*pow(s35n,3)*pow(zb,3))+s13*(5*s13+4*s35n*zb)*pow(s25n,4)*pow(zb,4)+s25n*zb*((9*s12+2*s13)*(s13*s13)*((s12+s13)*(s12+s13))+s35n*s35n*(zb*zb)*(47*s13*(s12*s12)+53*s12*(s13*s13)+9*pow(s12,3)+4*pow(s13,3))+s13*s35n*zb*(57*s13*(s12*s12)+45*s12*(s13*s13)+17*pow(s12,3)+5*pow(s13,3))+(25*s12*s13+10*(s12*s12)+s13*s13)*pow(s35n,3)*pow(zb,3)+4*s12*pow(s35n,4)*pow(zb,4))))+pow(s15n,5)*((72*s12*s13+11*(s12*s12)+56*(s13*s13))*(s35n*s35n)*(zb*zb)+s25n*s25n*(zb*zb)*(18*s13*(3*s12+2*s13)+3*(2*s12+7*s13)*s35n*zb+5*(s35n*s35n)*(zb*zb))+s13*(80*s13*(s12*s12)+22*s12*(s13*s13)+38*pow(s12,3)+pow(s13,3))+s35n*zb*(91*s13*(s12*s12)+74*s12*(s13*s13)+6*pow(s12,3)+25*pow(s13,3))+8*s13*pow(s25n,3)*pow(zb,3)+(18*s12+41*s13)*pow(s35n,3)*pow(zb,3)+s25n*zb*(3*s13*(39*s12*s13+28*(s12*s12)+8*(s13*s13))+2*s35n*zb*(57*s12*s13+6*(s12*s12)+31*(s13*s13))+(17*s12+52*s13)*(s35n*s35n)*(zb*zb)+12*pow(s35n,3)*pow(zb,3))+9*pow(s35n,4)*pow(zb,4))*pow(zb,5)+pow(s15n,4)*pow(zb,4)*(s35n*s35n*(zb*zb)*(28*s13*(s12*s12)-7*s12*(s13*s13)+5*pow(s12,3)+59*pow(s13,3))+s13*(5*(s12*s12)*(s13*s13)+70*s13*pow(s12,3)+22*pow(s12,4)-37*s12*pow(s13,3)+pow(s13,4))+s35n*zb*(20*(s12*s12)*(s13*s13)+71*s13*pow(s12,3)+4*pow(s12,4)-71*s12*pow(s13,3)+20*pow(s13,4))+(28*s13*(s12+s13)+(4*s12-13*s13)*s35n*zb-s35n*s35n*(zb*zb))*pow(s25n,3)*pow(zb,3)+(45*s12*s13+9*(s12*s12)+67*(s13*s13))*pow(s35n,3)*pow(zb,3)+s25n*s25n*(zb*zb)*(3*s13*(43*s12*s13+24*(s12*s12)+12*(s13*s13))+s35n*zb*(51*s12*s13+12*(s12*s12)+25*(s13*s13))+2*(3*s12-7*s13)*(s35n*s35n)*(zb*zb)-7*pow(s35n,3)*pow(zb,3))+2*s13*pow(s25n,4)*pow(zb,4)+2*(9*s12+16*s13)*pow(s35n,4)*pow(zb,4)+s25n*zb*(4*(s12*s13+3*(s12*s12)+17*(s13*s13))*(s35n*s35n)*(zb*zb)+s13*(171*s13*(s12*s12)+29*s12*(s13*s13)+68*pow(s12,3)+9*pow(s13,3))+s35n*zb*(135*s13*(s12*s12)+22*s12*(s13*s13)+12*pow(s12,3)+44*pow(s13,3))+(3*s12+34*s13)*pow(s35n,3)*pow(zb,3)+pow(s35n,4)*pow(zb,4))+5*pow(s35n,5)*pow(zb,5))+pow(s15n,6)*(s13*(40*s12*s13+32*(s12*s12)+7*(s13*s13))+s35n*zb*(52*s12*s13+4*(s12*s12)+29*(s13*s13))+12*s13*(s25n*s25n)*(zb*zb)+(8*s12+29*s13)*(s35n*s35n)*(zb*zb)+s25n*zb*(s13*(44*s12+25*s13)+(4*s12+31*s13)*s35n*zb+6*(s35n*s35n)*(zb*zb))+7*pow(s35n,3)*pow(zb,3))*pow(zb,6)-s15n*zb*(s13+s35n*zb)*(pow(s25n,4)*(2*s13*(21*s12+11*s13)*s35n*zb+(21*s12+13*s13)*(s13*s13)+(13*s12+15*s13)*(s35n*s35n)*(zb*zb)+6*pow(s35n,3)*pow(zb,3))*pow(zb,4)+pow(s25n,3)*pow(zb,3)*(2*s13*s35n*zb*(122*s12*s13+55*(s12*s12)+12*(s13*s13))+s13*s13*(111*s12*s13+62*(s12*s12)+12*(s13*s13))+(190*s12*s13+36*(s12*s12)+25*(s13*s13))*(s35n*s35n)*(zb*zb)+(57*s12+25*s13)*pow(s35n,3)*pow(zb,3)+12*pow(s35n,4)*pow(zb,4))+s12*(s13+s35n*zb)*(s12*s13*(7*s12+23*s13)*((s12+s13)*(s12+s13))+s35n*s35n*(zb*zb)*(43*s13*(s12*s12)+35*s12*(s13*s13)+12*pow(s12,3)-8*pow(s13,3))+s35n*zb*(92*(s12*s12)*(s13*s13)+44*s13*pow(s12,3)+5*pow(s12,4)+50*s12*pow(s13,3)-3*pow(s13,4))+(7*s12*s13+3*(s12*s12)-7*(s13*s13))*pow(s35n,3)*pow(zb,3)-(s12+2*s13)*pow(s35n,4)*pow(zb,4))+s25n*s25n*(zb*zb)*(s35n*s35n*(zb*zb)*(352*s13*(s12*s12)+377*s12*(s13*s13)+38*pow(s12,3)-25*pow(s13,3))+s13*s35n*zb*(464*s13*(s12*s12)+337*s12*(s13*s13)+114*pow(s12,3)-4*pow(s13,3))+s13*s13*(207*s13*(s12*s12)+117*s12*(s13*s13)+68*pow(s12,3)+4*pow(s13,3))+(208*s12*s13+95*(s12*s12)-16*(s13*s13))*pow(s35n,3)*pow(zb,3)+(51*s12+7*s13)*pow(s35n,4)*pow(zb,4)+6*pow(s35n,5)*pow(zb,5))+s25n*zb*(2*s12*(s13*s13)*(73*s13*(s12*s12)+78*s12*(s13*s13)+17*pow(s12,3)+22*pow(s13,3))+s35n*s35n*(zb*zb)*(475*(s12*s12)*(s13*s13)+233*s13*pow(s12,3)+20*pow(s12,4)+178*s12*pow(s13,3)-15*pow(s13,4))+s13*s35n*zb*(450*(s12*s12)*(s13*s13)+323*s13*pow(s12,3)+56*pow(s12,4)+141*s12*pow(s13,3)-4*pow(s13,4))+(221*s13*(s12*s12)+118*s12*(s13*s13)+56*pow(s12,3)-21*pow(s13,3))*pow(s35n,3)*pow(zb,3)+(46*s12*s13+40*(s12*s12)-13*(s13*s13))*pow(s35n,4)*pow(zb,4)+3*(3*s12-s13)*pow(s35n,5)*pow(zb,5))+2*s13*s35n*pow(s25n,5)*pow(zb,6))+pow(s15n,3)*pow(zb,3)*(-(s35n*s35n*(zb*zb)*(278*(s12*s12)*(s13*s13)+61*s13*pow(s12,3)+pow(s12,4)+148*s12*pow(s13,3)-65*pow(s13,4)))+s13*(-35*(s13*s13)*pow(s12,3)+25*s13*pow(s12,4)+5*pow(s12,5)-116*(s12*s12)*pow(s13,3)-45*s12*pow(s13,4)+4*pow(s13,5))+s35n*zb*(-85*(s13*s13)*pow(s12,3)+22*s13*pow(s12,4)+pow(s12,5)-318*(s12*s12)*pow(s13,3)-144*s12*pow(s13,4)+26*pow(s13,5))-(73*s13*(s12*s12)+35*s12*(s13*s13)+13*pow(s12,3)-80*pow(s13,3))*pow(s35n,3)*pow(zb,3)+pow(s25n,3)*pow(zb,3)*(2*s35n*zb*(-16*s12*s13+2*(s12*s12)-11*(s13*s13))+s13*(67*s12*s13+20*(s12*s12)+28*(s13*s13))-7*(s12+10*s13)*(s35n*s35n)*(zb*zb)-22*pow(s35n,3)*pow(zb,3))+(s13*(5*s12+13*s13)+(s12-20*s13)*s35n*zb-3*(s35n*s35n)*(zb*zb))*pow(s25n,4)*pow(zb,4)+(23*s12*s13+3*(s12*s12)+50*(s13*s13))*pow(s35n,4)*pow(zb,4)-s25n*s25n*(zb*zb)*((251*s12*s13+6*(s12*s12)+29*(s13*s13))*(s35n*s35n)*(zb*zb)-s13*(120*s13*(s12*s12)-17*s12*(s13*s13)+30*pow(s12,3)+16*pow(s13,3))-s35n*zb*(18*s13*(s12*s12)-211*s12*(s13*s13)+6*pow(s12,3)+24*pow(s13,3))+(63*s12+73*s13)*pow(s35n,3)*pow(zb,3)+36*pow(s35n,4)*pow(zb,4))+(9*s12+14*s13)*pow(s35n,5)*pow(zb,5)+s25n*zb*(-(s35n*s35n*(zb*zb)*(218*s13*(s12*s12)+457*s12*(s13*s13)+3*pow(s12,3)-91*pow(s13,3)))+s13*(-72*(s12*s12)*(s13*s13)+91*s13*pow(s12,3)+20*pow(s12,4)-143*s12*pow(s13,3)+6*pow(s13,4))+2*s35n*zb*(-125*(s12*s12)*(s13*s13)+26*s13*pow(s12,3)+2*pow(s12,4)-215*s12*pow(s13,3)+22*pow(s13,4))-2*(105*s12*s13+23*(s12*s12)-32*(s13*s13))*pow(s35n,3)*pow(zb,3)+(-40*s12+s13)*pow(s35n,4)*pow(zb,4)-10*pow(s35n,5)*pow(zb,5))+pow(s35n,6)*pow(zb,6))-s15n*s15n*(zb*zb)*(pow(s25n,4)*(s13*(21*s12+16*s13)*s35n*zb-3*(5*s12+3*s13)*(s13*s13)+(4*s12+35*s13)*(s35n*s35n)*(zb*zb)+10*pow(s35n,3)*pow(zb,3))*pow(zb,4)+pow(s25n,3)*pow(zb,3)*(s12*(-29*s12+45*s13)*(s13*s13)+2*s13*s35n*zb*(111*s12*s13+13*(s12*s12)+8*(s13*s13))+(238*s12*s13+7*(s12*s12)+65*(s13*s13))*(s35n*s35n)*(zb*zb)+(61*s12+83*s13)*pow(s35n,3)*pow(zb,3)+34*pow(s35n,4)*pow(zb,4))+(6*s13*s35n*zb-3*(s13*s13)+s35n*s35n*(zb*zb))*pow(s25n,5)*pow(zb,5)+s25n*s25n*(zb*zb)*(s35n*s35n*(zb*zb)*(386*s13*(s12*s12)+791*s12*(s13*s13)+7*pow(s12,3)-56*pow(s13,3))+s13*s35n*zb*(427*s13*(s12*s12)+625*s12*(s13*s13)+12*pow(s12,3)-33*pow(s13,3))+s13*s13*(133*s13*(s12*s12)+190*s12*(s13*s13)-27*pow(s12,3)-2*pow(s13,3))+(470*s12*s13+92*(s12*s12)+9*(s13*s13))*pow(s35n,3)*pow(zb,3)+2*(57*s12+32*s13)*pow(s35n,4)*pow(zb,4)+30*pow(s35n,5)*pow(zb,5))-(s13+s35n*zb)*(-(s35n*s35n*(zb*zb)*(193*(s12*s12)*(s13*s13)+119*s13*pow(s12,3)+16*pow(s12,4)+22*s12*pow(s13,3)-24*pow(s13,4)))-s35n*zb*(218*(s13*s13)*pow(s12,3)+47*s13*pow(s12,4)+pow(s12,5)+238*(s12*s12)*pow(s13,3)+34*s12*pow(s13,4)-11*pow(s13,5))+s13*(-113*(s13*s13)*pow(s12,3)-32*s13*pow(s12,4)+2*pow(s12,5)-94*(s12*s12)*pow(s13,3)-13*s12*pow(s13,4)+2*pow(s13,5))+(-47*s13*(s12*s12)+8*s12*(s13*s13)-15*pow(s12,3)+26*pow(s13,3))*pow(s35n,3)*pow(zb,3)+(11*s12*s13+2*(s12*s12)+14*(s13*s13))*pow(s35n,4)*pow(zb,4)+(2*s12+3*s13)*pow(s35n,5)*pow(zb,5))+s25n*zb*(s35n*zb*(s13*s13)*(900*s13*(s12*s12)+498*s12*(s13*s13)+300*pow(s12,3)-35*pow(s13,3))+s35n*s35n*(zb*zb)*(1003*(s12*s12)*(s13*s13)+246*s13*pow(s12,3)+4*pow(s12,4)+701*s12*pow(s13,3)-95*pow(s13,4))+s13*s13*(293*(s12*s12)*(s13*s13)+111*s13*pow(s12,3)-12*pow(s12,4)+139*s12*pow(s13,3)-4*pow(s13,4))+(481*s13*(s12*s12)+503*s12*(s13*s13)+57*pow(s12,3)-110*pow(s13,3))*pow(s35n,3)*pow(zb,3)+5*(40*s12*s13+17*(s12*s12)-10*(s13*s13))*pow(s35n,4)*pow(zb,4)+(39*s12+s13)*pow(s35n,5)*pow(zb,5)+5*pow(s35n,6)*pow(zb,6)))+((s12+11*s13)*s35n*zb+s13*(13*s12+7*s13+8*s25n*zb)+2*(s35n*s35n)*(zb*zb))*pow(s15n,7)*pow(zb,7)+2*s13*pow(s15n,8)*pow(zb,8))*pow(s12+s15n*zb+s25n*zb,-1)*pow(s13+s35n*zb,-3)*pow(s13+s15n*zb+s35n*zb,-1)*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-2))/2.;
}

// Coefficient order epsilon^1 of master 5
template<>
double qq2yygz6CF<5,1>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return -((s15n+s25n)*pow(s12,-1)*pow(s13,-1)*pow(s15n,-1)*pow(s25n,-1)*pow(zb,-1)*(pow(s15n,5)*(-3*(111*s12*s13+14*(s12*s12)+114*(s13*s13))*(s35n*s35n)*(zb*zb)+s25n*s25n*(zb*zb)*(2*(21*s12-10*s13)*s13+9*(2*s12-7*s13)*s35n*zb-33*(s35n*s35n)*(zb*zb))+s35n*zb*(-49*s13*(s12*s12)-336*s12*(s13*s13)+6*pow(s12,3)-248*pow(s13,3))-s13*(13*s13*(s12*s12)+114*s12*(s13*s13)-14*pow(s12,3)+67*pow(s13,3))+2*(7*s13+3*s35n*zb)*pow(s25n,3)*pow(zb,3)-(111*s12+208*s13)*pow(s35n,3)*pow(zb,3)-s25n*zb*(s13*(31*s12*s13-42*(s12*s12)+117*(s13*s13))+s35n*zb*(108*s12*s13-18*(s12*s12)+325*(s13*s13))+(73*s12+294*s13)*(s35n*s35n)*(zb*zb)+86*pow(s35n,3)*pow(zb,3))-47*pow(s35n,4)*pow(zb,4))*pow(zb,5)+pow(s15n,4)*pow(zb,4)*(-2*(s35n*s35n)*(zb*zb)*(268*s13*(s12*s12)+575*s12*(s13*s13)+18*pow(s12,3)+249*pow(s13,3))+s35n*zb*(-548*(s12*s12)*(s13*s13)-51*s13*pow(s12,3)+4*pow(s12,4)-795*s12*pow(s13,3)-267*pow(s13,4))-s13*(190*(s12*s12)*(s13*s13)+21*s13*pow(s12,3)-6*pow(s12,4)+205*s12*pow(s13,3)+57*pow(s13,4))+(24*(s12-s13)*s13+(16*s12-69*s13)*s35n*zb-27*(s35n*s35n)*(zb*zb))*pow(s25n,3)*pow(zb,3)-(735*s12*s13+178*(s12*s12)+462*(s13*s13))*pow(s35n,3)*pow(zb,3)-s25n*s25n*(zb*zb)*(s13*(63*s12*s13-36*(s12*s12)+169*(s13*s13))+s35n*zb*(177*s12*s13-24*(s12*s12)+455*(s13*s13))+(84*s12+389*s13)*(s35n*s35n)*(zb*zb)+103*pow(s35n,3)*pow(zb,3))+2*(3*s13+2*s35n*zb)*pow(s25n,4)*pow(zb,4)-(175*s12+213*s13)*pow(s35n,4)*pow(zb,4)-s25n*zb*((944*s12*s13+93*(s12*s12)+995*(s13*s13))*(s35n*s35n)*(zb*zb)+3*s13*(20*s13*(s12*s12)+124*s12*(s13*s13)-8*pow(s12,3)+73*pow(s13,3))+s35n*zb*(159*s13*(s12*s12)+1032*s12*(s13*s13)-16*pow(s12,3)+768*pow(s13,3))+2*(142*s12+281*s13)*pow(s35n,3)*pow(zb,3)+116*pow(s35n,4)*pow(zb,4))-39*pow(s35n,5)*pow(zb,5))-(s13+s35n*zb)*(s13+s35n*zb)*(pow(s25n,3)*pow(zb,3)*(s13*s13*(90*s12*s13+86*(s12*s12)+23*(s13*s13))+s13*s35n*zb*(165*s12*s13+96*(s12*s12)+56*(s13*s13))+2*(46*s12*s13+14*(s12*s12)+21*(s13*s13))*(s35n*s35n)*(zb*zb)+(17*s12+9*s13)*pow(s35n,3)*pow(zb,3))+(s13*(49*s12+44*s13)*s35n*zb+3*(17*s12+9*s13)*(s13*s13)+(10*s12+17*s13)*(s35n*s35n)*(zb*zb))*pow(s25n,4)*pow(zb,4)+(s13+s35n*zb)*(s12*s12)*(2*s13*(3*s12+s13)*((s12+s13)*(s12+s13))+2*(10*s12*s13+5*(s12*s12)+3*(s13*s13))*(s35n*s35n)*(zb*zb)+s35n*zb*(22*s13*(s12*s12)+21*s12*(s13*s13)+5*pow(s12,3)+4*pow(s13,3))+2*(5*s12+3*s13)*pow(s35n,3)*pow(zb,3)+2*pow(s35n,4)*pow(zb,4))+s25n*s25n*(zb*zb)*(s13*s13*(107*s13*(s12*s12)+52*s12*(s13*s13)+69*pow(s12,3)+10*pow(s13,3))+2*s13*s35n*zb*(108*s13*(s12*s12)+63*s12*(s13*s13)+44*pow(s12,3)+14*pow(s13,3))+s35n*s35n*(zb*zb)*(147*s13*(s12*s12)+98*s12*(s13*s13)+31*pow(s12,3)+26*pow(s13,3))+(29*s12*s13+38*(s12*s12)+8*(s13*s13))*pow(s35n,3)*pow(zb,3)+5*s12*pow(s35n,4)*pow(zb,4))+s13*(11*s13+8*s35n*zb)*pow(s25n,5)*pow(zb,5)+s12*s25n*zb*(s35n*s35n*(zb*zb)*(104*s13*(s12*s12)+85*s12*(s13*s13)+18*pow(s12,3)-4*pow(s13,3))+s13*s13*(58*s13*(s12*s12)+37*s12*(s13*s13)+29*pow(s12,3)+8*pow(s13,3))+s13*s35n*zb*(131*s13*(s12*s12)+93*s12*(s13*s13)+44*pow(s12,3)+14*pow(s13,3))+(42*s12*s13+31*(s12*s12)-20*(s13*s13))*pow(s35n,3)*pow(zb,3)+(13*s12-12*s13)*pow(s35n,4)*pow(zb,4)-2*pow(s35n,5)*pow(zb,5)))+pow(s15n,6)*(s35n*zb*(-26*s12*s13+4*(s12*s12)-85*(s13*s13))+s13*(-3*s12*s13+16*(s12*s12)-29*(s13*s13))+4*(4*s13+s35n*zb)*(s25n*s25n)*(zb*zb)-(25*s12+83*s13)*(s35n*s35n)*(zb*zb)-2*s25n*zb*(-16*s12*s13-4*s12*s35n*zb+15*s13*s35n*zb+3*(s13*s13)+11*(s35n*s35n)*(zb*zb))-27*pow(s35n,3)*pow(zb,3))*pow(zb,6)+pow(s15n,3)*pow(zb,3)*(-(s35n*s35n*(zb*zb)*(1512*(s12*s12)*(s13*s13)+436*s13*pow(s12,3)+16*pow(s12,4)+1277*s12*pow(s13,3)+245*pow(s13,4)))+s35n*zb*(-463*(s13*s13)*pow(s12,3)-29*s13*pow(s12,4)+pow(s12,5)-1031*(s12*s12)*pow(s13,3)-629*s12*pow(s13,4)-100*pow(s13,5))-s13*(167*(s13*s13)*pow(s12,3)+15*s13*pow(s12,4)-pow(s12,5)+265*(s12*s12)*pow(s13,3)+123*s12*pow(s13,4)+17*pow(s13,5))-5*(198*s13*(s12*s12)+257*s12*(s13*s13)+28*pow(s12,3)+64*pow(s13,3))*pow(s35n,3)*pow(zb,3)-pow(s25n,3)*pow(zb,3)*(s13*(45*s12*s13-10*(s12*s12)+103*(s13*s13))+s35n*zb*(134*s12*s13-10*(s12*s12)+269*(s13*s13))+(49*s12+218*s13)*(s35n*s35n)*(zb*zb)+52*pow(s35n,3)*pow(zb,3))+((5*s12-12*s13)*s13+(5*s12-39*s13)*s35n*zb-13*(s35n*s35n)*(zb*zb))*pow(s25n,4)*pow(zb,4)-(640*s12*s13+244*(s12*s12)+235*(s13*s13))*pow(s35n,4)*pow(zb,4)-s25n*s25n*(zb*zb)*((962*s12*s13+75*(s12*s12)+986*(s13*s13))*(s35n*s35n)*(zb*zb)+5*s35n*zb*(36*s13*(s12*s12)+223*s12*(s13*s13)-2*pow(s12,3)+166*pow(s13,3))+s13*(69*s13*(s12*s12)+419*s12*(s13*s13)-10*pow(s12,3)+258*pow(s13,3))+19*(14*s12+27*s13)*pow(s35n,3)*pow(zb,3)+99*pow(s35n,4)*pow(zb,4))+(s13+s35n*zb)*pow(s25n,5)*pow(zb,5)-2*(63*s12+46*s13)*pow(s35n,5)*pow(zb,5)-s25n*zb*(s35n*s35n*(zb*zb)*(1132*s13*(s12*s12)+2642*s12*(s13*s13)+55*pow(s12,3)+1183*pow(s13,3))+s13*(467*(s12*s12)*(s13*s13)+51*s13*pow(s12,3)-5*pow(s12,4)+563*s12*pow(s13,3)+166*pow(s13,4))+s35n*zb*(1261*(s12*s12)*(s13*s13)+114*s13*pow(s12,3)-5*pow(s12,4)+1989*s12*pow(s13,3)+704*pow(s13,4))+(1567*s12*s13+338*(s12*s12)+983*(s13*s13))*pow(s35n,3)*pow(zb,3)+13*(27*s12+31*s13)*pow(s35n,4)*pow(zb,4)+65*pow(s35n,5)*pow(zb,5))-15*pow(s35n,6)*pow(zb,6))+s15n*zb*(s13+s35n*zb)*(pow(s25n,4)*(-(s13*(34*s12+71*s13)*s35n*zb)-3*(11*s12+17*s13)*(s13*s13)-(9*s12+17*s13)*(s35n*s35n)*(zb*zb)+3*pow(s35n,3)*pow(zb,3))*pow(zb,4)-pow(s25n,3)*pow(zb,3)*(s13*s13*(243*s12*s13+96*(s12*s12)+98*(s13*s13))+2*s13*s35n*zb*(236*s12*s13+64*(s12*s12)+117*(s13*s13))+(303*s12*s13+44*(s12*s12)+180*(s13*s13))*(s35n*s35n)*(zb*zb)+2*(37*s12+24*s13)*pow(s35n,3)*pow(zb,3)+4*pow(s35n,4)*pow(zb,4))-s25n*s25n*(zb*zb)*(s13*s13*(361*s13*(s12*s12)+281*s12*(s13*s13)+106*pow(s12,3)+65*pow(s13,3))+s13*s35n*zb*(799*s13*(s12*s12)+775*s12*(s13*s13)+152*pow(s12,3)+201*pow(s13,3))+s35n*s35n*(zb*zb)*(611*s13*(s12*s12)+796*s12*(s13*s13)+54*pow(s12,3)+223*pow(s13,3))+(385*s12*s13+173*(s12*s12)+104*(s13*s13))*pow(s35n,3)*pow(zb,3)+(83*s12+18*s13)*pow(s35n,4)*pow(zb,4)+pow(s35n,5)*pow(zb,5))-s12*(s13+s35n*zb)*(3*(s35n*s35n)*(zb*zb)*(60*s13*(s12*s12)+45*s12*(s13*s13)+15*pow(s12,3)+2*pow(s13,3))+s35n*zb*(175*(s12*s12)*(s13*s13)+98*s13*pow(s12,3)+9*pow(s12,4)+74*s12*pow(s13,3)-4*pow(s13,4))+s13*(65*(s12*s12)*(s13*s13)+57*s13*pow(s12,3)+13*pow(s12,4)+19*s12*pow(s13,3)-2*pow(s13,4))+(115*s12*s13+68*(s12*s12)+20*(s13*s13))*pow(s35n,3)*pow(zb,3)+(35*s12+16*s13)*pow(s35n,4)*pow(zb,4)+4*pow(s35n,5)*pow(zb,5))+2*s35n*(3*s13+2*s35n*zb)*pow(s25n,5)*pow(zb,6)+s25n*zb*(-2*(s13*s13)*(120*(s12*s12)*(s13*s13)+113*s13*pow(s12,3)+28*pow(s12,4)+44*s12*pow(s13,3)+5*pow(s13,4))-s13*s35n*zb*(749*(s12*s12)*(s13*s13)+553*s13*pow(s12,3)+86*pow(s12,4)+297*s12*pow(s13,3)+40*pow(s13,4))-s35n*s35n*(zb*zb)*(923*(s12*s12)*(s13*s13)+468*s13*pow(s12,3)+32*pow(s12,4)+404*s12*pow(s13,3)+60*pow(s13,4))-(553*s13*(s12*s12)+292*s12*(s13*s13)+141*pow(s12,3)+38*pow(s13,3))*pow(s35n,3)*pow(zb,3)-(120*s12*s13+139*(s12*s12)+4*(s13*s13))*pow(s35n,4)*pow(zb,4)+(-23*s12+6*s13)*pow(s35n,5)*pow(zb,5)+2*pow(s35n,6)*pow(zb,6)))-s15n*s15n*(zb*zb)*(pow(s25n,4)*(3*s13*(13*s12+16*s13)*s35n*zb+2*(5*s12+11*s13)*(s13*s13)+(13*s12+30*s13)*(s35n*s35n)*(zb*zb)+4*pow(s35n,3)*pow(zb,3))*pow(zb,4)+pow(s25n,3)*pow(zb,3)*(2*(s13*s13)*(97*s12*s13+11*(s12*s12)+73*(s13*s13))+2*s13*s35n*zb*(243*s12*s13+35*(s12*s12)+201*(s13*s13))+2*(197*s12*s13+12*(s12*s12)+197*(s13*s13))*(s35n*s35n)*(zb*zb)+3*(34*s12+55*s13)*pow(s35n,3)*pow(zb,3)+27*pow(s35n,4)*pow(zb,4))+(9*s13*s35n*zb+2*(s13*s13)+3*(s35n*s35n)*(zb*zb))*pow(s25n,5)*pow(zb,5)+s25n*s25n*(zb*zb)*(s13*s13*(365*s13*(s12*s12)+542*s12*(s13*s13)+26*pow(s12,3)+180*pow(s13,3))+s13*s35n*zb*(933*s13*(s12*s12)+1734*s12*(s13*s13)+66*pow(s12,3)+659*pow(s13,3))+s35n*s35n*(zb*zb)*(783*s13*(s12*s12)+2096*s12*(s13*s13)+24*pow(s12,3)+939*pow(s13,3))+5*(231*s12*s13+43*(s12*s12)+131*(s13*s13))*pow(s35n,3)*pow(zb,3)+(251*s12+229*s13)*pow(s35n,4)*pow(zb,4)+34*pow(s35n,5)*pow(zb,5))+(s13+s35n*zb)*(s35n*s35n*(zb*zb)*(770*(s12*s12)*(s13*s13)+474*s13*pow(s12,3)+55*pow(s12,4)+287*s12*pow(s13,3)+10*pow(s13,4))+s12*s13*(178*(s12*s12)*(s13*s13)+75*s13*pow(s12,3)+4*pow(s12,4)+121*s12*pow(s13,3)+18*pow(s13,4))+s35n*zb*(495*(s13*s13)*pow(s12,3)+126*s13*pow(s12,4)+3*pow(s12,5)+490*(s12*s12)*pow(s13,3)+117*s12*pow(s13,4)+2*pow(s13,5))+(546*s13*(s12*s12)+335*s12*(s13*s13)+156*pow(s12,3)+20*pow(s13,3))*pow(s35n,3)*pow(zb,3)+(187*s12*s13+145*(s12*s12)+20*(s13*s13))*pow(s35n,4)*pow(zb,4)+10*(4*s12+s13)*pow(s35n,5)*pow(zb,5)+2*pow(s35n,6)*pow(zb,6))+s25n*zb*(s13*s13*(552*(s12*s12)*(s13*s13)+268*s13*pow(s12,3)+16*pow(s12,4)+334*s12*pow(s13,3)+59*pow(s13,4))+s13*s35n*zb*(1917*(s12*s12)*(s13*s13)+696*s13*pow(s12,3)+33*pow(s12,4)+1396*s12*pow(s13,3)+280*pow(s13,4))+s35n*s35n*(zb*zb)*(2539*(s12*s12)*(s13*s13)+600*s13*pow(s12,3)+13*pow(s12,4)+2373*s12*pow(s13,3)+544*pow(s13,4))+(1532*s13*(s12*s12)+2067*s12*(s13*s13)+172*pow(s12,3)+552*pow(s13,3))*pow(s35n,3)*pow(zb,3)+(929*s12*s13+358*(s12*s12)+307*(s13*s13))*pow(s35n,4)*pow(zb,4)+(173*s12+88*s13)*pow(s35n,5)*pow(zb,5)+10*pow(s35n,6)*pow(zb,6)))+(9*s12*s13+(s12-6*s13)*s35n*zb+s25n*zb*(9*s13+s35n*zb)-6*(s35n*s35n)*(zb*zb))*pow(s15n,7)*pow(zb,7)+2*s13*pow(s15n,8)*pow(zb,8))*pow(s12+s15n*zb+s25n*zb,-1)*pow(s13+s35n*zb,-3)*pow(s13+s15n*zb+s35n*zb,-1)*pow(s12+s13+s15n*zb+s25n*zb+s35n*zb,-2))/2.;
}

// Coefficient of master 5 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz6CF<5>(const double& s12, const double& s13, const double& s15n, const double& s25n, const double& s35n, const double& zb)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz6CF<5,-1>(s12,s13,s15n,s25n,s35n,zb),
        qq2yygz6CF<5,0>(s12,s13,s15n,s25n,s35n,zb),
        qq2yygz6CF<5,1>(s12,s13,s15n,s25n,s35n,zb)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 13: box(s23,(-s12-s23-s24),s14)

// Coefficient order epsilon^0 of master 13
template<>
double qq2yygCA<13,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (-(pow(s13,-1)*(-(s24*(3*s14+2*s24)*(s13*s13))+s13*s14*(s14*(3*s23-10*s24)+(s23-6*s24)*s24-4*(s14*s14))+s14*s14*(2*s14*(s23-4*s24)+2*s23*s24-4*(s14*s14)+s23*s23-4*(s24*s24))+s12*s12*(13*s23*s24+4*s14*(5*s23+3*s24)+s13*(s14+3*s23+4*s24)-2*(s13*s13)-4*(s14*s14)+s23*s23+12*(s24*s24))+(2*s13+6*s14+9*s23+12*s24)*pow(s12,3)+4*pow(s12,4)+s12*(-((3*s14+4*s24)*(s13*s13))+s13*(s14*(6*s23-5*s24)+s24*(3*s23+2*s24)-5*(s14*s14))+(13*s23-8*s24)*(s14*s14)+4*(s23+s24)*(s24*s24)+s14*(15*s23*s24+2*(s23*s23)+6*(s24*s24))-10*pow(s14,3)))*pow(s12+s14+s24,-2))-pow(s14,-1)*pow(s12+s13+s14,-1)*(s13*s13*(3*s14*s24*(s23+s24)+2*s23*(s14*s14)+s23*(s24*s24))+(2*s13*(5*s14+3*s24)+s14*(8*s23+22*s24)+14*(s14*s14)-s23*s23+6*(s24*s24))*pow(s12,3)+(2*s13+9*s14+s23+6*s24)*pow(s12,4)+2*pow(s12,5)+s14*s23*(-4*s14*s23*s24+(s23+2*s24)*(s14*s14)-2*s23*(s24*s24)+2*pow(s14,3))+s12*s12*((3*s14+s23)*(s13*s13)+3*(5*s23+8*s24)*(s14*s14)+s24*(-3*s23*s24-3*(s23*s23)+2*(s24*s24))+s13*(-2*s23*s24+s14*(5*s23+22*s24)+10*(s14*s14)-3*(s23*s23)+6*(s24*s24))+s14*(6*s23*s24-s23*s23+17*(s24*s24))+9*pow(s14,3))+s12*((2*s23*s24+3*s14*(s23+2*s24))*(s13*s13)-2*s23*(s23+s24)*(s24*s24)+s14*s24*(-2*s23*s24-7*(s23*s23)+4*(s24*s24))+s14*s14*(8*s23*s24+s23*s23+10*(s24*s24))+2*(5*s23+4*s24)*pow(s14,3)+s13*(3*(3*s23+4*s24)*(s14*s14)+s24*(-4*s23*s24-7*(s23*s23)+2*(s24*s24))+s14*(5*s23*s24-6*(s23*s23)+14*(s24*s24))+2*pow(s14,3))+2*pow(s14,4))+s13*(-2*s23*(2*s23+s24)*(s24*s24)+s14*s14*(-3*(s23*s23)+2*(s24*s24))+4*s23*pow(s14,3)+s14*(-7*s24*(s23*s23)+2*pow(s24,3))))*pow(s12+s14+s24,-2)+pow(s13,-1)*pow(s14,-1)*(-(s14*s24*(3*s14+s24)*(s13*s13))+(15*s23*s24+s13*(3*s14+6*s23+5*s24)+2*s14*(8*s23+7*s24)+9*(s14*s14)+5*(s23*s23)+9*(s24*s24))*pow(s12,3)+(s13+5*(s14+s23+s24))*pow(s12,4)+pow(s12,5)+s14*s23*((3*s23+2*s24)*(s14*s14)+s24*(3*s23*s24+s23*s23+2*(s24*s24))+s14*(4*s23*s24+s23*s23+2*(s24*s24))+2*pow(s14,3))+s12*s12*(-(s14*(s13*s13))+(19*s23+13*s24)*(s14*s14)+s13*(17*s23*s24+2*s14*(7*s23+4*s24)+s14*s14+3*(s23*s23)+9*(s24*s24))+s14*(28*s23*s24+13*(s23*s23)+13*(s24*s24))+(s23+7*s24)*((s23+s24)*(s23+s24))+7*pow(s14,3))+s13*(s14*s14*(-2*s23*s24+4*(s23*s23)-6*(s24*s24))+s24*s24*(5*s23*s24+3*(s23*s23)+2*(s24*s24))+s14*s24*(8*s23*s24+7*(s23*s23)+2*(s24*s24))-(s23+6*s24)*pow(s14,3))+s12*(-(s14*(3*s14+2*s24)*(s13*s13))+s14*s14*(15*s23*s24+11*(s23*s23)+4*(s24*s24))+s24*(s23+2*s24)*((s23+s24)*(s23+s24))+s13*((7*s23-5*s24)*(s14*s14)+s24*(16*s23*s24+6*(s23*s23)+7*(s24*s24))+s14*(22*s23*s24+7*(s23*s23)+7*(s24*s24))-pow(s14,3))+2*(5*s23+2*s24)*pow(s14,3)+2*pow(s14,4)+s14*(13*s24*(s23*s23)+14*s23*(s24*s24)+2*pow(s23,3)+4*pow(s24,3))))*pow(s12+s14+s24,-2)-pow(s12+s13+s14,-1)*(s24*(2*s14+s24)*(s13*s13)+26*s23*s24*(s14*s14)+s12*s12*(s13*(16*s14+9*s23+24*s24)+s24*(28*s23+31*s24)+s14*(28*s23+62*s24)+s13*s13+29*(s14*s14))+5*s14*s24*(s23*s23)+24*s14*s23*(s24*s24)+22*(s14*s14)*(s24*s24)+4*(s23*s23)*(s24*s24)+(8*s13+24*s14+9*s23+24*s24)*pow(s12,3)+7*pow(s12,4)+10*s23*pow(s14,3)+12*s24*pow(s14,3)+s13*(s14*s24*(19*s23+16*s24)+2*(3*s23+5*s24)*(s14*s14)+(11*s23+8*s24)*(s24*s24)+2*pow(s14,3))+s12*(54*s14*s24*(s23+s24)+2*(s14+s24)*(s13*s13)+(29*s23+50*s24)*(s14*s14)+s13*(4*s24*(5*s23+6*s24)+s14*(15*s23+32*s24)+10*(s14*s14))+s24*(27*s23*s24+5*(s23*s23)+18*(s24*s24))+14*pow(s14,3))+2*pow(s14,4)+16*s14*pow(s24,3)+8*s23*pow(s24,3)+4*pow(s24,4))*pow(s12+s14+s24,-2)+pow(s13,-1)*pow(s24,-1)*(2*s12*s13*s14+4*s12*s13*s23+6*s12*s14*s23+20*s13*s14*s23-8*s12*s13*s24-6*s12*s14*s24+4*s12*s23*s24+2*s13*s23*s24+10*s14*s23*s24+2*s14*(s12*s12)+4*s12*(s13*s13)+10*s23*(s13*s13)-2*s12*(s14*s14)+10*s23*(s14*s14)+6*s13*(s23*s23)+10*s14*(s23*s23)+12*s24*(s23*s23)+4*s12*(s24*s24)-4*s13*(s24*s24)+12*s23*(s24*s24)+4*pow(s23,3)+4*pow(s24,3)+(3*s23+2*s24)*(s13*s13*(2*s14*s24+2*(s14*s14)+s24*s24)+2*s13*s14*(s14*(s23+3*s24)+2*(s14*s14)+s24*s24)+s14*s14*(2*s23*s24+2*s14*(s23+2*s24)+2*(s14*s14)+s23*s23+2*(s24*s24))+s12*s12*(6*s14*(s23+s24)+2*s13*(3*s14+s23+2*s24)+s13*s13+7*(s14*s14)+(s23+s24)*(s23+s24))+2*s12*(s13*(2*s14+s24)*(2*s14+s23+s24)+(s14+s24)*(s13*s13)+s14*(s14*(3*s23+4*s24)+3*(s14*s14)+(s23+s24)*(s23+s24)))+2*(s13+2*s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s12+s14+s24,-2)-(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(6*s14*s23+s12*(2*s13-s14+3*s23-s24)+2*s14*s24+5*s23*s24+2*s13*(3*s23+s24)+s12*s12+5*(s23*s23))*pow(s12+s14+s24,-1))+pow(s13,-1)*pow(s14,-1)*pow(s24,-1)*(8*s13*s14*s23*s24+4*s14*s23*(s13*s13)-2*s14*s24*(s13*s13)+2*s23*s24*(s13*s13)+10*s13*s23*(s14*s14)-2*s13*s24*(s14*s14)+6*s23*s24*(s14*s14)-2*(s12*s12)*(-(s13*s23)+4*s14*s24+s14*s14)+2*s13*s14*(s23*s23)-2*s13*s24*(s23*s23)-2*s14*s24*(s23*s23)+2*(s14*s14)*(s23*s23)-2*s12*((2*s14-s23)*(s13*s13)-s14*(s14*(3*s23+s24)-s24*(s23+2*s24)+s14*s14)+s13*(-(s14*(5*s23+2*s24))+s14*s14+s23*s23))+2*s13*s14*(s24*s24)-2*s13*s23*(s24*s24)-2*s14*s23*(s24*s24)+2*(s14*s14)*(s24*s24)-4*s14*pow(s12,3)+6*s23*pow(s14,3)+2*(s12+s24)*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(-3*s14*(s12+s13+s14)*(s12+s14+s24)*(s12+s13+s14+s23+s24)+(pow(s12,2)+s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24))*(pow(s12,2)+s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)))*pow(s12+s14+s24,-3)-(s12*(3*s13+2*s14-s24)+(3*s13-2*s23-s24)*s24+s14*(-3*s23+4*s24))*(s13*s13*(2*s14*s24+2*(s14*s14)+s24*s24)+2*s13*s14*(s14*(s23+3*s24)+2*(s14*s14)+s24*s24)+s14*s14*(2*s23*s24+2*s14*(s23+2*s24)+2*(s14*s14)+s23*s23+2*(s24*s24))+s12*s12*(6*s14*(s23+s24)+2*s13*(3*s14+s23+2*s24)+s13*s13+7*(s14*s14)+(s23+s24)*(s23+s24))+2*s12*(s13*(2*s14+s24)*(2*s14+s23+s24)+(s14+s24)*(s13*s13)+s14*(s14*(3*s23+4*s24)+3*(s14*s14)+(s23+s24)*(s23+s24)))+2*(s13+2*s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s12+s14+s24,-2)+(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(-5*s14*s23*s24-s13*(s14*(5*s23-4*s24)+s24*(3*s23+s24))+(s14+4*s24)*(s12*s12)+s24*(s13*s13)+s12*(s13*(3*s14-s23-s24)+s24*(s23+2*s24)-s14*(3*s23+2*s24)+s13*s13-s14*s14)-6*s23*(s14*s14)+2*s24*(s14*s14)-s14*(s23*s23)+s24*(s23*s23)-2*s14*(s24*s24)+s23*(s24*s24)+2*pow(s12,3))*pow(s12+s14+s24,-1))-2*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(s12*(s13+s14-s23-s24)+(s13+s14)*(s13+s14))*pow(s13,-1)*pow((-s12-s13-s14),-1)*pow((-s12-s13-s14-s23-s24),-1)+(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(4*s14*s23+s12*(2*s13+3*s14+2*s23)+2*s14*s24+2*s23*s24+s13*(s14+3*s23+s24)+s14*s14+2*(s23*s23))*pow(s14,-1)*pow((-s12-s13-s14),-1)*pow((-s12-s13-s14-s23-s24),-1)-2*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*pow(s13,-1)*pow(s14,-1)*pow((-s12-s13-s14),-1)*pow(s24,-1)*(3*s14*s23*(s13*s13)+s12*(s14*s24*(-s14+s23+s24)+s13*(s14*(s23-s24)-s23*(s23+s24))+s23*(s13*s13))+3*s13*s23*(s14*s14)+s23*pow(s13,3)+s14*(s23*(s14*s14)+pow(s23+s24,3)))*pow((-s12-s13-s14-s23-s24),-1))/8.;
}

// Coefficient order epsilon^1 of master 13
template<>
double qq2yygCA<13,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (8*pow(s13,-1)*pow(s14,-1)*(s13*s13*(s14*s24*(2*s23+9*s24)+2*(2*s23+5*s24)*(s14*s14)+(2*s23+s24)*(s24*s24))+(s13*(10*s14+s23+s24)+s13*s13-2*(2*s23*s24+s14*s14-2*(s23*s23)+3*(s24*s24)))*pow(s12,3)+(s13-2*s24)*pow(s12,4)-2*s14*s23*(-(s24*(s14*s14))+(s23+s24)*(s24*s24)-s14*(2*(s23*s23)+s24*s24)+pow(s14,3))+s13*(s24*(13*s23+18*s24)*(s14*s14)-2*(s24*s24)*(2*s23*s24+3*(s23*s23)+s24*s24)+s14*s24*(s23*s24-14*(s23*s23)+2*(s24*s24))+2*(2*s23+7*s24)*pow(s14,3))+s12*s12*((9*s14+2*s23+3*s24)*(s13*s13)+s13*(-(s24*(2*s23+3*s24))+s14*(4*s23+22*s24)+17*(s14*s14))-2*(s14*s23*(-4*s23+s24)+(s23-s24)*(s14*s14)-(2*s23-3*s24)*((s23+s24)*(s23+s24))+2*pow(s14,3)))+s12*(s13*s13*(s24*(4*s23+3*s24)+2*s14*(s23+9*s24)+10*(s14*s14))+s13*(s14*s24*(5*s23+14*s24)+7*(s23+5*s24)*(s14*s14)-s24*(7*s23*s24+6*(s23*s23)+5*(s24*s24))+8*pow(s14,3))-2*(-(s14*s23*(s23*s24+4*(s23*s23)-2*(s24*s24)))-2*(s14*s14)*(s23*s24+s23*s23+s24*s24)+s24*s24*((s23+s24)*(s23+s24))+2*s23*pow(s14,3)+pow(s14,4))))*pow(s12+s14+s24,-2)-8*pow(s13,-1)*(s24*(4*s14+5*s24)*(s13*s13)+43*s23*s24*(s14*s14)+15*s14*s24*(s23*s23)+10*(s14*s14)*(s23*s23)+47*s14*s23*(s24*s24)+16*(s14*s14)*(s24*s24)+6*(s23*s23)*(s24*s24)+s12*s12*(50*s23*s24+6*s14*(7*s23+8*s24)+s13*(15*s14+s23+11*s24)+5*(s13*s13)+34*(s14*s14)+10*(s23*s23)+24*(s24*s24))+(5*s13+22*s14+16*s23+19*s24)*pow(s12,3)+6*pow(s12,4)+10*s23*pow(s14,3)+18*s24*pow(s14,3)+s13*(3*s14*s24*(s23+5*s24)+2*(s23+9*s24)*(s14*s14)+(3*s23+s24)*(s24*s24)+4*pow(s14,3))+s12*(2*(2*s14+5*s24)*(s13*s13)+(36*s23+47*s24)*(s14*s14)+s13*(s24*(4*s23+7*s24)+3*s14*(s23+10*s24)+14*(s14*s14))+3*s24*(16*s23*s24+5*(s23*s23)+5*(s24*s24))+s14*(93*s23*s24+20*(s23*s23)+36*(s24*s24))+26*pow(s14,3))+8*pow(s14,4)+10*s14*pow(s24,3)+14*s23*pow(s24,3)+4*pow(s24,4))*pow(s12+s14+s24,-2)-8*pow(s14,-1)*pow(s12+s13+s14,-1)*(-(s13*s13*(s14*s24*(4*s23+s24)+4*s23*(s14*s14)+(2*s23+s24)*(s24*s24)))-(s14*(7*s23-6*s24)-2*s23*s24+s13*(4*s14+s23+2*s24)+s13*s13+2*(s14*s14)+6*(s23*s23)-9*(s24*s24))*pow(s12,3)+(-2*s13-3*s14-3*s23+s24)*pow(s12,4)-pow(s12,5)+s13*(s24*(5*s23+9*s24)*(s14*s14)+2*(s24*s24)*(4*s23*s24+3*(s23*s23)+2*(s24*s24))+s14*s24*(15*s23*s24+10*(s23*s23)+8*(s24*s24))+(-2*s23+5*s24)*pow(s14,3))+s12*s12*(-((s14+2*s23+3*s24)*(s13*s13))+(-4*s23+15*s24)*(s14*s14)-4*s24*(s23*s23)+13*s23*(s24*s24)+s13*(-2*s14*s23+6*s23*s24-s14*s14+2*(s23*s23)+6*(s24*s24))+s14*(10*s23*s24-12*(s23*s23)+25*(s24*s24))+pow(s14,3)-4*pow(s23,3)+11*pow(s24,3))+s12*(-((2*s14*(2*s23+s24)+s24*(4*s23+3*s24))*(s13*s13))+s14*s14*(19*s23*s24-6*(s23*s23)+29*(s24*s24))+(s23+14*s24)*pow(s14,3)+s13*((-3*s23+8*s24)*(s14*s14)+s24*(15*s23*s24+8*(s23*s23)+10*(s24*s24))+s14*(13*s23*s24+2*(s23*s23)+12*(s24*s24))+pow(s14,3))+pow(s14,4)+2*s24*(s24*(s23*s23)+4*s23*(s24*s24)-pow(s23,3)+2*pow(s24,3))+s14*(-4*s24*(s23*s23)+21*s23*(s24*s24)-8*pow(s23,3)+20*pow(s24,3)))+s14*(s24*(11*s23+12*s24)*(s14*s14)+(s23+4*s24)*pow(s14,3)-2*s24*pow(s23,3)+4*s23*pow(s24,3)+s14*(14*s23*(s24*s24)-4*pow(s23,3)+12*pow(s24,3))+4*pow(s24,4)))*pow(s12+s14+s24,-2)-8*pow(s13,-1)*pow(s24,-1)*(63*s24*(s14*s14)*(s23*s23)+99*s23*(s14*s14)*(s24*s24)+74*s14*(s23*s23)*(s24*s24)+s13*s13*(3*s14*(3*s23-s24)*s24+4*s23*(s14*s14)+(5*s23-2*s24)*(s24*s24))+(93*s23*s24+s13*(11*s14+10*s23+3*s24)+s14*(92*s23+94*s24)+2*(s13*s13)+56*(s14*s14)+12*(s23*s23)+35*(s24*s24))*pow(s12,3)+(2*s13+28*s14+23*s23+19*s24)*pow(s12,4)+4*pow(s12,5)+68*s23*s24*pow(s14,3)+18*(s23*s23)*pow(s14,3)+30*(s24*s24)*pow(s14,3)+s13*((6*s23*s24+7*(s23*s23)-s24*s24)*(s24*s24)+s14*s24*(31*s23*s24+6*(s23*s23)+7*(s24*s24))+s14*s14*(49*s23*s24-s23*s23+20*(s24*s24))+2*(11*s23+6*s24)*pow(s14,3))+20*s23*pow(s14,4)+12*s24*pow(s14,4)+9*s14*s24*pow(s23,3)+s14*s14*pow(s23,3)+8*(s24*s24)*pow(s23,3)+77*s14*s23*pow(s24,3)+28*(s14*s14)*pow(s24,3)+30*(s23*s23)*pow(s24,3)+s12*s12*((s14+5*s23+2*s24)*(s13*s13)+135*(s23+s24)*(s14*s14)+s13*((26*s23-s24)*s24+s14*(40*s23+31*s24)+21*(s14*s14))+57*s24*(s23*s23)+141*s23*(s24*s24)+s14*(257*s23*s24+42*(s23*s23)+114*(s24*s24))+44*pow(s14,3)+pow(s23,3)+33*pow(s24,3))+s12*((s14*(9*s23-2*s24)+2*(5*s23-s24)*s24)*(s13*s13)+s14*s14*(232*s23*s24+48*(s23*s23)+105*(s24*s24))+(86*s23+72*s24)*pow(s14,3)+s13*((52*s23+45*s24)*(s14*s14)+s24*(22*s23*s24+7*(s23*s23)-3*(s24*s24))+s14*(71*s23*s24-s23*s23+27*(s24*s24))+12*pow(s14,3))+12*pow(s14,4)+s24*(75*s24*(s23*s23)+97*s23*(s24*s24)+9*pow(s23,3)+17*pow(s24,3))+2*s14*(60*s24*(s23*s23)+120*s23*(s24*s24)+pow(s23,3)+31*pow(s24,3)))+14*s14*pow(s24,4)+26*s23*pow(s24,4)+4*pow(s24,5))*pow(s12+s14+s24,-2)-8*pow(s12+s13+s14,-1)*(-7*s14*s23*s24+(6*s13+11*s14+3*s23+4*s24)*(s12*s12)+(2*s14-s24)*(s13*s13)-2*s23*(s14*s14)+12*s24*(s14*s14)+s13*(s24*(-4*s23+5*s24)+s14*(-3*s23+10*s24)+10*(s14*s14))+2*s14*(s23*s23)+4*s24*(s23*s23)+s12*(3*s23*s24+s13*(13*s14-3*s23+11*s24)+s14*(s23+13*s24)-s13*s13+16*(s14*s14)+2*(s23*s23)-3*(s24*s24))+3*pow(s12,3)+8*pow(s14,3)-4*pow(s24,3))*pow(s12+s14+s24,-1)+pow(s13,-1)*pow(s14,-1)*pow(s24,-1)*(-112*s13*s14*s23*s24-16*s14*s23*(s13*s13)-32*s23*s24*(s13*s13)-64*s13*s23*(s14*s14)+48*s13*s24*(s14*s14)+32*s23*s24*(s14*s14)+16*(s12*s12)*(s13*(s14-7*s23)-12*s23*(s23+s24)+s14*(-2*s23+9*s24)+5*(s14*s14))-16*s13*s14*(s23*s23)-48*s13*s24*(s23*s23)-32*s14*s24*(s23*s23)+16*(s14*s14)*(s23*s23)+48*s13*s14*(s24*s24)-80*s13*s23*(s24*s24)-32*s14*s23*(s24*s24)+128*(s14*s14)*(s24*s24)+64*s14*pow(s12,3)-96*s23*pow(s12,3)-48*s23*pow(s14,3)+48*s24*pow(s14,3)+16*s12*((s14-2*s23)*(s13*s13)+12*s24*(s14*s14)+s13*(s14*(-8*s23+3*s24)-3*s23*(s23+4*s24)+2*(s14*s14))+s14*(-5*s23*s24-2*(s23*s23)+7*(s24*s24))-6*s23*((s23+s24)*(s23+s24))+pow(s14,3))+32*s14*pow(s24,3)+(8*(s12*(3*s13+2*s14-s24)+(3*s13-2*s23-s24)*s24+s14*(-3*s23+4*s24))*((pow(s12,2)+s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24))*(pow(s12,2)+s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)))+8*(s14*(3*s23+s24)+s12*(3*s14+10*s23+6*s24)+4*(s12*s12)+2*(5*s23*s24+3*(s23*s23)+s24*s24))*(s13*s13*(2*s14*s24+2*(s14*s14)+s24*s24)+2*s13*s14*(s14*(s23+3*s24)+2*(s14*s14)+s24*s24)+s14*s14*(2*s23*s24+2*s14*(s23+2*s24)+2*(s14*s14)+s23*s23+2*(s24*s24))+s12*s12*(6*s14*(s23+s24)+2*s13*(3*s14+s23+2*s24)+s13*s13+7*(s14*s14)+(s23+s24)*(s23+s24))+2*s12*(s13*(2*s14+s24)*(2*s14+s23+s24)+(s14+s24)*(s13*s13)+s14*(s14*(3*s23+4*s24)+3*(s14*s14)+(s23+s24)*(s23+s24)))+2*(s13+2*s14+s23+s24)*pow(s12,3)+pow(s12,4)))*pow(s12+s14+s24,-2)-8*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(8*s14*s23*s24+(4*s13+9*s14+4*s23+9*s24)*(s12*s12)+2*s24*(s13*s13)-6*s23*(s14*s14)+8*s24*(s14*s14)+7*s14*(s23*s23)+4*s24*(s23*s23)+8*s14*(s24*s24)-2*s23*(s24*s24)+s13*(5*s23*s24+s14*(-4*s23+7*s24)+6*(s23*s23)+s24*s24)+s12*(s23*s24+s13*(6*s14+8*s23+5*s24)+2*s14*(5*s23+8*s24)+2*(s13*s13)+4*(s14*s14)+10*(s23*s23)+7*(s24*s24))+4*pow(s12,3)+6*pow(s23,3)+2*pow(s24,3))*pow(s12+s14+s24,-1)-8*(s12+s24)*pow(s12+s14+s24,-3)*pow(pow(s12,2)+s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24),3))+8*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(s13*(5*s14-s23+s24)+s12*(7*s14+5*s23+7*s24)+3*(s12*s12)+s13*s13+2*(3*s23*s24+s14*(3*s23+4*s24)+2*(s14*s14)+s23*s23+2*(s24*s24)))*pow(s14,-1)*pow((-s12-s13-s14),-1)*pow((-s12-s13-s14-s23-s24),-1)+8*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(2*(s14+s23)*(s12*s12)+2*s14*(s13*s13)+s12*(7*s14*s23+2*s13*(2*s14+s23)+2*s23*(s23+s24)+6*(s14*s14))+s13*(s14*(4*s23-s24)+2*s23*(s23+s24)+6*(s14*s14))+s14*(7*s23*s24+s14*(6*s23+s24)+4*(s14*s14)+5*(s23*s23)+2*(s24*s24)))*pow(s13,-1)*pow(s14,-1)*pow((-s12-s13-s14),-1)*pow((-s12-s13-s14-s23-s24),-1)+8*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*pow(s13,-1)*pow(s14,-1)*pow((-s12-s13-s14),-1)*pow(s24,-1)*((s23*(3*s23+5*s24)+s14*(13*s23+6*s24))*(s13*s13)+4*(s12*s12)*(3*s13*(s14+s23)+3*s23*(s23+s24)+s14*(7*s23+3*s24)+3*(s14*s14))+s13*(3*(7*s23+4*s24)*(s14*s14)+s14*(24*s23*s24+16*(s23*s23)+5*(s24*s24))+6*s23*((s23+s24)*(s23+s24)))+6*(s14+s23)*pow(s12,3)+2*s23*pow(s13,3)+s12*((6*s14+8*s23)*(s13*s13)+14*(2*s23+s24)*(s14*s14)+s13*(2*s23*(7*s23+8*s24)+s14*(37*s23+16*s24)+12*(s14*s14))+s14*(37*s23*s24+28*(s23*s23)+8*(s24*s24))+6*s23*((s23+s24)*(s23+s24))+6*pow(s14,3))+s14*(2*(5*s23+3*s24)*(s14*s14)+21*s24*(s23*s23)+13*s23*(s24*s24)+s14*(16*s23*s24+12*(s23*s23)+3*(s24*s24))+10*pow(s23,3)+2*pow(s24,3)))*pow((-s12-s13-s14-s23-s24),-1))/64.;
}

// Coefficient order epsilon^2 of master 13
template<>
double qq2yygCA<13,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s14,-1)*pow(s24,-1)*((s13*(61*s14+44*s23+90*s24)+2*s14*(55*s23+94*s24)+8*(s13*s13)+73*(s14*s14)+2*(54*s23*s24+17*(s23*s23)+56*(s24*s24)))*pow(s12,5)+(18*s13+43*s14+28*s23+52*s24)*pow(s12,6)+10*pow(s12,7)+s14*(-s14+s24)*(s24*s24)*pow(s13,3)+s24*(s13*s13)*(s14*s14*(2*s23*s24-8*(s23*s23)+15*(s24*s24))+s14*s24*(15*s23*s24-2*(s23*s23)+15*(s24*s24))+6*(s24*s24)*((s23+s24)*(s23+s24))+(5*s23+6*s24)*pow(s14,3))+s14*s23*((15*s23*s24+3*(s23*s23)+20*(s24*s24))*pow(s14,3)+(3*s23+4*s24)*pow(s14,4)+2*(s24*s24)*(-7*s24*(s23*s23)-2*s23*(s24*s24)-4*pow(s23,3)+2*pow(s24,3))+s14*s24*(-11*s24*(s23*s23)+4*s23*(s24*s24)-2*pow(s23,3)+20*pow(s24,3))+s14*s14*(8*s24*(s23*s23)+20*s23*(s24*s24)+6*pow(s23,3)+32*pow(s24,3)))+s13*((19*s23*s24+8*(s23*s23)+12*(s24*s24))*pow(s14,4)+2*pow(s24,3)*(-6*s24*(s23*s23)+s23*(s24*s24)-4*pow(s23,3)+2*pow(s24,3))+s14*(s24*s24)*(-67*s24*(s23*s23)+12*s23*(s24*s24)-52*pow(s23,3)+20*pow(s24,3))+pow(s14,3)*(4*s24*(s23*s23)+52*s23*(s24*s24)-8*pow(s23,3)+36*pow(s24,3))+s24*(s14*s14)*(-65*s24*(s23*s23)+43*s23*(s24*s24)-52*pow(s23,3)+40*pow(s24,3)))+pow(s12,4)*((21*s14+12*s23+38*s24)*(s13*s13)+(169*s23+259*s24)*(s14*s14)+s14*(351*s23*s24+118*(s23*s23)+330*(s24*s24))+s13*(s14*(127*s23+251*s24)+78*(s14*s14)+2*(87*s23*s24+18*(s23*s23)+92*(s24*s24)))+61*pow(s14,3)+2*(41*s24*(s23*s23)+78*s23*(s24*s24)+11*pow(s23,3)+64*pow(s24,3)))+pow(s12,3)*(44*(s23*s23)*(s24*s24)+s14*s14*(431*s23*s24+153*(s23*s23)+351*(s24*s24))+3*(s13*s13)*(s14*(5*s23+26*s24)+6*(s14*s14)+2*(8*s23*s24+s23*s23+12*(s24*s24)))+s14*pow(s13,3)+(127*s23+166*s24)*pow(s14,3)+25*pow(s14,4)+22*s24*pow(s23,3)+6*pow(s23,4)+100*s23*pow(s24,3)+s14*(226*s24*(s23*s23)+408*s23*(s24*s24)+69*pow(s23,3)+292*pow(s24,3))+s13*((141*s23+260*s24)*(s14*s14)+s14*(378*s23*s24+75*(s23*s23)+407*(s24*s24))+45*pow(s14,3)+4*(24*s24*(s23*s23)+65*s23*(s24*s24)+3*pow(s23,3)+49*pow(s24,3)))+82*pow(s24,4))+s12*s12*(-(s14*(s14-3*s24)*pow(s13,3))+(245*s23*s24+91*(s23*s23)+157*(s24*s24))*pow(s14,3)+s13*s13*((6*s23+51*s24)*(s14*s14)+2*s24*(36*s23*s24+9*(s23*s23)+34*(s24*s24))+s14*(45*s23*s24-2*(s23*s23)+108*(s24*s24))+5*pow(s14,3))+47*(s23+s24)*pow(s14,4)+4*pow(s14,5)+s14*s14*(221*s24*(s23*s23)+385*s23*(s24*s24)+75*pow(s23,3)+217*pow(s24,3))+s13*(s14*s14*(307*s23*s24+50*(s23*s23)+326*(s24*s24))+11*(7*s23+11*s24)*pow(s14,3)+10*pow(s14,4)+2*s24*(36*s24*(s23*s23)+88*s23*(s24*s24)+8*pow(s23,3)+57*pow(s24,3))+s14*(84*s24*(s23*s23)+387*s23*(s24*s24)+16*pow(s23,3)+325*pow(s24,3)))-2*s24*(11*(s23*s23)*(s24*s24)+11*s24*pow(s23,3)+pow(s23,4)-12*s23*pow(s24,3)-14*pow(s24,4))+s14*(95*(s23*s23)*(s24*s24)+52*s24*pow(s23,3)+18*pow(s23,4)+207*s23*pow(s24,3)+131*pow(s24,4)))+s12*(s14*s24*(-2*s14+3*s24)*pow(s13,3)+s13*s13*(-8*(s14*s14)*(-(s23*s24)+s23*s23-6*(s24*s24))+s14*s24*(45*s23*s24-4*(s23*s23)+66*(s24*s24))+2*(s24*s24)*((3*s23+4*s24)*(3*s23+4*s24))+(3*s23+11*s24)*pow(s14,3))+(61*s23*s24+25*(s23*s23)+24*(s24*s24))*pow(s14,4)+(7*s23+4*s24)*pow(s14,5)+pow(s14,3)*(92*s24*(s23*s23)+153*s23*(s24*s24)+31*pow(s23,3)+52*pow(s24,3))+2*(s24*s24)*(-9*(s23*s23)*(s24*s24)-11*s24*pow(s23,3)-4*pow(s23,4)+2*pow(s24,4))+s14*s24*(-17*(s23*s23)*(s24*s24)-33*s24*pow(s23,3)-4*pow(s23,4)+44*s23*pow(s24,3)+24*pow(s24,4))+s14*s14*(71*(s23*s23)*(s24*s24)+38*s24*pow(s23,3)+18*pow(s23,4)+143*s23*pow(s24,3)+52*pow(s24,4))+s13*((122*s23*s24+19*(s23*s23)+112*(s24*s24))*pow(s14,3)+(19*s23+22*s24)*pow(s14,4)-4*(s24*s24)*pow(s23,3)+2*s14*s24*(-29*s24*(s23*s23)+74*s23*(s24*s24)-18*pow(s23,3)+64*pow(s24,3))+s14*s14*(-8*s24*(s23*s23)+209*s23*(s24*s24)-4*pow(s23,3)+184*pow(s24,3))+48*s23*pow(s24,4)+34*pow(s24,5))))*pow(s12+s14+s24,-3)-pow(s13,-1)*pow(s14,-1)*(s13*s13*(s14*s24*(2*s23+9*s24)+(4*s23+7*s24)*(s14*s14)-2*(s23-2*s24)*(s24*s24))+(s14*(25*s23+52*s24)+s13*(27*s14+8*(s23+4*s24))+4*(s13*s13)+24*(s14*s14)-6*(-2*s23*s24+s23*s23-4*(s24*s24)))*pow(s12,3)+2*(5*s13+10*s14+3*s23+10*s24)*pow(s12,4)+6*pow(s12,5)+s14*s23*(5*(s23+2*s24)*(s14*s14)+2*s24*(-(s23*s24)-3*(s23*s23)+s24*s24)+s14*(5*s23*s24-6*(s23*s23)+10*(s24*s24))+2*pow(s14,3))+s13*(2*(s24*s24)*(2*s23*s24-2*(s23*s23)+s24*s24)+s14*s24*(11*s23*s24-8*(s23*s23)+10*(s24*s24))+s14*s14*(20*s23*s24-4*(s23*s23)+18*(s24*s24))+(9*s23+10*s24)*pow(s14,3))+s12*s12*((9*s14-2*s23+12*s24)*(s13*s13)+(34*s23+44*s24)*(s14*s14)-14*s24*(s23*s23)+6*s23*(s24*s24)+s13*(20*s23*s24+s14*(26*s23+64*s24)+26*(s14*s14)-6*(s23*s23)+36*(s24*s24))+s14*(46*s23*s24-7*(s23*s23)+44*(s24*s24))+12*pow(s14,3)-6*pow(s23,3)+12*pow(s24,3))+s12*(s13*s13*(4*s24*(-s23+3*s24)+2*s14*(s23+9*s24)+7*(s14*s14))+4*(s14*s14)*(11*s23*s24+s23*s23+5*(s24*s24))+(17*s23+12*s24)*pow(s14,3)+s13*((27*s23+44*s24)*(s14*s14)+2*s24*(8*s23*s24-5*(s23*s23)+8*(s24*s24))+s14*(37*s23*s24-10*(s23*s23)+47*(s24*s24))+9*pow(s14,3))+2*pow(s14,4)+2*s24*(-4*s24*(s23*s23)-3*pow(s23,3)+pow(s24,3))+s14*(-9*s24*(s23*s23)+23*s23*(s24*s24)-12*pow(s23,3)+12*pow(s24,3))))*pow(s12+s14+s24,-2)+pow(s14,-1)*pow(s12+s13+s14,-1)*(s24*(s13*s13)*(4*s14*s24+2*s24*(s23+3*s24)+s14*s14)+(62*s23*s24+s13*(38*s14+26*s23+54*s24)+s14*(77*s23+113*s24)+6*(s13*s13)+55*(s14*s14)+20*(s23*s23)+48*(s24*s24))*pow(s12,3)+2*(8*s13+19*s14+12*s23+18*s24)*pow(s12,4)+10*pow(s12,5)+s13*(2*(s24*s24)*(6*s23*s24+2*(s23*s23)+3*(s24*s24))+s14*s24*(32*s23*s24+8*(s23*s23)+25*(s24*s24))+s14*s14*(37*s23*s24+8*(s23*s23)+30*(s24*s24))+11*(s23+s24)*pow(s14,3))+s12*s12*(2*(2*s14+s23+9*s24)*(s13*s13)+(91*s23+128*s24)*(s14*s14)+32*s24*(s23*s23)+52*s23*(s24*s24)+3*s14*(54*s23*s24+17*(s23*s23)+40*(s24*s24))+s13*(64*s23*s24+s14*(59*s23+101*s24)+32*(s14*s14)+8*(s23*s23)+66*(s24*s24))+36*pow(s14,3)+6*pow(s23,3)+28*pow(s24,3))+s14*(s14*s14*(38*s23*s24+11*(s23*s23)+24*(s24*s24))+(9*s23+8*s24)*pow(s14,3)+2*s24*(8*s24*(s23*s23)+11*s23*(s24*s24)+2*pow(s23,3)+4*pow(s24,3))+3*s14*(10*s24*(s23*s23)+17*s23*(s24*s24)+2*pow(s23,3)+8*pow(s24,3)))+s12*(s13*s13*(8*s14*s24+2*s24*(2*s23+9*s24)+s14*s14)+s14*s14*(138*s23*s24+42*(s23*s23)+97*(s24*s24))+(47*s23+59*s24)*pow(s14,3)+s13*((44*s23+62*s24)*(s14*s14)+2*s24*(25*s23*s24+6*(s23*s23)+17*(s24*s24))+s14*(91*s23*s24+16*(s23*s23)+88*(s24*s24))+10*pow(s14,3))+9*pow(s14,4)+2*s24*(6*s24*(s23*s23)+7*s23*(s24*s24)+2*pow(s23,3)+3*pow(s24,3))+s14*(62*s24*(s23*s23)+107*s23*(s24*s24)+12*pow(s23,3)+53*pow(s24,3))))*pow(s12+s14+s24,-2)-pow(s12+s13+s14,-1)*(18*s23*s24*(s14*s14)+20*s14*s24*(s23*s23)+7*(s14*s14)*(s23*s23)+33*s14*s23*(s24*s24)+40*(s14*s14)*(s24*s24)+12*(s23*s23)*(s24*s24)-s13*s13*(3*s14*s24+2*(s14*s14)+2*(s24*s24))+s12*s12*(s13*(8*s14-7*s23-s24)+40*s23*s24+s14*(23*s23+88*s24)-2*(s13*s13)+54*(s14*s14)+7*(s23*s23)+38*(s24*s24))+(-2*s13+33*s14+11*s23+29*s24)*pow(s12,3)+8*pow(s12,4)+s23*pow(s14,3)+36*s24*pow(s14,3)+s13*(s14*s24*(-11*s23+16*s24)+(-9*s23+23*s24)*(s14*s14)+(-4*s23+3*s24)*(s24*s24)+10*pow(s14,3))+s12*(-((3*s14+4*s24)*(s13*s13))+(13*s23+95*s24)*(s14*s14)+s13*(-8*s14*(2*s23-3*s24)+s24*(-11*s23+4*s24)+20*(s14*s14))+s24*(45*s23*s24+20*(s23*s23)+21*(s24*s24))+s14*(58*s23*s24+14*(s23*s23)+75*(s24*s24))+41*pow(s14,3))+12*pow(s14,4)+20*s14*pow(s24,3)+16*s23*pow(s24,3)+4*pow(s24,4))*pow(s12+s14+s24,-2)+pow(s13,-1)*(s14*s24*(s13*s13)+17*s23*s24*(s14*s14)-19*s14*s24*(s23*s23)-10*(s14*s14)*(s23*s23)+7*s14*s23*(s24*s24)+76*(s14*s14)*(s24*s24)-10*(s23*s23)*(s24*s24)+s12*s12*(9*s23*s24+3*s13*(10*s14+9*s24)+14*s14*(s23+11*s24)+89*(s14*s14)-10*(s23*s23)+63*(s24*s24))+(9*s13+56*s14+3*s23+47*s24)*pow(s12,3)+13*pow(s12,4)+8*s23*pow(s14,3)+58*s24*pow(s14,3)+s13*(s14*s24*(12*s23+29*s24)+(5*s23+28*s24)*(s14*s14)+(5*s23+9*s24)*(s24*s24)+8*pow(s14,3))+s12*(s14*(s13*s13)+(19*s23+165*s24)*(s14*s14)+s13*(s24*(5*s23+27*s24)+s14*(5*s23+59*s24)+29*(s14*s14))+s24*(4*s23*s24-19*(s23*s23)+37*(s24*s24))+s14*(26*s23*s24-20*(s23*s23)+140*(s24*s24))+62*pow(s14,3))+16*pow(s14,4)+42*s14*pow(s24,3)-2*s23*pow(s24,3)+8*pow(s24,4))*pow(s12+s14+s24,-2)-pow(s13,-1)*pow(s24,-1)*(19*s24*(s14*s14)*(s23*s23)+63*s23*(s14*s14)*(s24*s24)+2*s14*(s23*s23)*(s24*s24)+2*(s13*s13)*(5*s14*s23*s24+2*s23*(s14*s14)+(2*s23+s24)*(s24*s24))+(93*s23*s24+s13*(47*s14+23*s23+58*s24)+s14*(83*s23+170*s24)+4*(s13*s13)+81*(s14*s14)+8*(s23*s23)+78*(s24*s24))*pow(s12,3)+(17*s13+52*s14+28*s23+50*s24)*pow(s12,4)+13*pow(s12,5)+22*s23*s24*pow(s14,3)+9*(s23*s23)*pow(s14,3)+50*(s24*s24)*pow(s14,3)+s13*(s24*s24*(24*s23*s24+15*(s23*s23)+11*(s24*s24))+s14*s24*(59*s23*s24+39*(s23*s23)+45*(s24*s24))+s14*s14*(49*s23*s24+20*(s23*s23)+52*(s24*s24))+2*(5*s23+9*s24)*pow(s14,3))+16*s24*pow(s14,4)-25*s14*s24*pow(s23,3)-11*(s14*s14)*pow(s23,3)-16*(s24*s24)*pow(s23,3)+55*s14*s23*pow(s24,3)+56*(s14*s14)*pow(s24,3)-6*(s23*s23)*pow(s24,3)+s12*s12*(2*(2*s23+5*s24)*(s13*s13)+(82*s23+212*s24)*(s14*s14)+13*s24*(s23*s23)+120*s23*(s24*s24)+s13*(66*s23*s24+15*s14*(4*s23+9*s24)+46*(s14*s14)+7*(s23*s23)+76*(s24*s24))+s14*(213*s23*s24+25*(s23*s23)+214*(s24*s24))+58*pow(s14,3)-11*pow(s23,3)+62*pow(s24,3))+s12*(2*(5*s14*s23+4*s24*(s23+s24))*(s13*s13)+s14*s14*(142*s23*s24+26*(s23*s23)+189*(s24*s24))+27*(s23+4*s24)*pow(s14,3)+s13*((47*s23+98*s24)*(s14*s14)+s24*(67*s23*s24+22*(s23*s23)+46*(s24*s24))+s14*(111*s23*s24+27*(s23*s23)+133*(s24*s24))+16*pow(s14,3))+16*pow(s14,4)+s24*(s24*(s23*s23)+69*s23*(s24*s24)-25*pow(s23,3)+25*pow(s24,3))+s14*(32*s24*(s23*s23)+189*s23*(s24*s24)-22*pow(s23,3)+122*pow(s24,3)))+26*s14*pow(s24,4)+14*s23*pow(s24,4)+4*pow(s24,5))*pow(s12+s14+s24,-2)-(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(2*s14*s23+10*s23*s24+s13*(-5*s14+5*s23+3*s24)+s12*(3*s13+10*s23+8*s24)+4*(s12*s12)+s13*s13-6*(s14*s14)+6*(s23*s23)+4*(s24*s24))*pow(s14,-1)*pow((-s12-s13-s14),-1)*pow((-s12-s13-s14-s23-s24),-1)-(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(s12*(s13*(13*s14-s23)+s14*(17*s14+4*s23+11*s24))+9*s14*(s12*s12)+(4*s14-s23)*(s13*s13)+s13*(s23*(s23+s24)+s14*(s23+9*s24)+12*(s14*s14))+s14*(-(s23*s24)+s14*(6*s23+13*s24)+8*(s14*s14)-5*(s23*s23)+4*(s24*s24)))*pow(s13,-1)*pow(s14,-1)*pow((-s12-s13-s14),-1)*pow((-s12-s13-s14-s23-s24),-1)+(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*pow(s13,-1)*pow(s14,-1)*((s23*(9*s23+8*s24)+s14*(7*s23+9*s24))*(s13*s13)+s12*s12*(s13*(16*s14+15*s23)+16*s23*(s23+s24)+s14*(22*s23+15*s24)+16*(s14*s14))+s14*(8*s24*(s14*s14)+s24*(7*s23*s24+5*(s23*s23)+2*(s24*s24))+s14*(14*s23*s24+4*(s23*s23)+9*(s24*s24)))+s13*((5*s23+17*s24)*(s14*s14)+2*s14*(11*s23*s24+7*(s23*s23)+4*(s24*s24))+s23*(17*s23*s24+8*(s23*s23)+9*(s24*s24)))+8*(s14+s23)*pow(s12,3)+2*s23*pow(s13,3)+s12*((8*s14+9*s23)*(s13*s13)+(16*s23+25*s24)*(s14*s14)+s13*(s23*(25*s23+24*s24)+s14*(26*s23+24*s24)+16*(s14*s14))+s14*(26*s23*s24+16*(s23*s23)+9*(s24*s24))+8*s23*((s23+s24)*(s23+s24))+8*pow(s14,3)))*pow((-s12-s13-s14),-1)*pow(s24,-1)*pow((-s12-s13-s14-s23-s24),-1))/8.;
}

// Coefficient of master 13 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCA<13>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCA<13,0>(s12,s13,s14,s23,s24),
        qq2yygCA<13,1>(s12,s13,s14,s23,s24),
        qq2yygCA<13,2>(s12,s13,s14,s23,s24)
    });
}


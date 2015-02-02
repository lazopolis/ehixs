/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 28: box((-s12-s23-s24),(s12+s13+s23),s13)

// Coefficient order epsilon^0 of master 28
template<>
double qq2yygCAm2CF<28,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (8*(s12+s23)*(s12+s23+s24)*(s14*s23+(s13+2*s23)*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s24,-2)-4*(s12+s23+s24)*(s23*(s14*s23+(3*s13+2*s23)*s24)+s12*(s13*(s23+s24)+s23*(2*s14+s23+3*s24))+(s13+s14+2*s23+s24)*(s12*s12)+pow(s12,3))*pow(s13,-1)*pow(s24,-2)+8*s12*(-(s14*s24*(s13*(s23+s24)+s23*(s23+2*s24)))+s23*(s23+s24)*(s14*s14)+s12*(s23*s24*(-s23+s24)+s24*(s13*s13)+(2*s23+s24)*(s14*s14)+s14*(2*(s23*s23)-3*(s24*s24))+s13*(2*s14*(s23+s24)-s24*s24))-s13*s23*(s24*s24)+s12*s12*(-(s23*s24)+s13*(2*s14+2*s23+s24)+s14*(4*s23+s24)+s13*s13+s14*s14+s23*s23+s24*s24)+2*(s13+s14+s23)*pow(s12,3)+pow(s12,4))*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-2)-4*((s13+s23)*((s13-2*s23)*s24+s14*(s23+2*s24))+(2*s13+s14+2*s23-s24)*(s12*s12)+s12*(s23*(s23-3*s24)+2*s14*(s23+s24)+s13*(s14+2*(s23+s24))+s13*s13)+pow(s12,3))*pow(s13,-1)*pow(s24,-1)+pow(s13,-1)*(8*(s13+s23)*(s13+s14+s24)+8*s12*(s13+s14+s23+s24)+8*(s12*s12)+4*s13*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s24,-1))+4*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*(2*s12*(s13+s14)*s23*(s13+2*s23)+4*(s13+s14)*(s13+2*s23)*(s12*s12)+4*(s13+s14)*pow(s12,3)+(s12+s23)*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(3*s12*s13*s24*(s12+s13+s14+s23+s24)+(pow(s12,2)+s14*s23-s13*s24+s12*(s13+s14+s23+s24))*(pow(s12,2)+s14*s23-s13*s24+s12*(s13+s14+s23+s24)))*pow(s24,-3)-(-(s12*(s14-3*s23))+s23*(-s13-s14+s23)+2*(s12*s12))*(2*s12*s14*s23*(s13+s14+s23+s24)+s12*s12*(2*s13*(s14+s23+s24)+2*s14*(2*s23+s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24))+(s14*s23-s13*s24)*(s14*s23-s13*s24)+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s24,-2)+(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(-2*(s14-2*s23)*(s12*s12)-s12*((3*s14-2*s23)*s23+s13*(s14+2*s23)+s13*s13)-(s13+s14)*(s23*s23)+2*pow(s12,3))*pow(s24,-1))+4*s12*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow((-s12-s13-s14-s23-s24),-1)+8*s12*(s13+s14)*(s12+s23)*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow((-s12-s13-s14-s23-s24),-1)+4*s23*(s12+s23+s24)*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s24,-1)*pow((-s12-s13-s14-s23-s24),-1))/16.;
}

// Coefficient order epsilon^1 of master 28
template<>
double qq2yygCAm2CF<28,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s12,-1)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-3)*((6*(-s12-s13-s14-s23-s24)*(s23*s23)+s23*(3*(3*s13+4*s14)*(-s12-s13-s14-s23-s24)+13*s24*(-s12-s13-s14-s23-s24)-6*(s24*s24))+(-s12-s13-s14-s23-s24)*(8*s14*s24+s13*(6*s14+7*s24)+3*(s13*s13)+3*(s14*s14)+3*(s24*s24)))*pow(s12,6)+(3*s13+3*s14+4*s23+3*s24)*(-s12-s13-s14-s23-s24)*pow(s12,7)+(-s12-s13-s14-s23-s24)*pow(s12,8)+2*(s13+s23)*(-((s14+s23+s24)*(s14*s14)*(s23*s23))-s13*s14*s23*(4*s23*s24+s14*(4*s23+s24)+3*(s23*s23)+s24*s24)+s13*s13*(s14*(2*s23*s24-3*(s23*s23)+2*(s24*s24))+s24*(5*s23*s24+3*(s23*s23)+2*(s24*s24)))+s24*(3*s23+2*s24)*pow(s13,3))*pow(s24,3)+pow(s12,5)*((3*s14+6*s23+5*s24)*(-s12-s13-s14-s23-s24)*(s13*s13)+(12*s23+7*s24)*(-s12-s13-s14-s23-s24)*(s14*s14)+21*s24*(-s12-s13-s14-s23-s24)*(s23*s23)+10*s23*(-s12-s13-s14-s23-s24)*(s24*s24)-14*(s23*s23)*(s24*s24)+s14*(-16*s23*s24*(s24-2*(-s12-s13-s14-s23-s24))+18*(-s12-s13-s14-s23-s24)*(s23*s23)+(2*s24-(-s12-s13-s14-s23-s24))*(s24*s24))+(-s12-s13-s14-s23-s24)*pow(s13,3)+(-s12-s13-s14-s23-s24)*pow(s14,3)+4*(-s12-s13-s14-s23-s24)*pow(s23,3)+s13*(12*s14*s24*(-s12-s13-s14-s23-s24)+3*(-s12-s13-s14-s23-s24)*(s14*s14)+9*(-s12-s13-s14-s23-s24)*(s23*s23)-3*(-s12-s13-s14-s23-s24)*(s24*s24)-3*s23*(-6*s14*(-s12-s13-s14-s23-s24)-7*s24*(-s12-s13-s14-s23-s24)+6*(s24*s24))-2*pow(s24,3))-12*s23*pow(s24,3)-7*(-s12-s13-s14-s23-s24)*pow(s24,3))-s12*(s24*s24*(2*s14*s23*(s23+8*s24)+s23*s24*(-6*s24+7*(-s12-s13-s14-s23-s24))+4*(3*s24-(-s12-s13-s14-s23-s24))*(s23*s23)+2*(-2*s24+5*(-s12-s13-s14-s23-s24))*(s24*s24))*pow(s13,3)+2*(2*s23-s24)*pow(s13,4)*pow(s24,3)+s14*(s23*s23)*(2*(s24*s24)*(s23*s24*(2*s24-3*(-s12-s13-s14-s23-s24))+(s24-5*(-s12-s13-s14-s23-s24))*(s23*s23)+(s24-(-s12-s13-s14-s23-s24))*(s24*s24))+2*s14*s24*(s23*s24*(7*s24+4*(-s12-s13-s14-s23-s24))+(s24-2*(-s12-s13-s14-s23-s24))*(s23*s23)+(3*s24+(-s12-s13-s14-s23-s24))*(s24*s24))+s14*s14*(2*s23*s24*(2*s24-(-s12-s13-s14-s23-s24))-(-s12-s13-s14-s23-s24)*(s23*s23)+6*pow(s24,3)))+s13*s13*(s24*s24)*(2*(s14*s14)*(8*s23*s24+2*(s23*s23)+s24*s24)+s14*(2*s23*s24*(9*s24+4*(-s12-s13-s14-s23-s24))+(50*s24+(-s12-s13-s14-s23-s24))*(s23*s23)+2*(2*s24+5*(-s12-s13-s14-s23-s24))*(s24*s24)+6*pow(s23,3))+2*(6*s24*(s24+(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(s24+14*(-s12-s13-s14-s23-s24))*(s24*s24)+(8*s24-2*(-s12-s13-s14-s23-s24))*pow(s23,3)+7*(-s12-s13-s14-s23-s24)*pow(s24,3)))+s13*s23*s24*(s14*s14*(4*s23*s24*(9*s24+2*(-s12-s13-s14-s23-s24))+(10*s24+(-s12-s13-s14-s23-s24))*(s23*s23)+2*(4*s24+(-s12-s13-s14-s23-s24))*(s24*s24))+2*s24*(s23+2*s24)*pow(s14,3)+2*s14*(2*s24*(10*s24+(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(19*s24+4*(-s12-s13-s14-s23-s24))*(s24*s24)+(s24-2*(-s12-s13-s14-s23-s24))*pow(s23,3)+2*(3*s24+2*(-s12-s13-s14-s23-s24))*pow(s24,3))+2*s24*(s24*(8*s24+3*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(7*s24+11*(-s12-s13-s14-s23-s24))*(s24*s24)+(3*s24-4*(-s12-s13-s14-s23-s24))*pow(s23,3)+(2*s24+7*(-s12-s13-s14-s23-s24))*pow(s24,3))))+pow(s12,4)*(s13*s13*(3*(-s12-s13-s14-s23-s24)*(s23*s23)+s23*(6*s14*(-s12-s13-s14-s23-s24)+7*s24*(-s12-s13-s14-s23-s24)-20*(s24*s24))+s24*(4*s14*(-s12-s13-s14-s23-s24)-11*s24*(-s12-s13-s14-s23-s24)-14*(s24*s24)))+21*(-s12-s13-s14-s23-s24)*(s23*s23)*(s24*s24)+s14*s14*(s23*s24*(-14*s24+25*(-s12-s13-s14-s23-s24))+18*(-s12-s13-s14-s23-s24)*(s23*s23)-4*(-s12-s13-s14-s23-s24)*(s24*s24))+(s23+s24)*(-s12-s13-s14-s23-s24)*pow(s13,3)+2*(2*s23+s24)*(-s12-s13-s14-s23-s24)*pow(s14,3)+15*s24*(-s12-s13-s14-s23-s24)*pow(s23,3)-10*(s24*s24)*pow(s23,3)+(-s12-s13-s14-s23-s24)*pow(s23,4)-13*s23*(-s12-s13-s14-s23-s24)*pow(s24,3)-20*(s23*s23)*pow(s24,3)+2*s14*(-6*s24*(3*s24-4*(-s12-s13-s14-s23-s24))*(s23*s23)-s23*(10*s24+3*(-s12-s13-s14-s23-s24))*(s24*s24)+6*(-s12-s13-s14-s23-s24)*pow(s23,3)+(s24-6*(-s12-s13-s14-s23-s24))*pow(s24,3))+s13*((9*s23+5*s24)*(-s12-s13-s14-s23-s24)*(s14*s14)+s24*(-34*s24+25*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(-40*s24+(-s12-s13-s14-s23-s24))*(s24*s24)+2*s14*(s23*s24*(-17*s24+15*(-s12-s13-s14-s23-s24))+9*(-s12-s13-s14-s23-s24)*(s23*s23)-(5*s24+7*(-s12-s13-s14-s23-s24))*(s24*s24))+3*(-s12-s13-s14-s23-s24)*pow(s23,3)-(10*s24+29*(-s12-s13-s14-s23-s24))*pow(s24,3))-6*s23*pow(s24,4)-14*(-s12-s13-s14-s23-s24)*pow(s24,4))-pow(s12,3)*(s13*s13*(s14*(2*s23*s24*(12*s24-(-s12-s13-s14-s23-s24))-3*(-s12-s13-s14-s23-s24)*(s23*s23)+3*(10*s24+3*(-s12-s13-s14-s23-s24))*(s24*s24))+s24*(2*s23*s24*(32*s24+3*(-s12-s13-s14-s23-s24))+(28*s24-6*(-s12-s13-s14-s23-s24))*(s23*s23)+(30*s24+49*(-s12-s13-s14-s23-s24))*(s24*s24)))+s24*(s23*(10*s24+(-s12-s13-s14-s23-s24))+s24*(18*s24+5*(-s12-s13-s14-s23-s24)))*pow(s13,3)+2*pow(s14,3)*(s23*s24*(2*s24-3*(-s12-s13-s14-s23-s24))-3*(-s12-s13-s14-s23-s24)*(s23*s23)+pow(s24,3))+s14*s14*(3*s24*(10*s24-11*(-s12-s13-s14-s23-s24))*(s23*s23)+2*s23*(7*s24+8*(-s12-s13-s14-s23-s24))*(s24*s24)-12*(-s12-s13-s14-s23-s24)*pow(s23,3)+2*(s24+3*(-s12-s13-s14-s23-s24))*pow(s24,3))+s13*(s14*s14*(9*s23*s24*(2*s24-(-s12-s13-s14-s23-s24))-9*(-s12-s13-s14-s23-s24)*(s23*s23)+2*(7*s24+2*(-s12-s13-s14-s23-s24))*(s24*s24))+s24*(2*s24*(31*s24-9*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(48*s24+47*(-s12-s13-s14-s23-s24))*(s24*s24)+3*(6*s24-5*(-s12-s13-s14-s23-s24))*pow(s23,3)+2*(6*s24+23*(-s12-s13-s14-s23-s24))*pow(s24,3))+s14*(4*s24*(15*s24-7*(-s12-s13-s14-s23-s24))*(s23*s23)+2*s23*(36*s24+19*(-s12-s13-s14-s23-s24))*(s24*s24)-6*(-s12-s13-s14-s23-s24)*pow(s23,3)+2*(12*s24+23*(-s12-s13-s14-s23-s24))*pow(s24,3)))+2*s24*((3*s24+(-s12-s13-s14-s23-s24))*(s23*s23)*(s24*s24)+4*s24*(s24-3*(-s12-s13-s14-s23-s24))*pow(s23,3)+(s24-2*(-s12-s13-s14-s23-s24))*pow(s23,4)+9*s23*(-s12-s13-s14-s23-s24)*pow(s24,3)+3*(-s12-s13-s14-s23-s24)*pow(s24,4))+s14*((40*s24-(-s12-s13-s14-s23-s24))*(s23*s23)*(s24*s24)+8*s24*(3*s24-4*(-s12-s13-s14-s23-s24))*pow(s23,3)-3*(-s12-s13-s14-s23-s24)*pow(s23,4)+8*s23*(s24+2*(-s12-s13-s14-s23-s24))*pow(s24,3)+8*(-s12-s13-s14-s23-s24)*pow(s24,4)))-s12*s12*((2*s14*(3*s23+7*s24)+s23*(36*s24+(-s12-s13-s14-s23-s24))+s24*(10*s24+19*(-s12-s13-s14-s23-s24))+8*(s23*s23))*(s24*s24)*pow(s13,3)+2*(s23+3*s24)*(s24*s24)*pow(s13,4)+s24*(s13*s13)*(2*s24*(3*s23+5*s24)*(s14*s14)+s24*(58*s24-11*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(50*s24+49*(-s12-s13-s14-s23-s24))*(s24*s24)+2*s14*(s23*s24*(38*s24+7*(-s12-s13-s14-s23-s24))+(14*s24+(-s12-s13-s14-s23-s24))*(s23*s23)+(9*s24+13*(-s12-s13-s14-s23-s24))*(s24*s24))+(8*s24-4*(-s12-s13-s14-s23-s24))*pow(s23,3)+6*(2*s24+7*(-s12-s13-s14-s23-s24))*pow(s24,3))+s23*(2*(-s12-s13-s14-s23-s24)*(s24*s24)*(-2*s24*(s23*s23)+2*s23*(s24*s24)-5*pow(s23,3)+3*pow(s24,3))+pow(s14,3)*(2*s23*s24*(4*s24-3*(-s12-s13-s14-s23-s24))-4*(-s12-s13-s14-s23-s24)*(s23*s23)+6*pow(s24,3))+2*s14*s24*(2*s24*(5*s24-4*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(7*s24-(-s12-s13-s14-s23-s24))*(s24*s24)+2*(s24-2*(-s12-s13-s14-s23-s24))*pow(s23,3)+(s24+3*(-s12-s13-s14-s23-s24))*pow(s24,3))+s14*s14*(s24*(18*s24-19*(-s12-s13-s14-s23-s24))*(s23*s23)+2*s23*(13*s24+10*(-s12-s13-s14-s23-s24))*(s24*s24)-3*(-s12-s13-s14-s23-s24)*pow(s23,3)+2*(3*s24+4*(-s12-s13-s14-s23-s24))*pow(s24,3)))+s13*(2*(s23+s24)*(s24*s24)*pow(s14,3)+s14*s14*(s24*(28*s24-3*(-s12-s13-s14-s23-s24))*(s23*s23)+6*s23*(7*s24+2*(-s12-s13-s14-s23-s24))*(s24*s24)-3*(-s12-s13-s14-s23-s24)*pow(s23,3)+2*(2*s24+3*(-s12-s13-s14-s23-s24))*pow(s24,3))+2*s14*s24*(s24*(47*s24+14*(-s12-s13-s14-s23-s24))*(s23*s23)+5*s23*(6*s24+5*(-s12-s13-s14-s23-s24))*(s24*s24)+7*(2*s24-(-s12-s13-s14-s23-s24))*pow(s23,3)+(4*s24+9*(-s12-s13-s14-s23-s24))*pow(s24,3))+2*s24*(12*(2*s24+(-s12-s13-s14-s23-s24))*(s23*s23)*(s24*s24)+s24*(15*s24-11*(-s12-s13-s14-s23-s24))*pow(s23,3)+(s24-2*(-s12-s13-s14-s23-s24))*pow(s23,4)+s23*(14*s24+29*(-s12-s13-s14-s23-s24))*pow(s24,3)+2*(s24+5*(-s12-s13-s14-s23-s24))*pow(s24,4)))))*pow((-s12-s13-s14-s23-s24),-1))/8.;
}

// Coefficient order epsilon^2 of master 28
template<>
double qq2yygCAm2CF<28,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s12,-1)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-3)*((3*s13+3*s14+4*s23+s24)*(-s12-s13-s14-s23-s24)*pow(s12,7)+(-s12-s13-s14-s23-s24)*pow(s12,8)+pow(s12,6)*((7*s13+3*s14)*s24*(-s12-s13-s14-s23-s24)+3*(-s12-s13-s14-s23-s24)*((s13+s14)*(s13+s14))+6*(-s12-s13-s14-s23-s24)*(s23*s23)+s23*(3*(3*s13+4*s14)*(-s12-s13-s14-s23-s24)+3*s24*(-s12-s13-s14-s23-s24)-4*(s24*s24))+3*(-s12-s13-s14-s23-s24)*(s24*s24)-6*pow(s24,3))+(s13+s23)*(s24*s24)*(s24*(-7*s23*s24+3*(s23*s23)-7*(s24*s24))*pow(s13,3)-3*(s14+s23+s24)*(s14*s14)*pow(s23,3)+s13*s14*s23*(7*s24*(s23*s23)+17*s23*(s24*s24)+s14*(10*s23*s24-6*(s23*s23)+7*(s24*s24))-3*pow(s23,3)+7*pow(s24,3))-s13*s13*(s24*(4*s24*(s23*s23)+14*s23*(s24*s24)-3*pow(s23,3)+7*pow(s24,3))+s14*(-13*s24*(s23*s23)+3*pow(s23,3)+7*pow(s24,3))))+s12*((10*s23*s24-3*(s23*s23)-4*(s24*s24))*(s24*s24)*pow(s13,4)+s24*s24*pow(s13,3)*(3*s23*s24*(2*s24+9*(-s12-s13-s14-s23-s24))+(29*s24-(-s12-s13-s14-s23-s24))*(s23*s23)+(-7*s24+19*(-s12-s13-s14-s23-s24))*(s24*s24)+s14*(31*s23*s24-12*(s23*s23)+5*(s24*s24))-9*pow(s23,3))+s14*(s23*s23)*(s14*(3*s24*(-2*s24+(-s12-s13-s14-s23-s24))+s23*(-18*s24+5*(-s12-s13-s14-s23-s24))-18*(s23*s23))*(s24*s24)+s24*s24*(s23*s24*(-13*s24+10*(-s12-s13-s14-s23-s24))+(-13*s24+5*(-s12-s13-s14-s23-s24))*(s23*s23)+(-6*s24+7*(-s12-s13-s14-s23-s24))*(s24*s24)-6*pow(s23,3))+s14*s14*(s23*s24*(-10*s24+(-s12-s13-s14-s23-s24))+(-s12-s13-s14-s23-s24)*(s23*s23)-2*pow(s24,3)))+s24*(s13*s13)*(s24*(s14*s14)*(19*s23*s24-15*(s23*s23)+9*(s24*s24))+s24*(2*s24*(20*s24+23*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(13*s24+56*(-s12-s13-s14-s23-s24))*(s24*s24)+3*(8*s24+(-s12-s13-s14-s23-s24))*pow(s23,3)-9*pow(s23,4)+19*(-s12-s13-s14-s23-s24)*pow(s24,3))+s14*(6*s24*(11*s24+2*(-s12-s13-s14-s23-s24))*(s23*s23)+2*s23*(25*s24+18*(-s12-s13-s14-s23-s24))*(s24*s24)+(-35*s24+(-s12-s13-s14-s23-s24))*pow(s23,3)+(7*s24+19*(-s12-s13-s14-s23-s24))*pow(s24,3)))+s13*s23*s24*(s14*s14*(s23*s24*(21*s24+10*(-s12-s13-s14-s23-s24))+(-36*s24+2*(-s12-s13-s14-s23-s24))*(s23*s23)+(16*s24+5*(-s12-s13-s14-s23-s24))*(s24*s24))-2*s24*(3*s23+s24)*pow(s14,3)+s14*(2*s24*(7*s24+10*(-s12-s13-s14-s23-s24))*(s23*s23)+59*s23*(s24+(-s12-s13-s14-s23-s24))*(s24*s24)+(-30*s24+(-s12-s13-s14-s23-s24))*pow(s23,3)+9*(3*s24+4*(-s12-s13-s14-s23-s24))*pow(s24,3))+s24*(5*s24*(5*s24+3*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(31*s24+42*(-s12-s13-s14-s23-s24))*(s24*s24)+2*(2*s24+(-s12-s13-s14-s23-s24))*pow(s23,3)-3*pow(s23,4)+(13*s24+27*(-s12-s13-s14-s23-s24))*pow(s24,3))))+pow(s12,5)*((3*s14+6*s23+11*s24)*(-s12-s13-s14-s23-s24)*(s13*s13)+3*(4*s23+s24)*(-s12-s13-s14-s23-s24)*(s14*s14)+3*s24*(-s12-s13-s14-s23-s24)*(s23*s23)+10*s23*(-s12-s13-s14-s23-s24)*(s24*s24)-14*(s23*s23)*(s24*s24)+s14*(3*s23*s24*(-4*s24+3*(-s12-s13-s14-s23-s24))+18*(-s12-s13-s14-s23-s24)*(s23*s23)+(-11*s24+6*(-s12-s13-s14-s23-s24))*(s24*s24))+(-s12-s13-s14-s23-s24)*pow(s13,3)+(-s12-s13-s14-s23-s24)*pow(s14,3)+4*(-s12-s13-s14-s23-s24)*pow(s23,3)+s13*(14*s14*s24*(-s12-s13-s14-s23-s24)+3*(-s12-s13-s14-s23-s24)*(s14*s14)+9*(-s12-s13-s14-s23-s24)*(s23*s23)+s23*(18*s14*(-s12-s13-s14-s23-s24)+19*s24*(-s12-s13-s14-s23-s24)-13*(s24*s24))+19*(-s12-s13-s14-s23-s24)*(s24*s24)-4*pow(s24,3))-31*s23*pow(s24,3)+5*(-s12-s13-s14-s23-s24)*pow(s24,3)-18*pow(s24,4))+pow(s12,3)*(s13*s13*(s14*(2*s23*s24*(-13*s24+9*(-s12-s13-s14-s23-s24))+3*(-s12-s13-s14-s23-s24)*(s23*s23)+(32*s24+23*(-s12-s13-s14-s23-s24))*(s24*s24))+s24*(2*s23*s24*(25*s24+26*(-s12-s13-s14-s23-s24))+(-43*s24+12*(-s12-s13-s14-s23-s24))*(s23*s23)+(43*s24+74*(-s12-s13-s14-s23-s24))*(s24*s24)))+s24*(s23*(-11*s24+5*(-s12-s13-s14-s23-s24))+s24*(27*s24+16*(-s12-s13-s14-s23-s24)))*pow(s13,3)+(2*s23+s24)*(3*s23*(-s12-s13-s14-s23-s24)-2*(s24*s24))*pow(s14,3)-s24*s24*(s24*(54*s24-17*(-s12-s13-s14-s23-s24))*(s23*s23)+2*s23*(17*s24-4*(-s12-s13-s14-s23-s24))*(s24*s24)+2*(19*s24-7*(-s12-s13-s14-s23-s24))*pow(s23,3)+12*pow(s23,4)+(6*s24+(-s12-s13-s14-s23-s24))*pow(s24,3))+s14*s14*(9*s24*(-4*s24+(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(-27*s24+11*(-s12-s13-s14-s23-s24))*(s24*s24)+12*(-s12-s13-s14-s23-s24)*pow(s23,3)-(8*s24+(-s12-s13-s14-s23-s24))*pow(s24,3))+s13*(s14*s14*(s23*s24*(-19*s24+16*(-s12-s13-s14-s23-s24))+9*(-s12-s13-s14-s23-s24)*(s23*s23)+(3*s24+7*(-s12-s13-s14-s23-s24))*(s24*s24))+s14*(s24*(-80*s24+29*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(8*s24+73*(-s12-s13-s14-s23-s24))*(s24*s24)+6*(-s12-s13-s14-s23-s24)*pow(s23,3)+(21*s24+43*(-s12-s13-s14-s23-s24))*pow(s24,3))+s24*(s24*(-23*s24+45*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(33*s24+109*(-s12-s13-s14-s23-s24))*(s24*s24)+(-45*s24+7*(-s12-s13-s14-s23-s24))*pow(s23,3)+(21*s24+59*(-s12-s13-s14-s23-s24))*pow(s24,3)))+s14*((-86*s24+29*(-s12-s13-s14-s23-s24))*(s23*s23)*(s24*s24)+3*s24*(-16*s24+(-s12-s13-s14-s23-s24))*pow(s23,3)+3*(-s12-s13-s14-s23-s24)*pow(s23,4)+s23*(-60*s24+29*(-s12-s13-s14-s23-s24))*pow(s24,3)+2*(-6*s24+(-s12-s13-s14-s23-s24))*pow(s24,4)))+s12*s12*(s24*(s14*s24*(-9*s23+20*s24)+s23*s24*(52*s24+11*(-s12-s13-s14-s23-s24))+(-20*s24+(-s12-s13-s14-s23-s24))*(s23*s23)+(23*s24+39*(-s12-s13-s14-s23-s24))*(s24*s24))*pow(s13,3)+(-3*s23+11*s24)*(s24*s24)*pow(s13,4)+s24*(s13*s13)*(s24*(-9*s23+7*s24)*(s14*s14)+s24*(53*s24+25*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(89*s24+121*(-s12-s13-s14-s23-s24))*(s24*s24)+s14*(16*s23*s24*(5*s24+2*(-s12-s13-s14-s23-s24))+(-55*s24+8*(-s12-s13-s14-s23-s24))*(s23*s23)+(37*s24+34*(-s12-s13-s14-s23-s24))*(s24*s24))+(-35*s24+2*(-s12-s13-s14-s23-s24))*pow(s23,3)+(27*s24+65*(-s12-s13-s14-s23-s24))*pow(s24,3))+s23*(s14*(s24*s24)*(10*s23*s24*(-5*s24+3*(-s12-s13-s14-s23-s24))+(-56*s24+19*(-s12-s13-s14-s23-s24))*(s23*s23)+9*(-2*s24+(-s12-s13-s14-s23-s24))*(s24*s24)-27*pow(s23,3))+pow(s14,3)*(s23*s24*(-11*s24+3*(-s12-s13-s14-s23-s24))+4*(-s12-s13-s14-s23-s24)*(s23*s23)-4*pow(s24,3))+s14*s14*(3*s24*(-13*s24+(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(-35*s24+13*(-s12-s13-s14-s23-s24))*(s24*s24)+3*(-s12-s13-s14-s23-s24)*pow(s23,3)+2*(-7*s24+(-s12-s13-s14-s23-s24))*pow(s24,3))+s24*s24*(s24*(-17*s24+7*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(-16*s24+7*(-s12-s13-s14-s23-s24))*(s24*s24)+5*(-2*s24+(-s12-s13-s14-s23-s24))*pow(s23,3)-3*pow(s23,4)+(-6*s24+(-s12-s13-s14-s23-s24))*pow(s24,3)))+s13*(-((3*s23+2*s24)*(s24*s24)*pow(s14,3))+s14*s14*(s24*(-46*s24+11*(-s12-s13-s14-s23-s24))*(s23*s23)+17*s23*(s24+(-s12-s13-s14-s23-s24))*(s24*s24)+3*(-s12-s13-s14-s23-s24)*pow(s23,3)+(7*s24+(-s12-s13-s14-s23-s24))*pow(s24,3))+s14*s24*(s24*(11*s24+67*(-s12-s13-s14-s23-s24))*(s23*s23)+s23*(65*s24+103*(-s12-s13-s14-s23-s24))*(s24*s24)+(-78*s24+9*(-s12-s13-s14-s23-s24))*pow(s23,3)+(20*s24+27*(-s12-s13-s14-s23-s24))*pow(s24,3))+s24*(27*(2*s24+3*(-s12-s13-s14-s23-s24))*(s23*s23)*(s24*s24)-3*s24*(s24-6*(-s12-s13-s14-s23-s24))*pow(s23,3)+(-21*s24+(-s12-s13-s14-s23-s24))*pow(s23,4)+52*s23*(s24+2*(-s12-s13-s14-s23-s24))*pow(s24,3)+(13*s24+30*(-s12-s13-s14-s23-s24))*pow(s24,4))))+pow(s12,4)*(16*(-s12-s13-s14-s23-s24)*(s23*s23)*(s24*s24)+s14*s14*(3*s23*s24*(-4*s24+3*(-s12-s13-s14-s23-s24))+18*(-s12-s13-s14-s23-s24)*(s23*s23)+(-7*s24+3*(-s12-s13-s14-s23-s24))*(s24*s24))+s13*s13*(3*(-s12-s13-s14-s23-s24)*(s23*s23)+s23*(6*s14*(-s12-s13-s14-s23-s24)+21*s24*(-s12-s13-s14-s23-s24)-17*(s24*s24))+s24*(11*s14*(-s12-s13-s14-s23-s24)+32*s24*(-s12-s13-s14-s23-s24)+18*(s24*s24)))+(s23+5*s24)*(-s12-s13-s14-s23-s24)*pow(s13,3)+(4*s23+s24)*(-s12-s13-s14-s23-s24)*pow(s14,3)+s24*(-s12-s13-s14-s23-s24)*pow(s23,3)-19*(s24*s24)*pow(s23,3)+(-s12-s13-s14-s23-s24)*pow(s23,4)+15*s23*(-s12-s13-s14-s23-s24)*pow(s24,3)-53*(s23*s23)*pow(s24,3)+s14*(s23*s23*(9*s24*(-s12-s13-s14-s23-s24)-39*(s24*s24))+3*s23*(-18*s24+7*(-s12-s13-s14-s23-s24))*(s24*s24)+12*(-s12-s13-s14-s23-s24)*pow(s23,3)+(-23*s24+9*(-s12-s13-s14-s23-s24))*pow(s24,3))+s13*((9*s23+7*s24)*(-s12-s13-s14-s23-s24)*(s14*s14)+2*s24*(-20*s24+9*(-s12-s13-s14-s23-s24))*(s23*s23)+4*s23*(-5*s24+12*(-s12-s13-s14-s23-s24))*(s24*s24)+s14*(s23*s24*(-29*s24+35*(-s12-s13-s14-s23-s24))+18*(-s12-s13-s14-s23-s24)*(s23*s23)+2*(2*s24+13*(-s12-s13-s14-s23-s24))*(s24*s24))+3*(-s12-s13-s14-s23-s24)*pow(s23,3)+2*(2*s24+23*(-s12-s13-s14-s23-s24))*pow(s24,3))-55*s23*pow(s24,4)+(-s12-s13-s14-s23-s24)*pow(s24,4)-18*pow(s24,5)))*pow((-s12-s13-s14-s23-s24),-1))/4.;
}

// Coefficient of master 28 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCAm2CF<28>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCAm2CF<28,0>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<28,1>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<28,2>(s12,s13,s14,s23,s24)
    });
}


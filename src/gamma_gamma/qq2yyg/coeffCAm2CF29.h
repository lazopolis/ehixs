/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 29: box(-s12-s13-s14-s23-s24,s12+s14+s24,s12)

// Coefficient order epsilon^0 of master 29
template<>
double qq2yygCAm2CF<29,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s13+s23,-2)*pow(s24,-1)*((10*s23*s24+s13*(5*s23+6*s24)+s14*(5*s23+7*s24)+3*(s23*s23)+7*(s24*s24))*pow(s12,6)+(s23+2*s24)*pow(s12,7)+pow(s12,5)*(2*(5*s23+3*s24)*(s13*s13)+2*(5*s23+4*s24)*(s14*s14)+17*s24*(s23*s23)+25*s23*(s24*s24)+s14*(32*s23*s24+12*(s23*s23)+19*(s24*s24))+s13*(28*s23*s24+s14*(19*s23+14*s24)+12*(s23*s23)+22*(s24*s24))+3*pow(s23,3)+9*pow(s24,3))+pow(s12,4)*(32*(s23*s23)*(s24*s24)+3*(s14*s14)*(12*s23*s24+7*(s23*s23)+5*(s24*s24))+s13*s13*(32*s23*s24+7*s14*(4*s23+s24)+18*(s23*s23)+24*(s24*s24))+2*(5*s23+s24)*pow(s13,3)+3*(3*s23+s24)*pow(s14,3)+12*s24*pow(s23,3)+pow(s23,4)+26*s23*pow(s24,3)+s14*(42*s24*(s23*s23)+55*s23*(s24*s24)+9*pow(s23,3)+17*pow(s24,3))+s13*((26*s23+7*s24)*(s14*s14)+38*s24*(s23*s23)+61*s23*(s24*s24)+s14*(65*s23*s24+36*(s23*s23)+41*(s24*s24))+9*pow(s23,3)+29*pow(s24,3))+5*pow(s24,4))+pow(s12,3)*(2*(10*s14*s23+11*s23*s24+6*(s23*s23)+5*(s24*s24))*pow(s13,3)+5*s23*pow(s13,4)+(14*s23*s24+17*(s23*s23)+3*(s24*s24))*pow(s14,3)+3*s23*pow(s14,4)+s14*s14*(40*s24*(s23*s23)+36*s23*(s24*s24)+11*pow(s23,3)+7*pow(s24,3))+s13*s13*((26*s23-s24)*(s14*s14)+36*s24*(s23*s23)+50*s23*(s24*s24)+s14*(56*s23*s24+40*(s23*s23)+25*(s24*s24))+9*pow(s23,3)+33*pow(s24,3))+s24*(26*(s23*s23)*(s24*s24)+17*s24*pow(s23,3)+3*pow(s23,4)+12*s23*pow(s24,3)+pow(s24,4))+s14*(47*(s23*s23)*(s24*s24)+19*s24*pow(s23,3)+2*pow(s23,4)+34*s23*pow(s24,3)+5*pow(s24,4))+s13*(54*(s23*s23)*(s24*s24)+s14*s14*(44*s23*s24+40*(s23*s23)+15*(s24*s24))+(13*s23-s24)*pow(s14,3)+18*s24*pow(s23,3)+2*pow(s23,4)+61*s23*pow(s24,3)+s14*(65*s24*(s23*s23)+85*s23*(s24*s24)+21*pow(s23,3)+39*pow(s24,3))+16*pow(s24,4)))+(s13+s23)*((s14*s23*s24*(2*s23+3*s24)+s23*(s23+3*s24)*(s14*s14)+s24*s24*(s23*s23+s24*s24))*pow(s13,3)+s23*s24*(s14+s24)*pow(s13,4)-s14*(s23*s23)*(s14*s14*(3*s23*s24+s23*s23+s24*s24)+s24*s24*(5*s23*s24+2*(s23*s23)+3*(s24*s24))+s14*s24*(6*s23*s24+2*(s23*s23)+5*(s24*s24))+s23*pow(s14,3))-s13*s23*(2*s14*s23*(s23+2*s24)*(s24*s24)+s24*(s14*s14)*(s23*s23+3*(s24*s24))+s23*(s23-2*s24)*pow(s14,3)-(s23+s24)*pow(s24,4))+s13*s13*(s23*(s14*s14)*(6*s23*s24+s23*s23+3*(s24*s24))+s23*(s23+2*s24)*pow(s14,3)+s14*s24*(4*s24*(s23*s23)+pow(s23,3)+2*pow(s24,3))+(2*s23+s24)*pow(s24,4)))+s12*((2*s14*s23*(2*s23+5*s24)+4*s23*(s14*s14)+s24*(5*s23*s24+5*(s23*s23)+2*(s24*s24)))*pow(s13,4)+s23*(s14+2*s24)*pow(s13,5)+s13*s23*(s14*(s24*s24)*(-(s23*s24)-3*(s23*s23)+8*(s24*s24))-s24*(3*s23+2*s24)*pow(s14,3)+s14*s14*(3*s24*(s23*s23)-9*s23*(s24*s24)+pow(s23,3)-8*pow(s24,3))+(12*s23*s24+7*(s23*s23)+4*(s24*s24))*pow(s24,3))+s23*s23*((-3*(s23*s23)+s24*s24)*pow(s14,3)+s23*pow(s14,4)+(s23+s24)*(s23+s24)*pow(s24,3)+s14*s24*(-7*s24*(s23*s23)-4*s23*(s24*s24)-2*pow(s23,3)+2*pow(s24,3))-s14*s14*(5*s24*(s23*s23)+5*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3)))+pow(s13,3)*(5*s23*(2*s23+3*s24)*(s14*s14)+5*s23*pow(s14,3)+s14*(21*s24*(s23*s23)+14*s23*(s24*s24)+5*pow(s23,3)+5*pow(s24,3))+s24*(8*s24*(s23*s23)+9*s23*(s24*s24)+4*pow(s23,3)+8*pow(s24,3)))+s13*s13*(s23*(5*s23+6*s24)*pow(s14,3)+2*s23*pow(s14,4)+s14*s14*(17*s24*(s23*s23)+5*s23*(s24*s24)+8*pow(s23,3)+pow(s24,3))+s24*(13*(s23*s23)*(s24*s24)+3*s24*pow(s23,3)+pow(s23,4)+18*s23*pow(s24,3)+3*pow(s24,4))+s14*(15*(s23*s23)*(s24*s24)+13*s24*pow(s23,3)+2*pow(s23,4)+11*s23*pow(s24,3)+9*pow(s24,4))))+s12*s12*((7*s14*s23+10*s23*s24+3*(s23*s23)+s24*s24)*pow(s13,4)+s23*pow(s13,5)+pow(s13,3)*(14*s23*(s14*s14)+20*s24*(s23*s23)+18*s23*(s24*s24)+s14*(32*s23*s24+20*(s23*s23)+3*(s24*s24))+3*pow(s23,3)+15*pow(s24,3))+s23*(2*s14*(s24*s24)*(9*s23*s24+5*(s23*s23)+3*(s24*s24))+(14*s23*s24+6*(s23*s23)+5*(s24*s24))*pow(s14,3)+5*s23*pow(s14,4)+s24*s24*(10*s24*(s23*s23)+9*s23*(s24*s24)+3*pow(s23,3)+2*pow(s24,3))+s14*s14*(9*s24*(s23*s23)+18*s23*(s24*s24)-pow(s23,3)+9*pow(s24,3)))+s13*s13*(s23*(28*s23+29*s24)*(s14*s14)+28*(s23*s23)*(s24*s24)+9*s23*pow(s14,3)+12*s24*pow(s23,3)+pow(s23,4)+44*s23*pow(s24,3)+s14*(48*s24*(s23*s23)+41*s23*(s24*s24)+17*pow(s23,3)+27*pow(s24,3))+18*pow(s24,4))+s13*((7*s23*s24+15*(s23*s23)-s24*s24)*pow(s14,3)+s23*pow(s14,4)+s14*s14*(28*s24*(s23*s23)+15*s23*(s24*s24)+15*pow(s23,3)+8*pow(s24,3))+s24*(39*(s23*s23)*(s24*s24)+15*s24*pow(s23,3)+2*pow(s23,4)+27*s23*pow(s24,3)+3*pow(s24,4))+s14*(38*(s23*s23)*(s24*s24)+19*s24*pow(s23,3)+4*pow(s23,4)+45*s23*pow(s24,3)+12*pow(s24,4)))))*pow(s12+s23+s24,-1))/4.;
}

// Coefficient order epsilon^1 of master 29
template<>
double qq2yygCAm2CF<29,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s12,-1)*pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s13+s23,-2)*pow(s24,-1)*(((4*s14+29*s23+7*s24)*(s13*s13)+s23*(38*s23*s24+s14*(60*s23+47*s24)+33*(s14*s14)+20*(s23*s23)+15*(s24*s24))+s13*(s14*(69*s23+7*s24)+3*(s14*s14)+8*(7*s23*s24+6*(s23*s23)+s24*s24)))*pow(s12,7)+(s23*(14*s14+10*s23+9*s24)+s13*(s14+2*(6*s23+s24)))*pow(s12,8)+2*s23*pow(s12,9)+pow(s12,6)*(s13*s13*(125*s23*s24+s14*(136*s23+29*s24)+10*(s14*s14)+89*(s23*s23)+28*(s24*s24))+2*(3*s14+18*s23+5*s24)*pow(s13,3)+s23*((133*s23+88*s24)*(s14*s14)+62*s24*(s23*s23)+52*s23*(s24*s24)+s14*(173*s23*s24+104*(s23*s23)+58*(s24*s24))+35*pow(s14,3)+20*pow(s23,3)+11*pow(s24,3))+s13*((133*s23+10*s24)*(s14*s14)+176*s24*(s23*s23)+101*s23*(s24*s24)+2*s14*(109*s23*s24+118*(s23*s23)+9*(s24*s24))+3*pow(s14,3)+74*pow(s23,3)+12*pow(s24,3)))+3*s14*(s14+s24)*((s13+s23)*(s13+s23))*((s14+s23+s24)*(s23+2*s24)*(s14*s14)*(s23*s23)+s24*(-(s23*s24)-s23*s23+s24*s24)*pow(s13,3)+s13*s14*s23*(s24*(s23*s23)+s14*(2*s23*s24+2*(s23*s23)-3*(s24*s24))-3*s23*(s24*s24)+pow(s23,3)-3*pow(s24,3))+s13*s13*(s24*(-2*s24*(s23*s23)-pow(s23,3)+pow(s24,3))+s14*(-(s24*(s23*s23))-4*s23*(s24*s24)+pow(s23,3)+pow(s24,3))))+pow(s12,5)*((137*s23*s24+s14*(135*s23+43*s24)+12*(s14*s14)+79*(s23*s23)+38*(s24*s24))*pow(s13,3)+4*(s14+6*s23+2*s24)*pow(s13,4)+s13*s13*((217*s23+44*s24)*(s14*s14)+294*s24*(s23*s23)+215*s23*(s24*s24)+s14*(392*s23*s24+357*(s23*s23)+73*(s24*s24))+8*pow(s14,3)+99*pow(s23,3)+41*pow(s24,3))+s23*(66*(s23*s23)*(s24*s24)+s14*s14*(309*s23*s24+214*(s23*s23)+88*(s24*s24))+(138*s23+77*s24)*pow(s14,3)+17*pow(s14,4)+48*s24*pow(s23,3)+10*pow(s23,4)+30*s23*pow(s24,3)+s14*(251*s24*(s23*s23)+182*s23*(s24*s24)+92*pow(s23,3)+31*pow(s24,3))+3*pow(s24,4))+s13*(241*(s23*s23)*(s24*s24)+s14*s14*(315*s23*s24+426*(s23*s23)+20*(s24*s24))+(113*s23+7*s24)*pow(s14,3)+pow(s14,4)+216*s24*pow(s23,3)+54*pow(s23,4)+86*s23*pow(s24,3)+s14*(605*s24*(s23*s23)+265*s23*(s24*s24)+321*pow(s23,3)+22*pow(s24,3))+8*pow(s24,4)))+pow(s12,4)*((80*s23*s24+3*s14*(23*s23+9*s24)+6*(s14*s14)+33*(s23*s23)+26*(s24*s24))*pow(s13,4)+(s14+8*s23+4*s24)*pow(s13,5)+pow(s13,3)*((177*s23+53*s24)*(s14*s14)+227*s24*(s23*s23)+206*s23*(s24*s24)+s14*(330*s23*s24+255*(s23*s23)+103*(s24*s24))+7*pow(s14,3)+56*pow(s23,3)+52*pow(s24,3))+s13*s13*(366*(s23*s23)*(s24*s24)+s14*s14*(442*s23*s24+525*(s23*s23)+85*(s24*s24))+2*(76*s23+13*s24)*pow(s14,3)+2*pow(s14,4)+255*s24*pow(s23,3)+47*pow(s23,4)+178*s23*pow(s24,3)+s14*(773*s24*(s23*s23)+437*s23*(s24*s24)+356*pow(s23,3)+83*pow(s24,3))+26*pow(s24,4))+s23*((267*s23*s24+214*(s23*s23)+63*(s24*s24))*pow(s14,3)+(67*s23+33*s24)*pow(s14,4)+3*pow(s14,5)+s14*s14*(422*s24*(s23*s23)+269*s23*(s24*s24)+174*pow(s23,3)+39*pow(s24,3))+3*s14*(71*(s23*s23)*(s24*s24)+59*s24*pow(s23,3)+14*pow(s23,4)+27*s23*pow(s24,3)+2*pow(s24,4))+s23*(36*(s23*s23)*(s24*s24)+17*s24*pow(s23,3)+2*pow(s23,4)+27*s23*pow(s24,3)+6*pow(s24,4)))+s13*((219*s23*s24+351*(s23*s23)+13*(s24*s24))*pow(s14,3)+(43*s23+2*s24)*pow(s14,4)+221*(s24*s24)*pow(s23,3)+120*s24*pow(s23,4)+18*pow(s23,5)+147*(s23*s23)*pow(s24,3)+s14*s14*(814*s24*(s23*s23)+282*s23*(s24*s24)+540*pow(s23,3)+22*pow(s24,3))+33*s23*pow(s24,4)+s14*(548*(s23*s23)*(s24*s24)+640*s24*pow(s23,3)+210*pow(s23,4)+155*s23*pow(s24,3)+13*pow(s24,4))+2*pow(s24,5)))+pow(s12,3)*((23*s23*s24+2*s14*(8*s23+3*s24)+s14*s14+5*(s23*s23)+10*(s24*s24))*pow(s13,5)+(s23+s24)*pow(s13,6)+pow(s13,4)*((70*s23+22*s24)*(s14*s14)+80*s24*(s23*s23)+97*s23*(s24*s24)+s14*(126*s23*s24+83*(s23*s23)+59*(s24*s24))+2*pow(s14,3)+11*pow(s23,3)+30*pow(s24,3))+pow(s13,3)*(235*(s23*s23)*(s24*s24)+s14*s14*(260*s23*s24+298*(s23*s23)+99*(s24*s24))+5*(19*s23+4*s24)*pow(s14,3)+pow(s14,4)+121*s24*pow(s23,3)+13*pow(s23,4)+148*s23*pow(s24,3)+s14*(419*s24*(s23*s23)+300*s23*(s24*s24)+166*pow(s23,3)+109*pow(s24,3))+30*pow(s24,4))+s23*(s24*(2*s23+3*s24)*(s23*s23)*((s23+s24)*(s23+s24))+3*(37*s23*s24+34*(s23*s23)+6*(s24*s24))*pow(s14,4)+6*(2*s23+s24)*pow(s14,5)+2*pow(s14,3)*(175*s24*(s23*s23)+93*s23*(s24*s24)+82*pow(s23,3)+9*pow(s24,3))+s14*s14*(300*(s23*s23)*(s24*s24)+276*s24*pow(s23,3)+73*pow(s23,4)+99*s23*pow(s24,3)+6*pow(s24,4))+s14*s23*(108*(s23*s23)*(s24*s24)+58*s24*pow(s23,3)+8*pow(s23,4)+69*s23*pow(s24,3)+12*pow(s24,4)))+s13*s13*((226*s23*s24+340*(s23*s23)+41*(s24*s24))*pow(s14,3)+(45*s23+4*s24)*pow(s14,4)+225*(s24*s24)*pow(s23,3)+89*s24*pow(s23,4)+8*pow(s23,5)+203*(s23*s23)*pow(s24,3)+s14*s14*(746*s24*(s23*s23)+331*s23*(s24*s24)+482*pow(s23,3)+75*pow(s24,3))+67*s23*pow(s24,4)+s14*(536*(s23*s23)*(s24*s24)+540*s24*pow(s23,3)+153*pow(s23,4)+241*s23*pow(s24,3)+44*pow(s24,4))+6*pow(s24,5))+s13*((78*s23*s24+131*(s23*s23)+3*(s24*s24))*pow(s14,4)+6*s23*pow(s14,5)+pow(s14,3)*(538*s24*(s23*s23)+150*s23*(s24*s24)+424*pow(s23,3)+9*pow(s24,3))+s14*s14*(533*(s23*s23)*(s24*s24)+789*s24*pow(s23,3)+325*pow(s23,4)+119*s23*pow(s24,3)+9*pow(s24,4))+s14*(392*(s24*s24)*pow(s23,3)+297*s24*pow(s23,4)+62*pow(s23,5)+201*(s23*s23)*pow(s24,3)+45*s23*pow(s24,4)+3*pow(s24,5))+s23*(83*(s24*s24)*pow(s23,3)+28*s24*pow(s23,4)+2*pow(s23,5)+90*(s23*s23)*pow(s24,3)+36*s23*pow(s24,4)+4*pow(s24,5))))+s12*s12*(((10*s23+3*s24)*(s14*s14)+s24*(21*s23*s24+9*(s23*s23)+8*(s24*s24))+s14*(18*s23*s24+9*(s23*s23)+11*(s24*s24)))*pow(s13,5)+(s14*s23+2*s24*(s23+s24))*pow(s13,6)+pow(s13,4)*(s14*s14*(53*s23*s24+70*(s23*s23)+39*(s24*s24))+(21*s23+5*s24)*pow(s14,3)+s24*(62*s24*(s23*s23)+51*s23*(s24*s24)+18*pow(s23,3)+14*pow(s24,3))+s14*(85*s24*(s23*s23)+67*s23*(s24*s24)+27*pow(s23,3)+57*pow(s24,3)))+s14*(s23*s23)*((135*s23*s24+74*(s23*s23)+48*(s24*s24))*pow(s14,3)+18*(s23+s24)*pow(s14,4)+s23*s24*(19*s24*(s23*s23)+19*s23*(s24*s24)+6*pow(s23,3)+6*pow(s24,3))+3*(s14*s14)*(70*s24*(s23*s23)+64*s23*(s24*s24)+21*pow(s23,3)+14*pow(s24,3))+s14*(141*(s23*s23)*(s24*s24)+82*s24*pow(s23,3)+13*pow(s23,4)+81*s23*pow(s24,3)+12*pow(s24,4)))+pow(s13,3)*(2*(37*s23*s24+67*(s23*s23)+16*(s24*s24))*pow(s14,3)+(15*s23+2*s24)*pow(s14,4)+s14*s14*(241*s24*(s23*s23)+109*s23*(s24*s24)+173*pow(s23,3)+79*pow(s24,3))+s24*(101*(s23*s23)*(s24*s24)+81*s24*pow(s23,3)+19*pow(s23,4)+47*s23*pow(s24,3)+6*pow(s24,4))+s14*(161*(s23*s23)*(s24*s24)+162*s24*pow(s23,3)+36*pow(s23,4)+138*s23*pow(s24,3)+52*pow(s24,4)))+s13*s23*((180*s23*s24+149*(s23*s23)+33*(s24*s24))*pow(s14,4)+6*(3*s23+2*s24)*pow(s14,5)+pow(s14,3)*(478*s24*(s23*s23)+252*s23*(s24*s24)+235*pow(s23,3)+36*pow(s24,3))+s23*s24*(17*(s23*s23)*(s24*s24)+10*s24*pow(s23,3)+2*pow(s23,4)+11*s23*pow(s24,3)+2*pow(s24,4))+s14*s14*(329*(s23*s23)*(s24*s24)+325*s24*pow(s23,3)+87*pow(s23,4)+118*s23*pow(s24,3)+21*pow(s24,4))+s14*(92*(s24*s24)*pow(s23,3)+54*s24*pow(s23,4)+5*pow(s23,5)+62*(s23*s23)*pow(s24,3)+27*s23*pow(s24,4)+6*pow(s24,5)))+s13*s13*((53*s23*s24+91*(s23*s23)+6*(s24*s24))*pow(s14,4)+3*s23*pow(s14,5)+pow(s14,3)*(340*s24*(s23*s23)+96*s23*(s24*s24)+281*pow(s23,3)+21*pow(s24,3))+s23*s24*(75*(s23*s23)*(s24*s24)+48*s24*pow(s23,3)+10*pow(s23,4)+44*s23*pow(s24,3)+8*pow(s24,4))+s14*s14*(261*(s23*s23)*(s24*s24)+426*s24*pow(s23,3)+187*pow(s23,4)+118*s23*pow(s24,3)+24*pow(s24,4))+s14*(174*(s24*s24)*pow(s23,3)+143*s24*pow(s23,4)+22*pow(s23,5)+125*(s23*s23)*pow(s24,3)+74*s23*pow(s24,4)+9*pow(s24,5))))+s12*(s13+s23)*((s24*s24*(5*s23*s24+3*(s23*s23)+2*(s24*s24))+s14*s14*(4*(s23*s23)+5*(s24*s24))+s14*s24*(-3*s23*s24+4*(s23*s23)+9*(s24*s24)))*pow(s13,4)+s24*(s14*s23+s24*(s23+s24))*pow(s13,5)+pow(s13,3)*(2*(2*s23+s24)*(s24*s24)*((s23+s24)*(s23+s24))+(s23*s24+14*(s23*s23)+8*(s24*s24))*pow(s14,3)+s14*s24*(-9*s24*(s23*s23)+9*s23*(s24*s24)+10*pow(s23,3)+24*pow(s24,3))+s14*s14*(9*s24*(s23*s23)-12*s23*(s24*s24)+15*pow(s23,3)+29*pow(s24,3)))+s14*s14*(s23*s23)*(s14*s14*(69*s23*s24+25*(s23*s23)+42*(s24*s24))+6*(2*s23+3*s24)*pow(s14,3)+s24*(22*s24*(s23*s23)+21*s23*(s24*s24)+7*pow(s23,3)+6*pow(s24,3))+s14*(53*s24*(s23*s23)+78*s23*(s24*s24)+10*pow(s23,3)+30*pow(s24,3)))+s13*s14*s23*((65*s23*s24+47*(s23*s23)+3*(s24*s24))*pow(s14,3)+6*(s23+s24)*pow(s14,4)+s14*s14*(114*s24*(s23*s23)+52*s23*(s24*s24)+45*pow(s23,3)-9*pow(s24,3))+s14*(39*(s23*s23)*(s24*s24)+41*s24*pow(s23,3)+6*pow(s23,4)-9*s23*pow(s24,3)-3*pow(s24,4))+s24*(-6*(s23*s23)*(s24*s24)+s24*pow(s23,3)+3*pow(s23,4)-s23*pow(s24,3)+3*pow(s24,4)))+s13*s13*((8*s23*s24+16*(s23*s23)+3*(s24*s24))*pow(s14,4)+pow(s14,3)*(44*s24*(s23*s23)-18*s23*(s24*s24)+49*pow(s23,3)+15*pow(s24,3))+s14*s24*(-12*(s23*s23)*(s24*s24)-5*s24*pow(s23,3)+10*pow(s23,4)+17*s23*pow(s24,3)+9*pow(s24,4))+s14*s14*(-18*(s23*s23)*(s24*s24)+43*s24*pow(s23,3)+17*pow(s23,4)-13*s23*pow(s24,3)+21*pow(s24,4))+2*s23*(s24*s24)*pow(s23+s24,3))))*pow(s12+s23+s24,-1))/4.;
}

// Coefficient order epsilon^2 of master 29
template<>
double qq2yygCAm2CF<29,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s12,-1)*pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s13+s23,-2)*pow(s24,-1)*(((37*s23-14*s24)*(s13*s13)+s13*(4*s14*(28*s23-5*s24)+53*s23*s24+81*(s23*s23)-16*(s24*s24))+4*s23*(17*s23*s24+3*s14*(10*s23+7*s24)+16*(s14*s14)+10*(s23*s23)+6*(s24*s24)))*pow(s12,7)+(s13*(19*s23-4*s24)+4*s23*(7*s14+5*s23+4*s24))*pow(s12,8)+4*s23*pow(s12,9)+pow(s12,6)*(s13*s13*(s14*(181*s23-50*s24)+78*s23*s24+130*(s23*s23)-54*(s24*s24))+19*(2*s23-s24)*pow(s13,3)+s13*(4*(51*s23-7*s24)*(s14*s14)+219*s24*(s23*s23)+s14*(235*s23*s24+415*(s23*s23)-56*(s24*s24))+36*s23*(s24*s24)+133*pow(s23,3)-24*pow(s24,3))+4*s23*((64*s23+37*s24)*(s14*s14)+28*s24*(s23*s23)+21*s23*(s24*s24)+s14*(77*s23*s24+52*(s23*s23)+23*(s24*s24))+16*pow(s14,3)+10*pow(s23,3)+4*pow(s24,3)))+4*s14*(s14+s24)*((s13+s23)*(s13+s23))*((s14+s23+s24)*(s23+2*s24)*(s14*s14)*(s23*s23)+s24*(-(s23*s24)-s23*s23+s24*s24)*pow(s13,3)+s13*s14*s23*(s24*(s23*s23)+s14*(2*s23*s24+2*(s23*s23)-3*(s24*s24))-3*s23*(s24*s24)+pow(s23,3)-3*pow(s24,3))+s13*s13*(s24*(-2*s24*(s23*s23)-pow(s23,3)+pow(s24,3))+s14*(-(s24*(s23*s23))-4*s23*(s24*s24)+pow(s23,3)+pow(s24,3))))+pow(s12,5)*((s14*(152*s23-43*s24)+69*s23*s24+104*(s23*s23)-68*(s24*s24))*pow(s13,3)+(22*s23-13*s24)*pow(s13,4)+s13*s13*(3*(87*s23-14*s24)*(s14*s14)+298*s24*(s23*s23)+s14*(295*s23*s24+570*(s23*s23)-128*(s24*s24))-2*s23*(s24*s24)+165*pow(s23,3)-78*pow(s24,3))+s13*(s14*s14*(339*s23*s24+717*(s23*s23)-44*(s24*s24))+184*(s23*s23)*(s24*s24)+12*(13*s23-s24)*pow(s14,3)+302*s24*pow(s23,3)+103*pow(s23,4)+3*s14*(278*s24*(s23*s23)+43*s23*(s24*s24)+199*pow(s23,3)-16*pow(s24,3))-19*s23*pow(s24,3)-16*pow(s24,4))+4*s23*(27*(s23*s23)*(s24*s24)+2*(s14*s14)*(64*s23*s24+51*(s23*s23)+16*(s24*s24))+(62*s23+29*s24)*pow(s14,3)+7*pow(s14,4)+22*s24*pow(s23,3)+5*pow(s23,4)+11*s23*pow(s24,3)+s14*(112*s24*(s23*s23)+72*s23*(s24*s24)+46*pow(s23,3)+11*pow(s24,3))+pow(s24,4)))+s12*(s13+s23)*(s24*(s14*s23+(s23-s24)*s24)*pow(s13,5)+4*(s14*s14)*(s23*s23)*(s14*s14*(23*s23*s24+9*(s23*s23)+14*(s24*s24))+s24*(3*s23+2*s24)*((s23+s24)*(s23+s24))+(4*s23+6*s24)*pow(s14,3)+s14*(19*s24*(s23*s23)+26*s23*(s24*s24)+4*pow(s23,3)+10*pow(s24,3)))+pow(s13,3)*((-6*s23*s24+33*(s23*s23)+8*(s24*s24))*pow(s14,3)+s24*s24*(4*s24*(s23*s23)-7*s23*(s24*s24)+8*pow(s23,3)-4*pow(s24,3))+s14*s24*(-9*s24*(s23*s23)-14*s23*(s24*s24)+29*pow(s23,3)+20*pow(s24,3))+s14*s14*(17*s24*(s23*s23)-31*s23*(s24*s24)+33*pow(s23,3)+30*pow(s24,3)))+pow(s13,4)*(5*(s23*s23)*(s24*s24)+2*s14*s24*(-(s23*s24)+7*(s23*s23)+3*(s24*s24))+s14*s14*(-5*s23*s24+13*(s23*s23)+4*(s24*s24))-2*pow(s24,4))+s13*s14*s23*((84*s23*s24+69*(s23*s23)+4*(s24*s24))*pow(s14,3)+8*(s23+s24)*pow(s14,4)+s14*s14*(157*s24*(s23*s23)+63*s23*(s24*s24)+72*pow(s23,3)-12*pow(s24,3))+s14*(55*(s23*s23)*(s24*s24)+71*s24*pow(s23,3)+12*pow(s23,4)-17*s23*pow(s24,3)-4*pow(s24,4))+s24*(-7*(s23*s23)*(s24*s24)+7*s24*pow(s23,3)+8*pow(s23,4)-2*s23*pow(s24,3)+4*pow(s24,4)))+s13*s13*(4*(2*s23*s24+7*(s23*s23)+s24*s24)*pow(s14,4)+s23*(s24*s24)*(3*s24*(s23*s23)-5*s23*(s24*s24)+4*pow(s23,3)-4*pow(s24,3))+pow(s14,3)*(60*s24*(s23*s23)-30*s23*(s24*s24)+86*pow(s23,3)+20*pow(s24,3))+s14*s24*(-32*(s23*s23)*(s24*s24)-3*s24*pow(s23,3)+24*pow(s23,4)+13*s23*pow(s24,3)+12*pow(s24,4))+s14*s14*(-27*(s23*s23)*(s24*s24)+75*s24*pow(s23,3)+32*pow(s23,4)-25*s23*pow(s24,3)+28*pow(s24,4))))+pow(s12,4)*(2*(s14*(35*s23-8*s24)+19*s23*s24+21*(s23*s23)-20*(s24*s24))*pow(s13,4)+(7*s23-5*s24)*pow(s13,5)+pow(s13,3)*(2*(86*s23-9*s24)*(s14*s14)+215*s24*(s23*s23)+s14*(209*s23*s24+395*(s23*s23)-89*(s24*s24))-14*s23*(s24*s24)+91*pow(s23,3)-91*pow(s24,3))+s13*s13*(s14*s14*(347*s23*s24+801*(s23*s23)-40*(s24*s24))+177*(s23*s23)*(s24*s24)+2*(79*s23-3*s24)*pow(s14,3)+324*s24*pow(s23,3)+88*pow(s23,4)+s14*(920*s24*(s23*s23)+52*s23*(s24*s24)+645*pow(s23,3)-94*pow(s24,3))-106*s23*pow(s24,3)-50*pow(s24,4))+4*s23*(s23*(6*s23*s24+s23*s23+2*(s24*s24))*((s23+s24)*(s23+s24))+(99*s23*s24+94*(s23*s23)+21*(s24*s24))*pow(s14,3)+(27*s23+11*s24)*pow(s14,4)+pow(s14,5)+s14*s14*(173*s24*(s23*s23)+97*s23*(s24*s24)+82*pow(s23,3)+13*pow(s24,3))+s14*(85*(s23*s23)*(s24*s24)+80*s24*pow(s23,3)+21*pow(s23,4)+29*s23*pow(s24,3)+2*pow(s24,4)))+s13*(s23*(538*s23+237*s24)*pow(s14,3)+53*s23*pow(s14,4)+207*(s24*s24)*pow(s23,3)+183*s24*pow(s23,4)+36*pow(s23,5)+s14*s14*(1089*s24*(s23*s23)+198*s23*(s24*s24)+961*pow(s23,3)-4*pow(s24,3))+32*(s23*s23)*pow(s24,3)+s14*(514*(s23*s23)*(s24*s24)+991*s24*pow(s23,3)+409*pow(s23,4)+11*s23*pow(s24,3)-8*pow(s24,4))-29*s23*pow(s24,4)-4*pow(s24,5)))+s12*s12*((6*s23*(s14*s14)+s24*(4*s23*s24+13*(s23*s23)-9*(s24*s24))+s14*(16*s23*s24+20*(s23*s23)-s24*s24))*pow(s13,5)+(s14*s23+2*(s23-s24)*s24)*pow(s13,6)+pow(s13,4)*(s14*s14*(17*s23*s24+124*(s23*s23)+18*(s24*s24))+13*s23*pow(s14,3)+s24*(43*s24*(s23*s23)-22*s23*(s24*s24)+32*pow(s23,3)-22*pow(s24,3))+s14*(134*s24*(s23*s23)-2*s23*(s24*s24)+61*pow(s23,3)+14*pow(s24,3)))+4*s14*(s23*s23)*(s23*s24*(3*s23+2*s24)*((s23+s24)*(s23+s24))+(45*s23*s24+28*(s23*s23)+16*(s24*s24))*pow(s14,3)+6*(s23+s24)*pow(s14,4)+2*(s14*s14)*(38*s24*(s23*s23)+32*s23*(s24*s24)+13*pow(s23,3)+7*pow(s24,3))+s14*(51*(s23*s23)*(s24*s24)+34*s24*pow(s23,3)+6*pow(s23,4)+27*s23*pow(s24,3)+4*pow(s24,4)))+pow(s13,3)*((48*s23*s24+202*(s23*s23)+26*(s24*s24))*pow(s14,3)+12*s23*pow(s14,4)+s14*s14*(276*s24*(s23*s23)+11*s23*(s24*s24)+332*pow(s23,3)+64*pow(s24,3))+s24*(-9*(s23*s23)*(s24*s24)+79*s24*pow(s23,3)+37*pow(s23,4)-55*s23*pow(s24,3)-12*pow(s24,4))+s14*(67*(s23*s23)*(s24*s24)+295*s24*pow(s23,3)+78*pow(s23,4)-19*s23*pow(s24,3)+20*pow(s24,4)))+s13*s23*(s23*s24*(3*s23*s24+4*(s23*s23)-4*(s24*s24))*((s23+s24)*(s23+s24))+(232*s23*s24+215*(s23*s23)+44*(s24*s24))*pow(s14,4)+8*(3*s23+2*s24)*pow(s14,5)+pow(s14,3)*(639*s24*(s23*s23)+306*s23*(s24*s24)+390*pow(s23,3)+48*pow(s24,3))+s14*s14*(409*(s23*s23)*(s24*s24)+523*s24*pow(s23,3)+163*pow(s23,4)+119*s23*pow(s24,3)+28*pow(s24,4))+s14*(133*(s24*s24)*pow(s23,3)+110*s24*pow(s23,4)+12*pow(s23,5)+37*(s23*s23)*pow(s24,3)+11*s23*pow(s24,4)+8*pow(s24,5)))+s13*s13*(2*(30*s23*s24+61*(s23*s23)+4*(s24*s24))*pow(s14,4)+4*s23*pow(s14,5)+pow(s14,3)*(404*s24*(s23*s23)+87*s23*(s24*s24)+464*pow(s23,3)+28*pow(s24,3))+s23*s24*(10*(s23*s23)*(s24*s24)+53*s24*pow(s23,3)+20*pow(s23,4)-38*s23*pow(s24,3)-16*pow(s24,4))+s14*s14*(219*(s23*s23)*(s24*s24)+624*s24*pow(s23,3)+353*pow(s23,4)+89*s23*pow(s24,3)+32*pow(s24,4))+s14*(158*(s24*s24)*pow(s23,3)+275*s24*pow(s23,4)+48*pow(s23,5)-17*(s23*s23)*pow(s24,3)+30*s23*pow(s24,4)+12*pow(s24,5))))+pow(s12,3)*((s14*(16*s23-3*s24)+12*s23*s24+7*(s23*s23)-12*(s24*s24))*pow(s13,5)+(s23-s24)*pow(s13,6)+pow(s13,4)*((57*s23-4*s24)*(s14*s14)+81*s24*(s23*s23)+2*s14*(40*s23*s24+70*(s23*s23)-9*(s24*s24))+3*s23*(s24*s24)+19*pow(s23,3)-45*pow(s24,3))+pow(s13,3)*(114*(s23*s23)*(s24*s24)+s14*s14*(160*s23*s24+451*(s23*s23)+18*(s24*s24))+(79*s23-2*s24)*pow(s14,3)+163*s24*pow(s23,3)+25*pow(s23,4)+s14*(500*s24*(s23*s23)+15*s23*(s24*s24)+317*pow(s23,3)-38*pow(s24,3))-92*s23*pow(s24,3)-54*pow(s24,4))+s13*s13*((201*s23*s24+476*(s23*s23)+26*(s24*s24))*pow(s14,3)+45*s23*pow(s14,4)+174*(s24*s24)*pow(s23,3)+138*s24*pow(s23,4)+16*pow(s23,5)-25*(s23*s23)*pow(s24,3)+s14*s14*(881*s24*(s23*s23)+158*s23*(s24*s24)+849*pow(s23,3)+34*pow(s24,3))+s14*(327*(s23*s23)*(s24*s24)+816*s24*pow(s23,3)+304*pow(s23,4)-48*s23*pow(s24,3)-4*pow(s24,4))-79*s23*pow(s24,4)-12*pow(s24,5))+s13*((96*s23*s24+179*(s23*s23)+4*(s24*s24))*pow(s14,4)+8*s23*pow(s14,5)+pow(s14,3)*(675*s24*(s23*s23)+159*s23*(s24*s24)+688*pow(s23,3)+12*pow(s24,3))+s14*s14*(568*(s23*s23)*(s24*s24)+1166*s24*pow(s23,3)+599*pow(s23,4)+92*s23*pow(s24,3)+12*pow(s24,4))+s23*(86*(s24*s24)*pow(s23,3)+47*s24*pow(s23,4)+4*pow(s23,5)+33*(s23*s23)*pow(s24,3)-18*s23*pow(s24,4)-8*pow(s24,5))+s14*(455*(s24*s24)*pow(s23,3)+514*s24*pow(s23,4)+127*pow(s23,5)+103*(s23*s23)*pow(s24,3)+13*s23*pow(s24,4)+4*pow(s24,5)))+4*s23*((37*s23*s24+40*(s23*s23)+6*(s24*s24))*pow(s14,4)+2*(2*s23+s24)*pow(s14,5)+2*pow(s14,3)*(64*s24*(s23*s23)+31*s23*(s24*s24)+35*pow(s23,3)+3*pow(s24,3))+s14*s14*(108*(s23*s23)*(s24*s24)+113*s24*pow(s23,3)+34*pow(s23,4)+33*s23*pow(s24,3)+2*pow(s24,4))+s14*s23*(44*(s23*s23)*(s24*s24)+27*s24*pow(s23,3)+4*pow(s23,4)+25*s23*pow(s24,3)+4*pow(s24,4))+s24*(s23*s23)*pow(s23+s24,3))))*pow(s12+s23+s24,-1))/4.;
}

// Coefficient of master 29 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCAm2CF<29>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCAm2CF<29,0>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<29,1>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<29,2>(s12,s13,s14,s23,s24)
    });
}


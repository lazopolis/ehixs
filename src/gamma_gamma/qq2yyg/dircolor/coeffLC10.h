/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 10: box(s14,-s12-s13-s14,s23)

// Coefficient order epsilon^0 of master 10
template<>
double qq2yygLC<10,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s23,-1)*pow(s12+s13+s23,-3)*pow(s24,-1)*((2*s13*(29*s14+60*s23+31*s24)+55*(s13*s13)+2*(32*s23*s24+2*s14*(17*s23+7*s24)+7*(s14*s14)+31*(s23*s23)+7*(s24*s24)))*pow(s12,7)+(23*s13+12*(s14+2*s23+s24))*pow(s12,8)+4*pow(s12,9)+(s14*(s23*s24+2*(s23*s23)-6*(s24*s24))+2*s24*(7*s23*s24+11*(s23*s23)+s24*s24))*pow(s13,6)+2*s23*s24*pow(s13,7)+2*(s23+s24)*(s14*s14)*(s14*s14+(s23+s24)*(s23+s24))*pow(s23,4)+pow(s13,5)*(s14*s14*(-4*s23*s24+7*(s23*s23)-13*(s24*s24))+s24*(67*s24*(s23*s23)+16*s23*(s24*s24)+64*pow(s23,3)+pow(s24,3))+s14*(38*s24*(s23*s23)+3*s23*(s24*s24)+6*pow(s23,3)+2*pow(s24,3)))+pow(s12,6)*((113*s14+245*s23+133*s24)*(s13*s13)+s13*(293*s23*s24+s14*(281*s23+115*s24)+56*(s14*s14)+263*(s23*s23)+66*(s24*s24))+70*pow(s13,3)+2*((37*s23+12*s24)*(s14*s14)+72*s24*(s23*s23)+33*s23*(s24*s24)+2*s14*(35*s23*s24+41*(s23*s23)+6*(s24*s24))+4*pow(s14,3)+45*pow(s23,3)+4*pow(s24,3)))+s13*s14*pow(s23,3)*(3*(s14*s14)*(-(s23*s24)+s23*s23-3*(s24*s24))+5*(s23+s24)*pow(s14,3)+s14*(26*s24*(s23*s23)+22*s23*(s24*s24)+6*pow(s23,3)+4*pow(s24,3))+2*(11*(s23*s23)*(s24*s24)+7*s24*pow(s23,3)+pow(s23,4)+6*s23*pow(s24,3)+pow(s24,4)))+pow(s12,5)*(s13*s13*(561*s23*s24+s14*(458*s23+179*s24)+87*(s14*s14)+442*(s23*s23)+128*(s24*s24))+2*(56*s14+131*s23+76*s24)*pow(s13,3)+50*pow(s13,4)+s13*((248*s23+75*s24)*(s14*s14)+584*s24*(s23*s23)+279*s23*(s24*s24)+s14*(493*s23*s24+561*(s23*s23)+81*(s24*s24))+26*pow(s14,3)+314*pow(s23,3)+36*pow(s24,3))+2*(63*(s23*s23)*(s24*s24)+s14*s14*(54*s23*s24+81*(s23*s23)+6*(s24*s24))+5*(4*s23+s24)*pow(s14,3)+pow(s14,4)+88*s24*pow(s23,3)+40*pow(s23,4)+16*s23*pow(s24,3)+s14*(147*s24*(s23*s23)+54*s23*(s24*s24)+110*pow(s23,3)+5*pow(s24,3))+pow(s24,4)))+pow(s13,4)*((-2*s23*s24+8*(s23*s23)-7*(s24*s24))*pow(s14,3)+s14*s14*(13*s24*(s23*s23)-36*s23*(s24*s24)+16*pow(s23,3)-pow(s24,3))+s23*s24*(112*s24*(s23*s23)+43*s23*(s24*s24)+80*pow(s23,3)+3*pow(s24,3))+s14*(86*(s23*s23)*(s24*s24)+112*s24*pow(s23,3)+8*pow(s23,4)+17*s23*pow(s24,3)+2*pow(s24,4)))+s13*s13*(s23*s23)*((-7*s23*s24+9*(s23*s23)-25*(s24*s24))*pow(s14,3)+4*(s23+s24)*pow(s14,4)+2*s23*s24*(10*s24*(s23*s23)+6*s23*(s24*s24)+5*pow(s23,3)+pow(s24,3))+s14*s14*(40*s24*(s23*s23)+19*s23*(s24*s24)+8*pow(s23,3)+pow(s24,3))+s14*(91*(s23*s23)*(s24*s24)+67*s24*pow(s23,3)+6*pow(s23,4)+40*s23*pow(s24,3)+6*pow(s24,4)))+s23*pow(s13,3)*((2*s23*s24+14*(s23*s23)-23*(s24*s24))*pow(s14,3)+(3*s23+s24)*pow(s14,4)+s14*s14*(37*s24*(s23*s23)-8*s23*(s24*s24)+13*pow(s23,3)-2*pow(s24,3))+s23*s24*(79*s24*(s23*s23)+41*s23*(s24*s24)+46*pow(s23,3)+6*pow(s24,3))+s14*(146*(s23*s23)*(s24*s24)+128*s24*pow(s23,3)+8*pow(s23,4)+51*s23*pow(s24,3)+6*pow(s24,4)))+pow(s12,4)*((586*s23*s24+s14*(377*s23+122*s24)+65*(s14*s14)+376*(s23*s23)+132*(s24*s24))*pow(s13,3)+2*(29*s14+79*s23+49*s24)*pow(s13,4)+19*pow(s13,5)+s13*s13*((315*s23+73*s24)*(s14*s14)+1005*s24*(s23*s23)+497*s23*(s24*s24)+s14*(659*s23*s24+730*(s23*s23)+89*(s24*s24))+31*pow(s14,3)+414*pow(s23,3)+66*pow(s24,3))+s13*(485*(s23*s23)*(s24*s24)+s14*s14*(267*s23*s24+436*(s23*s23)+21*(s24*s24))+(107*s23+25*s24)*pow(s14,3)+5*pow(s14,4)+630*s24*pow(s23,3)+221*pow(s23,4)+125*s23*pow(s24,3)+s14*(889*s24*(s23*s23)+318*s23*(s24*s24)+598*pow(s23,3)+34*pow(s24,3))+9*pow(s24,4))+2*(s23*(18*s23*s24+22*(s23*s23)+3*(s24*s24))*((s23+s24)*(s23+s24))+(20*s23*s24+40*(s23*s23)+s24*s24)*pow(s14,3)+(5*s23+s24)*pow(s14,4)+s14*s14*(99*s24*(s23*s23)+24*s23*(s24*s24)+95*pow(s23,3)+pow(s24,3))+s14*(99*(s23*s23)*(s24*s24)+168*s24*pow(s23,3)+90*pow(s23,4)+20*s23*pow(s24,3)+pow(s24,4))))+s12*((s14*(9*s23-5*s24)+27*s23*s24+8*(s23*s23)+4*(s24*s24))*pow(s13,6)+2*s23*pow(s13,7)+pow(s13,5)*((16*s23-9*s24)*(s14*s14)+171*s24*(s23*s23)+4*s14*(4*s23*s24+10*(s23*s23)-7*(s24*s24))+98*s23*(s24*s24)+14*pow(s23,3)+12*pow(s24,3))+pow(s13,4)*(s14*s14*(-29*s23*s24+66*(s23*s23)-53*(s24*s24))+340*(s23*s23)*(s24*s24)+(13*s23-3*s24)*pow(s14,3)+355*s24*pow(s23,3)+16*pow(s23,4)+77*s23*pow(s24,3)+s14*(228*s24*(s23*s23)+35*s23*(s24*s24)+69*pow(s23,3)+10*pow(s24,3))+6*pow(s24,4))+s13*(s23*s23)*(2*s23*(6*s23*s24+s23*s23+2*(s24*s24))*((s23+s24)*(s23+s24))+2*(7*s23*s24+19*(s23*s23)-10*(s24*s24))*pow(s14,3)+5*(4*s23+3*s24)*pow(s14,4)+s14*s14*(124*s24*(s23*s23)+68*s23*(s24*s24)+46*pow(s23,3)+13*pow(s24,3))+s14*(169*(s23*s23)*(s24*s24)+138*s24*pow(s23,3)+27*pow(s23,4)+72*s23*pow(s24,3)+12*pow(s24,4)))+pow(s13,3)*((-9*s23*s24+46*(s23*s23)-20*(s24*s24))*pow(s14,3)+(4*s23+s24)*pow(s14,4)+s14*s14*(37*s24*(s23*s23)-105*s23*(s24*s24)+92*pow(s23,3)-pow(s24,3))+s14*(325*(s23*s23)*(s24*s24)+478*s24*pow(s23,3)+73*pow(s23,4)+65*s23*pow(s24,3)+8*pow(s24,4))+s23*(422*(s23*s23)*(s24*s24)+321*s24*pow(s23,3)+14*pow(s23,4)+158*s23*pow(s24,3)+15*pow(s24,4)))+s23*(s13*s13)*((-3*s23*s24+58*(s23*s23)-42*(s24*s24))*pow(s14,3)+4*(3*s23+2*s24)*pow(s14,4)+s14*s14*(130*s24*(s23*s23)-5*s23*(s24*s24)+74*pow(s23,3)+4*pow(s24,3))+2*s23*(103*(s23*s23)*(s24*s24)+63*s24*pow(s23,3)+4*pow(s23,4)+54*s23*pow(s24,3)+9*pow(s24,4))+s14*(398*(s23*s23)*(s24*s24)+395*s24*pow(s23,3)+58*pow(s23,4)+130*s23*pow(s24,3)+18*pow(s24,4)))+2*s14*pow(s23,3)*(s14*s14*(5*s23*s24+4*(s23*s23)+s24*s24)+s14*(7*s23+4*s24)*((s23+s24)*(s23+s24))+(5*s23+4*s24)*pow(s14,3)+(2*s23+s24)*pow(s23+s24,3)))+pow(s12,3)*((364*s23*s24+s14*(173*s23+22*s24)+23*(s14*s14)+176*(s23*s23)+78*(s24*s24))*pow(s13,4)+2*(7*s14+28*s23+17*s24)*pow(s13,5)+3*pow(s13,6)+pow(s13,3)*(2*(99*s23+4*s24)*(s14*s14)+961*s24*(s23*s23)+491*s23*(s24*s24)+s14*(413*s23*s24+469*(s23*s23)+15*(s24*s24))+16*pow(s14,3)+260*pow(s23,3)+64*pow(s24,3))+s13*s13*(s14*s14*(186*s23*s24+427*(s23*s23)-21*(s24*s24))+798*(s23*s23)*(s24*s24)+17*(6*s23+s24)*pow(s14,3)+4*pow(s14,4)+965*s24*pow(s23,3)+215*pow(s23,4)+199*s23*pow(s24,3)+s14*(1050*s24*(s23*s23)+341*s23*(s24*s24)+584*pow(s23,3)+44*pow(s24,3))+16*pow(s24,4))+s13*(2*(35*s23*s24+84*(s23*s23)-s24*s24)*pow(s14,3)+5*(4*s23+s24)*pow(s14,4)+s14*s14*(381*s24*(s23*s23)+62*s23*(s24*s24)+386*pow(s23,3)+5*pow(s24,3))+s14*(522*(s23*s23)*(s24*s24)+869*s24*pow(s23,3)+370*pow(s23,4)+111*s23*pow(s24,3)+8*pow(s24,4))+s23*(427*(s23*s23)*(s24*s24)+386*s24*pow(s23,3)+92*pow(s23,4)+168*s23*pow(s24,3)+21*pow(s24,4)))+2*s23*((30*s23*s24+40*(s23*s23)+3*(s24*s24))*pow(s14,3)+2*(5*s23+2*s24)*pow(s14,4)+s14*s14*(96*s24*(s23*s23)+39*s23*(s24*s24)+65*pow(s23,3)+4*pow(s24,3))+s14*(93*(s23*s23)*(s24*s24)+112*s24*pow(s23,3)+46*pow(s23,4)+30*s23*pow(s24,3)+3*pow(s24,4))+s23*(7*s23+3*s24)*pow(s23+s24,3)))+s12*s12*((s14*(50*s23-13*s24)+135*s23*s24+3*(s14*s14)+49*(s23*s23)+26*(s24*s24))*pow(s13,5)+(s14+13*s23+5*s24)*pow(s13,6)+pow(s13,4)*((73*s23-23*s24)*(s14*s14)+545*s24*(s23*s23)+s14*(122*s23*s24+174*(s23*s23)-39*(s24*s24))+291*s23*(s24*s24)+3*pow(s14,3)+84*pow(s23,3)+36*pow(s24,3))+pow(s13,3)*(2*(s14*s14)*(s23*s24+106*(s23*s23)-35*(s24*s24))+712*(s23*s23)*(s24*s24)+(48*s23-s24)*pow(s14,3)+pow(s14,4)+802*s24*pow(s23,3)+90*pow(s23,4)+167*s23*pow(s24,3)+s14*(645*s24*(s23*s23)+163*s23*(s24*s24)+269*pow(s23,3)+28*pow(s24,3))+14*pow(s24,4))+s13*s23*((62*s23*s24+122*(s23*s23)-13*(s24*s24))*pow(s14,3)+15*(2*s23+s24)*pow(s14,4)+s14*s14*(287*s24*(s23*s23)+87*s23*(s24*s24)+182*pow(s23,3)+14*pow(s24,3))+s23*(185*(s23*s23)*(s24*s24)+125*s24*pow(s23,3)+21*pow(s23,4)+99*s23*pow(s24,3)+18*pow(s24,4))+s14*(432*(s23*s23)*(s24*s24)+482*s24*pow(s23,3)+135*pow(s23,4)+139*s23*pow(s24,3)+18*pow(s24,4)))+s13*s13*((21*s23*s24+120*(s23*s23)-17*(s24*s24))*pow(s14,3)+4*(3*s23+s24)*pow(s14,4)+s14*s14*(203*s24*(s23*s23)-55*s23*(s24*s24)+265*pow(s23,3)+3*pow(s24,3))+s14*(563*(s23*s23)*(s24*s24)+898*s24*pow(s23,3)+251*pow(s23,4)+119*s23*pow(s24,3)+12*pow(s24,4))+s23*(615*(s23*s23)*(s24*s24)+504*s24*pow(s23,3)+61*pow(s23,4)+235*s23*pow(s24,3)+27*pow(s24,4)))+2*(s23*s23)*(s14*(14*s23*s24+14*(s23*s23)+3*(s24*s24))*((s23+s24)*(s23+s24))+(20*s23*s24+20*(s23*s23)+3*(s24*s24))*pow(s14,3)+2*(5*s23+3*s24)*pow(s14,4)+3*(s14*s14)*(18*s24*(s23*s23)+11*s23*(s24*s24)+9*pow(s23,3)+2*pow(s24,3))+s23*pow(s23+s24,4))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/8.;
}

// Coefficient order epsilon^1 of master 10
template<>
double qq2yygLC<10,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s13,-1)*pow(s23,-1)*pow(s12+s13+s23,-3)*pow(s24,-1)*((s13*(12*s14+49*s23+6*s24)-s14*(7*s23+10*s24)+32*(s13*s13)-2*(s14*s14)+2*(s23*s24+7*(s23*s23)-s24*s24))*pow(s12,7)+(9*s13+5*s23)*pow(s12,8)+pow(s12,9)+pow(s12,6)*((56*s14+169*s23+36*s24)*(s13*s13)-3*(9*s23+4*s24)*(s14*s14)+s13*(s14*(49*s23-24*s24)+60*s23*s24+18*(s14*s14)+130*(s23*s23)-5*(s24*s24))+2*s23*(11*s23*s24+15*(s23*s23)+s24*s24)-s14*(53*s23*s24+26*(s23*s23)+18*(s24*s24))+60*pow(s13,3))+(s14*(8*s23*s24+4*(s23*s23)+s24*s24)+s24*(24*s23*s24+32*(s23*s23)+7*(s24*s24)))*pow(s13,6)+s24*(4*s23+s24)*pow(s13,7)-2*(s14*s14)*pow(s23,4)*((s23-2*s24)*(s14*s14)-5*s24*(s23*s23)-5*s23*(s24*s24)+s14*(3*s23*s24+3*(s23*s23)+s24*s24)-2*pow(s23,3)-2*pow(s24,3))+pow(s13,5)*(s14*s14*(-6*s23*s24+13*(s23*s23)-8*(s24*s24))+2*s24*(50*s24*(s23*s23)+20*s23*(s24*s24)+42*pow(s23,3)+pow(s24,3))+s14*(81*s24*(s23*s23)+33*s23*(s24*s24)+16*pow(s23,3)+7*pow(s24,3)))+pow(s13,4)*((-10*s23*s24+23*(s23*s23)-8*(s24*s24))*pow(s14,3)+s14*s14*(64*s24*(s23*s23)-13*s23*(s24*s24)+39*pow(s23,3)-pow(s24,3))+s23*s24*(164*s24*(s23*s23)+95*s23*(s24*s24)+100*pow(s23,3)+16*pow(s24,3))+s14*(176*(s23*s23)*(s24*s24)+211*s24*pow(s23,3)+28*pow(s23,4)+51*s23*pow(s24,3)-3*pow(s24,4)))+pow(s12,5)*(38*(s23*s23)*(s24*s24)-4*(s14*s14)*(19*s23*s24+24*(s23*s23)+5*(s24*s24))+s13*s13*(278*s23*s24+15*s14*(18*s23+s24)+86*(s14*s14)+390*(s23*s23)+13*(s24*s24))+(104*s14+290*s23+87*s24)*pow(s13,3)+65*pow(s13,4)+(-13*s23+2*s24)*pow(s14,3)+pow(s14,4)+68*s24*pow(s23,3)+45*pow(s23,4)+10*s23*pow(s24,3)+s13*((43*s23-2*s24)*(s14*s14)+248*s24*(s23*s23)+s14*(-57*s23*s24+113*(s23*s23)-67*(s24*s24))+60*s23*(s24*s24)+34*pow(s14,3)+214*pow(s23,3)+4*pow(s24,3))-s14*(85*s24*(s23*s23)+57*s23*(s24*s24)+26*pow(s23,3)+10*pow(s24,3))+pow(s24,4))+s13*s14*pow(s23,3)*(38*(s23*s23)*(s24*s24)+s14*s14*(26*s23*s24-6*(s23*s23)+11*(s24*s24))+(9*s23+26*s24)*pow(s14,3)+6*pow(s14,4)+20*s24*pow(s23,3)+4*pow(s23,4)+34*s23*pow(s24,3)+s14*(73*s24*(s23*s23)+94*s23*(s24*s24)+14*pow(s23,3)+43*pow(s24,3))+12*pow(s24,4))+pow(s12,4)*((544*s23*s24+s14*(465*s23+99*s24)+130*(s14*s14)+550*(s23*s23)+63*(s24*s24))*pow(s13,3)+(96*s14+275*s23+108*s24)*pow(s13,4)+41*pow(s13,5)-(5*s23*s24+58*(s23*s23)+4*(s24*s24))*pow(s14,3)+2*(s23+2*s24)*pow(s14,4)-s14*s14*(164*s24*(s23*s23)+73*s23*(s24*s24)+150*pow(s23,3)+10*pow(s24,3))+s13*s13*(4*(79*s23+19*s24)*(s14*s14)+821*s24*(s23*s23)+s14*(262*s23*s24+563*(s23*s23)-86*(s24*s24))+273*s23*(s24*s24)+98*pow(s14,3)+496*pow(s23,3)+23*pow(s24,3))+s14*(-30*(s23*s23)*(s24*s24)-13*s24*pow(s23,3)+16*pow(s23,4)-15*s23*pow(s24,3)-2*pow(s24,4))+s23*(82*(s23*s23)*(s24*s24)+92*s24*pow(s23,3)+41*pow(s23,4)+34*s23*pow(s24,3)+5*pow(s24,4))+s13*(s14*s14*(-31*s23*s24+16*(s23*s23)-55*(s24*s24))+298*(s23*s23)*(s24*s24)+(109*s23+50*s24)*pow(s14,3)+25*pow(s14,4)+468*s24*pow(s23,3)+221*pow(s23,4)+s14*(146*s24*(s23*s23)-101*s23*(s24*s24)+207*pow(s23,3)-36*pow(s24,3))+76*s23*pow(s24,3)+6*pow(s24,4)))+s23*pow(s13,3)*((40*s23*s24+48*(s23*s23)-19*(s24*s24))*pow(s14,3)+20*s23*pow(s14,4)+s14*s14*(187*s24*(s23*s23)+96*s23*(s24*s24)+45*pow(s23,3)+27*pow(s24,3))+s23*s24*(119*s24*(s23*s23)+94*s23*(s24*s24)+56*pow(s23,3)+27*pow(s24,3))+s14*(283*(s23*s23)*(s24*s24)+231*s24*pow(s23,3)+28*pow(s23,4)+125*s23*pow(s24,3)+6*pow(s24,4)))+s13*s13*(s23*s23)*((78*s23*s24+25*(s23*s23)+2*(s24*s24))*pow(s14,3)+(30*s23+22*s24)*pow(s14,4)+6*pow(s14,5)+4*s23*s24*(8*s24*(s23*s23)+8*s23*(s24*s24)+3*pow(s23,3)+3*pow(s24,3))+s14*s14*(180*s24*(s23*s23)+179*s23*(s24*s24)+29*pow(s23,3)+67*pow(s24,3))+s14*(177*(s23*s23)*(s24*s24)+113*s24*pow(s23,3)+16*pow(s23,4)+111*s23*pow(s24,3)+21*pow(s24,4)))+pow(s12,3)*((538*s23*s24+s14*(370*s23+117*s24)+84*(s14*s14)+400*(s23*s23)+92*(s24*s24))*pow(s13,4)+(44*s14+145*s23+72*s24)*pow(s13,5)+14*pow(s13,6)+pow(s13,3)*((437*s23+123*s24)*(s14*s14)+1209*s24*(s23*s23)+s14*(602*s23*s24+776*(s23*s23)-35*(s24*s24))+503*s23*(s24*s24)+94*pow(s14,3)+514*pow(s23,3)+52*pow(s24,3))+s13*s13*(s14*s14*(262*s23*s24+462*(s23*s23)-63*(s24*s24))+808*(s23*s23)*(s24*s24)+(305*s23+111*s24)*pow(s14,3)+46*pow(s14,4)+1107*s24*pow(s23,3)+358*pow(s23,4)+s14*(974*s24*(s23*s23)+81*s23*(s24*s24)+654*pow(s23,3)-41*pow(s24,3))+208*s23*pow(s24,3)+14*pow(s24,4))+s23*(-((33*s23*s24+102*(s23*s23)+14*(s24*s24))*pow(s14,3))-2*(s23-8*s24)*pow(s14,4)-2*(s14*s14)*(73*s24*(s23*s23)+42*s23*(s24*s24)+55*pow(s23,3)+10*pow(s24,3))+s14*s23*(91*s24*(s23*s23)+69*s23*(s24*s24)+49*pow(s23,3)+19*pow(s24,3))+2*s23*(34*(s23*s23)*(s24*s24)+29*s24*pow(s23,3)+10*pow(s23,4)+19*s23*pow(s24,3)+4*pow(s24,4)))+s13*(3*(48*s23*s24+39*(s23*s23)-s24*s24)*pow(s14,3)+(84*s23+26*s24)*pow(s14,4)+6*pow(s14,5)-s14*s14*(-30*s24*(s23*s23)+109*s23*(s24*s24)+16*pow(s23,3)+22*pow(s24,3))+s14*(218*(s23*s23)*(s24*s24)+546*s24*pow(s23,3)+257*pow(s23,4)-s23*pow(s24,3)-9*pow(s24,4))+s23*(446*(s23*s23)*(s24*s24)+426*s24*pow(s23,3)+133*pow(s23,4)+198*s23*pow(s24,3)+31*pow(s24,4))))+s12*((62*s23*s24+s14*(19*s23+10*s24)+20*(s23*s23)+17*(s24*s24))*pow(s13,6)+(4*s23+3*s24)*pow(s13,7)+pow(s13,5)*((42*s23+13*s24)*(s14*s14)+287*s24*(s23*s23)+177*s23*(s24*s24)+s14*(135*s23*s24+101*(s23*s23)+10*(s24*s24))+44*pow(s23,3)+32*pow(s24,3))+s14*pow(s23,3)*(30*(s23*s23)*(s24*s24)-s14*s14*(29*s23*s24+37*(s23*s23)+10*(s24*s24))+(-7*s23+16*s24)*pow(s14,3)+24*s24*pow(s23,3)+8*pow(s23,4)+18*s23*pow(s24,3)+s14*(22*s24*(s23*s23)+22*s23*(s24*s24)+8*pow(s23,3)+8*pow(s24,3))+4*pow(s24,4))+pow(s13,4)*(s14*s14*(90*s23*s24+184*(s23*s23)-29*(s24*s24))+526*(s23*s23)*(s24*s24)+3*(19*s23+6*s24)*pow(s14,3)+531*s24*pow(s23,3)+56*pow(s23,4)+166*s23*pow(s24,3)+s14*(584*s24*(s23*s23)+178*s23*(s24*s24)+207*pow(s23,3)+15*pow(s24,3))+9*pow(s24,4))+s13*(s23*s23)*((96*s23*s24-11*(s23*s23)+13*(s24*s24))*pow(s14,3)+26*(2*s23+3*s24)*pow(s14,4)+18*pow(s14,5)+s14*s14*(232*s24*(s23*s23)+207*s23*(s24*s24)+37*pow(s23,3)+70*pow(s24,3))+2*s23*(25*(s23*s23)*(s24*s24)+12*s24*pow(s23,3)+2*pow(s23,4)+23*s23*pow(s24,3)+8*pow(s24,4))+s14*(286*(s23*s23)*(s24*s24)+219*s24*pow(s23,3)+54*pow(s23,4)+154*s23*pow(s24,3)+27*pow(s24,4)))+pow(s13,3)*((87*s23*s24+194*(s23*s23)-11*(s24*s24))*pow(s14,3)+6*(7*s23+2*s24)*pow(s14,4)+s14*s14*(401*s24*(s23*s23)-10*s23*(s24*s24)+259*pow(s23,3)-4*pow(s24,3))+s14*(645*(s23*s23)*(s24*s24)+966*s24*pow(s23,3)+225*pow(s23,4)+131*s23*pow(s24,3)-11*pow(s24,4))+s23*(635*(s23*s23)*(s24*s24)+458*s24*pow(s23,3)+44*pow(s23,4)+320*s23*pow(s24,3)+53*pow(s24,4)))+s23*(s13*s13)*((211*s23*s24+159*(s23*s23)-6*(s24*s24))*pow(s14,3)+2*(53*s23+28*s24)*pow(s14,4)+12*pow(s14,5)+s14*s14*(522*s24*(s23*s23)+225*s23*(s24*s24)+146*pow(s23,3)+52*pow(s24,3))+s14*(721*(s23*s23)*(s24*s24)+702*s24*pow(s23,3)+146*pow(s23,4)+263*s23*pow(s24,3)+12*pow(s24,4))+s23*(319*(s23*s23)*(s24*s24)+179*s24*pow(s23,3)+20*pow(s23,4)+228*s23*pow(s24,3)+62*pow(s24,4))))+s12*s12*((272*s23*s24+3*s14*(46*s23+19*s24)+20*(s14*s14)+144*(s23*s23)+61*(s24*s24))*pow(s13,5)+(8*s14+39*s23+24*s24)*pow(s13,6)+2*pow(s13,7)+pow(s13,4)*((233*s23+70*s24)*(s14*s14)+869*s24*(s23*s23)+441*s23*(s24*s24)+s14*(463*s23*s24+449*(s23*s23)+11*(s24*s24))+30*pow(s14,3)+246*pow(s23,3)+58*pow(s24,3))+s23*s23*(2*s23*(3*s23*s24+2*(s23*s23)+2*(s24*s24))*((s23+s24)*(s23+s24))-(49*s23*s24+88*(s23*s23)+18*(s24*s24))*pow(s14,3)-8*(s23-3*s24)*pow(s14,4)-s14*s14*(34*s24*(s23*s23)+19*s23*(s24*s24)+27*pow(s23,3)+6*pow(s24,3))+s14*(90*(s23*s23)*(s24*s24)+86*s24*pow(s23,3)+34*pow(s23,4)+42*s23*pow(s24,3)+6*pow(s24,4)))+pow(s13,3)*(s14*s14*(313*s23*s24+521*(s23*s23)-49*(s24*s24))+3*(80*s23+27*s24)*pow(s14,3)+22*pow(s14,4)+s14*(1246*s24*(s23*s23)+270*s23*(s24*s24)+612*pow(s23,3)-7*pow(s24,3))+2*(487*(s23*s23)*(s24*s24)+577*s24*pow(s23,3)+119*pow(s23,4)+134*s23*pow(s24,3)+8*pow(s24,4)))+s13*s23*((164*s23*s24+37*(s23*s23)-s24*s24)*pow(s14,3)+6*(17*s23+13*s24)*pow(s14,4)+18*pow(s14,5)+s14*s14*(218*s24*(s23*s23)+59*s23*(s24*s24)+16*pow(s23,3)+5*pow(s24,3))+s14*(500*(s23*s23)*(s24*s24)+566*s24*pow(s23,3)+176*pow(s23,4)+157*s23*pow(s24,3)+6*pow(s24,4))+s23*(263*(s23*s23)*(s24*s24)+176*s24*pow(s23,3)+40*pow(s23,4)+172*s23*pow(s24,3)+43*pow(s24,4)))+s13*s13*((244*s23*s24+341*(s23*s23)-2*(s24*s24))*pow(s14,3)+2*(61*s23+17*s24)*pow(s14,4)+6*pow(s14,5)+s14*s14*(528*s24*(s23*s23)-33*s23*(s24*s24)+349*pow(s23,3)-15*pow(s24,3))+s14*(717*(s23*s23)*(s24*s24)+1316*s24*pow(s23,3)+435*pow(s23,4)+94*s23*pow(s24,3)-15*pow(s24,4))+s23*(835*(s23*s23)*(s24*s24)+695*s24*pow(s23,3)+135*pow(s23,4)+389*s23*pow(s24,3)+63*pow(s24,4)))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/8.;
}

// Coefficient order epsilon^2 of master 10
template<>
double qq2yygLC<10,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s23,-1)*pow(s12+s13+s23,-3)*pow(s24,-1)*(2*(82*s23*s24+2*s14*(64*s23+33*s24)+s13*(118*s14+158*s23+101*s24)+82*(s13*s13)+43*(s14*s14)+71*(s23*s23)+22*(s24*s24))*pow(s12,7)+(62*s13+46*s14+58*s23+34*s24)*pow(s12,8)+10*pow(s12,9)+(2*s14*(9*s23*s24+s23*s23+5*(s24*s24))+s24*(24*s23*s24+10*(s23*s23)+15*(s24*s24)))*pow(s13,6)+s24*(2*s23+3*s24)*pow(s13,7)+2*(s14*s14)*pow(s23,4)*((10*s23+7*s24)*(s14*s14)+s24*(s23*s23)+s23*(s24*s24)+6*s14*((s23+s24)*(s23+s24))+3*pow(s14,3)+pow(s23,3)+pow(s24,3))+pow(s12,6)*((496*s14+720*s23+511*s24)*(s13*s13)+s13*(852*s23*s24+s14*(1142*s23+651*s24)+344*(s14*s14)+666*(s23*s23)+254*(s24*s24))+240*pow(s13,3)+2*((227*s23+95*s24)*(s14*s14)+160*s24*(s23*s23)+86*s23*(s24*s24)+s14*(306*s23*s24+298*(s23*s23)+69*(s24*s24))+40*pow(s14,3)+95*pow(s23,3)+13*pow(s24,3)))+pow(s13,5)*(s14*s14*(13*s23*s24+9*(s23*s23)-7*(s24*s24))+s24*(64*s24*(s23*s23)+65*s23*(s24*s24)+20*pow(s23,3)+10*pow(s24,3))+s14*(76*s24*(s23*s23)+73*s23*(s24*s24)+10*pow(s23,3)+25*pow(s24,3)))+pow(s12,5)*(s13*s13*(1841*s23*s24+s14*(2040*s23+1313*s24)+522*(s14*s14)+1268*(s23*s23)+618*(s24*s24))+(544*s14+882*s23+711*s24)*pow(s13,3)+210*pow(s13,4)+s13*(4*(391*s23+180*s24)*(s14*s14)+1426*s24*(s23*s23)+869*s23*(s24*s24)+s14*(2565*s23*s24+2253*(s23*s23)+652*(s24*s24))+222*pow(s14,3)+746*pow(s23,3)+148*pow(s24,3))+2*(128*(s23*s23)*(s24*s24)+s14*s14*(416*s23*s24+495*(s23*s23)+70*(s24*s24))+(198*s23+59*s24)*pow(s14,3)+18*pow(s14,4)+160*s24*pow(s23,3)+75*pow(s23,4)+40*s23*pow(s24,3)+s14*(569*s24*(s23*s23)+260*s23*(s24*s24)+374*pow(s23,3)+31*pow(s24,3))+3*pow(s24,4)))+s13*s14*pow(s23,3)*(2*(s14*s14)*(25*s23*s24+31*(s23*s23)-9*(s24*s24))+8*(6*s23-s24)*pow(s14,3)-2*pow(s14,4)+s14*(33*s24*(s23*s23)+6*s23*(s24*s24)+19*pow(s23,3)-6*pow(s24,3))+2*(13*(s23*s23)*(s24*s24)+4*s24*pow(s23,3)+pow(s23,4)+17*s23*pow(s24,3)+7*pow(s24,4)))+pow(s13,4)*((3*s23*s24+9*(s23*s23)-14*(s24*s24))*pow(s14,3)+s14*s14*(33*s24*(s23*s23)-6*s23*(s24*s24)+44*pow(s23,3)-2*pow(s24,3))+s23*s24*(82*s24*(s23*s23)+106*s23*(s24*s24)+20*pow(s23,3)+31*pow(s24,3))+2*s14*(85*(s23*s23)*(s24*s24)+68*s24*pow(s23,3)+10*pow(s23,4)+56*s23*pow(s24,3)+7*pow(s24,4)))+s23*pow(s13,3)*(s23*(s14*s14)*(65*s23*s24+76*(s23*s23)-22*(s24*s24))+(-31*s23*s24+59*(s23*s23)-28*(s24*s24))*pow(s14,3)-6*(s23-s24)*pow(s14,4)+s23*s24*(53*s24*(s23*s23)+80*s23*(s24*s24)+10*pow(s23,3)+35*pow(s24,3))+2*s14*(98*(s23*s23)*(s24*s24)+62*s24*pow(s23,3)+10*pow(s23,4)+89*s23*pow(s24,3)+21*pow(s24,4)))+s13*s13*(s23*s23)*(4*(-4*s23*s24+25*(s23*s23)-11*(s24*s24))*pow(s14,3)+4*(5*s23-4*s24)*pow(s14,4)-8*pow(s14,5)+s14*s14*(76*s24*(s23*s23)-31*s23*(s24*s24)+58*pow(s23,3)-6*pow(s24,3))+2*s23*s24*(7*s24*(s23*s23)+12*s23*(s24*s24)+pow(s23,3)+6*pow(s24,3))+s14*(115*(s23*s23)*(s24*s24)+54*s24*pow(s23,3)+10*pow(s23,4)+117*s23*pow(s24,3)+42*pow(s24,4)))+pow(s12,4)*((2112*s23*s24+s14*(1843*s23+1376*s24)+362*(s14*s14)+1240*(s23*s23)+815*(s24*s24))*pow(s13,3)+(326*s14+618*s23+584*s24)*pow(s13,4)+110*pow(s13,5)+s13*s13*(13*(154*s23+79*s24)*(s14*s14)+2554*s24*(s23*s23)+1793*s23*(s24*s24)+s14*(4263*s23*s24+3293*(s23*s23)+1237*(s24*s24))+188*pow(s14,3)+1136*pow(s23,3)+347*pow(s24,3))+s13*(1121*(s23*s23)*(s24*s24)+s14*s14*(2566*s23*s24+2843*(s23*s23)+494*(s24*s24))+3*(314*s23+99*s24)*pow(s14,3)+50*pow(s14,4)+1196*s24*pow(s23,3)+474*pow(s23,4)+404*s23*pow(s24,3)+s14*(3923*s24*(s23*s23)+2039*s23*(s24*s24)+2310*pow(s23,3)+283*pow(s24,3))+34*pow(s24,4))+2*((238*s23*s24+398*(s23*s23)+23*(s24*s24))*pow(s14,3)+(82*s23+13*s24)*pow(s14,4)+3*pow(s14,5)+s14*s14*(721*s24*(s23*s23)+246*s23*(s24*s24)+571*pow(s23,3)+18*pow(s24,3))+s14*(370*(s23*s23)*(s24*s24)+534*s24*pow(s23,3)+271*pow(s23,4)+92*s23*pow(s24,3)+5*pow(s24,4))+s23*(88*(s23*s23)*(s24*s24)+85*s24*pow(s23,3)+35*pow(s23,4)+43*s23*pow(s24,3)+7*pow(s24,4))))+pow(s12,3)*((1352*s23*s24+s14*(869*s23+780*s24)+104*(s14*s14)+642*(s23*s23)+620*(s24*s24))*pow(s13,4)+20*(5*s14+12*s23+14*s24)*pow(s13,5)+32*pow(s13,6)+pow(s13,3)*((1154*s23+659*s24)*(s14*s14)+2298*s24*(s23*s23)+1909*s23*(s24*s24)+s14*(3524*s23*s24+2308*(s23*s23)+1189*(s24*s24))+30*pow(s14,3)+824*pow(s23,3)+428*pow(s24,3))+s13*s13*(1903*(s23*s23)*(s24*s24)+s14*s14*(2824*s23*s24+2920*(s23*s23)+627*(s24*s24))+(694*s23+219*s24)*pow(s14,3)-8*pow(s14,4)+1704*s24*pow(s23,3)+540*pow(s23,4)+797*s23*pow(s24,3)+s14*(5114*s24*(s23*s23)+3072*s23*(s24*s24)+2609*pow(s23,3)+502*pow(s24,3))+76*pow(s24,4))+2*s23*(4*(93*s23*s24+103*(s23*s23)+18*(s24*s24))*pow(s14,3)+2*(74*s23+23*s24)*pow(s14,4)+12*pow(s14,5)+s14*s14*(618*s24*(s23*s23)+321*s23*(s24*s24)+369*pow(s23,3)+48*pow(s24,3))+s23*(26*(s23*s23)*(s24*s24)+22*s24*pow(s23,3)+9*pow(s23,4)+18*s23*pow(s24,3)+5*pow(s24,4))+s14*(239*(s23*s23)*(s24*s24)+258*s24*pow(s23,3)+112*pow(s23,4)+94*s23*pow(s24,3)+11*pow(s24,4)))+s13*((893*s23*s24+1556*(s23*s23)+102*(s24*s24))*pow(s14,3)+2*(99*s23+14*s24)*pow(s14,4)-2*pow(s14,5)+3*(s14*s14)*(1145*s24*(s23*s23)+436*s23*(s24*s24)+868*pow(s23,3)+38*pow(s24,3))+s14*(2318*(s23*s23)*(s24*s24)+2891*s24*pow(s23,3)+1294*pow(s23,4)+696*s23*pow(s24,3)+44*pow(s24,4))+s23*(675*(s23*s23)*(s24*s24)+522*s24*pow(s23,3)+168*pow(s23,4)+396*s23*pow(s24,3)+73*pow(s24,4))))+s12*((67*s23*s24+s14*(15*s23+23*s24)+12*(s23*s23)+54*(s24*s24))*pow(s13,6)+(2*s23+7*s24)*pow(s13,7)+pow(s13,5)*((26*s23+5*s24)*(s14*s14)+208*s24*(s23*s23)+290*s23*(s24*s24)+s14*(281*s23*s24+101*(s23*s23)+139*(s24*s24))+30*pow(s23,3)+104*pow(s24,3))+2*s14*pow(s23,3)*(3*(s23*s23)*(s24*s24)+s14*s14*(97*s23*s24+62*(s23*s23)+38*(s24*s24))+(58*s23+34*s24)*pow(s14,3)+12*pow(s14,4)+3*s24*pow(s23,3)+2*pow(s23,4)+3*s23*pow(s24,3)+s14*(46*s24*(s23*s23)+41*s23*(s24*s24)+21*pow(s23,3)+14*pow(s24,3))+pow(s24,4))+pow(s13,4)*(557*(s23*s23)*(s24*s24)+s14*s14*(215*s23*s24+224*(s23*s23)+45*(s24*s24))+(s23-21*s24)*pow(s14,3)+302*s24*pow(s23,3)+40*pow(s23,4)+359*s23*pow(s24,3)+2*s14*(413*s24*(s23*s23)+350*s23*(s24*s24)+125*pow(s23,3)+86*pow(s24,3))+46*pow(s24,4))+s13*(s23*s23)*((399*s23*s24+462*(s23*s23)+68*(s24*s24))*pow(s14,3)+2*(97*s23+6*s24)*pow(s14,4)-6*pow(s14,5)+s14*s14*(529*s24*(s23*s23)+328*s23*(s24*s24)+272*pow(s23,3)+88*pow(s24,3))+2*s23*(12*(s23*s23)*(s24*s24)+5*s24*pow(s23,3)+pow(s23,4)+15*s23*pow(s24,3)+7*pow(s24,4))+s14*(248*(s23*s23)*(s24*s24)+152*s24*pow(s23,3)+49*pow(s23,4)+203*s23*pow(s24,3)+56*pow(s24,4)))+s23*(s13*s13)*((181*s23*s24+518*(s23*s23)+16*(s24*s24))*pow(s14,3)+8*(4*s23-3*s24)*pow(s14,4)-16*pow(s14,5)+s14*s14*(920*s24*(s23*s23)+457*s23*(s24*s24)+598*pow(s23,3)+112*pow(s24,3))+s23*(195*(s23*s23)*(s24*s24)+79*s24*pow(s23,3)+12*pow(s23,4)+214*s23*pow(s24,3)+80*pow(s24,4))+s14*(871*(s23*s23)*(s24*s24)+635*s24*pow(s23,3)+179*pow(s23,4)+574*s23*pow(s24,3)+106*pow(s24,4)))+pow(s13,3)*(2*(2*s23*s24+89*(s23*s23)-9*(s24*s24))*pow(s14,3)-2*(14*s23+5*s24)*pow(s14,4)+s14*s14*(684*s24*(s23*s23)+295*s23*(s24*s24)+566*pow(s23,3)+38*pow(s24,3))+s14*(1181*(s23*s23)*(s24*s24)+1057*s24*pow(s23,3)+298*pow(s23,4)+552*s23*pow(s24,3)+52*pow(s24,4))+s23*(492*(s23*s23)*(s24*s24)+223*s24*pow(s23,3)+30*pow(s23,4)+436*s23*pow(s24,3)+107*pow(s24,4))))+s12*s12*((458*s23*s24+13*s14*(15*s23+17*s24)+6*(s14*s14)+158*(s23*s23)+264*(s24*s24))*pow(s13,5)+(12*s14+44*s23+71*s24)*pow(s13,6)+4*pow(s13,7)+pow(s13,4)*((288*s23+167*s24)*(s14*s14)+1048*s24*(s23*s23)+1079*s23*(s24*s24)+s14*(1477*s23*s24+771*(s23*s23)+595*(s24*s24))-16*pow(s14,3)+274*pow(s23,3)+292*pow(s24,3))+pow(s13,3)*(1531*(s23*s23)*(s24*s24)+s14*s14*(1292*s23*s24+1282*(s23*s23)+325*(s24*s24))+(149*s23+19*s24)*pow(s14,3)-22*pow(s14,4)+1110*s24*pow(s23,3)+256*pow(s23,4)+767*s23*pow(s24,3)+s14*(3079*s24*(s23*s23)+2180*s23*(s24*s24)+1287*pow(s23,3)+428*pow(s24,3))+84*pow(s24,4))+2*(s23*s23)*(s23*(s23*s23+s24*s24)*((s23+s24)*(s23+s24))+(278*s23*s24+228*(s23*s23)+81*(s24*s24))*pow(s14,3)+12*(11*s23+5*s24)*pow(s14,4)+18*pow(s14,5)+s14*s14*(263*s24*(s23*s23)+185*s23*(s24*s24)+129*pow(s23,3)+43*pow(s24,3))+s14*(63*(s23*s23)*(s24*s24)+56*s24*pow(s23,3)+24*pow(s23,4)+36*s23*pow(s24,3)+7*pow(s24,4)))+s13*s23*((945*s23*s24+1236*(s23*s23)+188*(s24*s24))*pow(s14,3)+6*(49*s23+8*s24)*pow(s14,4)-6*pow(s14,5)+s14*s14*(2085*s24*(s23*s23)+1136*s23*(s24*s24)+1234*pow(s23,3)+208*pow(s24,3))+s23*(193*(s23*s23)*(s24*s24)+112*s24*pow(s23,3)+30*pow(s23,4)+170*s23*pow(s24,3)+55*pow(s24,4))+s14*(1153*(s23*s23)*(s24*s24)+1026*s24*pow(s23,3)+378*pow(s23,4)+584*s23*pow(s24,3)+86*pow(s24,4)))+s13*s13*((416*s23*s24+924*(s23*s23)+52*(s24*s24))*pow(s14,3)+4*(s23-2*s24)*pow(s14,4)-8*pow(s14,5)+s14*s14*(2641*s24*(s23*s23)+1117*s23*(s24*s24)+1980*pow(s23,3)+118*pow(s24,3))+s14*(2589*(s23*s23)*(s24*s24)+2745*s24*pow(s23,3)+1029*pow(s23,4)+952*s23*pow(s24,3)+72*pow(s24,4))+s23*(909*(s23*s23)*(s24*s24)+557*s24*pow(s23,3)+128*pow(s23,4)+640*s23*pow(s24,3)+135*pow(s24,4)))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/8.;
}

// Coefficient of master 10 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygLC<10>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygLC<10,0>(s12,s13,s14,s23,s24),
        qq2yygLC<10,1>(s12,s13,s14,s23,s24),
        qq2yygLC<10,2>(s12,s13,s14,s23,s24)
    });
}


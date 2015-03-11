/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 4: bubble(s23)

// Coefficient order epsilon^-1 of master 4
template<>
double qq2yyg6CF<4,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s23,-1)*pow(s12+s13+s14+s23,-3)*(s23*(s23+s24)*((s23-s24)*(s14*s14)+2*(s23+s24)*(s23*s23)+s14*(s23*s24+4*(s23*s23)-2*(s24*s24)))-(s13*(-5*s23+2*s24)+2*(s14*(-3*s23+s24)-5*(s23*s23)+s24*s24))*pow(s12,3)+(2*s23-s24)*pow(s12,4)+s23*(2*s23+3*s24)*pow(s13,3)+s13*(s23*(s23+2*s24)*(s14*s14)+s23*(10*s24*(s23*s23)+3*s23*(s24*s24)+6*pow(s23,3)-pow(s24,3))+s14*(9*s24*(s23*s23)+s23*(s24*s24)+7*pow(s23,3)-pow(s24,3)))+s12*(2*(s13*s13)*(2*s14*s23+4*s23*s24+6*(s23*s23)-s24*s24)+s14*s14*(s23*s24+7*(s23*s23)-s24*s24)+s23*pow(s13,3)+2*s23*pow(s14,3)+s13*(5*s23*(s14*s14)+20*s24*(s23*s23)+s14*(8*s23*s24+18*(s23*s23)-3*(s24*s24))-s23*(s24*s24)+21*pow(s23,3)-2*pow(s24,3))+s23*(13*s24*(s23*s23)+2*s23*(s24*s24)+10*pow(s23,3)-pow(s24,3))+s14*(9*s24*(s23*s23)-5*s23*(s24*s24)+15*pow(s23,3)-pow(s24,3)))+s13*s13*(s14*s23*(3*s23+5*s24)+9*s24*(s23*s23)+2*s23*(s24*s24)+6*pow(s23,3)-pow(s24,3))-s12*s12*((-4*s23+s24)*(s13*s13)+(-6*s23+s24)*(s14*s14)-10*s24*(s23*s23)-s14*(s23*s24+17*(s23*s23)-3*(s24*s24))+3*s23*(s24*s24)+s13*(-5*s23*s24+2*s14*(-5*s23+s24)-20*(s23*s23)+4*(s24*s24))-16*pow(s23,3)+pow(s24,3)))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1));
}

// Coefficient order epsilon^0 of master 4
template<>
double qq2yyg6CF<4,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s13+s14+s23,-3)*pow(s24,-2)*(s24*(2*s14*(5*s14+5*s23+2*s24)+s13*(43*s14+30*s23+17*s24)+30*(s13*s13))*pow(s12,7)+2*(3*s13+s14)*s24*pow(s12,8)+pow(s12,6)*(s24*(155*s14+126*s23+78*s24)*(s13*s13)+s14*s24*(17*s23*s24+2*s14*(22*s23+9*s24)+20*(s14*s14)+17*(s23*s23)+2*(s24*s24))+s13*(115*s24*(s14*s14)+s24*(71*s23*s24+60*(s23*s23)+16*(s24*s24))+s14*(182*s23*s24-2*(s23*s23)+103*(s24*s24)))+60*s24*pow(s13,3))+s24*(s14*s14*(-9*s23*s24+3*(s23*s23)-43*(s24*s24))+s23*s24*(14*s23*s24+16*(s23*s23)+s24*s24)-3*s14*s24*(14*s23*s24+2*(s23*s23)+3*(s24*s24)))*pow(s13,5)-s24*s24*(s14*(4*s23+13*s24)-4*(s23*s23)+s24*s24)*pow(s13,6)-pow(s13,7)*pow(s24,3)+s24*pow(s13,4)*(s24*(s23*s23)*(32*s23*s24+24*(s23*s23)+9*(s24*s24))+(5*s23*s24+12*(s23*s23)-63*(s24*s24))*pow(s14,3)+s14*s14*(-17*s24*(s23*s23)-132*s23*(s24*s24)+10*pow(s23,3)-21*pow(s24,3))+2*s14*s24*(-19*s24*(s23*s23)-9*s23*(s24*s24)+3*pow(s23,3)+3*pow(s24,3)))+s23*(s14+s23)*s24*(s14*s14)*(s24*(s23*s23)*(5*s23*s24+2*(s23*s23)+3*(s24*s24))+s23*(s14*s14)*(5*s23*s24-2*(s23*s23)+6*(s24*s24))+2*s24*(3*s23+s24)*pow(s14,3)+2*s24*pow(s14,4)+s14*s23*(s24*(s23*s23)+9*s23*(s24*s24)-3*pow(s23,3)+4*pow(s24,3)))+pow(s12,5)*(s13*s13*(300*s24*(s14*s14)+s24*(271*s23*s24+204*(s23*s23)+67*(s24*s24))+s14*(535*s23*s24-8*(s23*s23)+345*(s24*s24)))+s24*(230*s14+204*s23+141*s24)*pow(s13,3)+60*s24*pow(s13,4)+s14*s24*((76*s23+34*s24)*(s14*s14)+s14*(68*s23*s24+65*(s23*s23)+10*(s24*s24))+s23*(23*s23*s24+8*(s23*s23)+12*(s24*s24))+20*pow(s14,3))+s13*(s14*s14*(410*s23*s24-8*(s23*s23)+236*(s24*s24))+150*s24*pow(s14,3)+s24*(112*s24*(s23*s23)+54*s23*(s24*s24)+60*pow(s23,3)+5*pow(s24,3))+s14*(287*s24*(s23*s23)+337*s23*(s24*s24)-8*pow(s23,3)+83*pow(s24,3))))+pow(s13,3)*(s24*(31*s23*s24+18*(s23*s23)-46*(s24*s24))*pow(s14,4)+s24*s24*(27*s23*s24+16*(s23*s23)+11*(s24*s24))*pow(s23,3)+s14*s23*(s24*s24)*(4*s24*(s23*s23)-3*s23*(s24*s24)+18*pow(s23,3)+8*pow(s24,3))+pow(s14,3)*(23*(s23*s23)*(s24*s24)+24*s24*pow(s23,3)-2*pow(s23,4)-148*s23*pow(s24,3)-19*pow(s24,4))+s24*(s14*s14)*(-125*(s23*s23)*(s24*s24)-14*s24*pow(s23,3)+10*pow(s23,4)-41*s23*pow(s24,3)+14*pow(s24,4)))+pow(s12,4)*((310*s24*(s14*s14)+s24*(392*s23*s24+252*(s23*s23)+107*(s24*s24))+s14*(616*s23*s24-12*(s23*s23)+454*(s24*s24)))*pow(s13,3)+4*s24*(40*s14+39*s23+31*s24)*pow(s13,4)+30*s24*pow(s13,5)+s14*s24*(s14*s14*(114*s23*s24+95*(s23*s23)+22*(s24*s24))+4*(16*s23+9*s24)*pow(s14,3)+10*pow(s14,4)+s14*(84*s24*(s23*s23)+50*s23*(s24*s24)+25*pow(s23,3)+2*pow(s24,3))+s23*(9*s24*(s23*s23)+26*s23*(s24*s24)-8*pow(s23,3)+9*pow(s24,3)))+s13*s13*(s14*s14*(840*s23*s24-24*(s23*s23)+564*(s24*s24))+270*s24*pow(s14,3)+s24*(350*s24*(s23*s23)+186*s23*(s24*s24)+156*pow(s23,3)+19*pow(s24,3))+s14*(666*s24*(s23*s23)+877*s23*(s24*s24)-24*pow(s23,3)+245*pow(s24,3)))+s13*(-6*(-74*s23*s24+2*(s23*s23)-45*(s24*s24))*pow(s14,3)+100*s24*pow(s14,4)+s23*s24*(78*s24*(s23*s23)+62*s23*(s24*s24)+30*pow(s23,3)+13*pow(s24,3))+s14*s14*(512*s24*(s23*s23)+601*s23*(s24*s24)-28*pow(s23,3)+165*pow(s24,3))+s14*(367*(s23*s23)*(s24*s24)+198*s24*pow(s23,3)-12*pow(s23,4)+214*s23*pow(s24,3)+29*pow(s24,4))))+s13*s13*(s24*(33*s23*s24+12*(s23*s23)-16*(s24*s24))*pow(s14,5)+4*(s24*s24)*((s23+s24)*(s23+s24))*pow(s23,4)+s14*(s23*s23)*(s24*s24)*(23*s24*(s23*s23)+11*s23*(s24*s24)+14*pow(s23,3)+3*pow(s24,3))+pow(s14,4)*(61*(s23*s23)*(s24*s24)+18*s24*pow(s23,3)-4*pow(s23,4)-66*s23*pow(s24,3)-6*pow(s24,4))+s23*s24*(s14*s14)*(-27*(s23*s23)*(s24*s24)-s24*pow(s23,3)+2*pow(s23,4)+s23*pow(s24,3)+22*pow(s24,4))+pow(s14,3)*(18*(s24*s24)*pow(s23,3)+7*s24*pow(s23,4)-4*pow(s23,5)-76*(s23*s23)*pow(s24,3)-24*s23*pow(s24,4)+10*pow(s24,5)))+s13*s14*(s24*(14*s23*s24+3*(s23*s23)-2*(s24*s24))*pow(s14,5)+s23*pow(s14,4)*(4*s24*(s23*s23)+39*s23*(s24*s24)-2*pow(s23,3)-6*pow(s24,3))+s24*s24*pow(s23,3)*(10*s24*(s23*s23)+7*s23*(s24*s24)+4*pow(s23,3)+pow(s24,3))+s14*s24*(s23*s23)*(18*(s23*s23)*(s24*s24)+7*s24*pow(s23,3)-pow(s23,4)+13*s23*pow(s24,3)+5*pow(s24,4))+pow(s14,3)*(33*(s24*s24)*pow(s23,3)-5*s24*pow(s23,4)-4*pow(s23,5)+5*(s23*s23)*pow(s24,3)-2*s23*pow(s24,4)+2*pow(s24,5))+s23*(s14*s14)*(7*(s24*s24)*pow(s23,3)-8*s24*pow(s23,4)-2*pow(s23,5)+15*(s23*s23)*pow(s24,3)+17*s23*pow(s24,4)+14*pow(s24,5)))+pow(s12,3)*(2*(65*s24*(s14*s14)+s24*(130*s23*s24+66*(s23*s23)+39*(s24*s24))+s14*(154*s23*s24-4*(s23*s23)+131*(s24*s24)))*pow(s13,4)+s24*(47*s14+54*s23+51*s24)*pow(s13,5)+6*s24*pow(s13,6)+pow(s13,3)*(s14*s14*(644*s23*s24-24*(s23*s23)+492*(s24*s24))+170*s24*pow(s14,3)+2*s24*(200*s24*(s23*s23)+117*s23*(s24*s24)+66*pow(s23,3)+13*pow(s24,3))+s14*(554*s24*(s23*s23)+829*s23*(s24*s24)-24*pow(s23,3)+244*pow(s24,3)))+s14*s24*((106*s23*s24+65*(s23*s23)+26*(s24*s24))*pow(s14,3)+(26*s23+24*s24)*pow(s14,4)+2*pow(s14,5)+s14*s14*(129*s24*(s23*s23)+84*s23*(s24*s24)+28*pow(s23,3)+6*pow(s24,3))+s14*s23*(37*s24*(s23*s23)+89*s23*(s24*s24)-25*pow(s23,3)+30*pow(s24,3))+s23*(31*(s23*s23)*(s24*s24)-s24*pow(s23,3)-10*pow(s23,4)+25*s23*pow(s24,3)+4*pow(s24,4)))+s13*s13*((606*s23*s24-24*(s23*s23)+426*(s24*s24))*pow(s14,3)+110*s24*pow(s14,4)+2*s23*s24*(104*s24*(s23*s23)+91*s23*(s24*s24)+27*pow(s23,3)+20*pow(s24,3))+s14*s14*(789*s24*(s23*s23)+999*s23*(s24*s24)-60*pow(s23,3)+302*pow(s24,3))+s14*(700*(s23*s23)*(s24*s24)+350*s24*pow(s23,3)-24*pow(s23,4)+426*s23*pow(s24,3)+78*pow(s24,4)))+s13*((242*s23*s24-8*(s23*s23)+169*(s24*s24))*pow(s14,4)+31*s24*pow(s14,5)+s24*(s23*s23)*(17*s24*(s23*s23)+22*s23*(s24*s24)+6*pow(s23,3)+9*pow(s24,3))+pow(s14,3)*(432*s24*(s23*s23)+535*s23*(s24*s24)-36*pow(s23,3)+161*pow(s24,3))+s14*s14*(456*(s23*s23)*(s24*s24)+238*s24*pow(s23,3)-36*pow(s23,4)+311*s23*pow(s24,3)+59*pow(s24,4))+s14*(132*(s24*s24)*pow(s23,3)+47*s24*pow(s23,4)-8*pow(s23,5)+167*(s23*s23)*pow(s24,3)+75*s23*pow(s24,4)+6*pow(s24,5))))+s12*s12*((15*s24*(s14*s14)+s24*(73*s23*s24+24*(s23*s23)+22*(s24*s24))+s14*(58*s23*s24-2*(s23*s23)+51*(s24*s24)))*pow(s13,5)+3*s24*(s14+2*(s23+s24))*pow(s13,6)+pow(s13,4)*(s14*s14*(188*s23*s24-8*(s23*s23)+146*(s24*s24))+30*s24*pow(s14,3)+2*s24*(101*s24*(s23*s23)+63*s23*(s24*s24)+18*pow(s23,3)+7*pow(s24,3))+s14*(173*s24*(s23*s23)+286*s23*(s24*s24)-8*pow(s23,3)+62*pow(s24,3)))+s14*s24*((59*s23*s24+20*(s23*s23)+16*(s24*s24))*pow(s14,4)+2*(2*s23+5*s24)*pow(s14,5)+pow(s14,3)*(106*s24*(s23*s23)+68*s23*(s24*s24)+13*pow(s23,3)+6*pow(s24,3))+s23*(s14*s14)*(59*s24*(s23*s23)+110*s23*(s24*s24)-28*pow(s23,3)+33*pow(s24,3))+s23*s23*(22*(s23*s23)*(s24*s24)+2*s24*pow(s23,3)-3*pow(s23,4)+24*s23*pow(s24,3)+7*pow(s24,4))+s14*s23*(80*(s23*s23)*(s24*s24)+6*s24*pow(s23,3)-25*pow(s23,4)+55*s23*pow(s24,3)+8*pow(s24,4)))+pow(s13,3)*((280*s23*s24-12*(s23*s23)+202*(s24*s24))*pow(s14,3)+30*s24*pow(s14,4)+6*s23*s24*(34*s24*(s23*s23)+32*s23*(s24*s24)+4*pow(s23,3)+7*pow(s24,3))+s14*s14*(413*s24*(s23*s23)+512*s23*(s24*s24)-36*pow(s23,3)+106*pow(s24,3))+s14*(421*(s23*s23)*(s24*s24)+186*s24*pow(s23,3)-12*pow(s23,4)+216*s23*pow(s24,3)+60*pow(s24,4)))+s13*((62*s23*s24-2*(s23*s23)+59*(s24*s24))*pow(s14,5)+3*s24*pow(s14,6)-s24*s24*(6*s23*s24+5*(s23*s23)+s24*s24)*pow(s23,3)+pow(s14,4)*(179*s24*(s23*s23)+274*s23*(s24*s24)-20*pow(s23,3)+77*pow(s24,3))+pow(s14,3)*(312*(s23*s23)*(s24*s24)+128*s24*pow(s23,3)-38*pow(s23,4)+198*s23*pow(s24,3)+51*pow(s24,4))+s14*s14*(76*(s24*s24)*pow(s23,3)+4*s24*pow(s23,4)-20*pow(s23,5)+167*(s23*s23)*pow(s24,3)+121*s23*pow(s24,4)+14*pow(s24,5))+s14*s23*(-4*s24*pow(s23,4)-2*pow(s23,5)+48*(s23*s23)*pow(s24,3)+61*s23*pow(s24,4)+16*pow(s24,5)))+s13*s13*((202*s23*s24-8*(s23*s23)+150*(s24*s24))*pow(s14,4)+15*s24*pow(s14,5)+s24*(s23*s23)*(64*s24*(s23*s23)+82*s23*(s24*s24)+6*pow(s23,3)+27*pow(s24,3))+pow(s14,3)*(423*s24*(s23*s23)+514*s23*(s24*s24)-48*pow(s23,3)+127*pow(s24,3))+s14*s14*(435*(s23*s23)*(s24*s24)+271*s24*pow(s23,3)-48*pow(s23,4)+226*s23*pow(s24,3)+91*pow(s24,4))+s14*(180*(s24*s24)*pow(s23,3)+67*s24*pow(s23,4)-8*pow(s23,5)+176*(s23*s23)*pow(s24,3)+105*s23*pow(s24,4)+18*pow(s24,5))))+s12*(s24*(18*s23*(s14*s14)+s14*(10*s23*s24+15*(s23*s23)-31*(s24*s24))+s24*(24*s23*s24+44*(s23*s23)+s24*s24))*pow(s13,5)-s24*(-3*s14*(s23-s24)+s24*(-5*s23+s24))*pow(s13,6)-s24*s24*pow(s13,7)+pow(s13,4)*(2*s23*(s24*s24)*(43*s23*s24+45*(s23*s23)+8*(s24*s24))+6*s24*(7*s23+2*s24)*pow(s14,3)+s14*s14*(74*s24*(s23*s23)+37*s23*(s24*s24)-4*pow(s23,3)-84*pow(s24,3))+s14*s24*(59*s24*(s23*s23)-50*s23*(s24*s24)+26*pow(s23,3)+2*pow(s24,3)))+s14*s24*(2*(9*s23*s24+s23*s23+2*(s24*s24))*pow(s14,5)+2*s24*pow(s14,6)+s24*(2*s23+3*s24)*((s23+s24)*(s23+s24))*pow(s23,3)+2*pow(s14,4)*(23*s24*(s23*s23)+12*s23*(s24*s24)+pow(s23,3)+pow(s24,3))+s23*pow(s14,3)*(42*s24*(s23*s23)+55*s23*(s24*s24)-13*pow(s23,3)+12*pow(s24,3))+s23*(s14*s14)*(64*(s23*s23)*(s24*s24)+13*s24*pow(s23,3)-20*pow(s23,4)+34*s23*pow(s24,3)+4*pow(s24,4))+s14*(s23*s23)*(36*(s23*s23)*(s24*s24)+5*s24*pow(s23,3)-6*pow(s23,4)+31*s23*pow(s24,3)+7*pow(s24,4)))+pow(s13,3)*(s23*s23*(s24*s24)*(92*s23*s24+71*(s23*s23)+27*(s24*s24))+s24*(48*s23+23*s24)*pow(s14,4)+pow(s14,3)*(134*s24*(s23*s23)+116*s23*(s24*s24)-12*pow(s23,3)-75*pow(s24,3))+3*s14*s24*(-(s23*s23*(s24*s24))+23*s24*pow(s23,3)+6*pow(s23,4)+7*s23*pow(s24,3)+6*pow(s24,4))+s14*s14*(64*(s23*s23)*(s24*s24)+92*s24*pow(s23,3)-12*pow(s23,4)-167*s23*pow(s24,3)+13*pow(s24,4)))+s13*(2*s24*(3*s23+5*s24)*pow(s14,6)-2*(s24*s24)*((s23+s24)*(s23+s24))*pow(s23,4)+pow(s14,5)*(35*s24*(s23*s23)+88*s23*(s24*s24)-4*pow(s23,3)+12*pow(s24,3))+s14*s24*(s23*s23)*(22*(s23*s23)*(s24*s24)+5*s24*pow(s23,3)-pow(s23,4)+25*s23*pow(s24,3)+10*pow(s24,4))+pow(s14,4)*(150*(s23*s23)*(s24*s24)+32*s24*pow(s23,3)-16*pow(s23,4)+41*s23*pow(s24,3)+16*pow(s24,4))+pow(s14,3)*(56*(s24*s24)*pow(s23,3)-18*s24*pow(s23,4)-16*pow(s23,5)+69*(s23*s23)*pow(s24,3)+57*s23*pow(s24,4)+10*pow(s24,5))+2*s23*(s14*s14)*(-4*(s24*s24)*pow(s23,3)-9*s24*pow(s23,4)-2*pow(s23,5)+23*(s23*s23)*pow(s24,3)+36*s23*pow(s24,4)+15*pow(s24,5)))+s13*s13*(3*s24*(9*s23+7*s24)*pow(s14,5)+s24*s24*(27*s23*s24+17*(s23*s23)+10*(s24*s24))*pow(s23,3)+pow(s14,4)*(108*s24*(s23*s23)+154*s23*(s24*s24)-12*pow(s23,3)-13*pow(s24,3))+s14*s23*s24*(27*(s23*s23)*(s24*s24)+26*s24*pow(s23,3)+3*pow(s23,4)+33*s23*pow(s24,3)+20*pow(s24,4))+pow(s14,3)*(153*(s23*s23)*(s24*s24)+96*s24*pow(s23,3)-28*pow(s23,4)-76*s23*pow(s24,3)+26*pow(s24,4))+s14*s14*(4*(s24*s24)*pow(s23,3)+24*s24*pow(s23,4)-12*pow(s23,5)-71*(s23*s23)*pow(s24,3)+50*s23*pow(s24,4)+28*pow(s24,5)))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/2.;
}

// Coefficient order epsilon^1 of master 4
template<>
double qq2yyg6CF<4,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s13+s14+s23,-3)*pow(s24,-2)*((s13*(4*s24*(30*s23+17*s24)+s14*(-5*s23+116*s24))+72*s24*(s13*s13)+(-5*s23+41*s24)*(s14*s14)+6*s24*(8*s23*s24+7*(s23*s23)+2*(s24*s24))+s14*(87*s23*s24-5*(s23*s23)+50*(s24*s24)))*pow(s12,7)+(-(s14*(s23-14*s24))+2*s24*(10*s13+7*s23+4*s24))*pow(s12,8)+2*s24*pow(s12,9)+s24*(s14*(21*s23*s24+2*(s23*s23)-15*(s24*s24))+s24*(3*s23*s24+16*(s23*s23)+2*(s24*s24)))*pow(s13,6)+(2*s23-3*s24)*(s24*s24)*pow(s13,7)+s24*pow(s13,5)*(2*(s14*s14)*(43*s23*s24+7*(s23*s23)-12*(s24*s24))+s24*(48*s24*(s23*s23)+24*s23*(s24*s24)+44*pow(s23,3)+5*pow(s24,3))+s14*(118*s24*(s23*s23)+35*s23*(s24*s24)+14*pow(s23,3)+20*pow(s24,3)))+pow(s12,6)*((-10*s14*(s23-34*s24)+s24*(368*s23+213*s24))*(s13*s13)+s14*s14*(224*s23*s24-23*(s23*s23)+130*(s24*s24))+s13*(-5*(4*s23-55*s24)*(s14*s14)+s24*(345*s23*s24+302*(s23*s23)+89*(s24*s24))+s14*(605*s23*s24-24*(s23*s23)+363*(s24*s24)))+130*s24*pow(s13,3)+(-10*s23+65*s24)*pow(s14,3)+2*s24*(60*s24*(s23*s23)+30*s23*(s24*s24)+35*pow(s23,3)+4*pow(s24,3))+s14*(229*s24*(s23*s23)+275*s23*(s24*s24)-10*pow(s23,3)+66*pow(s24,3)))+s23*(s14+s23)*(s14*s14)*(s24*(s23+2*s24)*pow(s14,4)+pow(s14,3)*(5*s24*(s23*s23)+17*s23*(s24*s24)-pow(s23,3)+4*pow(s24,3))+s24*(s23*s23)*(10*s24*(s23*s23)+15*s23*(s24*s24)+2*pow(s23,3)+7*pow(s24,3))+s14*s23*s24*(34*s24*(s23*s23)+34*s23*(s24*s24)+7*pow(s23,3)+10*pow(s24,3))+s14*s14*(41*(s23*s23)*(s24*s24)+10*s24*pow(s23,3)-pow(s23,4)+24*s23*pow(s24,3)+2*pow(s24,4)))+s24*pow(s13,4)*((174*s23*s24+37*(s23*s23)-8*(s24*s24))*pow(s14,3)+s23*s24*(96*s24*(s23*s23)+60*s23*(s24*s24)+56*pow(s23,3)+15*pow(s24,3))+s14*s14*(355*s24*(s23*s23)+134*s23*(s24*s24)+71*pow(s23,3)+64*pow(s24,3))+s14*(229*(s23*s23)*(s24*s24)+241*s24*pow(s23,3)+32*pow(s23,4)+120*s23*pow(s24,3)+27*pow(s24,4)))+pow(s12,5)*(s13*s13*(-30*(s23-21*s24)*(s14*s14)+s24*(908*s23*s24+768*(s23*s23)+247*(s24*s24))+s14*(1472*s23*s24-46*(s23*s23)+938*(s24*s24)))+(-10*s14*(s23-48*s24)+s24*(554*s23+331*s24))*pow(s13,3)+130*s24*pow(s13,4)+(308*s23*s24-42*(s23*s23)+182*(s24*s24))*pow(s14,3)-10*(s23-6*s24)*pow(s14,4)+s14*s14*(511*s24*(s23*s23)+641*s23*(s24*s24)-42*pow(s23,3)+149*pow(s24,3))+s13*(s14*s14*(1229*s23*s24-88*(s23*s23)+792*(s24*s24))+(-30*s23+340*s24)*pow(s14,3)+s24*(709*s24*(s23*s23)+373*s23*(s24*s24)+410*pow(s23,3)+54*pow(s24,3))+s14*(1301*s24*(s23*s23)+1614*s23*(s24*s24)-46*pow(s23,3)+429*pow(s24,3)))+2*s24*(60*(s23*s23)*(s24*s24)+80*s24*pow(s23,3)+35*pow(s23,4)+16*s23*pow(s24,3)+pow(s24,4))+s14*(614*(s23*s23)*(s24*s24)+330*s24*pow(s23,3)-10*pow(s23,4)+309*s23*pow(s24,3)+38*pow(s24,4)))+pow(s13,3)*(s24*(188*s23*s24+48*(s23*s23)+15*(s24*s24))*pow(s14,4)+s23*s23*(s24*s24)*(75*s24*(s23*s23)+56*s23*(s24*s24)+34*pow(s23,3)+15*pow(s24,3))+s24*(s14*s14)*(452*(s23*s23)*(s24*s24)+533*s24*pow(s23,3)+117*pow(s23,4)+231*s23*pow(s24,3)+51*pow(s24,4))+s14*s23*s24*(321*(s23*s23)*(s24*s24)+227*s24*pow(s23,3)+32*pow(s23,4)+203*s23*pow(s24,3)+54*pow(s24,4))+pow(s14,3)*(525*(s23*s23)*(s24*s24)+127*s24*pow(s23,3)-4*pow(s23,4)+220*s23*pow(s24,3)+92*pow(s24,4)))+pow(s12,4)*(6*s23*s24*(6*s23*s24+7*(s23*s23)+s24*s24)*((s23+s24)*(s23+s24))+((-20*s23+650*s24)*(s14*s14)+s24*(1162*s23*s24+928*(s23*s23)+335*(s24*s24))+s14*(1674*s23*s24-44*(s23*s23)+1146*(s24*s24)))*pow(s13,3)+(-5*s14*(s23-70*s24)+2*s24*(223*s23+137*s24))*pow(s13,4)+72*s24*pow(s13,5)+(242*s23*s24-38*(s23*s23)+148*(s24*s24))*pow(s14,4)+(-5*s23+32*s24)*pow(s14,5)+pow(s14,3)*(597*s24*(s23*s23)+784*s23*(s24*s24)-69*pow(s23,3)+177*pow(s24,3))+s13*s13*(s14*s14*(2264*s23*s24-126*(s23*s23)+1630*(s24*s24))-30*(s23-19*s24)*pow(s14,3)+s24*(1524*s24*(s23*s23)+857*s23*(s24*s24)+832*pow(s23,3)+138*pow(s24,3))+s14*(2542*s24*(s23*s23)+3366*s23*(s24*s24)-78*pow(s23,3)+977*pow(s24,3)))+s14*s14*(1247*(s23*s23)*(s24*s24)+625*s24*pow(s23,3)-38*pow(s23,4)+624*s23*pow(s24,3)+72*pow(s24,4))+s13*((1278*s23*s24-120*(s23*s23)+906*(s24*s24))*pow(s14,3)+(-20*s23+230*s24)*pow(s14,4)+s14*s14*(2211*s24*(s23*s23)+3002*s23*(s24*s24)-152*pow(s23,3)+828*pow(s24,3))+s24*(604*(s23*s23)*(s24*s24)+746*s24*pow(s23,3)+320*pow(s23,4)+181*s23*pow(s24,3)+13*pow(s24,4))+s14*(2838*(s23*s23)*(s24*s24)+1478*s24*pow(s23,3)-44*pow(s23,4)+1566*s23*pow(s24,3)+233*pow(s24,4)))+s14*(710*(s24*s24)*pow(s23,3)+280*s24*pow(s23,4)-5*pow(s23,5)+558*(s23*s23)*pow(s24,3)+146*s23*pow(s24,4)+8*pow(s24,5)))+s13*s14*(2*s24*(14*s23*s24+5*(s23*s23)+2*(s24*s24))*pow(s14,5)+s24*pow(s23,3)*(30*(s23*s23)*(s24*s24)+15*s24*pow(s23,3)+2*pow(s23,4)+22*s23*pow(s24,3)+5*pow(s24,4))+pow(s14,4)*(143*(s23*s23)*(s24*s24)+39*s24*pow(s23,3)-6*pow(s23,4)+53*s23*pow(s24,3)+16*pow(s24,4))+s14*s24*(s23*s23)*(144*(s23*s23)*(s24*s24)+101*s24*pow(s23,3)+21*pow(s23,4)+93*s23*pow(s24,3)+23*pow(s24,4))+pow(s14,3)*(285*(s24*s24)*pow(s23,3)+68*s24*pow(s23,4)-10*pow(s23,5)+189*(s23*s23)*pow(s24,3)+67*s23*pow(s24,4)+12*pow(s24,5))+s23*(s14*s14)*(254*(s24*s24)*pow(s23,3)+57*s24*pow(s23,4)-4*pow(s23,5)+255*(s23*s23)*pow(s24,3)+131*s23*pow(s24,4)+34*pow(s24,5)))+s13*s13*(s24*(107*s23*s24+32*(s23*s23)+15*(s24*s24))*pow(s14,5)+(8*s23+5*s24)*(s24*s24)*((s23+s24)*(s23+s24))*pow(s23,3)+s14*s24*(s23*s23)*(172*(s23*s23)*(s24*s24)+98*s24*pow(s23,3)+14*pow(s23,4)+123*s23*pow(s24,3)+32*pow(s24,4))+pow(s14,4)*(396*(s23*s23)*(s24*s24)+103*s24*pow(s23,3)-9*pow(s23,4)+167*s23*pow(s24,3)+62*pow(s24,4))+s23*s24*(s14*s14)*(423*(s23*s23)*(s24*s24)+355*s24*pow(s23,3)+79*pow(s23,4)+262*s23*pow(s24,3)+73*pow(s24,4))+pow(s14,3)*(563*(s24*s24)*pow(s23,3)+138*s24*pow(s23,4)-8*pow(s23,5)+432*(s23*s23)*pow(s24,3)+200*s23*pow(s24,4)+41*pow(s24,5)))+s12*(s24*(34*s23*s24+2*s14*(9*s23+5*s24)+16*(s23*s23)-s24*s24)*pow(s13,6)+(2*s23-s24)*s24*pow(s13,7)+s24*pow(s13,5)*((65*s23+70*s24)*(s14*s14)+183*s24*(s23*s23)+107*s23*(s24*s24)+s14*(291*s23*s24+127*(s23*s23)+41*(s24*s24))+46*pow(s23,3)+30*pow(s24,3))+pow(s13,4)*(2*s24*(61*s23+81*s24)*pow(s14,3)+s14*s24*(1053*s24*(s23*s23)+624*s23*(s24*s24)+324*pow(s23,3)+179*pow(s24,3))+s14*s14*(378*s24*(s23*s23)+902*s23*(s24*s24)-8*pow(s23,3)+207*pow(s24,3))+s24*(364*(s23*s23)*(s24*s24)+344*s24*pow(s23,3)+64*pow(s23,4)+157*s23*pow(s24,3)+22*pow(s24,4)))+pow(s13,3)*(s24*(128*s23+183*s24)*pow(s14,4)+pow(s14,3)*(553*s24*(s23*s23)+1331*s23*(s24*s24)-27*pow(s23,3)+365*pow(s24,3))+s23*s24*(422*(s23*s23)*(s24*s24)+289*s24*pow(s23,3)+46*pow(s23,4)+243*s23*pow(s24,3)+51*pow(s24,4))+s14*s24*(1425*(s23*s23)*(s24*s24)+1456*s24*pow(s23,3)+382*pow(s23,4)+612*s23*pow(s24,3)+89*pow(s24,4))+3*(s14*s14)*(770*(s23*s23)*(s24*s24)+255*s24*pow(s23,3)-8*pow(s23,4)+458*s23*pow(s24,3)+125*pow(s24,4)))+s14*(2*s24*(s23+s24)*pow(s14,6)+s24*(10*s23*s24+4*(s23*s23)+7*(s24*s24))*((s23+s24)*(s23+s24))*pow(s23,3)+pow(s14,5)*(21*s24*(s23*s23)+37*s23*(s24*s24)-3*pow(s23,3)+6*pow(s24,3))+pow(s14,4)*(191*(s23*s23)*(s24*s24)+73*s24*pow(s23,3)-11*pow(s23,4)+73*s23*pow(s24,3)+6*pow(s24,4))+s14*s24*(s23*s23)*(187*(s23*s23)*(s24*s24)+139*s24*pow(s23,3)+32*pow(s23,4)+96*s23*pow(s24,3)+17*pow(s24,4))+pow(s14,3)*(389*(s24*s24)*pow(s23,3)+116*s24*pow(s23,4)-11*pow(s23,5)+258*(s23*s23)*pow(s24,3)+50*s23*pow(s24,4)+2*pow(s24,5))+s23*(s14*s14)*(352*(s24*s24)*pow(s23,3)+89*s24*pow(s23,4)-3*pow(s23,5)+346*(s23*s23)*pow(s24,3)+117*s23*pow(s24,4)+12*pow(s24,5)))+s13*(7*s24*(3*s23+4*s24)*pow(s14,6)+s24*(5*s23*s24+2*(s23*s23)+7*(s24*s24))*((s23+s24)*(s23+s24))*pow(s23,3)+pow(s14,5)*(156*s24*(s23*s23)+338*s23*(s24*s24)-17*pow(s23,3)+90*pow(s24,3))+s14*s24*(s23*s23)*(346*(s23*s23)*(s24*s24)+223*s24*pow(s23,3)+51*pow(s23,4)+228*s23*pow(s24,3)+55*pow(s24,4))+pow(s14,4)*(1113*(s23*s23)*(s24*s24)+385*s24*pow(s23,3)-52*pow(s23,4)+581*s23*pow(s24,3)+119*pow(s24,4))+pow(s14,3)*(1536*(s24*s24)*pow(s23,3)+443*s24*pow(s23,4)-41*pow(s23,5)+1269*(s23*s23)*pow(s24,3)+434*s23*pow(s24,4)+49*pow(s24,5))+s23*(s14*s14)*(934*(s24*s24)*pow(s23,3)+240*s24*pow(s23,4)-8*pow(s23,5)+1093*(s23*s23)*pow(s24,3)+544*s23*pow(s24,4)+103*pow(s24,5)))+s13*s13*(2*s24*(37*s23+53*s24)*pow(s14,5)+pow(s14,4)*(421*s24*(s23*s23)+987*s23*(s24*s24)-33*pow(s23,3)+284*pow(s24,3))+s24*(s23*s23)*(185*(s23*s23)*(s24*s24)+102*s24*pow(s23,3)+16*pow(s23,4)+135*s23*pow(s24,3)+36*pow(s24,4))+s14*s23*s24*(1155*(s23*s23)*(s24*s24)+889*s24*pow(s23,3)+214*pow(s23,4)+640*s23*pow(s24,3)+132*pow(s24,4))+pow(s14,3)*(2362*(s23*s23)*(s24*s24)+799*s24*pow(s23,3)-65*pow(s23,4)+1365*s23*pow(s24,3)+339*pow(s24,4))+s14*s14*(2273*(s24*s24)*pow(s23,3)+660*s24*pow(s23,4)-24*pow(s23,5)+2077*(s23*s23)*pow(s24,3)+839*s23*pow(s24,4)+114*pow(s24,5))))+pow(s12,3)*((-5*(s23-61*s24)*(s14*s14)+2*s24*(385*s23*s24+283*(s23*s23)+115*(s24*s24))+s14*(935*s23*s24-21*(s23*s23)+690*(s24*s24)))*pow(s13,4)+(-(s14*(s23-124*s24))+2*s24*(94*s23+57*s24))*pow(s13,5)+20*s24*pow(s13,6)+(107*s23*s24-17*(s23*s23)+70*(s24*s24))*pow(s14,5)-(s23-9*s24)*pow(s14,6)+pow(s14,4)*(386*s24*(s23*s23)+538*s23*(s24*s24)-55*pow(s23,3)+117*pow(s24,3))+pow(s13,3)*(s14*s14*(1802*s23*s24-80*(s23*s23)+1474*(s24*s24))-10*(s23-38*s24)*pow(s14,3)+4*s24*(388*s24*(s23*s23)+234*s23*(s24*s24)+190*pow(s23,3)+43*pow(s24,3))+s14*(2220*s24*(s23*s23)+3239*s23*(s24*s24)-58*pow(s23,3)+1002*pow(s24,3)))+pow(s14,3)*(1285*(s23*s23)*(s24*s24)+601*s24*pow(s23,3)-55*pow(s23,4)+626*s23*pow(s24,3)+68*pow(s24,4))+s13*s13*(-2*(-829*s23*s24+57*(s23*s23)-702*(s24*s24))*pow(s14,3)-10*(s23-25*s24)*pow(s14,4)+s14*s14*(3131*s24*(s23*s23)+4757*s23*(s24*s24)-186*pow(s23,3)+1464*pow(s24,3))+s24*(1116*(s23*s23)*(s24*s24)+1262*s24*pow(s23,3)+488*pow(s23,4)+375*s23*pow(s24,3)+32*pow(s24,4))+s14*(4615*(s23*s23)*(s24*s24)+2214*s24*pow(s23,3)-58*pow(s23,4)+2759*s23*pow(s24,3)+491*pow(s24,4)))+s14*s14*(1210*(s24*s24)*pow(s23,3)+436*s24*pow(s23,4)-17*pow(s23,5)+953*(s23*s23)*pow(s24,3)+242*s23*pow(s24,4)+12*pow(s24,5))+s14*s23*(446*(s24*s24)*pow(s23,3)+139*s24*pow(s23,4)-pow(s23,5)+484*(s23*s23)*pow(s24,3)+201*s23*pow(s24,4)+24*pow(s24,5))+s13*((710*s23*s24-72*(s23*s23)+576*(s24*s24))*pow(s14,4)-5*(s23-16*s24)*pow(s14,5)+pow(s14,3)*(1863*s24*(s23*s23)+2824*s23*(s24*s24)-183*pow(s23,3)+807*pow(s24,3))+s23*s24*(464*(s23*s23)*(s24*s24)+414*s24*pow(s23,3)+140*pow(s23,4)+219*s23*pow(s24,3)+33*pow(s24,4))+s14*s14*(4358*(s23*s23)*(s24*s24)+2033*s24*pow(s23,3)-128*pow(s23,4)+2480*s23*pow(s24,3)+391*pow(s24,4))+s14*(2479*(s24*s24)*pow(s23,3)+940*s24*pow(s23,4)-21*pow(s23,5)+2131*(s23*s23)*pow(s24,3)+664*s23*pow(s24,4)+51*pow(s24,5)))+2*s24*(7*s23+3*s24)*(s23*s23)*pow(s23+s24,3))+s12*s12*((51*s24*(s14*s14)+s24*(251*s23*s24+162*(s23*s23)+67*(s24*s24))+s14*(233*s23*s24-4*(s23*s23)+179*(s24*s24)))*pow(s13,5)+s24*(16*s14+36*s23+17*s24)*pow(s13,6)+2*s24*pow(s13,7)-3*(-8*s23*s24+s23*s23-6*(s24*s24))*pow(s14,6)+s24*pow(s14,7)+pow(s14,5)*(133*s24*(s23*s23)+203*s23*(s24*s24)-21*pow(s23,3)+41*pow(s24,3))+pow(s13,4)*(s14*s14*(608*s23*s24-19*(s23*s23)+576*(s24*s24))+85*s24*pow(s14,3)+2*s24*(392*s24*(s23*s23)+248*s23*(s24*s24)+157*pow(s23,3)+54*pow(s24,3))+s14*(875*s24*(s23*s23)+1482*s23*(s24*s24)-16*pow(s23,3)+444*pow(s24,3)))+s14*s24*(s23*s23)*(200*(s23*s23)*(s24*s24)+143*s24*pow(s23,3)+37*pow(s23,4)+117*s23*pow(s24,3)+23*pow(s24,4))+pow(s14,4)*(704*(s23*s23)*(s24*s24)+303*s24*pow(s23,3)-37*pow(s23,4)+320*s23*pow(s24,3)+32*pow(s24,4))+pow(s13,3)*((810*s23*s24-36*(s23*s23)+842*(s24*s24))*pow(s14,3)+80*s24*pow(s14,4)+s14*s14*(1795*s24*(s23*s23)+3212*s23*(s24*s24)-84*pow(s23,3)+1016*pow(s24,3))+s24*(948*(s23*s23)*(s24*s24)+976*s24*pow(s23,3)+302*pow(s23,4)+359*s23*pow(s24,3)+38*pow(s24,4))+s14*(3326*(s23*s23)*(s24*s24)+1376*s24*pow(s23,3)-24*pow(s23,4)+2091*s23*pow(s24,3)+455*pow(s24,4)))+pow(s14,3)*(991*(s24*s24)*pow(s23,3)+327*s24*pow(s23,4)-21*pow(s23,5)+745*(s23*s23)*pow(s24,3)+176*s23*pow(s24,4)+8*pow(s24,5))+s23*(s14*s14)*(603*(s24*s24)*pow(s23,3)+169*s24*pow(s23,4)-3*pow(s23,5)+650*(s23*s23)*pow(s24,3)+258*s23*pow(s24,4)+30*pow(s24,5))+s13*((197*s23*s24-16*(s23*s23)+195*(s24*s24))*pow(s14,5)+11*s24*pow(s14,6)+pow(s14,4)*(797*s24*(s23*s23)+1401*s23*(s24*s24)-94*pow(s23,3)+405*pow(s24,3))+s24*(s23*s23)*(163*(s23*s23)*(s24*s24)+109*s24*pow(s23,3)+30*pow(s23,4)+111*s23*pow(s24,3)+27*pow(s24,4))+pow(s14,3)*(3199*(s23*s23)*(s24*s24)+1311*s24*pow(s23,3)-130*pow(s23,4)+1815*s23*pow(s24,3)+315*pow(s24,4))+s14*s14*(2981*(s24*s24)*pow(s23,3)+995*s24*pow(s23,4)-52*pow(s23,5)+2602*(s23*s23)*pow(s24,3)+850*s23*pow(s24,4)+75*pow(s24,5))+s14*s23*(1100*(s24*s24)*pow(s23,3)+323*s24*pow(s23,4)-4*pow(s23,5)+1310*(s23*s23)*pow(s24,3)+638*s23*pow(s24,4)+102*pow(s24,5)))+s13*s13*((572*s23*s24-34*(s23*s23)+605*(s24*s24))*pow(s14,4)+42*s24*pow(s14,5)+pow(s14,3)*(1746*s24*(s23*s23)+3179*s23*(s24*s24)-141*pow(s23,3)+1003*pow(s24,3))+s23*s24*(670*(s23*s23)*(s24*s24)+527*s24*pow(s23,3)+144*pow(s23,4)+354*s23*pow(s24,3)+63*pow(s24,4))+s14*s14*(5048*(s23*s23)*(s24*s24)+2078*s24*pow(s23,3)-114*pow(s23,4)+3096*s23*pow(s24,3)+630*pow(s24,4))+s14*(2978*(s24*s24)*pow(s23,3)+1004*s24*pow(s23,4)-16*pow(s23,5)+2769*(s23*s23)*pow(s24,3)+1010*s23*pow(s24,4)+105*pow(s24,5)))+2*s24*pow(s23,3)*pow(s23+s24,4)))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/2.;
}

// Coefficient of master 4 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6CF<4>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6CF<4,-1>(s12,s13,s14,s23,s24),
        qq2yyg6CF<4,0>(s12,s13,s14,s23,s24),
        qq2yyg6CF<4,1>(s12,s13,s14,s23,s24)
    });
}


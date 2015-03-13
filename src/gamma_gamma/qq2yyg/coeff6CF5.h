/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 5: bubble(s24)

// Coefficient order epsilon^-1 of master 5
template<>
double qq2yyg6CF<5,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return pow(s24,-1)*(2*s24*(s14*s14)*(s23*s23)+9*s23*(s14*s14)*(s24*s24)+3*s14*(s23*s23)*(s24*s24)+s24*(s13*s13)*(s14*(2*s23+s24)-s23*s23+s24*s24)+(-2*s13*(s23-3*s24)+s14*(-2*s23+5*s24)-2*(s23*s23-5*(s24*s24)))*pow(s12,3)-(s23-2*s24)*pow(s12,4)+3*s23*s24*pow(s14,3)+2*(s24*s24)*pow(s14,3)-s14*s24*pow(s23,3)-s14*s14*pow(s23,3)+10*s14*s23*pow(s24,3)+6*(s14*s14)*pow(s24,3)+2*(s23*s23)*pow(s24,3)+s12*s12*(-((s23-6*s24)*(s13*s13))-(s23-4*s24)*(s14*s14)-3*s24*(s23*s23)+10*s23*(s24*s24)+s13*(-2*s14*(s23-5*s24)+s23*s24-3*(s23*s23)+17*(s24*s24))+s14*(5*s23*s24-4*(s23*s23)+20*(s24*s24))-pow(s23,3)+16*pow(s24,3))+s13*(s24*(5*s23+3*s24)*(s14*s14)+s24*(-(s24*(s23*s23))+5*s23*(s24*s24)-2*pow(s23,3)+4*pow(s24,3))+s14*(s24*(s23*s23)+9*s23*(s24*s24)-pow(s23,3)+7*pow(s24,3)))+s12*(s13*s13*(s23*s24+s24*(5*s14+7*s24)-s23*s23)-2*(s14*s14)*(-4*s23*s24+s23*s23-6*(s24*s24))+2*s24*pow(s13,3)+s24*pow(s14,3)+s24*(2*s24*(s23*s23)+13*s23*(s24*s24)-pow(s23,3)+10*pow(s24,3))+s13*(4*s24*(s14*s14)-5*s24*(s23*s23)+9*s23*(s24*s24)+s14*(8*s23*s24-3*(s23*s23)+18*(s24*s24))-pow(s23,3)+15*pow(s24,3))+s14*(-(s24*(s23*s23))+20*s23*(s24*s24)-2*pow(s23,3)+21*pow(s24,3)))+6*s14*pow(s24,4)+4*s23*pow(s24,4)+2*pow(s24,5))*pow(s12+s13+s14+s24,-3)*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1);
}

// Coefficient order epsilon^0 of master 5
template<>
double qq2yyg6CF<5,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*pow(s24,-1)*((s13*(11*s23-10*s24)+2*s14*(4*s23-5*s24)+7*s23*s24+8*(s23*s23)-10*(s24*s24))*pow(s12,7)+2*(s23-s24)*pow(s12,8)+s23*s24*(4*s23+s24)*pow(s13,6)+pow(s12,6)*(5*(5*s23-4*s24)*(s13*s13)+10*(s23-2*s24)*(s14*s14)+39*s24*(s23*s23)+s13*(5*s14*(7*s23-8*s24)+34*s23*s24+43*(s23*s23)-46*(s24*s24))+s14*(22*s23*s24+34*(s23*s23)-40*(s24*s24))+6*s23*(s24*s24)+11*pow(s23,3)-20*pow(s24,3))+s14*(s23*s23)*((s14+s24)*(s14+s24))*(s14*s14*(6*s23*s24-s23*s23+6*(s24*s24))+6*(s24*s24)*((s23+s24)*(s23+s24))+(s23+2*s24)*pow(s14,3)+s14*(7*s24*(s23*s23)+16*s23*(s24*s24)+pow(s23,3)+10*pow(s24,3)))+s13*s23*((4*s23*s24-3*(s23*s23)-2*(s24*s24))*pow(s14,5)-2*(-4*s23*s24-2*(s23*s23)+s24*s24)*((s23+s24)*(s23+s24))*pow(s24,3)-pow(s14,4)*(2*s24*(s23*s23)-16*s23*(s24*s24)+7*pow(s23,3)+6*pow(s24,3))+pow(s14,3)*(31*(s23*s23)*(s24*s24)+5*s24*pow(s23,3)+10*pow(s23,4)+34*s23*pow(s24,3)-8*pow(s24,4))+s24*(s14*s14)*(73*(s23*s23)*(s24*s24)+43*s24*pow(s23,3)+18*pow(s23,4)+40*s23*pow(s24,3)-8*pow(s24,4))+s14*(s24*s24)*(61*(s23*s23)*(s24*s24)+45*s24*pow(s23,3)+12*pow(s23,4)+22*s23*pow(s24,3)-6*pow(s24,4)))+s23*(s13*s13)*((s23*s24-21*(s23*s23)-7*(s24*s24))*pow(s14,4)-pow(s14,3)*(46*s24*(s23*s23)-13*s23*(s24*s24)+13*pow(s23,3)+12*pow(s24,3))+s14*s14*(5*(s23*s23)*(s24*s24)+7*s24*pow(s23,3)+19*pow(s23,4)+35*s23*pow(s24,3)-5*pow(s24,4))+s14*s24*(62*(s23*s23)*(s24*s24)+62*s24*pow(s23,3)+34*pow(s23,4)+35*s23*pow(s24,3)-2*pow(s24,4))+2*(s24*s24)*(17*(s23*s23)*(s24*s24)+16*s24*pow(s23,3)+6*pow(s23,4)+6*s23*pow(s24,3)-pow(s24,4)))+pow(s12,5)*(s13*s13*(60*s14*(s23-s24)+68*s23*s24+94*(s23*s23)-84*(s24*s24))+s14*s14*(16*s23*s24+55*(s23*s23)-60*(s24*s24))+71*(s23*s23)*(s24*s24)+10*(3*s23-2*s24)*pow(s13,3)-20*s24*pow(s14,3)+49*s24*pow(s23,3)+6*pow(s23,4)+s13*(30*(s23-2*s24)*(s14*s14)+162*s24*(s23*s23)+3*s14*(27*s23*s24+49*(s23*s23)-48*(s24*s24))+29*s23*(s24*s24)+59*pow(s23,3)-84*pow(s24,3))+s14*(151*s24*(s23*s23)+12*s23*(s24*s24)+45*pow(s23,3)-60*pow(s24,3))-8*s23*pow(s24,3)-20*pow(s24,4))+pow(s13,5)*(15*(s23*s23)*(s24*s24)+s14*s23*(13*s23*s24-6*(s23*s23)+2*(s24*s24))+2*s24*pow(s23,3)+2*pow(s23,4)-2*pow(s24,4))+s12*s12*((3*s14*s23+14*s23*s24+19*(s23*s23)-6*(s24*s24))*pow(s13,5)+s23*pow(s13,6)+pow(s13,4)*(105*s24*(s23*s23)+6*s14*(5*s23*s24+11*(s23*s23)-4*(s24*s24))+26*s23*(s24*s24)+50*pow(s23,3)-42*pow(s24,3))+s23*((67*s23*s24+21*(s23*s23)-30*(s24*s24))*pow(s14,4)-2*(s23+7*s24)*pow(s14,5)-2*pow(s14,6)+2*pow(s14,3)*(83*s24*(s23*s23)+109*s23*(s24*s24)+9*pow(s23,3)-18*pow(s24,3))+s14*s14*(289*(s23*s23)*(s24*s24)+78*s24*pow(s23,3)+6*pow(s23,4)+229*s23*pow(s24,3)-38*pow(s24,4))+s14*s24*(152*(s23*s23)*(s24*s24)+65*s24*pow(s23,3)+6*pow(s23,4)+70*s23*pow(s24,3)-30*pow(s24,4))+s24*s24*(7*(s23*s23)*(s24*s24)+8*s24*pow(s23,3)+pow(s23,4)-10*s23*pow(s24,3)-10*pow(s24,4)))+pow(s13,3)*(s14*s14*(-8*s23*s24+82*(s23*s23)-36*(s24*s24))+190*(s23*s23)*(s24*s24)-10*s23*pow(s14,3)+175*s24*pow(s23,3)+59*pow(s23,4)+s14*(301*s24*(s23*s23)+36*s23*(s24*s24)+123*pow(s23,3)-108*pow(s24,3))+9*s23*pow(s24,3)-74*pow(s24,4))+s13*(3*(-18*s23*s24+s23*s23-2*(s24*s24))*pow(s14,4)-9*s23*pow(s14,5)+pow(s14,3)*(222*s24*(s23*s23)-76*s23*(s24*s24)+62*pow(s23,3)-24*pow(s24,3))+s14*s14*(475*(s23*s23)*(s24*s24)+369*s24*pow(s23,3)+90*pow(s23,4)-45*s23*pow(s24,3)-36*pow(s24,4))+s14*(407*(s24*s24)*pow(s23,3)+191*s24*pow(s23,4)+30*pow(s23,5)+298*(s23*s23)*pow(s24,3)-45*s23*pow(s24,4)-24*pow(s24,5))+s24*(110*(s24*s24)*pow(s23,3)+78*s24*pow(s23,4)+18*pow(s23,5)+36*(s23*s23)*pow(s24,3)-31*s23*pow(s24,4)-6*pow(s24,5)))+s13*s13*(8*(-8*s23*s24+5*(s23*s23)-3*(s24*s24))*pow(s14,3)-15*s23*pow(s14,4)+219*(s24*s24)*pow(s23,3)+139*s24*pow(s23,4)+19*pow(s23,5)+3*(s14*s14)*(117*s24*(s23*s23)-12*s23*(s24*s24)+38*pow(s23,3)-30*pow(s24,3))+144*(s23*s23)*pow(s24,3)+s14*(457*(s23*s23)*(s24*s24)+384*s24*pow(s23,3)+131*pow(s23,4)+6*s23*pow(s24,3)-108*pow(s24,4))-18*s23*pow(s24,4)-42*pow(s24,5)))+s12*(2*s23*(s23+s24)*pow(s13,6)+pow(s13,5)*(s14*s23*(7*s23+5*s24)+28*s24*(s23*s23)+7*s23*(s24*s24)+8*pow(s23,3)-6*pow(s24,3))+s13*s23*(-5*(-8*s23*s24+s23*s23+7*(s24*s24))*pow(s14,4)-(5*s23+11*s24)*pow(s14,5)+pow(s14,3)*(87*s24*(s23*s23)+156*s23*(s24*s24)+16*pow(s23,3)-39*pow(s24,3))+s14*s14*(231*(s23*s23)*(s24*s24)+103*s24*pow(s23,3)+30*pow(s23,4)+194*s23*pow(s24,3)-29*pow(s24,4))+s14*s24*(189*(s23*s23)*(s24*s24)+121*s24*pow(s23,3)+36*pow(s23,4)+93*s23*pow(s24,3)-26*pow(s24,4))+s24*s24*(51*(s23*s23)*(s24*s24)+41*s24*pow(s23,3)+12*pow(s23,4)+10*s23*pow(s24,3)-12*pow(s24,4)))+pow(s13,4)*(s23*(7*s23-2*s24)*(s14*s14)+77*(s23*s23)*(s24*s24)+45*s24*pow(s23,3)+20*pow(s23,4)+s14*(82*s24*(s23*s23)+10*s23*(s24*s24)+7*pow(s23,3)-18*pow(s24,3))+3*s23*pow(s24,3)-22*pow(s24,4))+s23*(s14+s24)*((16*s23*s24+5*(s23*s23)-6*(s24*s24))*pow(s14,4)-(s23+2*s24)*pow(s14,5)+pow(s14,3)*(50*s24*(s23*s23)+55*s23*(s24*s24)+2*pow(s23,3)-8*pow(s24,3))+s14*s24*(55*(s23*s23)*(s24*s24)+27*s24*pow(s23,3)+2*pow(s23,4)+24*s23*pow(s24,3)-6*pow(s24,4))+2*(s14*s14)*(50*(s23*s23)*(s24*s24)+17*s24*pow(s23,3)+2*pow(s23,4)+33*s23*pow(s24,3)-4*pow(s24,4))-2*((s23+s24)*(s23+s24))*pow(s24,4))+s13*s13*(-2*s23*(4*s23+11*s24)*pow(s14,4)-pow(s14,3)*(-61*s24*(s23*s23)+50*s23*(s24*s24)+34*pow(s23,3)+6*pow(s24,3))+s14*s14*(196*(s23*s23)*(s24*s24)+66*s24*pow(s23,3)+46*pow(s23,4)-21*s23*pow(s24,3)-18*pow(s24,4))+s14*(199*(s24*s24)*pow(s23,3)+146*s24*pow(s23,4)+38*pow(s23,5)+177*(s23*s23)*pow(s24,3)-2*s23*pow(s24,4)-18*pow(s24,5))+s24*(102*(s24*s24)*pow(s23,3)+94*s24*pow(s23,4)+34*pow(s23,5)+50*(s23*s23)*pow(s24,3)-9*s23*pow(s24,4)-6*pow(s24,5)))+pow(s13,3)*(-2*s23*(s23+9*s24)*pow(s14,3)+114*(s24*s24)*pow(s23,3)+79*s24*pow(s23,4)+12*pow(s23,5)+93*(s23*s23)*pow(s24,3)-s14*s14*(-90*s24*(s23*s23)+20*s23*(s24*s24)+25*pow(s23,3)+18*pow(s24,3))+s14*(188*(s23*s23)*(s24*s24)+79*s24*pow(s23,3)+52*pow(s23,4)+7*s23*pow(s24,3)-40*pow(s24,4))-22*pow(s24,5)))+pow(s13,4)*(s23*(s14*s14)*(13*s23*s24-24*(s23*s23)-2*(s24*s24))+s14*(41*(s23*s23)*(s24*s24)-14*s24*pow(s23,3)+2*pow(s23,4)-4*pow(s24,4))+2*(13*(s24*s24)*pow(s23,3)+6*s24*pow(s23,4)+pow(s23,5)+14*(s23*s23)*pow(s24,3)-2*pow(s24,5)))+pow(s12,3)*((10*s14*(2*s23-s24)+43*s23*s24+64*(s23*s23)-34*(s24*s24))*pow(s13,4)+(7*s23-2*s24)*pow(s13,5)+(-29*s23*s24+10*(s23*s23)-10*(s24*s24))*pow(s14,4)-2*(4*s23+s24)*pow(s14,5)+pow(s13,3)*(10*(s23-2*s24)*(s14*s14)+226*s24*(s23*s23)+2*s14*(41*s23*s24+95*(s23*s23)-56*(s24*s24))+52*s23*(s24*s24)+113*pow(s23,3)-110*pow(s24,3))+2*pow(s14,3)*(85*s24*(s23*s23)-18*s23*(s24*s24)+27*pow(s23,3)-10*pow(s24,3))+s13*s13*(6*(s14*s14)*(s23*s24+33*(s23*s23)-22*(s24*s24))+280*(s23*s23)*(s24*s24)-20*(s23+s24)*pow(s14,3)+272*s24*pow(s23,3)+72*pow(s23,4)+s14*(559*s24*(s23*s23)+62*s23*(s24*s24)+246*pow(s23,3)-222*pow(s24,3))+s23*pow(s24,3)-110*pow(s24,4))+s14*s14*(334*(s23*s23)*(s24*s24)+248*s24*pow(s23,3)+32*pow(s23,4)-38*s23*pow(s24,3)-20*pow(s24,4))+s13*((-62*s23*s24+82*(s23*s23)-64*(s24*s24))*pow(s14,3)-5*(5*s23+2*s24)*pow(s14,4)+207*(s24*s24)*pow(s23,3)+93*s24*pow(s23,4)+10*pow(s23,5)+2*(s14*s14)*(251*s24*(s23*s23)-13*s23*(s24*s24)+93*pow(s23,3)-66*pow(s24,3))+134*(s23*s23)*pow(s24,3)+s14*(558*(s23*s23)*(s24*s24)+469*s24*pow(s23,3)+104*pow(s23,4)-25*s23*pow(s24,3)-112*pow(s24,4))-40*s23*pow(s24,4)-34*pow(s24,5))+s14*(244*(s24*s24)*pow(s23,3)+68*s24*pow(s23,4)+4*pow(s23,5)+187*(s23*s23)*pow(s24,3)-44*s23*pow(s24,4)-10*pow(s24,5))+s24*(47*(s24*s24)*pow(s23,3)+23*s24*pow(s23,4)+2*pow(s23,5)+9*(s23*s23)*pow(s24,3)-21*s23*pow(s24,4)-2*pow(s24,5)))+pow(s13,3)*(s23*(3*s23*s24-35*(s23*s23)-8*(s24*s24))*pow(s14,3)-s14*s14*(-33*(s23*s23)*(s24*s24)+52*s24*pow(s23,3)+7*pow(s23,4)+6*s23*pow(s24,3)+2*pow(s24,4))+s14*(29*(s24*s24)*pow(s23,3)+19*s24*pow(s23,4)+12*pow(s23,5)+53*(s23*s23)*pow(s24,3)+3*s23*pow(s24,4)-4*pow(s24,5))+s24*(38*(s24*s24)*pow(s23,3)+38*s24*pow(s23,4)+18*pow(s23,5)+23*(s23*s23)*pow(s24,3)+s23*pow(s24,4)-2*pow(s24,5)))+pow(s12,4)*(2*(5*s14*(5*s23-4*s24)+36*s23*s24+53*(s23*s23)-38*(s24*s24))*pow(s13,3)+10*(2*s23-s24)*pow(s13,4)+8*(-2*s23*s24+5*(s23*s23)-5*(s24*s24))*pow(s14,3)-10*(s23+s24)*pow(s14,4)+76*(s24*s24)*pow(s23,3)+21*s24*pow(s23,4)+pow(s23,5)+s13*s13*(30*(s23-2*s24)*(s14*s14)+268*s24*(s23*s23)+4*s14*(29*s23*s24+61*(s23*s23)-48*(s24*s24))+55*s23*(s24*s24)+119*pow(s23,3)-138*pow(s24,3))+s14*s14*(228*s24*(s23*s23)-8*s23*(s24*s24)+71*pow(s23,3)-60*pow(s24,3))+55*(s23*s23)*pow(s24,3)+s13*(2*(s14*s14)*(14*s23*s24+89*(s23*s23)-78*(s24*s24))+223*(s23*s23)*(s24*s24)-10*(s23+4*s24)*pow(s14,3)+189*s24*pow(s23,3)+37*pow(s23,4)+s14*(478*s24*(s23*s23)+46*s23*(s24*s24)+181*pow(s23,3)-192*pow(s24,3))-13*s23*pow(s24,3)-76*pow(s24,4))+s14*(248*(s23*s23)*(s24*s24)+178*s24*pow(s23,3)+23*pow(s23,4)-24*s23*pow(s24,3)-40*pow(s24,4))-22*s23*pow(s24,4)-10*pow(s24,5)))*pow(s12+s13+s14+s24,-3)*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/2.;
}

// Coefficient order epsilon^1 of master 5
template<>
double qq2yyg6CF<5,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*pow(s24,-1)*((s13*(44*s23-15*s24)+3*s14*(13*s23-5*s24)+32*s23*s24+31*(s23*s23)-15*(s24*s24))*pow(s12,7)+(8*s23-3*s24)*pow(s12,8)+s23*(19*s23*s24+6*(s23*s23)+4*(s24*s24))*pow(s13,6)+pow(s12,6)*(10*(10*s23-3*s24)*(s13*s13)+15*(5*s23-2*s24)*(s14*s14)+130*s24*(s23*s23)+s13*(5*s14*(35*s23-12*s24)+159*s23*s24+169*(s23*s23)-69*(s24*s24))+s14*(131*s23*s24+149*(s23*s23)-60*(s24*s24))+45*s23*(s24*s24)+46*pow(s23,3)-30*pow(s24,3))+s14*(s14+s24)*(s23*s23)*(2*(14*s23*s24+5*(s23*s23)+12*(s24*s24))*pow(s14,3)+(4*s23+5*s24)*pow(s14,4)+s24*s24*(17*s24*(s23*s23)+24*s23*(s24*s24)+4*pow(s23,3)+11*pow(s24,3))+s14*s24*(42*s24*(s23*s23)+69*s23*(s24*s24)+11*pow(s23,3)+36*pow(s24,3))+s14*s14*(36*s24*(s23*s23)+69*s23*(s24*s24)+8*pow(s23,3)+44*pow(s24,3)))+pow(s12,5)*(s13*s13*(10*s14*(31*s23-9*s24)+324*s23*s24+385*(s23*s23)-126*(s24*s24))+s14*s14*(203*s23*s24+289*(s23*s23)-90*(s24*s24))+209*(s23*s23)*(s24*s24)+30*(4*s23-s24)*pow(s13,3)+10*(7*s23-3*s24)*pow(s14,3)+173*s24*pow(s23,3)+31*pow(s23,4)+s13*(10*(26*s23-9*s24)*(s14*s14)+622*s24*(s23*s23)+s14*(522*s23*s24+667*(s23*s23)-216*(s24*s24))+211*s23*(s24*s24)+243*pow(s23,3)-126*pow(s24,3))+s14*(539*s24*(s23*s23)+158*s23*(s24*s24)+212*pow(s23,3)-90*pow(s24,3))+19*s23*pow(s24,3)-30*pow(s24,4))+pow(s13,5)*(77*(s23*s23)*(s24*s24)+s14*s23*(90*s23*s24+29*(s23*s23)+15*(s24*s24))+58*s24*pow(s23,3)+20*pow(s23,4)+7*s23*pow(s24,3)-3*pow(s24,4))+s13*s23*((32*s23*s24+19*(s23*s23)-s24*s24)*pow(s14,5)+pow(s14,4)*(163*s24*(s23*s23)+165*s23*(s24*s24)+56*pow(s23,3)+pow(s24,3))+pow(s24,3)*(45*(s23*s23)*(s24*s24)+40*s24*pow(s23,3)+12*pow(s23,4)+16*s23*pow(s24,3)-pow(s24,4))+s14*(s24*s24)*(251*(s23*s23)*(s24*s24)+186*s24*pow(s23,3)+52*pow(s23,4)+114*s23*pow(s24,3)+pow(s24,4))+s24*(s14*s14)*(495*(s23*s23)*(s24*s24)+307*s24*pow(s23,3)+80*pow(s23,4)+281*s23*pow(s24,3)+8*pow(s24,4))+pow(s14,3)*(433*(s23*s23)*(s24*s24)+216*s24*pow(s23,3)+41*pow(s23,4)+316*s23*pow(s24,3)+8*pow(s24,4)))+s23*(s13*s13)*(11*s23*(4*s23+9*s24)*pow(s14,4)+pow(s14,3)*(361*s24*(s23*s23)+409*s23*(s24*s24)+128*pow(s23,3)+18*pow(s24,3))+s24*s24*(163*(s23*s23)*(s24*s24)+132*s24*pow(s23,3)+40*pow(s23,4)+74*s23*pow(s24,3)+5*pow(s24,4))+s14*s24*(608*(s23*s23)*(s24*s24)+398*s24*pow(s23,3)+107*pow(s23,4)+352*s23*pow(s24,3)+28*pow(s24,4))+s14*s14*(762*(s23*s23)*(s24*s24)+383*s24*pow(s23,3)+72*pow(s23,4)+588*s23*pow(s24,3)+41*pow(s24,4)))+s12*s12*((19*s14*s23+63*s23*s24+121*(s23*s23)-9*(s24*s24))*pow(s13,5)+4*s23*pow(s13,6)+pow(s13,4)*(35*s23*(s14*s14)+694*s24*(s23*s23)+s14*(231*s23*s24+493*(s23*s23)-36*(s24*s24))+178*s23*(s24*s24)+312*pow(s23,3)-63*pow(s24,3))+pow(s13,3)*(s14*s14*(314*s23*s24+790*(s23*s23)-54*(s24*s24))+1289*(s23*s23)*(s24*s24)+30*s23*pow(s14,3)+1134*s24*pow(s23,3)+283*pow(s23,4)+s14*(2249*s24*(s23*s23)+521*s23*(s24*s24)+1064*pow(s23,3)-162*pow(s24,3))+189*s23*pow(s24,3)-111*pow(s24,4))+s23*((296*s23*s24+182*(s23*s23)+23*(s24*s24))*pow(s14,4)+(37*s23-s24)*pow(s14,5)-pow(s14,6)+pow(s14,3)*(728*s24*(s23*s23)+713*s23*(s24*s24)+184*pow(s23,3)+58*pow(s24,3))+s24*s24*(36*(s23*s23)*(s24*s24)+48*s24*pow(s23,3)+15*pow(s23,4)-4*s23*pow(s24,3)-7*pow(s24,4))+s14*s24*(454*(s23*s23)*(s24*s24)+292*s24*pow(s23,3)+57*pow(s23,4)+230*s23*pow(s24,3)-pow(s24,4))+s14*s14*(965*(s23*s23)*(s24*s24)+429*s24*pow(s23,3)+48*pow(s23,4)+688*s23*pow(s24,3)+41*pow(s24,4)))+s13*((39*s23*s24+241*(s23*s23)-9*(s24*s24))*pow(s14,4)-s23*pow(s14,5)+pow(s14,3)*(1447*s24*(s23*s23)+211*s23*(s24*s24)+798*pow(s23,3)-36*pow(s24,3))+s14*s14*(2583*(s23*s23)*(s24*s24)+2477*s24*pow(s23,3)+645*pow(s23,4)+317*s23*pow(s24,3)-54*pow(s24,4))+s14*(2303*(s24*s24)*pow(s23,3)+1048*s24*pow(s23,4)+123*pow(s23,5)+1671*(s23*s23)*pow(s24,3)+144*s23*pow(s24,4)-36*pow(s24,5))+s24*(610*(s24*s24)*pow(s23,3)+404*s24*pow(s23,4)+80*pow(s23,5)+297*(s23*s23)*pow(s24,3)-2*s23*pow(s24,4)-9*pow(s24,5)))+s13*s13*(2*(93*s23*s24+311*(s23*s23)-18*(s24*s24))*pow(s14,3)+10*s23*pow(s14,4)+1399*(s24*s24)*pow(s23,3)+634*s24*pow(s23,4)+72*pow(s23,5)+3*(s14*s14)*(902*s24*(s23*s23)+177*s23*(s24*s24)+456*pow(s23,3)-45*pow(s24,3))+1030*(s23*s23)*pow(s24,3)+s14*(3170*(s23*s23)*(s24*s24)+2889*s24*pow(s23,3)+744*pow(s23,4)+456*s23*pow(s24,3)-162*pow(s24,4))+88*s23*pow(s24,4)-63*pow(s24,5)))+s12*(s23*(19*s23+8*s24)*pow(s13,6)+pow(s13,5)*(s14*s23*(91*s23+34*s24)+192*s24*(s23*s23)+42*s23*(s24*s24)+81*pow(s23,3)-9*pow(s24,3))+pow(s13,4)*(s23*(177*s23+55*s24)*(s14*s14)+517*(s23*s23)*(s24*s24)+437*s24*pow(s23,3)+123*pow(s23,4)+s14*(764*s24*(s23*s23)+146*s23*(s24*s24)+340*pow(s23,3)-27*pow(s24,3))+64*s23*pow(s24,3)-33*pow(s24,4))+s23*(s23*(44*s23+59*s24)*pow(s14,5)+(3*s23-s24)*pow(s14,6)-(s23*s24-4*(s23*s23)+s24*s24)*((s23+s24)*(s23+s24))*pow(s24,3)+pow(s14,4)*(249*s24*(s23*s23)+228*s23*(s24*s24)+71*pow(s23,3)+9*pow(s24,3))+s14*s23*(s24*s24)*(107*s24*(s23*s23)+141*s23*(s24*s24)+30*pow(s23,3)+66*pow(s24,3))+s24*(s14*s14)*(442*(s23*s23)*(s24*s24)+263*s24*pow(s23,3)+57*pow(s23,4)+260*s23*pow(s24,3)+9*pow(s24,4))+pow(s14,3)*(507*(s23*s23)*(s24*s24)+235*s24*pow(s23,3)+32*pow(s23,4)+363*s23*pow(s24,3)+16*pow(s24,4)))+s13*s23*((359*s23*s24+223*(s23*s23)+20*(s24*s24))*pow(s14,4)+(27*s23-2*s24)*pow(s14,5)+pow(s14,3)*(1093*s24*(s23*s23)+1093*s23*(s24*s24)+327*pow(s23,3)+88*pow(s24,3))+s24*s24*(223*(s23*s23)*(s24*s24)+186*s24*pow(s23,3)+52*pow(s23,4)+89*s23*pow(s24,3)-2*pow(s24,4))+s14*s24*(1117*(s23*s23)*(s24*s24)+711*s24*pow(s23,3)+160*pow(s23,4)+628*s23*pow(s24,3)+38*pow(s24,4))+s14*s14*(1767*(s23*s23)*(s24*s24)+848*s24*pow(s23,3)+123*pow(s23,4)+1300*s23*pow(s24,3)+106*pow(s24,4)))+s13*s13*(s23*(97*s23+10*s24)*pow(s14,4)+3*pow(s14,3)*(307*s24*(s23*s23)+34*s23*(s24*s24)+164*pow(s23,3)-3*pow(s24,3))+s14*s14*(2041*(s23*s23)*(s24*s24)+1888*s24*pow(s23,3)+564*pow(s23,4)+229*s23*pow(s24,3)-27*pow(s24,4))+s14*(2119*(s24*s24)*pow(s23,3)+1017*s24*pow(s23,4)+144*pow(s23,5)+1606*(s23*s23)*pow(s24,3)+166*s23*pow(s24,4)-27*pow(s24,5))+s24*(711*(s24*s24)*pow(s23,3)+469*s24*pow(s23,4)+107*pow(s23,5)+396*(s23*s23)*pow(s24,3)+29*s23*pow(s24,4)-9*pow(s24,5)))+pow(s13,3)*(2*s23*(89*s23+20*s24)*pow(s14,3)+854*(s24*s24)*pow(s23,3)+404*s24*pow(s23,4)+53*pow(s23,5)+s14*s14*(1193*s24*(s23*s23)+186*s23*(s24*s24)+572*pow(s23,3)-27*pow(s24,3))+655*(s23*s23)*pow(s24,3)+s14*(1693*(s23*s23)*(s24*s24)+1481*s24*pow(s23,3)+431*pow(s23,4)+214*s23*pow(s24,3)-60*pow(s24,4))+61*s23*pow(s24,4)-33*pow(s24,5)))+pow(s12,3)*((5*s14*(23*s23-3*s24)+204*s23*s24+325*(s23*s23)-51*(s24*s24))*pow(s13,4)+(28*s23-3*s24)*pow(s13,5)+(38*s23*s24+149*(s23*s23)-15*(s24*s24))*pow(s14,4)+3*(s23-s24)*pow(s14,5)+pow(s13,3)*(30*(6*s23-s24)*(s14*s14)+1250*s24*(s23*s23)+2*s14*(310*s23*s24+541*(s23*s23)-84*(s24*s24))+367*s23*(s24*s24)+556*pow(s23,3)-165*pow(s24,3))+2*pow(s14,3)*(357*s24*(s23*s23)+57*s23*(s24*s24)+184*pow(s23,3)-15*pow(s24,3))+s13*s13*(6*(s14*s14)*(111*s23*s24+223*(s23*s23)-33*(s24*s24))+1544*(s23*s23)*(s24*s24)+10*(13*s23-3*s24)*pow(s14,3)+1362*s24*pow(s23,3)+308*pow(s23,4)+s14*(3105*s24*(s23*s23)+822*s23*(s24*s24)+1436*pow(s23,3)-333*pow(s24,3))+233*s23*pow(s24,3)-165*pow(s24,4))+s14*s14*(1067*(s23*s23)*(s24*s24)+1010*s24*pow(s23,3)+226*pow(s23,4)+108*s23*pow(s24,3)-30*pow(s24,4))+s13*((288*s23*s24+730*(s23*s23)-96*(s24*s24))*pow(s14,3)+5*(8*s23-3*s24)*pow(s14,4)+969*(s24*s24)*pow(s23,3)+416*s24*pow(s23,4)+41*pow(s23,5)+s14*s14*(2567*s24*(s23*s23)+569*s23*(s24*s24)+1246*pow(s23,3)-198*pow(s24,3))+675*(s23*s23)*pow(s24,3)+s14*(2559*(s23*s23)*(s24*s24)+2327*s24*pow(s23,3)+533*pow(s23,4)+350*s23*pow(s24,3)-168*pow(s24,4))+23*s23*pow(s24,4)-51*pow(s24,5))+s14*(797*(s24*s24)*pow(s23,3)+337*s24*pow(s23,4)+32*pow(s23,5)+547*(s23*s23)*pow(s24,3)+11*s23*pow(s24,4)-15*pow(s24,5))+s24*(150*(s24*s24)*pow(s23,3)+107*s24*pow(s23,4)+19*pow(s23,5)+43*(s23*s23)*pow(s24,3)-18*s23*pow(s24,4)-3*pow(s24,5)))+pow(s13,3)*(2*s23*(87*s23*s24+32*(s23*s23)+5*(s24*s24))*pow(s14,3)+s14*s14*(516*(s23*s23)*(s24*s24)+419*s24*pow(s23,3)+148*pow(s23,4)+40*s23*pow(s24,3)-3*pow(s24,4))+s14*(608*(s24*s24)*pow(s23,3)+302*s24*pow(s23,4)+53*pow(s23,5)+486*(s23*s23)*pow(s24,3)+43*s23*pow(s24,4)-6*pow(s24,5))+s24*(250*(s24*s24)*pow(s23,3)+169*s24*pow(s23,4)+46*pow(s23,5)+146*(s23*s23)*pow(s24,3)+13*s23*pow(s24,4)-3*pow(s24,5)))+pow(s12,4)*(2*(15*s14*(9*s23-2*s24)+173*s23*s24+235*(s23*s23)-57*(s24*s24))*pow(s13,3)+5*(16*s23-3*s24)*pow(s13,4)+2*(71*s23*s24+143*(s23*s23)-30*(s24*s24))*pow(s14,3)+15*(2*s23-s24)*pow(s14,4)+242*(s24*s24)*pow(s23,3)+97*s24*pow(s23,4)+8*pow(s23,5)+s13*s13*(30*(11*s23-3*s24)*(s14*s14)+2*s14*(407*s23*s24+599*(s23*s23)-144*(s24*s24))+3*(407*s24*(s23*s23)+131*s23*(s24*s24)+172*pow(s23,3)-69*pow(s24,3)))+s14*s14*(881*s24*(s23*s23)+204*s23*(s24*s24)+392*pow(s23,3)-90*pow(s24,3))+154*(s23*s23)*pow(s24,3)+s13*(2*(s14*s14)*(305*s23*s24+507*(s23*s23)-117*(s24*s24))+904*(s23*s23)*(s24*s24)+10*(17*s23-6*s24)*pow(s14,3)+780*s24*pow(s23,3)+159*pow(s23,4)+s14*(2069*s24*(s23*s23)+590*s23*(s24*s24)+895*pow(s23,3)-288*pow(s24,3))+120*s23*pow(s24,3)-114*pow(s24,4))+2*s14*(381*(s23*s23)*(s24*s24)+336*s24*pow(s23,3)+67*pow(s23,4)+39*s23*pow(s24,3)-30*pow(s24,4))-14*s23*pow(s24,4)-15*pow(s24,5))+pow(s13,4)*(s23*(s14*s14)*(173*s23*s24+58*(s23*s23)+20*(s24*s24))+182*(s24*s24)*pow(s23,3)+89*s24*pow(s23,4)+14*pow(s23,5)+146*(s23*s23)*pow(s24,3)+s14*(320*(s23*s23)*(s24*s24)+247*s24*pow(s23,3)+86*pow(s23,4)+30*s23*pow(s24,3)-6*pow(s24,4))+10*s23*pow(s24,4)-6*pow(s24,5)))*pow(s12+s13+s14+s24,-3)*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/2.;
}

// Coefficient of master 5 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6CF<5>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6CF<5,-1>(s12,s13,s14,s23,s24),
        qq2yyg6CF<5,0>(s12,s13,s14,s23,s24),
        qq2yyg6CF<5,1>(s12,s13,s14,s23,s24)
    });
}

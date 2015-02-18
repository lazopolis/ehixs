/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 13: box(s12,-s12-s13-s14,-s12-s13-s14-s23-s24)

// Coefficient order epsilon^0 of master 13
template<>
double qq2yygCAm2CF<13,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (s12*(-2*(s12+s23+s24)*pow(s23,-1)+(s13*(-2*s24*(s23+s24)+s14*(3*s23+s24))+s14*(2*s23*(s23+s24)+s14*(3*s23+2*s24))+(s13+7*s14)*(s12*s12)-s24*(s13*s13)-s12*(s13*(-2*s14-s23+s24)-s14*(9*s23+7*s24)+s13*s13-3*(s14*s14)+2*((s23+s24)*(s23+s24)))+2*pow(s12,3))*pow(s13,-1)*pow(s12+s23+s24,-2)+(-(s23*s24*(s13*s13))+s12*s12*(2*s14*(s23-s24)+s13*(s14-s23+s24)+2*s24*(s23+s24)+s14*s14)+s13*(s14*s23*(s23-s24)+(s23+s24)*(s24*s24))+s14*(s14*(s23*s23)-s24*(5*s23*s24+3*(s23*s23)+2*(s24*s24)))+s12*(-(s23*(s13*s13))+2*s23*(s14*s14)+s14*(-5*s23*s24+s23*s23-5*(s24*s24))+s13*(s14*(2*s23+s24)-s23*s23+2*(s24*s24))+s24*((s23+s24)*(s23+s24)))+(s14+s24)*pow(s12,3))*pow(s13,-1)*pow(s24,-1)*pow(s12+s23+s24,-2)+4*s12*(s23+s24)*pow(s13,-1)*pow(s12+s23+s24,-1)-(-(s14*s23)+s12*(s13-s14+s23-s24)+s24*(s13-2*(s23+s24))+s12*s12)*pow(s23,-1)*pow(s12+s23+s24,-1)+((s13+s14+2*s23)*(s12*s12)+s12*(-(s24*(s14+s24))+s13*(3*s23+2*s24)+s23*s23)+(s23+s24)*(-(s14*s23)+4*s23*s24+s24*(s13+2*s24)+2*(s23*s23))+3*pow(s12,3))*pow(s13,-1)*pow(s24,-1)*pow(s12+s23+s24,-1)+s12*(s13*(2*s23+s24)+s12*(s13+s14+3*s23+s24)+s23*(s14+2*(s23+s24))+s12*s12)*pow(s23,-1)*pow(s12+s13+s23,-1)*pow(s12+s13+s14+s23+s24,-1)-(s23+s24)*(s12*(s13+s24)+(s13+s23)*(s23+s24)+s12*s12)*(s13*(2*s23+s24)+s12*(s13+s14+3*s23+s24)+s23*(s14+2*(s23+s24))+s12*s12)*pow(s13,-1)*pow(s23,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*pow(s12+s13+s14+s23+s24,-1)+(s12+s23+s24)*(s14*(s23+2*s24)+s12*(s13+s14+s23+3*s24)+s24*(s13+2*(s23+s24))+s12*s12)*pow(s23,-1)*pow(s12+s14+s24,-1)*pow(s12+s13+s14+s23+s24,-1)+s14*(s12+s13+s14)*(s14*(s23+2*s24)+s12*(s13+s14+s23+3*s24)+s24*(s13+2*(s23+s24))+s12*s12)*pow(s13,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)*pow(s12+s13+s14+s23+s24,-1)+2*s12*(s12+s13)*(s23+s24)*(s13*(2*s23+s24)+s12*(s13+s14+3*s23+s24)+s23*(s14+2*(s23+s24))+s12*s12)*pow(s13,-1)*pow(s23,-1)*pow(s12+s13+s23,-1)*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1)-(s12+s14)*(s14*(s23+2*s24)+s12*(s13+s14+s23+3*s24)+s24*(s13+2*(s23+s24))+s12*s12)*(s12*(2*s13+3*s14)+s14*(s13+s14+s23+s24)+2*(s12*s12))*pow(s13,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1)))/4.;
}

// Coefficient order epsilon^1 of master 13
template<>
double qq2yygCAm2CF<13,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (s12*pow(s13,-1)*pow(s23,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)*((51*s23*s24+s14*(39*s23+38*s24)+s13*(29*s14+44*s23+41*s24)+15*(s13*s13)+12*(s14*s14)+19*(s23*s23)+28*(s24*s24))*pow(s12,6)+(9*s13+8*s14+10*s23+12*s24)*pow(s12,7)+2*pow(s12,8)-s24*pow(s13,4)*(2*s23*(s24*s24)+s14*(3*s23*s24+3*(s23*s23)+s24*s24)-2*pow(s23,3)+pow(s24,3))+pow(s12,5)*((37*s14+69*s23+56*s24)*(s13*s13)+(57*s23+44*s24)*(s14*s14)+82*s24*(s23*s23)+98*s23*(s24*s24)+s14*(152*s23*s24+73*(s23*s23)+68*(s24*s24))+s13*(164*s23*s24+4*s14*(33*s23+26*s24)+33*(s14*s14)+83*(s23*s23)+69*(s24*s24))+11*pow(s13,3)+8*pow(s14,3)+16*pow(s23,3)+34*pow(s24,3))+s13*s23*(s23*(11*s23*s24+8*(s23*s23)+5*(s24*s24))*pow(s14,3)+(4*s23*s24+3*(s23*s23)+2*(s24*s24))*pow(s14,4)+s24*((s23+s24)*(s23+s24))*(s24*(s23*s23)-3*s23*(s24*s24)+2*pow(s23,3)-4*pow(s24,3))-s24*(s14*s14)*(7*s24*(s23*s23)+16*s23*(s24*s24)-4*pow(s23,3)+8*pow(s24,3))+s14*s24*(-15*(s23*s23)*(s24*s24)+5*s24*pow(s23,3)+7*pow(s23,4)-23*s23*pow(s24,3)-10*pow(s24,4)))+pow(s13,3)*(3*(s14*s14)*(-(s24*(s23*s23))-2*s23*(s24*s24)+pow(s23,3)-pow(s24,3))+s14*(-(s23*s23*(s24*s24))+7*s24*pow(s23,3)+2*pow(s23,4)-8*s23*pow(s24,3)-4*pow(s24,4))+s24*(4*(s23*s23)*(s24*s24)+12*s24*pow(s23,3)+8*pow(s23,4)-3*s23*pow(s24,3)-2*pow(s24,4)))+pow(s12,4)*(127*(s23*s23)*(s24*s24)+s14*s14*(165*s23*s24+104*(s23*s23)+58*(s24*s24))+s13*s13*(4*s14*(38*s23+27*s24)+29*(s14*s14)+4*(52*s23*s24+29*(s23*s23)+18*(s24*s24)))+(19*s14+46*s23+35*s24)*pow(s13,3)+3*pow(s13,4)+(37*s23+22*s24)*pow(s14,3)+2*pow(s14,4)+61*s24*pow(s23,3)+4*pow(s23,4)+96*s23*pow(s24,3)+3*s14*(76*s24*(s23*s23)+72*s23*(s24*s24)+21*pow(s23,3)+20*pow(s24,3))+s13*((141*s23+89*s24)*(s14*s14)+254*s24*(s23*s23)+207*s23*(s24*s24)+s14*(370*s23*s24+224*(s23*s23)+129*(s24*s24))+15*pow(s14,3)+77*pow(s23,3)+51*pow(s24,3))+24*pow(s24,4))+s14*(s23*s23)*(s24*(s23*s24+s23*s23+2*(s24*s24))*((s23+s24)*(s23+s24))+(4*s23*s24+3*(s23*s23)+2*(s24*s24))*pow(s14,3)+s14*s14*(8*s24*(s23*s23)+9*s23*(s24*s24)+2*pow(s23,3)+4*pow(s24,3))+s14*(6*(s23*s23)*(s24*s24)+s24*pow(s23,3)-pow(s23,4)+8*s23*pow(s24,3)+4*pow(s24,4)))+s13*s13*(pow(s14,3)*(4*s24*(s23*s23)-s23*(s24*s24)+6*pow(s23,3)-2*pow(s24,3))+s14*s14*(-13*(s23*s23)*(s24*s24)+3*s24*pow(s23,3)+5*pow(s23,4)-17*s23*pow(s24,3)-6*pow(s24,4))+s14*(10*(s24*s24)*pow(s23,3)+18*s24*pow(s23,4)+2*pow(s23,5)-18*(s23*s23)*pow(s24,3)-21*s23*pow(s24,4)-6*pow(s24,5))+s24*(10*(s24*s24)*pow(s23,3)+18*s24*pow(s23,4)+8*pow(s23,5)-7*(s23*s23)*pow(s24,3)-9*s23*pow(s24,4)-2*pow(s24,5)))+pow(s12,3)*((113*s23*s24+s14*(68*s23+45*s24)+8*(s14*s14)+63*(s23*s23)+37*(s24*s24))*pow(s13,3)+(3*s14+11*s23+8*s24)*pow(s13,4)+(76*s23*s24+65*(s23*s23)+20*(s24*s24))*pow(s14,3)+(9*s23+4*s24)*pow(s14,4)+76*(s24*s24)*pow(s23,3)+21*s24*pow(s23,4)-2*pow(s23,5)+97*(s23*s23)*pow(s24,3)+s13*s13*((109*s23+60*s24)*(s14*s14)+283*s24*(s23*s23)+201*s23*(s24*s24)+s14*(329*s23*s24+214*(s23*s23)+97*(s24*s24))+7*pow(s14,3)+91*pow(s23,3)+28*pow(s24,3))+s14*s14*(228*s24*(s23*s23)+165*s23*(s24*s24)+89*pow(s23,3)+34*pow(s24,3))+54*s23*pow(s24,4)+s13*(239*(s23*s23)*(s24*s24)+s14*s14*(275*s23*s24+216*(s23*s23)+75*(s24*s24))+30*(2*s23+s24)*pow(s14,3)+2*pow(s14,4)+200*s24*pow(s23,3)+36*pow(s23,4)+88*s23*pow(s24,3)+s14*(472*s24*(s23*s23)+314*s23*(s24*s24)+175*pow(s23,3)+60*pow(s24,3))+14*pow(s24,4))+s14*(247*(s23*s23)*(s24*s24)+155*s24*pow(s23,3)+21*pow(s23,4)+148*s23*pow(s24,3)+28*pow(s24,4))+10*pow(s24,5))+s12*s12*((20*s23*s24+s14*(9*s23+5*s24)+11*(s23*s23)+6*(s24*s24))*pow(s13,4)+(12*s23*s24+15*(s23*s23)+2*(s24*s24))*pow(s14,4)+pow(s14,3)*(94*s24*(s23*s23)+49*s23*(s24*s24)+53*pow(s23,3)+6*pow(s24,3))+pow(s13,3)*((26*s23+11*s24)*(s14*s14)+114*s24*(s23*s23)+85*s23*(s24*s24)+s14*(107*s23*s24+73*(s23*s23)+29*(s24*s24))+34*pow(s23,3)+11*pow(s24,3))+s13*s13*(207*(s23*s23)*(s24*s24)+s14*s14*(142*s23*s24+129*(s23*s23)+29*(s24*s24))+2*(12*s23+5*s24)*pow(s14,3)+183*s24*pow(s23,3)+33*pow(s23,4)+45*s23*pow(s24,3)+s14*(321*s24*(s23*s23)+180*s23*(s24*s24)+127*pow(s23,3)+12*pow(s24,3))-13*pow(s24,4))-(s23+s24)*(s23+s24)*(-12*(s23*s23)*(s24*s24)-6*s24*pow(s23,3)+pow(s23,4)-13*s23*pow(s24,3)-2*pow(s24,4))+s14*s14*(161*(s23*s23)*(s24*s24)+138*s24*pow(s23,3)+33*pow(s23,4)+71*s23*pow(s24,3)+8*pow(s24,4))+s13*((77*s23*s24+83*(s23*s23)+17*(s24*s24))*pow(s14,3)+(7*s23+4*s24)*pow(s14,4)+s14*s14*(277*s24*(s23*s23)+135*s23*(s24*s24)+141*pow(s23,3)+17*pow(s24,3))+s23*(143*(s23*s23)*(s24*s24)+87*s24*pow(s23,3)+7*pow(s23,4)+52*s23*pow(s24,3)-13*pow(s24,4))+s14*(246*(s23*s23)*(s24*s24)+265*s24*pow(s23,3)+61*pow(s23,4)+54*s23*pow(s24,3)+4*pow(s24,4)))+s14*(122*(s24*s24)*pow(s23,3)+45*s24*pow(s23,4)-2*pow(s23,5)+124*(s23*s23)*pow(s24,3)+53*s23*pow(s24,4)+6*pow(s24,5)))+s12*((s14*(6*s23*s24+7*(s23*s23)+s24*s24)+s23*(11*s23*s24+2*(s23*s23)+7*(s24*s24)))*pow(s13,4)+pow(s13,3)*(2*s23*(11*s23+8*s24)*(s14*s14)+55*(s23*s23)*(s24*s24)+47*s24*pow(s23,3)+6*pow(s23,4)+s14*(59*s24*(s23*s23)+31*s23*(s24*s24)+25*pow(s23,3)-pow(s24,3))+15*s23*pow(s24,3)-4*pow(s24,4))+s13*(2*((2*s23+s24)*(2*s23+s24))*pow(s14,4)+2*pow(s14,3)*(29*s24*(s23*s23)+11*s23*(s24*s24)+23*pow(s23,3)+pow(s24,3))+s23*s24*(16*(s23*s23)*(s24*s24)+45*s24*pow(s23,3)+20*pow(s23,4)-27*s23*pow(s24,3)-18*pow(s24,4))+s14*s14*(52*(s23*s23)*(s24*s24)+95*s24*pow(s23,3)+33*pow(s23,4)-5*s23*pow(s24,3)-2*pow(s24,4))+s14*(69*(s24*s24)*pow(s23,3)+66*s24*pow(s23,4)+7*pow(s23,5)-14*(s23*s23)*pow(s24,3)-32*s23*pow(s24,4)-2*pow(s24,5)))+s13*s13*((18*s23*s24+23*(s23*s23)+s24*s24)*pow(s14,3)+98*(s24*s24)*pow(s23,3)+58*s24*pow(s23,4)+4*pow(s23,5)+s14*s14*(77*s24*(s23*s23)+20*s23*(s24*s24)+54*pow(s23,3)-8*pow(s24,3))+33*(s23*s23)*pow(s24,3)+s14*(85*(s23*s23)*(s24*s24)+124*s24*pow(s23,3)+30*pow(s23,4)-18*s23*pow(s24,3)-20*pow(s24,4))-26*s23*pow(s24,4)-12*pow(s24,5))+s23*((12*s23*s24+11*(s23*s23)+4*(s24*s24))*pow(s14,4)+pow(s14,3)*(48*s24*(s23*s23)+38*s23*(s24*s24)+19*pow(s23,3)+10*pow(s24,3))+s14*s14*(60*(s23*s23)*(s24*s24)+32*s24*pow(s23,3)+2*pow(s23,4)+45*s23*pow(s24,3)+12*pow(s24,4))+s14*(26*(s24*s24)*pow(s23,3)+5*s24*pow(s23,4)-2*pow(s23,5)+41*(s23*s23)*pow(s24,3)+30*s23*pow(s24,4)+8*pow(s24,5))+s24*(s23*s24+s23*s23+2*(s24*s24))*pow(s23+s24,3))))*pow(s12+s23+s24,-2)*pow(s12+s13+s14+s23+s24,-1))/4.;
}

// Coefficient order epsilon^2 of master 13
template<>
double qq2yygCAm2CF<13,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (s12*pow(s13,-1)*pow(s23,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)*((51*s23*s24+s14*(76*s23+42*s24)+s13*(52*s14+73*s23+52*s24)+24*(s13*s13)+24*(s14*s14)+40*(s23*s23)+14*(s24*s24))*pow(s12,6)+(16*s13+16*s14+20*s23+13*s24)*pow(s12,7)+4*pow(s12,8)+pow(s12,5)*((60*s14+97*s23+72*s24)*(s13*s13)+(108*s23+47*s24)*(s14*s14)+72*s24*(s23*s23)+36*s23*(s24*s24)+2*s14*(79*s23*s24+72*(s23*s23)+16*(s24*s24))+s13*(187*s23*s24+s14*(231*s23+130*s24)+60*(s14*s14)+130*(s23*s23)+64*(s24*s24))+16*pow(s13,3)+16*pow(s14,3)+40*pow(s23,3)+3*pow(s24,3))+pow(s12,4)*(16*(s23*s23)*(s24*s24)+6*(s14*s14)*(28*s23*s24+32*(s23*s23)+3*(s24*s24))+s13*s13*(220*s23*s24+s14*(244*s23+127*s24)+48*(s14*s14)+149*(s23*s23)+76*(s24*s24))+(28*s14+55*s23+41*s24)*pow(s13,3)+4*pow(s13,4)+4*(17*s23+5*s24)*pow(s14,3)+4*pow(s14,4)+38*s24*pow(s23,3)+20*pow(s23,4)+2*s14*(109*s24*(s23*s23)+41*s23*(s24*s24)+68*pow(s23,3)-pow(s24,3))-8*s23*pow(s24,3)+s13*(2*(130*s23+53*s24)*(s14*s14)+241*s24*(s23*s23)+174*s23*(s24*s24)+s14*(446*s23*s24+399*(s23*s23)+102*(s24*s24))+28*pow(s14,3)+107*pow(s23,3)+42*pow(s24,3))-5*pow(s24,4))-(s23+s24)*((s24*(3*s23*s24-6*(s23*s23)+4*(s24*s24))+s14*(8*s23*s24-8*(s23*s23)+6*(s24*s24)))*pow(s13,4)+s14*(s23*s23)*(s14*s24*(3*s23*s24+s23*s23+3*(s24*s24))+s14*s14*(-3*s23*s24-4*(s23*s23)+4*(s24*s24))+(-4*s23+2*s24)*pow(s14,3)+s24*(4*s24*(s23*s23)+3*s23*(s24*s24)+2*pow(s23,3)+pow(s24,3)))+pow(s13,3)*(-4*(s14*s14)*(-5*s23*s24+6*(s23*s23)-3*(s24*s24))+s24*(5*s24*(s23*s23)+10*s23*(s24*s24)-4*pow(s23,3)+pow(s24,3))+s14*(-4*s24*(s23*s23)+31*s23*(s24*s24)-10*pow(s23,3)+12*pow(s24,3)))+s13*s13*((14*s23*s24-26*(s23*s23)+6*(s24*s24))*pow(s14,3)+s14*s24*(12*s24*(s23*s23)+10*s23*(s24*s24)-4*pow(s23,3)-3*pow(s24,3))+s14*s14*(-10*s24*(s23*s23)+32*s23*(s24*s24)-31*pow(s23,3)+6*pow(s24,3))+s24*(s23*s23*(s24*s24)+10*s24*pow(s23,3)+6*pow(s23,4)-7*s23*pow(s24,3)-3*pow(s24,4)))-s13*s23*((8*s23*s24+25*(s23*s23)-8*(s24*s24))*pow(s14,3)+2*(5*s23-s24)*pow(s14,4)+3*(s14*s14)*(6*s24*(s23*s23)+3*pow(s23,3)-pow(s24,3))-2*s14*(-(s23*s23*(s24*s24))+3*s24*pow(s23,3)+pow(s23,4)-6*s23*pow(s24,3)-4*pow(s24,4))+s24*(4*(s23*s23)*(s24*s24)-6*s24*pow(s23,3)-4*pow(s23,4)+11*s23*pow(s24,3)+5*pow(s24,4))))+pow(s12,3)*((100*s23*s24+6*s14*(17*s23+7*s24)+12*(s14*s14)+71*(s23*s23)+26*(s24*s24))*pow(s13,3)+(4*s14+11*s23+8*s24)*pow(s13,4)+(67*s23*s24+112*(s23*s23)-2*(s24*s24))*pow(s14,3)+2*(8*s23+s24)*pow(s14,4)-28*(s24*s24)*pow(s23,3)-3*s24*pow(s23,4)+4*pow(s23,5)+s14*s14*(221*s24*(s23*s23)+44*s23*(s24*s24)+168*pow(s23,3)-12*pow(s24,3))-38*(s23*s23)*pow(s24,3)+s13*s13*((195*s23+64*s24)*(s14*s14)+233*s24*(s23*s23)+154*s23*(s24*s24)+s14*(366*s23*s24+373*(s23*s23)+61*(s24*s24))+12*pow(s14,3)+98*pow(s23,3)+39*pow(s24,3))+s14*(52*(s23*s23)*(s24*s24)+124*s24*pow(s23,3)+64*pow(s23,4)-23*s23*pow(s24,3)-12*pow(s24,4))-21*s23*pow(s24,4)+s13*(148*(s23*s23)*(s24*s24)+s14*s14*(348*s23*s24+426*(s23*s23)+32*(s24*s24))+30*(4*s23+s24)*pow(s14,3)+4*pow(s14,4)+107*s24*pow(s23,3)+34*pow(s23,4)+96*s23*pow(s24,3)+s14*(563*s24*(s23*s23)+249*s23*(s24*s24)+324*pow(s23,3)+23*pow(s24,3))+22*pow(s24,4))-4*pow(s24,5))+s12*s12*((s14*(13*s23+2*s24)+3*s23*(4*s23+5*s24))*pow(s13,4)+(6*s23*s24+24*(s23*s23)-2*(s24*s24))*pow(s14,4)+pow(s13,3)*((44*s23+8*s24)*(s14*s14)+89*s24*(s23*s23)+s14*(87*s23*s24+136*(s23*s23)-12*(s24*s24))+24*s23*(s24*s24)+38*pow(s23,3)-8*pow(s24,3))+pow(s14,3)*(81*s24*(s23*s23)-5*s23*(s24*s24)+88*pow(s23,3)-6*pow(s24,3))-s24*((s23+s24)*(s23+s24))*(12*s24*(s23*s23)+7*s23*(s24*s24)+9*pow(s23,3)+pow(s24,3))+s14*s14*(30*(s23*s23)*(s24*s24)+125*s24*pow(s23,3)+72*pow(s23,4)-29*s23*pow(s24,3)-7*pow(s24,4))+s13*s13*(2*(s14*s14)*(86*s23*s24+141*(s23*s23)-6*(s24*s24))+81*(s23*s23)*(s24*s24)+(49*s23+8*s24)*pow(s14,3)+75*s24*pow(s23,3)+20*pow(s23,4)+s14*(372*s24*(s23*s23)+77*s23*(s24*s24)+247*pow(s23,3)-16*pow(s24,3))+46*s23*pow(s24,3)+21*pow(s24,4))+s14*(-20*(s24*s24)*pow(s23,3)+14*s24*pow(s23,4)+12*pow(s23,5)-47*(s23*s23)*pow(s24,3)-28*s23*pow(s24,4)-4*pow(s24,5))+s13*((99*s23*s24+181*(s23*s23)-8*(s24*s24))*pow(s14,3)+2*(9*s23+s24)*pow(s14,4)+9*(s24*s24)*pow(s23,3)-19*s24*pow(s23,4)-2*pow(s23,5)+s14*s14*(413*s24*(s23*s23)+64*s23*(s24*s24)+321*pow(s23,3)-23*pow(s24,3))+73*(s23*s23)*pow(s24,3)+s14*s23*(286*s24*(s23*s23)+216*s23*(s24*s24)+113*pow(s23,3)+43*pow(s24,3))+58*s23*pow(s24,4)+10*pow(s24,5)))+s12*(pow(s13,4)*(15*s24*(s23*s23)+s14*(-(s23*s24)+16*(s23*s23)-8*(s24*s24))-3*s23*(s24*s24)+6*pow(s23,3)-8*pow(s24,3))+pow(s13,3)*(s14*s14*(9*s23*s24+55*(s23*s23)-16*(s24*s24))+3*(s23*s23)*(s24*s24)+30*s24*pow(s23,3)+6*pow(s23,4)+s14*(65*s24*(s23*s23)-58*s23*(s24*s24)+73*pow(s23,3)-38*pow(s24,3))-32*s23*pow(s24,3)-10*pow(s24,4))+s13*s13*((20*s23*s24+63*(s23*s23)-10*(s24*s24))*pow(s14,3)-19*(s24*s24)*pow(s23,3)-13*s24*pow(s23,4)-2*pow(s23,5)+2*(s14*s14)*(77*s24*(s23*s23)-26*s23*(s24*s24)+83*pow(s23,3)-17*pow(s24,3))+3*(s23*s23)*pow(s24,3)+s14*(25*(s23*s23)*(s24*s24)+129*s24*pow(s23,3)+58*pow(s23,4)-52*s23*pow(s24,3)-7*pow(s24,4))+25*s23*pow(s24,4)+13*pow(s24,5))+s13*(2*(5*s23*s24+12*(s23*s23)-s24*s24)*pow(s14,4)-36*(s24*s24)*pow(s23,4)-24*s24*pow(s23,5)-2*pow(s23,6)+2*pow(s14,3)*(51*s24*(s23*s23)-7*s23*(s24*s24)+57*pow(s23,3)-5*pow(s24,3))+13*pow(s23,3)*pow(s24,3)+s14*s14*(62*(s23*s23)*(s24*s24)+198*s24*pow(s23,3)+104*pow(s23,4)-36*s23*pow(s24,3)-9*pow(s24,4))+52*(s23*s23)*pow(s24,4)+27*s23*pow(s24,5)+s14*(56*(s24*s24)*pow(s23,3)+31*s24*pow(s23,4)+7*pow(s23,5)+46*(s23*s23)*pow(s24,3)+17*s23*pow(s24,4)+pow(s24,5))+2*pow(s24,6))-s23*(-2*(3*s23*s24+8*(s23*s23)-2*(s24*s24))*pow(s14,4)+pow(s14,3)*(-41*s24*(s23*s23)+4*s23*(s24*s24)-32*pow(s23,3)+10*pow(s24,3))+s14*s24*(33*(s23*s23)*(s24*s24)+28*s24*pow(s23,3)+10*pow(s23,4)+20*s23*pow(s24,3)+5*pow(s24,4))+s14*s14*(-24*s24*pow(s23,3)-12*pow(s23,4)+23*s23*pow(s24,3)+10*pow(s24,4))+s24*(2*s23*s24+2*(s23*s23)+s24*s24)*pow(s23+s24,3))))*pow(s12+s23+s24,-2)*pow(s12+s13+s14+s23+s24,-1))/4.;
}

// Coefficient of master 13 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCAm2CF<13>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCAm2CF<13,0>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<13,1>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<13,2>(s12,s13,s14,s23,s24)
    });
}

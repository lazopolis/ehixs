/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 26: box6(s24,s12+s13+s23,s13)

// Coefficient order epsilon^-1 of master 26
template<>
double qq2yygSC<26,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*((2*s13*(s14*(s23+s24)+3*s24*(2*s23+s24))+s24*(s13*s13)+(2*s23+s24)*(s14*s14)+s24*(18*s23*s24+13*(s23*s23)+2*(s24*s24))+s14*(13*s23*s24+4*(s23*s23)+5*(s24*s24)))*pow(s12,5)+(s14*(s23+2*s24)+s24*(2*s13+6*s23+3*s24))*pow(s12,6)+s24*pow(s12,7)+s23*(s14+s24)*pow(s13,3)*pow(s24,3)+s14*(s23*s23)*((s23+3*s24)*(s24*s24)*((s23+s24)*(s23+s24))+s23*s24*pow(s14,3)+s14*s14*pow(s23,3)+2*s14*s24*(2*s24*(s23*s23)+2*s23*(s24*s24)+pow(s23,3)+pow(s24,3)))-s24*(s13*s13)*(s23*(2*s23-s24)*s24*(s14*s14)+s24*s24*(-3*s24*(s23*s23)+s23*(s24*s24)-3*pow(s23,3)+pow(s24,3))-s14*s23*(6*s24*(s23*s23)+5*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3)))+pow(s12,4)*((s14*s23+s24*(7*s23+3*s24))*(s13*s13)+s14*s14*(7*s23*s24+8*(s23*s23)+2*(s24*s24))+s13*(2*s23*(s14*s14)+s24*(35*s23*s24+22*(s23*s23)+4*(s24*s24))+s14*(18*s23*s24+7*(s23*s23)+5*(s24*s24)))+(s23+s24)*pow(s14,3)+s24*(35*s24*(s23*s23)+17*s23*(s24*s24)+13*pow(s23,3)-2*pow(s24,3))+s14*(30*s24*(s23*s23)+29*s23*(s24*s24)+6*pow(s23,3)+3*pow(s24,3)))+pow(s12,3)*(s13*s13*(s14*s23*(3*s23+7*s24)+s24*(20*s23*s24+9*(s23*s23)+2*(s24*s24)))+s23*s24*pow(s13,3)+(3*s23*s24+4*(s23*s23)+s24*s24)*pow(s14,3)+s24*pow(s14,4)+s14*s14*(17*s24*(s23*s23)+9*s23*(s24*s24)+12*pow(s23,3)+pow(s24,3))+s13*(s23*(7*s23+4*s24)*(s14*s14)+s24*pow(s14,3)+4*s24*(14*s24*(s23*s23)+8*s23*(s24*s24)+4*pow(s23,3)-pow(s24,3))+s14*(39*s24*(s23*s23)+37*s23*(s24*s24)+9*pow(s23,3)+3*pow(s24,3)))+s24*(31*(s23*s23)*(s24*s24)+29*s24*pow(s23,3)+6*pow(s23,4)+3*s23*pow(s24,3)-3*pow(s24,4))+s14*(56*(s23*s23)*(s24*s24)+32*s24*pow(s23,3)+4*pow(s23,4)+25*s23*pow(s24,3)-pow(s24,4)))+s13*s23*(s23*(s23-2*s24)*s24*pow(s14,3)+(s23-s24)*((s23+s24)*(s23+s24))*pow(s24,3)+s14*s14*(6*(s23*s23)*(s24*s24)+s24*pow(s23,3)+pow(s23,4)+5*s23*pow(s24,3)+2*pow(s24,4))+s14*s24*(11*(s23*s23)*(s24*s24)+5*s24*pow(s23,3)+2*pow(s23,4)+11*s23*pow(s24,3)+3*pow(s24,4)))+s12*(s23*(2*s14+3*s24)*(s24*s24)*pow(s13,3)+s13*s13*(s23*(s14*s14)*(s24*s24)+s24*s24*(15*s24*(s23*s23)+4*s23*(s24*s24)+6*pow(s23,3)-3*pow(s24,3))+s14*s23*(7*s24*(s23*s23)+15*s23*(s24*s24)+pow(s23,3)+11*pow(s24,3)))+s23*((2*s23*s24+s23*s23-s24*s24)*(s24*s24)*((s23+s24)*(s23+s24))+s23*(s23*s24+4*(s23*s23)+s24*s24)*pow(s14,3)+3*s23*s24*pow(s14,4)+s14*s14*(13*(s23*s23)*(s24*s24)+10*s24*pow(s23,3)+2*pow(s23,4)+9*s23*pow(s24,3)+2*pow(s24,4))+s14*s24*(28*(s23*s23)*(s24*s24)+15*s24*pow(s23,3)+3*pow(s23,4)+18*s23*pow(s24,3)+2*pow(s24,4)))+s13*(s23*(3*s23-s24)*s24*pow(s14,3)+s23*(s14*s14)*(6*s24*(s23*s23)+12*s23*(s24*s24)+5*pow(s23,3)+6*pow(s24,3))+s24*s24*(10*(s23*s23)*(s24*s24)+19*s24*pow(s23,3)+7*pow(s23,4)-4*s23*pow(s24,3)-2*pow(s24,4))+s14*(31*(s24*s24)*pow(s23,3)+13*s24*pow(s23,4)+pow(s23,5)+37*(s23*s23)*pow(s24,3)+13*s23*pow(s24,4)-pow(s24,5))))+s12*s12*(s23*s24*(s14+3*s24)*pow(s13,3)+s23*(3*s23*s24+6*(s23*s23)+2*(s24*s24))*pow(s14,3)+3*s23*s24*pow(s14,4)+s13*s13*(s14*s23*(13*s23*s24+3*(s23*s23)+14*(s24*s24))+s24*(21*s24*(s23*s23)+18*s23*(s24*s24)+3*pow(s23,3)-2*pow(s24,3)))+s23*(s14*s14)*(19*s24*(s23*s23)+16*s23*(s24*s24)+8*pow(s23,3)+6*pow(s24,3))+s13*(3*s23*(s14*s14)*(3*s23*s24+3*(s23*s23)+2*(s24*s24))+s24*(3*s23+s24)*pow(s14,3)+s24*(45*(s23*s23)*(s24*s24)+34*s24*pow(s23,3)+4*pow(s23,4)+6*s23*pow(s24,3)-6*pow(s24,4))+s14*(58*(s23*s23)*(s24*s24)+34*s24*pow(s23,3)+5*pow(s23,4)+31*s23*pow(s24,3)-pow(s24,4)))+s24*(20*(s24*s24)*pow(s23,3)+10*s24*pow(s23,4)+pow(s23,5)+9*(s23*s23)*pow(s24,3)-3*s23*pow(s24,4)-pow(s24,5))+s14*(46*(s24*s24)*pow(s23,3)+16*s24*pow(s23,4)+pow(s23,5)+45*(s23*s23)*pow(s24,3)+10*s23*pow(s24,4)-pow(s24,5))))*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-3))/2.;
}

// Coefficient of master 26 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygSC<26>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygSC<26,-1>(s12,s13,s14,s23,s24)
    });
}


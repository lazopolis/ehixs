/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 24: box6(s23,s12+s14+s24,s14)

// Coefficient order epsilon^0 of master 24
template<>
double qq2yygCAm2CF<24,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)*((2*(s23+s24)*(s13*s13)+s13*(12*s23*s24+2*s14*(s23+s24)+5*(s23*s23)+4*(s24*s24))+s23*(16*s23*s24+s14*(6*s23+9*s24)+s14*s14+3*(s23*s23)+10*(s24*s24)))*pow(s12,5)+(s13*(2*s23+s24)+s23*(2*s14+3*s23+5*s24))*pow(s12,6)+s23*pow(s12,7)+pow(s12,4)*(s13*s13*(10*s23*s24+2*s14*(s23+s24)+3*(s23*s23)+8*(s24*s24))+(2*s23+s24)*pow(s13,3)+s13*(s24*(s14*s14)+27*s24*(s23*s23)+27*s23*(s24*s24)+s14*(15*s23*s24+5*(s23*s23)+7*(s24*s24))+4*pow(s23,3)+6*pow(s24,3))+s23*((3*s23+4*s24)*(s14*s14)+18*s24*(s23*s23)+31*s23*(s24*s24)+s14*(29*s23*s24+6*(s23*s23)+16*(s24*s24))+pow(s23,3)+10*pow(s24,3)))+pow(s12,3)*((2*s14*s23+6*s23*s24+s23*s23+4*(s24*s24))*pow(s13,3)+s23*pow(s13,4)+s13*s13*(s23*(s14*s14)+12*s24*(s23*s23)+20*s23*(s24*s24)+s14*(9*s23*s24+2*(s23*s23)+7*(s24*s24))+pow(s23,3)+12*pow(s24,3))+s23*(s14*s14*(14*s23*s24+3*(s23*s23)+6*(s24*s24))+s24*(30*s24*(s23*s23)+27*s23*(s24*s24)+8*pow(s23,3)+5*pow(s24,3))+s14*(33*s24*(s23*s23)+48*s23*(s24*s24)+2*pow(s23,3)+13*pow(s24,3)))+s13*(s24*(4*s23+3*s24)*(s14*s14)+52*(s23*s23)*(s24*s24)+26*s24*pow(s23,3)+pow(s23,4)+29*s23*pow(s24,3)+s14*(32*s24*(s23*s23)+33*s23*(s24*s24)+4*pow(s23,3)+9*pow(s24,3))+4*pow(s24,4)))+s24*(s23*(s24*s24)*pow(s13,4)+s14*(s23*(s14*s14)+s14*(4*s23*s24+s23*s23+3*(s24*s24))+s24*((s23+s24)*(s23+s24)))*pow(s23,3)+s24*pow(s13,3)*(s14*s23*(-2*s23+s24)+pow(s24,3))+s13*s13*((s23-2*s24)*(s14*s14)*(s23*s23)+2*s23*s24*(2*s24*(s23*s23)+2*s23*(s24*s24)+pow(s23,3)+pow(s24,3))+s14*(6*(s23*s23)*(s24*s24)+5*s24*pow(s23,3)+2*pow(s23,4)+s23*pow(s24,3)+pow(s24,4)))+s13*s23*(s23*s24*(2*s23+s24)*((s23+s24)*(s23+s24))+s23*s23*pow(s14,3)+s14*s14*(5*s24*(s23*s23)+6*s23*(s24*s24)+3*pow(s23,3)+pow(s24,3))+s14*(10*(s23*s23)*(s24*s24)+9*s24*pow(s23,3)+2*pow(s23,4)+5*s23*pow(s24,3)+2*pow(s24,4))))+s12*s12*((s14*s23*(s23+5*s24)+2*s24*(3*s23*s24+s23*s23+3*(s24*s24)))*pow(s13,3)+3*s23*s24*pow(s13,4)+s23*(s24*(8*s23*s24+s23*s23+s24*s24)*((s23+s24)*(s23+s24))+s23*s24*pow(s14,3)+s14*s14*(17*s24*(s23*s23)+17*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3))+s14*s24*(45*s24*(s23*s23)+32*s23*(s24*s24)+15*pow(s23,3)+4*pow(s24,3)))+s13*s13*(s23*(s23+2*s24)*(s14*s14)+s14*s24*(13*s23*s24+8*(s23*s23)+9*(s24*s24))+s24*(19*s24*(s23*s23)+20*s23*(s24*s24)+6*pow(s23,3)+8*pow(s24,3)))+s13*(s24*(s14*s14)*(10*s23*s24+10*(s23*s23)+3*(s24*s24))+s24*(44*(s23*s23)*(s24*s24)+44*s24*pow(s23,3)+12*pow(s23,4)+15*s23*pow(s24,3)+pow(s24,4))+s14*(54*(s23*s23)*(s24*s24)+30*s24*pow(s23,3)+pow(s23,4)+31*s23*pow(s24,3)+5*pow(s24,4))))+s12*s24*((-(s14*s23*(s23-4*s24))+s24*(2*s23*s24+s23*s23+4*(s24*s24)))*pow(s13,3)+3*s23*s24*pow(s13,4)+s13*s13*(s23*s24*(s14*s14)+14*(s23*s23)*(s24*s24)+9*s24*pow(s23,3)+2*pow(s23,4)+10*s23*pow(s24,3)+s14*(12*s24*(s23*s23)+7*s23*(s24*s24)+6*pow(s23,3)+5*pow(s24,3))+2*pow(s24,4))+s13*(s23*s23*pow(s14,3)+s14*s14*(15*s24*(s23*s23)+7*s23*(s24*s24)+10*pow(s23,3)+pow(s24,3))+s14*(32*(s23*s23)*(s24*s24)+35*s24*pow(s23,3)+13*pow(s23,4)+13*s23*pow(s24,3)+pow(s24,4))+s23*(26*(s23*s23)*(s24*s24)+16*s24*pow(s23,3)+2*pow(s23,4)+15*s23*pow(s24,3)+3*pow(s24,4)))+s23*s23*(s14*s14*(15*s23*s24+8*(s23*s23)+6*(s24*s24))+2*s23*pow(s14,3)+s14*(14*s24*(s23*s23)+19*s23*(s24*s24)+2*pow(s23,3)+7*pow(s24,3))+s24*pow(s23+s24,3))))*pow(s12+s23+s24,-2))/2.;
}

// Coefficient of master 24 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCAm2CF<24>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCAm2CF<24,0>(s12,s13,s14,s23,s24)
    });
}


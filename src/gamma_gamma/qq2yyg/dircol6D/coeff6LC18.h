/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 18: box(-s12-s13-s14-s23-s24,s24,-s12-s13-s14)

// Coefficient order epsilon^-1 of master 18
template<>
double qq2yyg6LC<18,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-2)*pow(s24,-1)*(s14*(s23*s23)*(s14*s23*(s23+5*s24)+2*s24*(s23*s23-s24*s24))-2*(3*s23*s24+6*s13*(s14+s23+s24)+s14*(7*s23+4*s24)+3*(s13*s13)+3*(s14*s14)+3*(s23*s23)+s24*s24)*pow(s12,4)-2*(3*s13+3*s14+3*s23+2*s24)*pow(s12,5)-2*pow(s12,6)+s23*s24*(s23+4*s24)*pow(s13,3)-pow(s12,3)*(6*(s14+s23+2*s24)*(s13*s13)+s13*(4*s23*s24+16*s14*(s23+s24)+6*(s14*s14)+7*(s23*s23)+4*(s24*s24))+2*pow(s13,3)+2*((5*s23+2*s24)*(s14*s14)+s23*(-2*s23*s24+s23*s23-s24*s24)+s14*(3*s23*s24+4*(s23*s23)+s24*s24)+pow(s14,3)))-s12*s12*(2*(s24*(-3*s23+2*s24)+s14*(s23+4*s24))*(s13*s13)+4*s24*pow(s13,3)-s23*(-2*(s23-2*s24)*(s14*s14)+4*s24*(2*s23*s24+2*(s23*s23)+s24*s24)+s14*(15*s23*s24+s23*s23+4*(s24*s24))-2*pow(s14,3))+s13*(4*(s23+s24)*(s14*s14)-25*s24*(s23*s23)-24*s23*(s24*s24)+2*s14*(-6*s23*s24+s23*s23+s24*s24)+pow(s23,3)-4*pow(s24,3)))+s13*s13*(s14*s23*(3*s23*s24+s23*s23+8*(s24*s24))+2*s24*(7*s24*(s23*s23)+6*s23*(s24*s24)+4*pow(s23,3)+pow(s24,3)))+s13*s23*(s14*s14*(2*s23*s24+s23*s23+4*(s24*s24))+2*s24*(9*s24*(s23*s23)+8*s23*(s24*s24)+4*pow(s23,3)+3*pow(s24,3))+s14*(13*s24*(s23*s23)+9*s23*(s24*s24)+2*pow(s23,3)+6*pow(s24,3)))+s12*((4*s23*s24+s23*s23-2*(s24*s24))*pow(s13,3)+s13*s13*(18*s24*(s23*s23)+2*s14*(7*s23*s24+s23*s23-s24*s24)+24*s23*(s24*s24)+pow(s23,3)+4*pow(s24,3))+s23*(s14*s14*(11*s23*s24+3*(s23*s23)+4*(s24*s24))+4*s24*pow(s14,3)+2*s24*(s24*(s23*s23)+s23*(s24*s24)+pow(s23,3)+pow(s24,3))+s14*(14*s24*(s23*s23)+7*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3)))+s13*(s23*(s23+14*s24)*(s14*s14)+2*s24*(18*s24*(s23*s23)+11*s23*(s24*s24)+12*pow(s23,3)+pow(s24,3))+s14*(28*s24*(s23*s23)+24*s23*(s24*s24)+5*pow(s23,3)+2*pow(s24,3)))))*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/4.;
}

// Coefficient of master 18 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6LC<18>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6LC<18,-1>(s12,s13,s14,s23,s24)
    });
}


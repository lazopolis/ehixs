/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 20: box6(s14,s12+s13+s23,s23)

// Coefficient order epsilon^-1 of master 20
template<>
double qq2yygSC<20,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-3)*pow(s23,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*(2*s13*(4*s13+3*s14+3*s23+2*s24)*pow(s12,6)+2*s13*pow(s12,7)+pow(s12,5)*((18*s14+19*s23+16*s24)*(s13*s13)+s24*(s14*s14+s24*s24)+s13*(10*s23*s24+6*s14*(3*s23+2*s24)+6*(s14*s14)+7*(s23*s23)+3*(s24*s24))+12*pow(s13,3))+s24*(s24*(s23+s24)+s14*(2*s23+s24))*pow(s13,5)+s13*s23*(s14*s14)*((3*s23-s24)*s24*(s14*s14)+s14*(s23+s24)*(s23*s23)+s24*(s23+s24)*(s23*s23)+s24*pow(s14,3))+s14*(s13*s13)*(s23*(s14*s14)*(4*s23*s24+s23*s23-3*(s24*s24))-2*(s23+s24)*(s23*s23)*(s24*s24)-s14*s23*s24*((s23+s24)*(s23+s24))+(3*s23-s24)*s24*pow(s14,3))+s24*(s23*s23)*pow(s14,5)+s24*pow(s13,4)*(s14*s24*(2*s23+3*s24)+(5*s23+s24)*(s14*s14)-s24*(s23*s23)+pow(s24,3))+pow(s13,3)*((5*s23-s24)*s24*pow(s14,3)+s23*(s23+s24)*pow(s24,3)+s14*s24*(pow(s23,3)+pow(s24,3))+s14*s14*(2*s24*(s23*s23)-s23*(s24*s24)+pow(s23,3)+2*pow(s24,3)))+pow(s12,4)*(s13*s13*(32*s23*s24+s14*(43*s23+36*s24)+12*(s14*s14)+15*(s23*s23)+12*(s24*s24))+2*(9*s14+11*s23+12*s24)*pow(s13,3)+8*pow(s13,4)+s24*((2*s23+s24)*(s14*s14)+s14*(s24*s24)+(2*s23+s24)*(s24*s24)+3*pow(s14,3))+s13*(3*(6*s23+5*s24)*(s14*s14)+9*s24*(s23*s23)+4*s23*(s24*s24)+s14*(28*s23*s24+21*(s23*s23)+8*(s24*s24))+2*pow(s14,3)+4*pow(s23,3)+5*pow(s24,3)))+s12*((2*s24*(s14*s14)+s24*(6*s23*s24+5*(s23*s23)+5*(s24*s24))+s14*(14*s23*s24+3*(s23*s23)+10*(s24*s24)))*pow(s13,4)+s24*(s14+3*(s23+s24))*pow(s13,5)+s23*s24*(2*s14+3*s23+s24)*pow(s14,4)+s13*s14*(s24*(s23*s23)*(s23*s24+2*(s23*s23)-s24*s24)+s23*(s14*s14)*(11*s23*s24+4*(s23*s23)+2*(s24*s24))+8*s23*s24*pow(s14,3)+s24*pow(s14,4)+s14*s23*(6*s24*(s23*s23)+2*s23*(s24*s24)+3*pow(s23,3)+pow(s24,3)))+s13*s13*(s23*(4*s23+15*s24)*pow(s14,3)+2*s24*pow(s14,4)+3*s23*(s23+s24)*pow(s24,3)+s14*s24*(-(s24*(s23*s23))+2*s23*(s24*s24)+3*pow(s23,3)+2*pow(s24,3))+2*(s14*s14)*(7*s24*(s23*s23)+2*s23*(s24*s24)+3*pow(s23,3)+2*pow(s24,3)))+pow(s13,3)*(s14*s14*(20*s23*s24+7*(s23*s23)+7*(s24*s24))+2*s24*pow(s14,3)+s24*(-2*s24*(s23*s23)+3*s23*(s24*s24)+pow(s23,3)+4*pow(s24,3))+s14*(13*s24*(s23*s23)+13*s23*(s24*s24)+2*pow(s23,3)+11*pow(s24,3))))+pow(s12,3)*((37*s23*s24+s14*(34*s23+37*s24)+6*(s14*s14)+11*(s23*s23)+18*(s24*s24))*pow(s13,3)+(6*s14+11*s23+16*s24)*pow(s13,4)+2*pow(s13,5)+s24*(s23*(s23+s24)*(s14*s14)+2*s14*s23*(s24*s24)+s23*(s23+s24)*(s24*s24)+2*(3*s23+s24)*pow(s14,3)+3*pow(s14,4))+s13*s13*((29*s23+27*s24)*(s14*s14)+21*s24*(s23*s23)+12*s23*(s24*s24)+s14*(62*s23*s24+34*(s23*s23)+24*(s24*s24))+2*pow(s14,3)+4*pow(s23,3)+10*pow(s24,3))+s13*(s14*s14*(32*s23*s24+21*(s23*s23)+9*(s24*s24))+(6*s23+11*s24)*pow(s14,3)+4*s24*pow(s23,3)+pow(s23,4)+7*s23*pow(s24,3)+s14*(23*s24*(s23*s23)+11*s23*(s24*s24)+12*pow(s23,3)+7*pow(s24,3))+4*pow(s24,4)))+s12*s12*((s14*(9*s23+14*s24)+3*(6*s23*s24+s23*s23+4*(s24*s24)))*pow(s13,4)+2*(s23+2*s24)*pow(s13,5)+s14*s24*(s23*(3*s23+2*s24)*(s14*s14)+s23*s23*(s24*s24)+(6*s23+s24)*pow(s14,3)+pow(s14,4))+pow(s13,3)*(3*(4*s23+5*s24)*(s14*s14)+17*s24*(s23*s23)+13*s23*(s24*s24)+s14*(46*s23*s24+18*(s23*s23)+25*(s24*s24))+pow(s23,3)+10*pow(s24,3))+s13*s13*(s14*s14*(42*s23*s24+23*(s23*s23)+14*(s24*s24))+(5*s23+9*s24)*pow(s14,3)+s24*(-(s24*(s23*s23))+8*s23*(s24*s24)+4*pow(s23,3)+6*pow(s24,3))+s14*(31*s24*(s23*s23)+22*s23*(s24*s24)+9*pow(s23,3)+14*pow(s24,3)))+s13*((21*s23*s24+7*(s23*s23)+4*(s24*s24))*pow(s14,3)+5*s24*pow(s14,4)+2*(s14*s14)*(11*s24*(s23*s23)+5*s23*(s24*s24)+6*pow(s23,3)+pow(s24,3))+s23*s24*(3*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3))+s14*(2*(s23*s23)*(s24*s24)+9*s24*pow(s23,3)+3*pow(s23,4)+4*s23*pow(s24,3)+pow(s24,4)))))*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1))/2.;
}

// Coefficient of master 20 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygSC<20>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygSC<20,-1>(s12,s13,s14,s23,s24)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 10: box(s23,(-s12-s13-s14-s23-s24),(-s12-s13-s14))

// Coefficient order epsilon^0 of master 10
template<>
double qq2yygCF<10,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s24,-3)*((3*s24*(s13*s13)+s24*(s24*(s23+s24)+s14*(5*s23+3*s24)+3*(s14*s14))+s13*(2*s23*s24+s24*(6*s14+5*s24)-s23*s23))*pow(s12,4)+s24*(3*s13+3*s14+2*(s23+s24))*pow(s12,5)+s24*pow(s12,6)+pow(s12,3)*(s13*s13*(-2*s23*s24+s24*(3*s14+4*s24)-2*(s23*s23))+s24*pow(s13,3)+s24*(4*s23*(s14*s14)-s14*((s23+s24)*(s23+s24))-(2*s23-s24)*((s23+s24)*(s23+s24))+pow(s14,3))+s13*(3*s24*(s14*s14)+s14*(2*s23*s24-2*(s23*s23)+4*(s24*s24))-2*(3*s24*(s23*s23)+s23*(s24*s24)+pow(s23,3)-pow(s24,3))))+(s23+s24)*(s24*(s13*s13)*(s14*(3*s23*s24+s23*s23+s24*s24)+s24*(5*s23*s24+2*(s23*s23)+s24*s24))+s14*s23*s24*(-(s23*(s14*s14))+s14*(-(s23*s23)+s24*s24)+s24*((s23+s24)*(s23+s24)))+pow(s13,3)*pow(s24,3)-s13*(s23*(s14*s14)*(2*s23*s24+s23*s23-2*(s24*s24))+s14*(s24*s24)*(-5*s23*s24-4*(s23*s23)+s24*s24)+s24*s24*(-5*s24*(s23*s23)-2*s23*(s24*s24)-2*pow(s23,3)+pow(s24,3))))-s12*s12*((2*s23*s24+s23*s23-s24*s24)*pow(s13,3)+s13*s13*(7*s24*(s23*s23)+s14*(3*s23*s24+2*(s23*s23)-s24*s24)+4*s23*(s24*s24)+2*pow(s23,3)-2*pow(s24,3))+s13*(s14*s14*(s23*s23+s24*s24)+s23*(5*s24*(s23*s23)+2*s23*(s24*s24)+pow(s23,3)-4*pow(s24,3))+s14*(12*s24*(s23*s23)+9*s23*(s24*s24)+4*pow(s23,3)+pow(s24,3)))+s24*(s14*s14*(5*s23*s24+2*(s23*s23)+2*(s24*s24))+(-s23+s24)*pow(s14,3)+s14*(8*s24*(s23*s23)+2*s23*(s24*s24)+5*pow(s23,3)-pow(s24,3))+(s23-2*s24)*pow(s23+s24,3)))+s12*(s24*(-(s23*s24)-s23*s23+s24*s24)*pow(s13,3)-s13*(s23*(s14*s14)*(6*s23*s24+2*(s23*s23)+5*(s24*s24))+2*s14*s23*(4*s24*(s23*s23)+2*s23*(s24*s24)+pow(s23,3)-2*pow(s24,3))+s24*s24*(-17*s24*(s23*s23)-9*s23*(s24*s24)-7*pow(s23,3)+pow(s24,3)))+s13*s13*(s14*(-6*s24*(s23*s23)-4*s23*(s24*s24)-2*pow(s23,3)+pow(s24,3))+s24*(s24*(s23*s23)+6*s23*(s24*s24)-pow(s23,3)+2*pow(s24,3)))+s24*(-2*s14*(-(s23*s24)+s23*s23-s24*s24)*((s23+s24)*(s23+s24))-s23*(s23+2*s24)*pow(s14,3)+s14*s14*(-6*s24*(s23*s23)-s23*(s24*s24)-4*pow(s23,3)+pow(s24,3))+s24*pow(s23+s24,4))))*pow(s12+s23+s24,-1))/2.;
}

// Coefficient order epsilon^1 of master 10
template<>
double qq2yygCF<10,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s24,-3)*(-((4*s23*s24+3*s13*(s23+s24)+3*s14*(s23+s24)+3*(s23*s23)-s24*s24)*pow(s12,5))-(s23+s24)*pow(s12,6)+s24*s24*(7*s23*s24+2*(s23*s23)+7*(s24*s24))*pow(s13,3)-pow(s12,4)*(3*(s23+s24)*(s13*s13)+3*(s23+s24)*(s14*s14)+5*s24*(s23*s23)+s13*(9*s23*s24+6*s14*(s23+s24)+8*(s23*s23)-5*(s24*s24))+s14*(9*s23*s24+9*(s23*s23)-4*(s24*s24))-9*s23*(s24*s24)+3*pow(s23,3)-11*pow(s24,3))+s13*s13*(s24*s24)*(8*s24*(s23*s23)+4*s23*(s24*s24)+s14*(-5*s23*s24-5*(s23*s23)+4*(s24*s24))+8*pow(s23,3)-2*pow(s24,3))+s14*s23*(6*s14*(s24*s24)*(s23*s24+s23*s23+s24*s24)+s14*s14*(s24*(s23*s23)+4*s23*(s24*s24)-pow(s23,3)+2*pow(s24,3))+2*(s24*s24)*(3*s24*(s23*s23)+3*s23*(s24*s24)+2*pow(s23,3)+2*pow(s24,3)))-pow(s12,3)*(s13*s13*(6*s23*s24+3*s14*(s23+s24)+7*(s23*s23)-7*(s24*s24))+s14*s14*(6*s23*s24+9*(s23*s23)-5*(s24*s24))-18*(s23*s23)*(s24*s24)+(s23+s24)*pow(s13,3)+(s23+s24)*pow(s14,3)+2*s24*pow(s23,3)+pow(s23,4)+s13*(3*(s23+s24)*(s14*s14)+9*s24*(s23*s23)+4*s14*(3*s23*s24+4*(s23*s23)-3*(s24*s24))-25*s23*(s24*s24)+7*pow(s23,3)-23*pow(s24,3))+s14*(9*s24*(s23*s23)-25*s23*(s24*s24)+9*pow(s23,3)-23*pow(s24,3))-32*s23*pow(s24,3)-19*pow(s24,4))-s13*(2*s14*(s24*s24)*(3*s24*(s23*s23)+6*s23*(s24*s24)-4*pow(s23,3)+5*pow(s24,3))+s14*s14*(3*(s23*s23)*(s24*s24)+s24*pow(s23,3)+2*pow(s23,4)+8*s23*pow(s24,3)+2*pow(s24,4))+2*(s24*s24)*(s23*s23*(s24*s24)-6*s24*pow(s23,3)-4*pow(s23,4)+7*s23*pow(s24,3)+4*pow(s24,4)))+s12*s12*(-((s23*s24+2*(s23*s23)-3*(s24*s24))*pow(s13,3))-(s23*s24+3*(s23*s23)-2*(s24*s24))*pow(s14,3)+2*(s24*s24)*(15*s24*(s23*s23)+15*s23*(s24*s24)+7*pow(s23,3)+7*pow(s24,3))+s14*s14*(-3*s24*(s23*s23)+22*s23*(s24*s24)-9*pow(s23,3)+14*pow(s24,3))+s13*s13*(-4*s24*(s23*s23)+23*s23*(s24*s24)+s14*(-3*s23*s24-7*(s23*s23)+8*(s24*s24))-4*pow(s23,3)+23*pow(s24,3))+s13*(39*(s23*s23)*(s24*s24)+s14*s14*(-3*s23*s24-8*(s23*s23)+7*(s24*s24))-3*s24*pow(s23,3)-2*pow(s23,4)-2*s14*(5*s24*(s23*s23)-22*s23*(s24*s24)+7*pow(s23,3)-15*pow(s24,3))+47*s23*pow(s24,3)+11*pow(s24,4))+s14*(37*(s23*s23)*(s24*s24)-3*s24*pow(s23,3)-3*pow(s23,4)+51*s23*pow(s24,3)+26*pow(s24,4)))+s12*(-((s23+s24)*(s13*s13)*(s14*(-2*s23*s24+4*(s23*s23)-17*(s24*s24))-(21*s23+11*s24)*(s24*s24)))+2*(s24*s24)*(s23*s24+2*(s23*s23)+2*(s24*s24))*((s23+s24)*(s23+s24))+(7*s23+11*s24)*(s24*s24)*pow(s13,3)+pow(s14,3)*(s24*(s23*s23)+6*s23*(s24*s24)-3*pow(s23,3)+2*pow(s24,3))+2*s14*(s24*s24)*(16*s24*(s23*s23)+14*s23*(s24*s24)+10*pow(s23,3)+5*pow(s24,3))+s14*s14*(23*(s23*s23)*(s24*s24)-3*pow(s23,4)+22*s23*pow(s24,3)+8*pow(s24,4))+s13*(2*(s24*s24)*(18*s24*(s23*s23)+s23*(s24*s24)+14*pow(s23,3)-6*pow(s24,3))+s14*s14*(-(s24*(s23*s23))+18*s23*(s24*s24)-7*pow(s23,3)+8*pow(s24,3))+s14*(32*(s23*s23)*(s24*s24)-4*s24*pow(s23,3)-4*pow(s23,4)+28*s23*pow(s24,3)+2*pow(s24,4)))))*pow(s12+s23+s24,-1))/4.;
}

// Coefficient order epsilon^2 of master 10
template<>
double qq2yygCF<10,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s24,-3)*(-((6*s24*(s13*s13)+s13*(10*s23*s24+4*s24*(3*s14+2*s24)-s23*s23)+s24*(11*s23*s24+5*s14*(3*s23+s24)+6*(s14*s14)+7*(s23*s23)+2*(s24*s24)))*pow(s12,4))-2*s24*(3*s13+3*s14+3*s23+2*s24)*pow(s12,5)-2*s24*pow(s12,6)+(-2*s23+3*s24)*pow(s13,3)*pow(s24,3)-pow(s12,3)*(s13*s13*(6*s14*s24+2*s23*s24-2*(s23*s23)+4*(s24*s24))+2*s24*pow(s13,3)+s13*(6*s24*(s14*s14)-2*s14*(-7*s23*s24+s23*s23-s24*s24)+13*s23*(s24*s24)-2*pow(s23,3)-9*pow(s24,3))+s24*(2*(6*s23-s24)*(s14*s14)+13*s24*(s23*s23)+s14*(12*s23*s24+14*(s23*s23)-3*(s24*s24))+11*s23*(s24*s24)+2*pow(s14,3)+4*pow(s23,3)+pow(s24,3)))+s14*s23*s24*(s14*s14*(s23*s23-s24*s24)-s24*(4*s24*(s23*s23)+4*s23*(s24*s24)+pow(s23,3)+pow(s24,3))-s14*(5*s24*(s23*s23)+5*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3)))+s24*(s13*s13)*(s14*(-(s24*(s23*s23))-6*s23*(s24*s24)+pow(s23,3)+6*pow(s24,3))+s24*(-3*s24*(s23*s23)+4*s23*(s24*s24)-pow(s23,3)+7*pow(s24,3)))+s13*(s14*s14*(2*(s23*s23)*(s24*s24)+5*s24*pow(s23,3)+pow(s23,4)-4*s23*pow(s24,3)+3*pow(s24,4))+s24*s24*(6*(s23*s23)*(s24*s24)-3*s24*pow(s23,3)-2*pow(s23,4)+12*s23*pow(s24,3)+5*pow(s24,4))+s14*s24*(3*(s23*s23)*(s24*s24)+2*s24*pow(s23,3)+pow(s23,4)+8*s23*pow(s24,3)+8*pow(s24,4)))+s12*s12*(s23*(s23+2*s24)*pow(s13,3)+s13*s13*(s14*(s23*s24+2*(s23*s23)+3*(s24*s24))+2*(5*s24*(s23*s23)-s23*(s24*s24)+pow(s23,3)+8*pow(s24,3)))-s24*(s14*s14*(-2*s23*s24+7*(s23*s23)-4*(s24*s24))+15*(s23*s23)*(s24*s24)+3*(s23-s24)*pow(s14,3)+7*s24*pow(s23,3)+pow(s23,4)+11*s23*pow(s24,3)+s14*(18*s24*(s23*s23)+10*s23*(s24*s24)+7*pow(s23,3)+pow(s24,3))+2*pow(s24,4))+s13*(-11*(s23*s23)*(s24*s24)+s14*s14*(-4*s23*s24+s23*s23+6*(s24*s24))+5*s24*pow(s23,3)+pow(s23,4)+2*s23*pow(s24,3)+s14*(7*s24*(s23*s23)+7*s23*(s24*s24)+4*pow(s23,3)+25*pow(s24,3))+23*pow(s24,4)))+s12*(pow(s13,3)*(3*s24*(s23*s23)+5*pow(s24,3))+s13*s13*(2*s14*(5*s24*(s23*s23)+5*s23*(s24*s24)+pow(s23,3)+7*pow(s24,3))+s24*(-3*s24*(s23*s23)+3*s23*(s24*s24)+4*pow(s23,3)+21*pow(s24,3)))-s24*(s24*(3*s23*s24+s23*s23+s24*s24)*((s23+s24)*(s23+s24))+s24*(-3*s23+s24)*pow(s14,3)+s14*s14*(5*s24*(s23*s23)+4*s23*(s24*s24)+2*pow(s23,3)+3*pow(s24,3))+s14*(20*(s23*s23)*(s24*s24)+12*s24*pow(s23,3)+2*pow(s23,4)+12*s23*pow(s24,3)+3*pow(s24,4)))+s13*(s14*s14*(7*s24*(s23*s23)+13*s23*(s24*s24)+2*pow(s23,3)+8*pow(s24,3))+s24*(-10*(s23*s23)*(s24*s24)-8*s24*pow(s23,3)+pow(s23,4)+17*s23*pow(s24,3)+17*pow(s24,4))+s14*(6*(s23*s23)*(s24*s24)+10*s24*pow(s23,3)+2*pow(s23,4)+12*s23*pow(s24,3)+23*pow(s24,4)))))*pow(s12+s23+s24,-1))/2.;
}

// Coefficient of master 10 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCF<10>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCF<10,0>(s12,s13,s14,s23,s24),
        qq2yygCF<10,1>(s12,s13,s14,s23,s24),
        qq2yygCF<10,2>(s12,s13,s14,s23,s24)
    });
}


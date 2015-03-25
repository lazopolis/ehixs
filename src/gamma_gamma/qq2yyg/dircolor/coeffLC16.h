/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 16: box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14)

// Coefficient order epsilon^0 of master 16
template<>
double qq2yygLC<16,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-3)*((5*s23*s24+3*s13*(s23+s24)+3*s14*(s23+s24)+3*(s23*s23)+3*(s24*s24))*pow(s12,5)+(s23+s24)*pow(s12,6)-s14*(s14-s24)*s24*(s14+s23+s24)*pow(s23,3)+s24*pow(s13,3)*(-2*s24*(s23*s23)-pow(s23,3)+pow(s24,3))+pow(s12,4)*(3*(s23+s24)*(s13*s13)+3*(s23+s24)*(s14*s14)+6*s24*(s23*s23)+7*s23*(s24*s24)+3*s13*(4*s23*s24+2*s14*(s23+s24)+2*(s23*s23)+3*(s24*s24))+s14*(10*s23*s24+7*(s23*s23)+6*(s24*s24))+3*pow(s23,3)+3*pow(s24,3))-s23*(s13*s13)*(s24*(8*s24*(s23*s23)+7*s23*(s24*s24)+4*pow(s23,3)+2*pow(s24,3))+s14*(3*s24*(s23*s23)+4*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3)))-s13*(s23*s23)*(s14*s14*(2*s23*s24+s23*s23-s24*s24)+2*s14*(4*s24*(s23*s23)+3*s23*(s24*s24)+pow(s23,3)+pow(s24,3))+s24*(10*s24*(s23*s23)+9*s23*(s24*s24)+4*pow(s23,3)+3*pow(s24,3)))+s12*s12*(s24*(2*s23+3*s24)*pow(s13,3)+s23*s23*((2*s23-3*s24)*(s14*s14)-s24*(s23*s23)+s14*(-4*s23*s24+s23*s23-2*(s24*s24))+pow(s14,3)+pow(s24,3))+s13*s13*(-2*s24*(s23*s23)+7*s23*(s24*s24)+s14*(4*s23*s24+s23*s23+6*(s24*s24))-pow(s23,3)+9*pow(s24,3))+s13*(s14*s14*(2*s23*s24+2*(s23*s23)+3*(s24*s24))+s24*(-9*s24*(s23*s23)+2*s23*(s24*s24)-11*pow(s23,3)+3*pow(s24,3))+s14*(-6*s24*(s23*s23)+3*s23*(s24*s24)+pow(s23,3)+6*pow(s24,3))))-s12*(-(s24*(s23*s23)*(-((5*s23+2*s24)*(s14*s14))+s14*(-2*(s23*s23)+s24*s24)+s24*((s23+s24)*(s23+s24))-2*pow(s14,3)))+pow(s13,3)*(2*s24*(s23*s23)-s23*(s24*s24)+pow(s23,3)-3*pow(s24,3))+s13*s23*(s14*s14*(7*s23*s24+s23*s23+3*(s24*s24))+s24*(21*s24*(s23*s23)+12*s23*(s24*s24)+12*pow(s23,3)+2*pow(s24,3))+s14*(17*s24*(s23*s23)+14*s23*(s24*s24)+3*pow(s23,3)+5*pow(s24,3)))+s13*s13*(12*(s23*s23)*(s24*s24)+11*s24*pow(s23,3)+pow(s23,4)+s14*(7*s24*(s23*s23)+2*s23*(s24*s24)+2*pow(s23,3)-3*pow(s24,3))+s23*pow(s24,3)-3*pow(s24,4)))+pow(s12,3)*(3*(s23*s23)*(s24*s24)+3*(s13*s13)*(3*s23*s24+s14*(s23+s24)+s23*s23+3*(s24*s24))+s14*s14*(5*s23*s24+5*(s23*s23)+3*(s24*s24))+(s23+s24)*pow(s13,3)+(s23+s24)*pow(s14,3)+s24*pow(s23,3)+pow(s23,4)+3*s23*pow(s24,3)+s14*(5*s24*(s23*s23)+7*s23*(s24*s24)+5*pow(s23,3)+3*pow(s24,3))+s13*(3*(s23+s24)*(s14*s14)+6*s24*(s23*s23)+13*s23*(s24*s24)+2*s14*(7*s23*s24+4*(s23*s23)+6*(s24*s24))+3*pow(s23,3)+9*pow(s24,3))+pow(s24,4)))*pow(s12+s23+s24,-1))/4.;
}

// Coefficient order epsilon^1 of master 16
template<>
double qq2yygLC<16,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-3)*((s13*(9*s23-3*s24)+s14*(9*s23-3*s24)+2*s23*s24+7*(s23*s23)-3*(s24*s24))*pow(s12,5)+(3*s23-s24)*pow(s12,6)-s14*(s23+s24)*(2*s14*(s23+2*s24)+3*(s14*s14)+2*(3*s23*s24+s23*s23+2*(s24*s24)))*pow(s23,3)+s13*(s23*s23)*(s14*s14*(5*s23*s24-2*(s23*s23)+11*(s24*s24))-2*(-(s23*s24)+2*(s23*s23)-6*(s24*s24))*((s23+s24)*(s23+s24))-2*s14*(3*s24*(s23*s23)-6*s23*(s24*s24)+2*pow(s23,3)-7*pow(s24,3)))+pow(s12,4)*((9*s23-3*s24)*(s13*s13)+(9*s23-3*s24)*(s14*s14)-5*s24*(s23*s23)+s13*(6*s14*(3*s23-s24)+5*s23*s24+14*(s23*s23)-9*(s24*s24))+s14*(s23*s24+21*(s23*s23)-6*(s24*s24))-9*s23*(s24*s24)+3*pow(s23,3)-3*pow(s24,3))-s24*pow(s13,3)*(4*s24*(s23*s23)+5*s23*(s24*s24)+4*pow(s23,3)+pow(s24,3))+s23*(s13*s13)*(2*s24*(s14+4*s24)*(s23*s23)+7*s23*(s14+2*s24)*(s24*s24)-8*s24*pow(s23,3)-4*pow(s23,4)+(-3*s14+2*s24)*pow(s24,3))+s12*s12*((s23-3*s24)*s24*pow(s13,3)-s13*s13*(8*s24*(s23*s23)+17*s23*(s24*s24)+s14*(s23*s24-7*(s23*s23)+6*(s24*s24))+12*pow(s23,3)+9*pow(s24,3))-s23*(s14*s14*(11*s23*s24-9*(s23*s23)+10*(s24*s24))+2*(8*s23*s24+3*(s23*s23)+2*(s24*s24))*((s23+s24)*(s23+s24))+(-7*s23+3*s24)*pow(s14,3)+s14*(39*s24*(s23*s23)+41*s23*(s24*s24)+7*pow(s23,3)+11*pow(s24,3)))+s13*(s14*s14*(-5*s23*s24+14*(s23*s23)-3*(s24*s24))-19*(s23*s23)*(s24*s24)-35*s24*pow(s23,3)-24*pow(s23,4)-17*s23*pow(s24,3)-2*s14*(8*s24*(s23*s23)+13*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3))-3*pow(s24,4)))+pow(s12,3)*(s13*s13*(s14*(9*s23-3*s24)+4*s23*s24+7*(s23*s23)-9*(s24*s24))+s14*s14*(-4*s23*s24+21*(s23*s23)-3*(s24*s24))-32*(s23*s23)*(s24*s24)+(3*s23-s24)*pow(s13,3)+(3*s23-s24)*pow(s14,3)-26*s24*pow(s23,3)-5*pow(s23,4)+s13*((9*s23-3*s24)*(s14*s14)-9*s24*(s23*s23)+4*s14*(7*(s23*s23)-3*(s24*s24))-19*s23*(s24*s24)-5*pow(s23,3)-9*pow(s24,3))+s14*(-11*s24*(s23*s23)-19*s23*(s24*s24)+11*pow(s23,3)-3*pow(s24,3))-12*s23*pow(s24,3)-pow(s24,4))-s12*(pow(s13,3)*(4*s24*(s23*s23)+7*s23*(s24*s24)+4*pow(s23,3)+3*pow(s24,3))+s13*s13*(s23*s23*(s24*s24)+20*s24*pow(s23,3)+16*pow(s23,4)+10*s23*pow(s24,3)+s14*(10*s24*(s23*s23)+13*s23*(s24*s24)+6*pow(s23,3)+3*pow(s24,3))+3*pow(s24,4))+s13*s23*(s14*s14*(11*s23*s24+s23*s23+6*(s24*s24))+2*s14*(13*s24*(s23*s23)+3*s23*(s24*s24)+8*pow(s23,3)+4*pow(s24,3))+2*(-4*(s23*s23)*(s24*s24)+14*s24*pow(s23,3)+9*pow(s23,4)-8*s23*pow(s24,3)+pow(s24,4)))+s23*s23*(s14*s14*(20*s23*s24+5*(s23*s23)+13*(s24*s24))-(s23-5*s24)*pow(s14,3)+4*s14*(8*s24*(s23*s23)+9*s23*(s24*s24)+2*pow(s23,3)+3*pow(s24,3))+2*(s23+2*s24)*pow(s23+s24,3))))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient order epsilon^2 of master 16
template<>
double qq2yygLC<16,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-3)*((9*s23*s24+3*s13*(2*s23+s24)+3*s14*(2*s23+s24)+7*(s23*s23)+3*(s24*s24))*pow(s12,5)+(2*s23+s24)*pow(s12,6)+pow(s12,4)*(3*(2*s23+s24)*(s13*s13)+3*(2*s23+s24)*(s14*s14)+23*s24*(s23*s23)+12*s23*(s24*s24)+s14*(20*s23*s24+17*(s23*s23)+6*(s24*s24))+s13*(23*s23*s24+6*s14*(2*s23+s24)+13*(s23*s23)+9*(s24*s24))+12*pow(s23,3)+3*pow(s24,3))+s14*pow(s23,3)*(2*s24*(s14*s14)+s14*(9*s23*s24+4*(s23*s23)+7*(s24*s24))+2*(2*s24*(s23*s23)+3*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3)))+s13*(s23*s23)*(s24*(s23+s24)*(s14*s14)-2*(s23*s23)*(s24*s24)+6*s24*pow(s23,3)+4*pow(s23,4)+s14*(-8*s23*(s24*s24)+6*pow(s23,3)-6*pow(s24,3))-10*s23*pow(s24,3)-6*pow(s24,4))+s23*(s13*s13)*(2*(s23*s23)*(s24*s24)+6*s24*pow(s23,3)+6*pow(s23,4)+2*s14*(s23*(s24*s24)+pow(s23,3)-pow(s24,3))-s23*pow(s24,3)-pow(s24,4))+pow(s13,3)*(4*(s23*s23)*(s24*s24)+2*s24*pow(s23,3)+2*pow(s23,4)+s23*pow(s24,3)+pow(s24,4))+pow(s12,3)*(26*(s23*s23)*(s24*s24)+s14*s14*(13*s23*s24+13*(s23*s23)+3*(s24*s24))+s13*s13*(19*s23*s24+3*s14*(2*s23+s24)+5*(s23*s23)+9*(s24*s24))+(2*s23+s24)*pow(s13,3)+(2*s23+s24)*pow(s14,3)+31*s24*pow(s23,3)+13*pow(s23,4)+5*s23*pow(s24,3)+s14*(41*s24*(s23*s23)+16*s23*(s24*s24)+24*pow(s23,3)+3*pow(s24,3))+s13*(3*(2*s23+s24)*(s14*s14)+34*s24*(s23*s23)+27*s23*(s24*s24)+2*s14*(16*s23*s24+9*(s23*s23)+6*(s24*s24))+16*pow(s23,3)+9*pow(s24,3))+pow(s24,4))+s12*(pow(s13,3)*(3*s24*(s23*s23)+4*s23*(s24*s24)+2*pow(s23,3)+3*pow(s24,3))+s23*s23*(s14*s14*(21*s23*s24+10*(s23*s23)+10*(s24*s24))+2*(s23*s24+s23*s23+2*(s24*s24))*((s23+s24)*(s23+s24))+(s23+3*s24)*pow(s14,3)+s14*(29*s24*(s23*s23)+32*s23*(s24*s24)+12*pow(s23,3)+11*pow(s24,3)))+s13*s23*(-2*s23*s24*(s14*s14)+3*(s23*s23)*(s24*s24)+18*s24*pow(s23,3)+14*pow(s23,4)+s14*(10*s24*(s23*s23)+15*pow(s23,3)-pow(s24,3))-6*s23*pow(s24,3)-pow(s24,4))+s13*s13*(8*(s23*s23)*(s24*s24)+11*s24*pow(s23,3)+12*pow(s23,4)+5*s23*pow(s24,3)+s14*(-2*s24*(s23*s23)+4*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3))+3*pow(s24,4)))+s12*s12*((5*s23*s24-s23*s23+3*(s24*s24))*pow(s13,3)+s13*s13*(14*s24*(s23*s23)+19*s23*(s24*s24)+s14*(12*s23*s24+s23*s23+6*(s24*s24))+6*pow(s23,3)+9*pow(s24,3))+s23*(s14*s14*(21*s23*s24+13*(s23*s23)+4*(s24*s24))+(3*s23+2*s24)*pow(s14,3)+s14*(49*s24*(s23*s23)+32*s23*(s24*s24)+23*pow(s23,3)+2*pow(s24,3))+2*s23*(11*s24*(s23*s23)+14*s23*(s24*s24)+4*pow(s23,3)+7*pow(s24,3)))+s13*(21*(s23*s23)*(s24*s24)+s14*s14*(9*s23*s24+5*(s23*s23)+3*(s24*s24))+28*s24*pow(s23,3)+19*pow(s23,4)+9*s23*pow(s24,3)+s14*(24*s24*(s23*s23)+19*s23*(s24*s24)+15*pow(s23,3)+6*pow(s24,3))+3*pow(s24,4))))*pow(s12+s23+s24,-1))/4.;
}

// Coefficient of master 16 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygLC<16>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygLC<16,0>(s12,s13,s14,s23,s24),
        qq2yygLC<16,1>(s12,s13,s14,s23,s24),
        qq2yygLC<16,2>(s12,s13,s14,s23,s24)
    });
}


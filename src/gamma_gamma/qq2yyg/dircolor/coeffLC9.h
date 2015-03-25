/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 9: box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24)

// Coefficient order epsilon^0 of master 9
template<>
double qq2yygLC<9,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s14,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)*(2*s13*(3*s24*(s13+s23+s24)+s14*(s23+2*s24))*pow(s12,5)+2*s13*s24*pow(s12,6)+pow(s12,4)*(6*(s14*(s23+s24)+s24*(2*s23+3*s24))*(s13*s13)+2*s13*((4*s23-s24)*(s14*s14)+2*s14*(3*s23*s24+s23*s23+2*(s24*s24))+3*s24*((s23+s24)*(s23+s24)))+6*s24*pow(s13,3)+(s23-2*s24)*pow(s14,3))+s13*(s14*s24*(s13*s13)*(5*s23*(s14*s14)-6*s23*(s24*s24)-2*s14*(-3*s23*s24+s23*s23+2*(s24*s24)))-s23*(s23*(s14*s14)+2*s24*(s23*s23)-s14*(-(s23*s24)+s23*s23+2*(s24*s24)))*pow(s14,3)+s13*(s14*s14)*(-(s23*(s23-3*s24)*(s14*s14))+6*(s23*s23)*(s24*s24)+s14*(-(s24*(s23*s23))+7*s23*(s24*s24)+2*pow(s23,3)-2*pow(s24,3)))+2*s24*pow(s13,3)*(s14*(s23-s24)*s24+s23*(s14*s14)+pow(s24,3)))+s12*(s23*(s23-2*s24)*(s14+s23+s24)*pow(s14,4)+2*s14*(s13*s13)*(s14*s14*(6*s23*s24+3*(s23*s23)-7*(s24*s24))-3*s23*(s23+s24)*(s24*s24)+s14*(2*s24*(s23*s23)-2*s23*(s24*s24)+pow(s23,3)-4*pow(s24,3)))+2*pow(s13,4)*(2*s14*(s23-s24)*s24+s23*(s14*s14)+3*pow(s24,3))-s13*pow(s14,3)*(s23*(s14*s14)-s24*(s23*s23)+4*s23*(s24*s24)+s14*(s23*s24-2*(s23*s23)+4*(s24*s24))-5*pow(s23,3)+4*pow(s24,3))+pow(s13,3)*(4*s14*s23*(s23-s24)*s24+2*(s14*s14)*(7*s23*s24+2*(s23*s23)-7*(s24*s24))+3*s23*pow(s14,3)+6*(s23+s24)*pow(s24,3)))+s12*s12*(s13*(s14*s14)*(2*(2*s23-5*s24)*(s14*s14)+s14*(-5*s23*s24+12*(s23*s23)-18*(s24*s24))+4*(s23-s24)*((s23+s24)*(s23+s24)))+2*(2*s14*s23*(s23+2*s24)+(6*s23-5*s24)*(s14*s14)+3*(2*s23+3*s24)*(s24*s24))*pow(s13,3)+2*(s14*(s23-s24)+3*(s24*s24))*pow(s13,4)+(s23-2*s24)*(s14*(3*s23+2*s24)+s14*s14+(s23+s24)*(s23+s24))*pow(s14,3)+s13*s13*(2*(s14*s14)*(5*s23*s24+8*(s23*s23)-10*(s24*s24))+6*(s24*s24)*((s23+s24)*(s23+s24))+(13*s23-16*s24)*pow(s14,3)+2*s14*(3*s24*(s23*s23)+2*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3))))+pow(s12,3)*(2*(s13*s13)*((9*s23-6*s24)*(s14*s14)+3*s24*(4*s23*s24+s23*s23+3*(s24*s24))+s14*(8*s23*s24+4*(s23*s23)+6*(s24*s24)))+6*(s14*s23+s24*(s23+3*s24))*pow(s13,3)+2*s24*pow(s13,4)+2*(s23-2*s24)*(s14+s23+s24)*pow(s14,3)+s13*(6*(s14*s14)*(s23*s24+2*(s23*s23)-s24*s24)+2*s14*(s23+2*s24)*((s23+s24)*(s23+s24))+(11*s23-14*s24)*pow(s14,3)+2*s24*pow(s23+s24,3))))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient order epsilon^1 of master 9
template<>
double qq2yygLC<9,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s14,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)*((s13*(-3*s24*(s23+s24)+s14*(6*s23+4*s24))-3*s24*(s13*s13)+s14*(s14*(6*s23+s24)+3*(3*s23*s24+2*(s23*s23)+s24*s24)))*pow(s12,5)+(-(s13*s24)+s14*(2*s23+s24))*pow(s12,6)+s14*(s14*s14*(4*s23*s24+2*(s23*s23)+s24*s24)+s24*s24*(5*s23*s24+2*(s23*s23)+s24*s24)-s14*s24*(-2*s23*s24+3*(s23*s23)+s24*s24))*pow(s13,3)+pow(s12,4)*((-3*s24*(2*s23+3*s24)+s14*(6*s23+5*s24))*(s13*s13)+s14*(s14*s23*(14*s23+11*s24)+(7*s23-2*s24)*(s14*s14)+3*(2*s23+s24)*((s23+s24)*(s23+s24)))-3*s24*pow(s13,3)+s13*((9*s23+4*s24)*(s14*s14)+s14*(29*s23*s24+14*(s23*s23)+11*(s24*s24))-3*s24*((s23+s24)*(s23+s24))-2*pow(s14,3)))+(-(s23*s24)+s14*(3*s23+s24)+s14*s14)*(s23*s23)*pow(s14,4)-s24*pow(s13,4)*(2*(2*s23+s24)*(s14*s14)+pow(s24,3))+s13*s23*pow(s14,3)*(3*(s23+2*s24)*(s14*s14)+13*s24*(s23*s23)+27*s23*(s24*s24)+s14*(14*s23*s24+5*(s23*s23)+19*(s24*s24))+2*pow(s23,3)+14*pow(s24,3))+s13*s13*(s14*s14)*(s14*s14*(14*s23*s24+4*(s23*s23)+3*(s24*s24))+s14*(11*s24*(s23*s23)+26*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3))-s24*(5*s24*(s23*s23)+3*s23*(s24*s24)+4*pow(s23,3)+2*pow(s24,3)))+s12*(s23*pow(s14,3)*((6*s23+s24)*(s14*s14)+s14*(-(s23*s24)+6*(s23*s23)-2*(s24*s24))-s24*(7*s23*s24+5*(s23*s23)+2*(s24*s24))+pow(s14,3))-pow(s13,4)*(2*(2*s23+s24)*(s14*s14)+3*pow(s24,3))-pow(s13,3)*(-4*s14*s24*(3*s23*s24+s23*s23+s24*s24)+s14*s14*(15*s23*s24+3*(s23*s23)+4*(s24*s24))+3*(4*s23+s24)*pow(s14,3)+3*(s23+s24)*pow(s24,3))+s14*(s13*s13)*(s14*s14*(-7*s23*s24+s23*s23+s24*s24)+(-13*s23+2*s24)*pow(s14,3)+s14*(-3*s24*(s23*s23)-5*s23*(s24*s24)+pow(s23,3)-5*pow(s24,3))+s24*(16*s24*(s23*s23)+15*s23*(s24*s24)+4*pow(s23,3)+3*pow(s24,3)))+s13*(s14*s14)*(s14*s14*(7*s23*s24+9*(s23*s23)+4*(s24*s24))+(-4*s23+3*s24)*pow(s14,3)+s14*(11*s24*(s23*s23)+10*s23*(s24*s24)+6*pow(s23,3)-3*pow(s24,3))-s24*(9*s24*(s23*s23)+10*s23*(s24*s24)+3*pow(s23,3)+4*pow(s24,3))))-s12*s12*(3*(s24*s24)*pow(s13,4)+pow(s13,3)*((11*s23+3*s24)*(s14*s14)+3*(2*s23+3*s24)*(s24*s24)-s14*(9*s23*s24+2*(s23*s23)+5*(s24*s24))+2*pow(s14,3))+s14*s14*(s14*s14*(4*s23*s24-11*(s23*s23)+4*(s24*s24))-(-3*s23*s24+2*(s23*s23)-2*(s24*s24))*((s23+s24)*(s23+s24))+(-3*s23+s24)*pow(s14,3)+s14*(6*s24*(s23*s23)+15*s23*(s24*s24)-7*pow(s23,3)+5*pow(s24,3)))+s13*s14*(s14*s14*(5*s23*s24-10*(s23*s23)+7*(s24*s24))-(9*s23*s24+2*(s23*s23)+3*(s24*s24))*((s23+s24)*(s23+s24))+(11*s23+s24)*pow(s14,3)+2*pow(s14,4)+s14*(-14*s24*(s23*s23)+s23*(s24*s24)-9*pow(s23,3)+7*pow(s24,3)))+s13*s13*(s14*s14*(2*s23*s24-s23*s23+s24*s24)+3*(s24*s24)*((s23+s24)*(s23+s24))+(23*s23+5*s24)*pow(s14,3)+4*pow(s14,4)-s14*(26*s24*(s23*s23)+38*s23*(s24*s24)+4*pow(s23,3)+11*pow(s24,3))))+pow(s12,3)*((2*s14*(s23+s24)-3*s24*(s23+3*s24))*pow(s13,3)-s24*pow(s13,4)+s13*s13*((-4*s23+2*s24)*(s14*s14)-3*s24*(4*s23*s24+s23*s23+3*(s24*s24))+s14*(29*s23*s24+10*(s23*s23)+13*(s24*s24))-4*pow(s14,3))-s13*(-(s14*s14*(21*s23*s24+18*(s23*s23)+s24*s24))+(4*s23+5*s24)*pow(s14,3)+4*pow(s14,4)-2*s14*(19*s24*(s23*s23)+19*s23*(s24*s24)+5*pow(s23,3)+5*pow(s24,3))+s24*pow(s23+s24,3))+s14*(s14*s14*(-3*s23*s24+14*(s23*s23)-7*(s24*s24))+(5*s23-3*s24)*pow(s14,3)+s14*(11*s24*(s23*s23)-2*s23*(s24*s24)+10*pow(s23,3)-3*pow(s24,3))+(2*s23+s24)*pow(s23+s24,3))))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient order epsilon^2 of master 9
template<>
double qq2yygLC<9,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s14,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)*(2*(s14*(3*s23*(s23+s24)+s14*(2*s23+s24))+s13*(3*s24*(s23+s24)+s14*(4*s23+5*s24))+3*s24*(s13*s13))*pow(s12,5)+2*(s14*s23+s13*s24)*pow(s12,6)+s14*(2*(s24*s24)*(s23*s24+s23*s23+3*(s24*s24))+s14*s24*(21*s23*s24-13*(s23*s23)+16*(s24*s24))+s14*s14*(17*s23*s24-10*(s23*s23)+23*(s24*s24)))*pow(s13,3)+pow(s12,4)*(6*(s14+s24)*(2*s23+3*s24)*(s13*s13)+2*s14*(s24*(s14*s14)+s14*(3*s23*s24+4*(s23*s23)+2*(s24*s24))+3*s23*((s23+s24)*(s23+s24)))+s13*((11*s23+30*s24)*(s14*s14)+2*s14*(24*s23*s24+9*(s23*s23)+13*(s24*s24))+6*s24*((s23+s24)*(s23+s24)))+6*s24*pow(s13,3))-(3*s14*(s23+s24)+4*s23*(s23+s24)+s14*s14)*(s23*s23)*pow(s14,4)+2*s24*pow(s13,4)*(s14*(s23-s24)*s24+(-2*s23+3*s24)*(s14*s14)+pow(s24,3))+2*s13*s23*pow(s14,3)*((-5*s23+8*s24)*(s14*s14)+4*s24*(s23*s23)+15*s23*(s24*s24)+s14*(4*s23*s24-9*(s23*s23)+18*(s24*s24))-2*pow(s23,3)+8*pow(s24,3))+s13*s13*(s14*s14)*(s14*s14*(37*s23*s24-19*(s23*s23)+17*(s24*s24))+2*s24*(-2*s24*(s23*s23)+6*s23*(s24*s24)-5*pow(s23,3)+6*pow(s24,3))+s14*(7*s24*(s23*s23)+73*s23*(s24*s24)-15*pow(s23,3)+27*pow(s24,3)))+s12*s12*(((-5*s23+18*s24)*(s14*s14)+6*(2*s23+3*s24)*(s24*s24)+2*s14*(10*s23*s24+3*(s23*s23)+9*(s24*s24)))*pow(s13,3)+2*(s14*(s23-s24)+3*(s24*s24))*pow(s13,4)-2*(s14*s14)*(s14*s14*(8*s23*s24+7*(s23*s23)+2*(s24*s24))+3*s23*s24*((s23+s24)*(s23+s24))+(2*s23+s24)*pow(s14,3)+s14*(14*s24*(s23*s23)+7*s23*(s24*s24)+6*pow(s23,3)+pow(s24,3)))+s13*s13*(s14*s14*(68*s23*s24-7*(s23*s23)+92*(s24*s24))+6*(s24*s24)*((s23+s24)*(s23+s24))+(-18*s23+64*s24)*pow(s14,3)+s14*(38*s24*(s23*s23)+70*s23*(s24*s24)+6*pow(s23,3)+42*pow(s24,3)))+s13*s14*(s14*s14*(71*s23*s24-38*(s23*s23)+82*(s24*s24))+2*(6*s23*s24+s23*s23+3*(s24*s24))*((s23+s24)*(s23+s24))+(-15*s23+42*s24)*pow(s14,3)+s14*(36*s24*(s23*s23)+91*s23*(s24*s24)-3*pow(s23,3)+50*pow(s24,3))))+s12*(-(s23*pow(s14,3)*((7*s23+6*s24)*(s14*s14)+2*s23*(8*s23*s24+3*(s23*s23)+5*(s24*s24))+s14*(18*s23*s24+13*(s23*s23)+5*(s24*s24))+pow(s14,3)))+pow(s13,4)*(4*s14*(s23-s24)*s24+(-4*s23+6*s24)*(s14*s14)+6*pow(s24,3))+pow(s13,3)*(2*s14*s24*(7*s23*s24+4*(s23*s23)+9*(s24*s24))+s14*s14*(14*s23*s24-9*(s23*s23)+34*(s24*s24))+(-11*s23+29*s24)*pow(s14,3)+6*(s23+s24)*pow(s24,3))+s14*(s13*s13)*(s14*s14*(82*s23*s24-43*(s23*s23)+93*(s24*s24))+(-11*s23+40*s24)*pow(s14,3)+4*s24*(4*s24*(s23*s23)+6*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3))+s14*(s24*(s23*s23)+72*s23*(s24*s24)-7*pow(s23,3)+64*pow(s24,3)))+s13*(s14*s14)*(20*(s23*s23)*(s24*s24)+s14*s14*(53*s23*s24-41*(s23*s23)+42*(s24*s24))+(-5*s23+17*s24)*pow(s14,3)-2*pow(s23,4)+30*s23*pow(s24,3)+s14*(12*s24*(s23*s23)+86*s23*(s24*s24)-31*pow(s23,3)+37*pow(s24,3))+12*pow(s24,4)))+pow(s12,3)*(2*(s13*s13)*((3*s23+20*s24)*(s14*s14)+3*s24*(4*s23*s24+s23*s23+3*(s24*s24))+s14*(29*s23*s24+9*(s23*s23)+24*(s24*s24)))+2*(3*s24*(s23+3*s24)+s14*(4*s23+3*s24))*pow(s13,3)+2*s24*pow(s13,4)+s14*(-2*s23*(3*s23+5*s24)*(s14*s14)-(5*s23+2*s24)*pow(s14,3)+2*s14*(-(s24*(s23*s23))-2*s23*(s24*s24)+2*pow(s23,3)+pow(s24,3))+2*s23*pow(s23+s24,3))+s13*(2*(s14*s14)*(38*s23*s24+5*(s23*s23)+34*(s24*s24))+(-7*s23+47*s24)*pow(s14,3)+2*s14*(27*s24*(s23*s23)+32*s23*(s24*s24)+6*pow(s23,3)+11*pow(s24,3))+2*s24*pow(s23+s24,3))))*pow(s12+s23+s24,-1))/8.;
}

// Coefficient of master 9 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygLC<9>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygLC<9,0>(s12,s13,s14,s23,s24),
        qq2yygLC<9,1>(s12,s13,s14,s23,s24),
        qq2yygLC<9,2>(s12,s13,s14,s23,s24)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 9: box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24)

// Coefficient order epsilon^0 of master 9
template<>
double qq2yygCF<9,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return ((-2*s14*(s13+s14)*(s14*s23+s12*(3*s13+s14+s23+s24)+s13*(2*s14+2*s23+s24)+s12*s12+2*(s13*s13))*pow(s13,-2)+(-2*s13*(s14-s24)*(s14*(-s23+s24)+s24*(s23+s24))-2*s14*s24*(s13*s13)-2*(s12*s12)*(s13*(5*s14-s23-3*s24)+6*s14*(s23+s24)-s24*(s23+s24)+4*(s14*s14))-2*s14*s24*(s23*s23)+2*(s14*s14)*(s23*s23)-2*s14*s23*(s24*s24)+2*(s13*s13)*(s24*s24)-2*s12*((s14-s24)*(s13*s13)+s13*(-(s24*(2*s23+3*s24))+s14*(4*s23+5*s24)+s14*s14)+s14*(7*s23*s24+2*s14*(s23+2*s24)+3*(s23*s23)+3*(s24*s24)))+2*(s13-3*s14+s23+2*s24)*pow(s12,3)+2*pow(s12,4)+(s13+s14)*(s14*s23-s13*s24+s12*(-s13+s14+s23+s24)+s12*s12)*(3*s12*s13*(s12+s14+s24)*(s12+s23+s24)+(pow(s12,2)+s14*s23-s13*s24+s12*(-s13+s14+s23+s24))*(pow(s12,2)+s14*s23-s13*s24+s12*(-s13+s14+s23+s24)))*pow(s13,-3)+(s14*(-s14+s24)+s12*(4*s13+2*s14+s23+s24)+s13*(2*s23+3*s24)+s12*s12+s13*s13)*(2*s12*(s14*s23*(s23+s24)+s24*(s13*s13)+s23*(s14*s14))+s12*s12*(2*s14*(2*s23+s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24))+(s14*s23-s13*s24)*(s14*s23-s13*s24)+2*(s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s13,-2)+(s14*s23-s13*s24+s12*(-s13+s14+s23+s24)+s12*s12)*(-(s14*(2*s23*(s23+s24)+s14*(2*s23+s24)))+(4*s13-2*s14+2*s23+3*s24)*(s12*s12)-(s14-2*s24)*(s13*s13)+s12*(s24*(s23+s24)-s14*(5*s23+2*s24)+s13*(-5*s14+3*s23+7*s24)+s13*s13-5*(s14*s14))-s13*(3*s14*s23-3*s24*(s23+s24)+s14*s14)+2*pow(s12,3))*pow(s13,-1)+2*s23*pow(s14,3))*pow(s12+s13+s14,-1)*pow(s23,-1)+(s13+s14)*pow(s13,-2)*((s14*s23-s13*s24+s12*(-s13+s14+s23+s24)+s12*s12)*(-(s14*s23)+s12*(3*s13-s14-2*s23-3*s24)-s23*s24+s13*(s14+s23+2*s24)-2*(s12*s12)+s13*s13-s24*s24)+2*s13*((s13+s14)*s24*(s13+s14+s23+s24)+(2*s13-s14-s23-2*s24)*(s12*s12)+s12*(s14*(-s23+s24)-s24*(s23+s24)+s13*(s14+s23+3*s24)+s13*s13)-pow(s12,3))-(s12-s13+s23+s24)*(2*s12*(s14*s23*(s23+s24)+s24*(s13*s13)+s23*(s14*s14))+s12*s12*(2*s14*(2*s23+s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24))+(s14*s23-s13*s24)*(s14*s23-s13*s24)+2*(s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s13,-1))*pow(s24,-1))*pow(s12+s23+s24,-1))/2.;
}

// Coefficient order epsilon^1 of master 9
template<>
double qq2yygCF<9,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)*((3*s14*(2*s23-s24)*(s14+s23+s24)+s13*(4*s14*(s23-s24)+3*s24*(s23+s24))-s24*(s13*s13))*pow(s12,5)+(s14*(2*s23-s24)+s13*s24)*pow(s12,6)+pow(s12,4)*(s13*s13*(s14*(2*s23-19*s24)-8*s23*s24+2*(s23*s23)-s24*s24)+s14*((6*s23-3*s24)*(s14*s14)+s14*(5*s23*s24+18*(s23*s23)-6*(s24*s24))+3*(2*s23-s24)*((s23+s24)*(s23+s24)))+s13*((8*s23-15*s24)*(s14*s14)+s14*(-3*s23*s24+10*(s23*s23)-13*(s24*s24))+3*s24*((s23+s24)*(s23+s24)))-(2*s23+9*s24)*pow(s13,3))-2*(4*(s23+s24)*(s24*s24)+s14*(s23*s23+2*(s24*s24)))*pow(s13,5)-2*s24*(s23+s24)*pow(s13,6)+s13*(s23*s23)*(-5*s23*s24+(2*s14-5*s24)*s24+2*(s23*s23))*pow(s14,3)+(2*s23+s24)*pow(s14,4)*pow(s23,3)+s23*(s13*s13)*(s14*s14)*(-2*(s23-4*s24)*(s14*s14)+3*s23*(s24*s24)+2*s14*(6*s23*s24+s23*s23+4*(s24*s24))+2*pow(s23,3)+5*pow(s24,3))-pow(s13,4)*(2*(s14*s14)*(-7*s23*s24+3*(s23*s23)+s24*s24)+2*s14*s24*(7*s23*s24+s23*s23+7*(s24*s24))+s24*(6*s24*(s23*s23)+12*s23*(s24*s24)-2*pow(s23,3)+5*pow(s24,3)))+s14*pow(s13,3)*(s14*s14*(20*s23*s24-6*(s23*s23))-6*(s23*s23)*(s24*s24)+4*s24*pow(s23,3)+2*pow(s23,4)+2*s14*(4*s24*(s23*s23)+s23*(s24*s24)+pow(s23,3)-3*pow(s24,3))-11*s23*pow(s24,3)-5*pow(s24,4))-s12*(2*(8*s23*s24+s14*(4*s23+3*s24)+s23*s23+8*(s24*s24))*pow(s13,5)+2*(s23+s24)*pow(s13,6)-(s14+s23+s24)*(6*s23+s24)*(s23*s23)*pow(s14,3)+s13*s23*(s14*s14)*(2*s24*(s14*s14)+5*s24*(s23*s23)+19*s23*(s24*s24)+s14*(10*s23*s24-8*(s23*s23)+12*(s24*s24))-4*pow(s23,3)+10*pow(s24,3))+pow(s13,4)*(6*(2*s23+s24)*(s14*s14)+12*s24*(s23*s23)+44*s23*(s24*s24)+s14*(24*s23*s24+8*(s23*s23)+38*(s24*s24))-2*pow(s23,3)+25*pow(s24,3))+pow(s13,3)*(22*(s23*s23)*(s24*s24)+4*(s14*s14)*(-(s23*s24)+2*(s23*s23)+8*(s24*s24))+2*(4*s23+s24)*pow(s14,3)+2*s24*pow(s23,3)-2*pow(s23,4)+23*s23*pow(s24,3)+s14*(16*s24*(s23*s23)+72*s23*(s24*s24)-8*pow(s23,3)+42*pow(s24,3))+5*pow(s24,4))+s14*(s13*s13)*(30*(s23*s23)*(s24*s24)+2*(s14*s14)*(-5*s23*s24+s23*s23+5*(s24*s24))+2*s23*pow(s14,3)+2*s24*pow(s23,3)-4*pow(s23,4)+33*s23*pow(s24,3)+s14*(5*s24*(s23*s23)+34*s23*(s24*s24)-10*pow(s23,3)+19*pow(s24,3))+9*pow(s24,4)))-s12*s12*(s23*(s14*s14)*((-6*s23+s24)*(s14*s14)+s14*(-11*s23*s24-18*(s23*s23)+2*(s24*s24))-(6*s23-s24)*((s23+s24)*(s23+s24)))+(36*s23*s24+2*s14*(8*s23+11*s24)+2*(s23*s23)+33*(s24*s24))*pow(s13,4)+(6*s23+8*s24)*pow(s13,5)+pow(s13,3)*(3*s14*s24*(19*s23+23*s24)+2*(7*s23+12*s24)*(s14*s14)+22*s24*(s23*s23)+54*s23*(s24*s24)-6*pow(s23,3)+19*pow(s24,3))+s13*s13*(s14*s14*(35*s23*s24-8*(s23*s23)+51*(s24*s24))-(-6*s23*s24+2*(s23*s23)+s24*s24)*((s23+s24)*(s23+s24))+2*(2*s23+7*s24)*pow(s14,3)+s14*(24*s24*(s23*s23)+76*s23*(s24*s24)-12*pow(s23,3)+35*pow(s24,3)))+s13*s14*(s14*s14*(19*s23*s24-10*(s23*s23)+13*(s24*s24))+4*s24*pow(s14,3)+s14*(13*s24*(s23*s23)+40*s23*(s24*s24)-16*pow(s23,3)+14*pow(s24,3))-(2*s23-5*s24)*pow(s23+s24,3)))+pow(s12,3)*(-((33*s23*s24+8*s14*(s23+4*s24)-2*(s23*s23)+23*(s24*s24))*pow(s13,3))-(6*s23+13*s24)*pow(s13,4)+s13*s13*(-32*s24*(s14*s14)-9*s24*(s23*s23)+s14*(-43*s23*s24+10*(s23*s23)-45*(s24*s24))-12*s23*(s24*s24)+4*pow(s23,3)+pow(s24,3))+s14*(s14*s14*(s23*s24+18*(s23*s23)-3*(s24*s24))+(2*s23-s24)*pow(s14,3)+s14*(19*s24*(s23*s23)-2*s23*(s24*s24)+18*pow(s23,3)-3*pow(s24,3))+(2*s23-s24)*pow(s23+s24,3))+s13*(s14*s14*(-23*s23*s24+20*(s23*s23)-29*(s24*s24))+2*s14*(4*s23-7*s24)*((s23+s24)*(s23+s24))+2*(2*s23-7*s24)*pow(s14,3)+s24*pow(s23+s24,3))))*pow(s12+s23+s24,-1))/4.;
}

// Coefficient order epsilon^2 of master 9
template<>
double qq2yygCF<9,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)*((-3*s14*(s23-s24)*(s14+s23+s24)+s13*(-2*s14*(s23-4*s24)+6*s24*(s23+s24))+6*s24*(s13*s13))*pow(s12,5)+(2*s13*s24+s14*(-s23+s24))*pow(s12,6)+pow(s12,4)*(s13*s13*(11*s23*s24+s14*(-3*s23+19*s24)-2*(s23*s23)+17*(s24*s24))+s14*(-3*(s23-s24)*(s14*s14)+s14*(2*s23*s24-9*(s23*s23)+6*(s24*s24))-3*(s23-s24)*((s23+s24)*(s23+s24)))+s13*((-2*s23+9*s24)*(s14*s14)+s14*(13*s23*s24-7*(s23*s23)+18*(s24*s24))+6*s24*((s23+s24)*(s23+s24)))-(s23-8*s24)*pow(s13,3))+(-(s14*(7*s23*s24+s23*s23-4*(s24*s24)))+s24*(s23*s24-3*(s23*s23)+3*(s24*s24)))*pow(s13,5)+s24*(-s23+s24)*pow(s13,6)+s13*(s23*s23)*(s14*(2*s23-3*s24)-3*(s23*s24+s23*s23+s24*s24))*pow(s14,3)-pow(s14,4)*pow(s23,4)+s23*(s13*s13)*(s14*s14)*(-2*(s23+s24)*(s14*s14)-3*s24*(s23*s23)+s23*(s24*s24)+s14*(-14*s23*s24-3*(s23*s23)+s24*s24)-4*pow(s23,3)+3*pow(s24,3))+pow(s13,4)*(s14*s14*(-9*s23*s24-4*(s23*s23)+9*(s24*s24))-2*(s23-s24)*s24*((s23+s24)*(s23+s24))+s14*(-15*s24*(s23*s23)+3*s23*(s24*s24)-3*pow(s23,3)+11*pow(s24,3)))+s12*s12*(s23*(s14*s14)*((-3*s23+2*s24)*(s14*s14)-s14*(s23*s24+9*(s23*s23)-4*(s24*s24))-(3*s23-2*s24)*((s23+s24)*(s23+s24)))-2*(s14*(6*s23-9*s24)-s23*s24+5*(s23*s23)-8*(s24*s24))*pow(s13,4)+(-3*s23+4*s24)*pow(s13,5)+pow(s13,3)*((-13*s23+31*s24)*(s14*s14)-11*s24*(s23*s23)+14*s23*(s24*s24)+s14*(5*s23*s24-29*(s23*s23)+51*(s24*s24))-9*pow(s23,3)+18*pow(s24,3))+s13*s14*(s23*(s23-2*s24)*(s14*s14)-(-(s23*s24)+3*(s23*s23)-2*(s24*s24))*((s23+s24)*(s23+s24))+(2*s23-s24)*pow(s14,3)+s14*(-10*s24*(s23*s23)+2*s23*(s24*s24)-14*pow(s23,3)+3*pow(s24,3)))+s13*s13*(s14*s14*(5*s23*s24-18*(s23*s23)+36*(s24*s24))-(s23*s24+2*(s23*s23)-5*(s24*s24))*((s23+s24)*(s23+s24))-2*(s23-8*s24)*pow(s14,3)+s14*(-19*s24*(s23*s23)+19*s23*(s24*s24)-20*pow(s23,3)+27*pow(s24,3))))-s12*((5*s14*(s23-s24)+s23*s24+4*(s23*s23)-7*(s24*s24))*pow(s13,5)+(s23-s24)*pow(s13,6)+(3*s23-s24)*(s14+s23+s24)*(s23*s23)*pow(s14,3)+pow(s13,4)*((9*s23-13*s24)*(s14*s14)+9*s24*(s23*s23)+s14*(8*s23*s24+17*(s23*s23)-29*(s24*s24))-7*s23*(s24*s24)+5*pow(s23,3)-11*pow(s24,3))+s13*s23*(s14*s14)*(-4*(s23-s24)*(s14*s14)+10*s24*(s23*s23)+7*s23*(s24*s24)+s14*(7*s23*s24+4*(s23*s23)+7*(s24*s24))+6*pow(s23,3)+3*pow(s24,3))+pow(s13,3)*(s14*s14*(5*s23*s24+22*(s23*s23)-41*(s24*s24))+3*(s23*s23)*(s24*s24)+(7*s23-15*s24)*pow(s14,3)+6*s24*pow(s23,3)+2*pow(s23,4)+s14*(28*s24*(s23*s23)-17*s23*(s24*s24)+19*pow(s23,3)-30*pow(s24,3))-6*s23*pow(s24,3)-5*pow(s24,4))+s14*(s13*s13)*(s14*s14*(6*s23*s24+5*(s23*s23)-15*(s24*s24))+10*(s23*s23)*(s24*s24)+2*(s23-3*s24)*pow(s14,3)+12*s24*pow(s23,3)+6*pow(s23,4)+s14*(26*s24*(s23*s23)-3*s23*(s24*s24)+19*pow(s23,3)-14*pow(s24,3))-s23*pow(s24,3)-5*pow(s24,4)))-s14*pow(s13,3)*(s14*s14*(5*s23*s24+5*(s23*s23)-6*(s24*s24))+4*(s23*s23)*(s24*s24)+6*s24*pow(s23,3)+2*pow(s23,4)+s14*(21*s24*(s23*s23)-7*s23*(s24*s24)+8*pow(s23,3)-10*pow(s24,3))-5*s23*pow(s24,3)-4*pow(s24,4))+pow(s12,3)*((7*s23*s24+s14*(-9*s23+25*s24)-8*(s23*s23)+21*(s24*s24))*pow(s13,3)+(-3*s23+7*s24)*pow(s13,4)+s13*s13*((-3*s23+23*s24)*(s14*s14)+s14*(17*s23*s24-17*(s23*s23)+41*(s24*s24))-4*(-5*s23*(s24*s24)+pow(s23,3)-4*pow(s24,3)))+s14*(s14*s14*(4*s23*s24-9*(s23*s23)+3*(s24*s24))+(-s23+s24)*pow(s14,3)+s14*(-5*s24*(s23*s23)+7*s23*(s24*s24)-9*pow(s23,3)+3*pow(s24,3))-(s23-s24)*pow(s23+s24,3))+s13*(s14*s14*(9*s23*s24-10*(s23*s23)+12*(s24*s24))+2*(s23+s24)*pow(s14,3)+s14*(20*s23*(s24*s24)-8*pow(s23,3)+12*pow(s24,3))+2*s24*pow(s23+s24,3))))*pow(s12+s23+s24,-1))/2.;
}

// Coefficient of master 9 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCF<9>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCF<9,0>(s12,s13,s14,s23,s24),
        qq2yygCF<9,1>(s12,s13,s14,s23,s24),
        qq2yygCF<9,2>(s12,s13,s14,s23,s24)
    });
}


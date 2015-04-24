/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 1: bubble(s12)

// Coefficient order epsilon^-1 of master 1
template<>
double qq2yyg6SC<1,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (6*s13*s14*s23*(s12*s12)+6*s13*s14*s24*(s12*s12)+6*s13*s23*s24*(s12*s12)+6*s14*s23*s24*(s12*s12)+3*s12*s14*s23*(s13*s13)+3*s12*s14*s24*(s13*s13)+3*s14*(s12*s12)*(s13*s13)+3*s23*(s12*s12)*(s13*s13)+3*s24*(s12*s12)*(s13*s13)+3*s12*s13*s23*(s14*s14)+3*s12*s13*s24*(s14*s14)+3*s13*(s12*s12)*(s14*s14)+3*s23*(s12*s12)*(s14*s14)+3*s24*(s12*s12)*(s14*s14)+3*s12*s13*s24*(s23*s23)+3*s12*s14*s24*(s23*s23)+3*s13*(s12*s12)*(s23*s23)+3*s14*(s12*s12)*(s23*s23)+3*s24*(s12*s12)*(s23*s23)+3*s12*s13*s23*(s24*s24)+3*s12*s14*s23*(s24*s24)+3*s13*(s12*s12)*(s24*s24)+3*s14*(s12*s12)*(s24*s24)+3*s23*(s12*s12)*(s24*s24)+6*s13*s14*pow(s12,3)+6*s13*s23*pow(s12,3)+6*s14*s23*pow(s12,3)+6*s13*s24*pow(s12,3)+6*s14*s24*pow(s12,3)+6*s23*s24*pow(s12,3)+3*(s13*s13)*pow(s12,3)+3*(s14*s14)*pow(s12,3)+3*(s23*s23)*pow(s12,3)+3*(s24*s24)*pow(s12,3)+4*s13*pow(s12,4)+4*s14*pow(s12,4)+4*s23*pow(s12,4)+4*s24*pow(s12,4)+2*pow(s12,5)+2*s12*s23*pow(s13,3)+s12*s24*pow(s13,3)+s12*s12*pow(s13,3)+s12*s23*pow(s14,3)+2*s12*s24*pow(s14,3)+s12*s12*pow(s14,3)+2*s12*s13*pow(s23,3)+s12*s14*pow(s23,3)+s12*s12*pow(s23,3)+s12*s13*pow(s24,3)+2*s12*s14*pow(s24,3)+s12*s12*pow(s24,3))*pow(s13*s14*s23*s24*(s12*s12)+s12*s14*s23*s24*(s13*s13)+s12*s13*s23*s24*(s14*s14)+s12*s13*s14*s24*(s23*s23)+s14*s24*(s13*s13)*(s23*s23)+s13*s24*(s14*s14)*(s23*s23)+s12*s13*s14*s23*(s24*s24)+s14*s23*(s13*s13)*(s24*s24)+s13*s23*(s14*s14)*(s24*s24),-1);
}

// Coefficient order epsilon^0 of master 1
template<>
double qq2yyg6SC<1,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (-14*s12*s13*s14*s23*s24-24*s13*s14*s23*(s12*s12)-24*s13*s14*s24*(s12*s12)-24*s13*s23*s24*(s12*s12)-24*s14*s23*s24*(s12*s12)-12*s12*s14*s23*(s13*s13)-12*s12*s14*s24*(s13*s13)-4*s12*s23*s24*(s13*s13)-11*s14*(s12*s12)*(s13*s13)-16*s23*(s12*s12)*(s13*s13)-9*s24*(s12*s12)*(s13*s13)-12*s12*s13*s23*(s14*s14)-12*s12*s13*s24*(s14*s14)-4*s12*s23*s24*(s14*s14)-11*s13*(s12*s12)*(s14*s14)-9*s23*(s12*s12)*(s14*s14)-16*s24*(s12*s12)*(s14*s14)-4*s12*s13*s14*(s23*s23)-12*s12*s13*s24*(s23*s23)-12*s12*s14*s24*(s23*s23)-16*s13*(s12*s12)*(s23*s23)-9*s14*(s12*s12)*(s23*s23)-11*s24*(s12*s12)*(s23*s23)-4*s12*(s13*s13)*(s23*s23)+s12*(s14*s14)*(s23*s23)-4*s12*s13*s14*(s24*s24)-12*s12*s13*s23*(s24*s24)-12*s12*s14*s23*(s24*s24)-9*s13*(s12*s12)*(s24*s24)-16*s14*(s12*s12)*(s24*s24)-11*s23*(s12*s12)*(s24*s24)+s12*(s13*s13)*(s24*s24)-4*s12*(s14*s14)*(s24*s24)-19*s13*s14*pow(s12,3)-24*s13*s23*pow(s12,3)-17*s14*s23*pow(s12,3)-17*s13*s24*pow(s12,3)-24*s14*s24*pow(s12,3)-19*s23*s24*pow(s12,3)-10*(s13*s13)*pow(s12,3)-10*(s14*s14)*pow(s12,3)-10*(s23*s23)*pow(s12,3)-10*(s24*s24)*pow(s12,3)-12*s13*pow(s12,4)-12*s14*pow(s12,4)-12*s23*pow(s12,4)-12*s24*pow(s12,4)-6*pow(s12,5)-8*s12*s23*pow(s13,3)-4*s12*s24*pow(s13,3)-4*(s12*s12)*pow(s13,3)-4*s12*s23*pow(s14,3)-8*s12*s24*pow(s14,3)-4*(s12*s12)*pow(s14,3)-8*s12*s13*pow(s23,3)-4*s12*s14*pow(s23,3)-4*(s12*s12)*pow(s23,3)-4*s12*s13*pow(s24,3)-8*s12*s14*pow(s24,3)-4*(s12*s12)*pow(s24,3))*pow(s13*s14*s23*s24*(s12*s12)+s12*s14*s23*s24*(s13*s13)+s12*s13*s23*s24*(s14*s14)+s12*s13*s14*s24*(s23*s23)+s14*s24*(s13*s13)*(s23*s23)+s13*s24*(s14*s14)*(s23*s23)+s12*s13*s14*s23*(s24*s24)+s14*s23*(s13*s13)*(s24*s24)+s13*s23*(s14*s14)*(s24*s24),-1);
}

// Coefficient order epsilon^1 of master 1
template<>
double qq2yyg6SC<1,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (34*s12*s13*s14*s23*s24+36*s13*s14*s23*(s12*s12)+36*s13*s14*s24*(s12*s12)+36*s13*s23*s24*(s12*s12)+36*s14*s23*s24*(s12*s12)+15*s12*s14*s23*(s13*s13)+15*s12*s14*s24*(s13*s13)+12*s12*s23*s24*(s13*s13)+11*s14*(s12*s12)*(s13*s13)+27*s23*(s12*s12)*(s13*s13)+13*s24*(s12*s12)*(s13*s13)+15*s12*s13*s23*(s14*s14)+15*s12*s13*s24*(s14*s14)+12*s12*s23*s24*(s14*s14)+11*s13*(s12*s12)*(s14*s14)+13*s23*(s12*s12)*(s14*s14)+27*s24*(s12*s12)*(s14*s14)+12*s12*s13*s14*(s23*s23)+15*s12*s13*s24*(s23*s23)+15*s12*s14*s24*(s23*s23)+27*s13*(s12*s12)*(s23*s23)+13*s14*(s12*s12)*(s23*s23)+11*s24*(s12*s12)*(s23*s23)+12*s12*(s13*s13)*(s23*s23)+s12*(s14*s14)*(s23*s23)+12*s12*s13*s14*(s24*s24)+15*s12*s13*s23*(s24*s24)+15*s12*s14*s23*(s24*s24)+13*s13*(s12*s12)*(s24*s24)+27*s14*(s12*s12)*(s24*s24)+11*s23*(s12*s12)*(s24*s24)+s12*(s13*s13)*(s24*s24)+12*s12*(s14*s14)*(s24*s24)+20*s13*s14*pow(s12,3)+36*s13*s23*pow(s12,3)+22*s14*s23*pow(s12,3)+22*s13*s24*pow(s12,3)+36*s14*s24*pow(s12,3)+20*s23*s24*pow(s12,3)+12*(s13*s13)*pow(s12,3)+12*(s14*s14)*pow(s12,3)+12*(s23*s23)*pow(s12,3)+12*(s24*s24)*pow(s12,3)+14*s13*pow(s12,4)+14*s14*pow(s12,4)+14*s23*pow(s12,4)+14*s24*pow(s12,4)+7*pow(s12,5)+10*s12*s23*pow(s13,3)+5*s12*s24*pow(s13,3)+5*(s12*s12)*pow(s13,3)+5*s12*s23*pow(s14,3)+10*s12*s24*pow(s14,3)+5*(s12*s12)*pow(s14,3)+10*s12*s13*pow(s23,3)+5*s12*s14*pow(s23,3)+5*(s12*s12)*pow(s23,3)+5*s12*s13*pow(s24,3)+10*s12*s14*pow(s24,3)+5*(s12*s12)*pow(s24,3))*pow(s13*s14*s23*s24*(s12*s12)+s12*s14*s23*s24*(s13*s13)+s12*s13*s23*s24*(s14*s14)+s12*s13*s14*s24*(s23*s23)+s14*s24*(s13*s13)*(s23*s23)+s13*s24*(s14*s14)*(s23*s23)+s12*s13*s14*s23*(s24*s24)+s14*s23*(s13*s13)*(s24*s24)+s13*s23*(s14*s14)*(s24*s24),-1);
}

// Coefficient of master 1 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6SC<1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6SC<1,-1>(s12,s13,s14,s23,s24),
        qq2yyg6SC<1,0>(s12,s13,s14,s23,s24),
        qq2yyg6SC<1,1>(s12,s13,s14,s23,s24)
    });
}


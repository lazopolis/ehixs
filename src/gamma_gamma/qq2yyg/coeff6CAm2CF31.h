/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 31: box6(s12+s14+s24,s12+s13+s23,s12)

// Coefficient order epsilon^-1 of master 31
template<>
double qq2yyg6CAm2CF<31,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s12+s13+s23,-1)*pow(s12+s14+s24,-1)*(s13*(-(s14*s23)+s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s12+s13+s14,-1)*pow(s23,-1)+s12*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s24,-1)+2*s12*(s12+s13)*(s12+s24)*(-(s14*s23)+s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s14,-1)*pow(s23,-1)*pow(s24,-1)+(s12+s14)*(s12*(2*s13+s14)+s14*(s13+s23))*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)+s23*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s12+s23+s24,-1)+(s12+s24)*((s13+s23)*s24+s12*(2*s23+s24))*(-(s14*s23)+s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s14,-1)*pow(s23,-1)*pow(s12+s23+s24,-1)-pow(s12+s13+s14,-1)*pow(s23,-1)*(-2*s12*s13*(2*s14-s23)-2*s13*s14*s23-4*s14*(s12*s12)-2*s14*(s13*s13)-2*(-(s14*s23)+s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(3*s12*s14*s23*(s12+s13+s14+s23+s24)+(pow(s12,2)-s14*s23+s13*s24+s12*(s13+s14+s23+s24))*(pow(s12,2)-s14*s23+s13*s24+s12*(s13+s14+s23+s24)))*pow(s12+s13+s14+s23+s24,-3)+2*(2*s12+s13-s14)*(2*s12*s13*s24*(s13+s14+s23+s24)+s12*s12*(2*s13*(s14+s23+2*s24)+s13*s13+(s14+s23+s24)*(s14+s23+s24))+(s14*s23-s13*s24)*(s14*s23-s13*s24)+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s12+s13+s14+s23+s24,-2)-(-(s14*s23)+s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(2*s12*(s13-2*s14)+s13*(s13-2*s14+s23)+2*(s12*s12))*pow(s12+s13+s14+s23+s24,-1))-pow(s13,-1)*pow(s12+s23+s24,-1)*(2*s12*s23*(s13-2*s24)-2*s13*s23*s24-4*s24*(s12*s12)-2*s24*(s23*s23)-2*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(3*s12*s13*s24*(s12+s13+s14+s23+s24)+(pow(s12,2)+s14*s23-s13*s24+s12*(s13+s14+s23+s24))*(pow(s12,2)+s14*s23-s13*s24+s12*(s13+s14+s23+s24)))*pow(s12+s13+s14+s23+s24,-3)+2*(2*s12+s23-s24)*(2*s12*s14*s23*(s13+s14+s23+s24)+s12*s12*(2*s13*(s14+s23+s24)+2*s14*(2*s23+s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24))+(s14*s23-s13*s24)*(s14*s23-s13*s24)+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s12+s13+s14+s23+s24,-2)-(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(2*s12*(s23-2*s24)+s23*(s13+s23-2*s24)+2*(s12*s12))*pow(s12+s13+s14+s23+s24,-1))-pow(s13,-1)*pow(s24,-1)*(6*s13*(s12*s12)+8*s14*(s12*s12)+8*s23*(s12*s12)+6*s24*(s12*s12)+2*s13*(s14*s14)+2*s23*(s14*s23+s23*s24+s14*s14)+2*s12*(5*s14*s23+s13*(3*s14-s24)+s23*(s23+3*s24)+s14*s14)+6*pow(s12,3)+2*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(3*s12*s13*s24*(s12+s13+s14+s23+s24)+(pow(s12,2)+s14*s23-s13*s24+s12*(s13+s14+s23+s24))*(pow(s12,2)+s14*s23-s13*s24+s12*(s13+s14+s23+s24)))*pow(s12+s13+s14+s23+s24,-3)-2*(s12-s13-s24)*(2*s12*s14*s23*(s13+s14+s23+s24)+s12*s12*(2*s13*(s14+s23+s24)+2*s14*(2*s23+s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24))+(s14*s23-s13*s24)*(s14*s23-s13*s24)+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s12+s13+s14+s23+s24,-2)-(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(3*s13*s14+s23*(4*s14+3*s24)+s12*(5*s13+4*s14+4*s23+5*s24)+3*(s12*s12))*pow(s12+s13+s14+s23+s24,-1))-pow(s12+s13+s14,-1)*pow(s23,-1)*(-2*s12*s14*(6*s13+s14+2*s23)-12*s13*(s12*s12)-2*s13*(s14*s14)-2*s23*(s14*s14)+4*pow(s12,3)-2*(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(3*s12*s13*s24*(s12+s13+s14+s23+s24)+(pow(s12,2)+s14*s23-s13*s24+s12*(s13+s14+s23+s24))*(pow(s12,2)+s14*s23-s13*s24+s12*(s13+s14+s23+s24)))*pow(s12+s13+s14+s23+s24,-3)+2*(3*s12-s13+s14)*(2*s12*s14*s23*(s13+s14+s23+s24)+s12*s12*(2*s13*(s14+s23+s24)+2*s14*(2*s23+s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24))+(s14*s23-s13*s24)*(s14*s23-s13*s24)+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s12+s13+s14+s23+s24,-2)-(s14*s23-s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(s12*(-6*s13+3*s14)-s14*(3*s13+s23)+6*(s12*s12))*pow(s12+s13+s14+s23+s24,-1))-2*pow(s13,-1)*pow(s24,-1)*(-4*s12*s13*s24-6*(s13+s24)*(s12*s12)-8*pow(s12,3)+(-(s14*s23)+s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(3*s12*s14*s23*(s12+s13+s14+s23+s24)+(pow(s12,2)-s14*s23+s13*s24+s12*(s13+s14+s23+s24))*(pow(s12,2)-s14*s23+s13*s24+s12*(s13+s14+s23+s24)))*pow(s12+s13+s14+s23+s24,-3)-(4*s12+s13+s24)*(2*s12*s13*s24*(s13+s14+s23+s24)+s12*s12*(2*s13*(s14+s23+2*s24)+s13*s13+(s14+s23+s24)*(s14+s23+s24))+(s14*s23-s13*s24)*(s14*s23-s13*s24)+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s12+s13+s14+s23+s24,-2)+(-(s14*s23)+s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(s13*s24+3*s12*(s13+s24)+6*(s12*s12))*pow(s12+s13+s14+s23+s24,-1))-pow(s13,-1)*pow(s12+s23+s24,-1)*(-2*s12*s24*(2*s13+6*s23+s24)-12*s23*(s12*s12)-2*s13*(s24*s24)-2*s23*(s24*s24)+4*pow(s12,3)-2*(-(s14*s23)+s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(3*s12*s14*s23*(s12+s13+s14+s23+s24)+(pow(s12,2)-s14*s23+s13*s24+s12*(s13+s14+s23+s24))*(pow(s12,2)-s14*s23+s13*s24+s12*(s13+s14+s23+s24)))*pow(s12+s13+s14+s23+s24,-3)+2*(3*s12-s23+s24)*(2*s12*s13*s24*(s13+s14+s23+s24)+s12*s12*(2*s13*(s14+s23+2*s24)+s13*s13+(s14+s23+s24)*(s14+s23+s24))+(s14*s23-s13*s24)*(s14*s23-s13*s24)+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s12+s13+s14+s23+s24,-2)-(-(s14*s23)+s13*s24+s12*(s13+s14+s23+s24)+s12*s12)*(-((s13+3*s23)*s24)+s12*(-6*s23+3*s24)+6*(s12*s12))*pow(s12+s13+s14+s23+s24,-1))))/2.;
}

// Coefficient of master 31 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6CAm2CF<31>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6CAm2CF<31,-1>(s12,s13,s14,s23,s24)
    });
}


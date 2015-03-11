/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 15: box6(s12,s24,s12+s14+s24)

// Coefficient order epsilon^-1 of master 15
template<>
double qq2yyg6CAm2CF<15,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (2*s14*pow(s13,-1)*pow(s24,-1)+(-(s14*(2*s14+s23))+s13*(-2*s14+s24)+s12*(s13-3*s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s14,-1)*pow(s24,-1)-(s12*(s14*(7*s14+s23+s24)+s13*(3*s14+2*s24))+(2*s13+9*s14+2*(s23+s24))*(s12*s12)+s14*(-(s14*s23)+s13*s24+2*(s14*s14))+2*pow(s12,3))*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)-pow(s14,-2)*(-(s13*s14*(s23*s24+s14*(3*s23+s24)))+s24*(s14+s24)*(s13*s13)+s12*((s14+2*s24)*(s13*s13)+s14*(s23*(s23+s24)-s14*(2*s23+s24)-3*(s14*s14))+s13*(2*s24*(s23+s24)+s14*(2*s23+3*s24)-2*(s14*s14)))+s12*s12*(s14*(3*s23+2*s24)+s13*(3*s14+2*s23+4*s24)+s13*s13-2*(s14*s14)+(s23+s24)*(s23+s24))+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4)-(3*s23+2*s24)*pow(s14,3))*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s24,-1)+s12*(s14*s23+s13*(2*s14+s24)+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)+(s12+s14)*(s12*(2*s13+s14)+s14*(s13+s23))*(s14*s23+s13*(2*s14+s24)+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)-(s13*s14*(-3*s14*s23+s24*(-s23+s24))+s24*(s14+s24)*(s13*s13)-s23*s24*(s14*s14)+s12*(s14*(s23+s24)*(s14+s23+s24)+(s14+2*s24)*(s13*s13)+s13*(2*s24*(s23+s24)+2*s14*(s23+2*s24)+s14*s14))+s12*s12*(3*s14*(s23+s24)+s13*(3*s14+2*s23+4*s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24))+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s13,-1)*pow(s14,-2)*pow(s24,-1)*pow(s12+s23+s24,-1)+(s12*(s13*(3*s14+2*s24)+s14*(-5*s14+s23+3*s24))+(2*s13-3*s14+2*(s23+s24))*(s12*s12)+s14*(-(s14*s23)+s13*s24-2*(s14*s14))+2*pow(s12,3))*pow(s13,-1)*pow(s14,-1)*pow(s24,-1)*pow(s12+s23+s24,-1)+s23*(s14*s23+s13*(2*s14+s24)+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s12+s13+s23,-1)*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1)-(s12+s23+s24)*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s14+s24,-1)*pow(s12+s13+s14+s23+s24,-1)-s14*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*pow(s13,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)*pow(s12+s13+s14+s23+s24,-1)+(s12+s14)*(s13*(2*s14+s24)+s12*(s13+3*s14+s23+s24)+s14*(2*s14+s23+2*s24)+s12*s12)*(s12*(2*s13+3*s14)+s14*(s13+s14+s23+s24)+2*(s12*s12))*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1))/2.;
}

// Coefficient of master 15 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6CAm2CF<15>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6CAm2CF<15,-1>(s12,s13,s14,s23,s24)
    });
}


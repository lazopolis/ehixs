/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 14: box6(s12,s23,s12+s13+s23)

// Coefficient order epsilon^-1 of master 14
template<>
double qq2yyg6CAm2CF<14,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s23,-1)*(-2*s13*pow(s12+s13+s14,-1)+(s14*s23-s13*(2*s14+s24)+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s12+s13+s14,-1)+(s13*s23*(2*s14-2*s23-3*s24)+(s13+s14+2*s23+s24)*(s12*s12)+s12*(s23*(2*s14+s23+s24)+s13*(2*s14+3*s23+s24)-2*(s13*s13))-2*s24*(s13*s13)+s14*(s23*s23)+pow(s12,3))*pow(s13,-2)*pow(s24,-1)+4*s12*(s12+s13)*pow(s13,-1)*pow(s24,-1)+s13*(s14*s23+s13*(2*s14+s24)+s12*(s13+s14+s23+s24)+s12*s12)*pow(s12+s13+s14,-1)*pow(s12+s13+s23,-1)*pow(s12+s14+s24,-1)+2*s12*(s12+s13)*(s12+s24)*(s14*s23+s13*(2*s14+s24)+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*pow(s12+s14+s24,-1)+(s13*s23*(2*s14-3*s24)+(3*s13+s14+2*s23+s24)*(s12*s12)-2*s24*(s13*s13)+s12*(s13*(2*s14-s23+s24)+s23*(2*s14+s23+s24)+2*(s13*s13))+s14*(s23*s23)+pow(s12,3))*pow(s13,-2)*pow(s12+s23+s24,-1)-4*s12*(s12+s13)*pow(s13,-1)*pow(s12+s23+s24,-1)+(s12+s24)*((s13+s23)*s24+s12*(2*s23+s24))*(s14*s23+s13*(2*s14+s24)+s12*(s13+s14+s23+s24)+s12*s12)*pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s23,-1)*pow(s12+s14+s24,-1)*pow(s12+s23+s24,-1)-s12*(s14*s23+s12*(3*s13+s14+s23+s24)+s13*(2*s14+2*s23+s24)+s12*s12+2*(s13*s13))*pow(s12+s13+s14,-1)*pow(s12+s13+s23,-1)*pow(s12+s13+s14+s23+s24,-1)+(s23+s24)*(s12*(s13+s24)+(s13+s23)*(s23+s24)+s12*s12)*(s14*s23+s12*(3*s13+s14+s23+s24)+s13*(2*s14+2*s23+s24)+s12*s12+2*(s13*s13))*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s12+s13+s23,-1)*pow(s24,-1)*pow(s12+s13+s14+s23+s24,-1)-2*s12*(s12+s13)*(s23+s24)*(s14*s23+s12*(3*s13+s14+s23+s24)+s13*(2*s14+2*s23+s24)+s12*s12+2*(s13*s13))*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s12+s13+s23,-1)*pow(s12+s23+s24,-1)*pow(s12+s13+s14+s23+s24,-1)))/2.;
}

// Coefficient of master 14 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6CAm2CF<14>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6CAm2CF<14,-1>(s12,s13,s14,s23,s24)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 11: box6(s14,-s12-s13-s14-s23-s24,-s12-s23-s24)

// Coefficient order epsilon^-1 of master 11
template<>
double qq2yyg6LC<11,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -((s12+s14)*(s12*s12)*pow(s13,-3)*pow(s12+s13+s14,-1))+s12*s14*pow(s13,-2)*pow(s12+s13+s14,-1)-(s12*s24*(s12*s12+s13*s13)*pow(s13,-3)*pow(s12+s13+s14,-1)*pow(s23,-1))/2.-(s12+s14)*pow(s12,3)*pow(s13,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)+((s12+s23)*pow(s12,3)*pow(s13,-3)*pow(s12+s13+s14,-1)*pow(s23,-1))/2.-pow(s12,3)*pow(s13,-2)*pow(s12+s13+s14,-1)*pow(s23,-1)+(s12*(s12+s23)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1))/2.-(s12*(s12*s12*(2*s13*s14+2*(s13*s13)+s14*s14)+2*s12*s23*((s13+s14)*(s13+s14))+2*s13*s14*(s23*s23)+s14*s14*(s23*s23)+s13*s13*(2*(s14*s14)+s23*s23))*pow(s13,-3)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow(s12+s23+s24,-1))/2.;
}

// Coefficient of master 11 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6LC<11>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6LC<11,-1>(s12,s13,s14,s23,s24)
    });
}


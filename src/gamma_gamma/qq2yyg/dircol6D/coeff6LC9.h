/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 9: box6(s13,-s12-s13-s14-s23-s24,-s12-s23-s24)

// Coefficient order epsilon^-1 of master 9
template<>
double qq2yyg6LC<9,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return -(s12*((s12+s13)*(s12+s13))*pow(s14,-3)*pow(s12+s13+s14,-1))/2.-(s12*(s14*s14*(2*(s13*s13)+s23*s23)+s12*s12*((s13+s14+s23)*(s13+s14+s23))+2*(s13+s14+s23)*pow(s12,3)+pow(s12,4))*pow(s14,-3)*pow(s12+s13+s14,-1)*pow(s12+s23,-1)*pow(s24,-1))/2.+(s12*(s12*s12*(s14*s14)+2*s13*s14*(s23*s23)+s14*s14*(s23*s23)+s13*s13*(2*(s14*s14)+s23*s23))*pow(s14,-3)*pow(s12+s13+s14,-1)*pow(s12+s23,-1)*pow(s12+s23+s24,-1))/2.;
}

// Coefficient of master 9 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6LC<9>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6LC<9,-1>(s12,s13,s14,s23,s24)
    });
}


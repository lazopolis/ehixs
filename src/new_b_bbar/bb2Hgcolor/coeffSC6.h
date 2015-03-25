/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 6: bubble(s12+s14+s24)

// Coefficient order epsilon^-1 of master 6
template<>
double bb2HgSC<6,-1>(const double& s12, const double& s14, const double& s24)
{
    return -4*(2*s12*s14+2*s12*s24+2*s14*s24+2*(s12*s12)+s14*s14+s24*s24)*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^0 of master 6
template<>
double bb2HgSC<6,0>(const double& s12, const double& s14, const double& s24)
{
    return 4*(4*s12*s14+4*s12*s24+6*s14*s24+4*(s12*s12)+3*(s14*s14)+3*(s24*s24))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^1 of master 6
template<>
double bb2HgSC<6,1>(const double& s12, const double& s14, const double& s24)
{
    return -2*pow(s14,-1)*pow(s12+s14,-1)*pow(s24,-1)*(13*s12*s24*(s14*s14)+13*s12*s14*(s24*s24)+6*(s12*s12)*(2*s14*s24+s14*s14+s24*s24)+4*s14*s24*((s14+s24)*(s14+s24))+4*(s14+s24)*pow(s12,3)+2*pow(s12,4)+4*s12*pow(s14,3)+4*s12*pow(s24,3))*pow(s12+s24,-1);
}

// Coefficient of master 6 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> bb2HgSC<6>(const double& s12, const double& s14, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        bb2HgSC<6,-1>(s12,s14,s24),
        bb2HgSC<6,0>(s12,s14,s24),
        bb2HgSC<6,1>(s12,s14,s24)
    });
}


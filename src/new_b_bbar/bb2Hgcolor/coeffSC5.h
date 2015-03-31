/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 5: bubble(s24)

// Coefficient order epsilon^-1 of master 5
template<>
double bb2HgSC<5,-1>(const double& s12, const double& s14, const double& s24)
{
    return 2*(2*s12*s14+2*s12*s24+2*s14*s24+2*(s12*s12)+s14*s14+s24*s24)*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^0 of master 5
template<>
double bb2HgSC<5,0>(const double& s12, const double& s14, const double& s24)
{
    return -2*(4*s12*s14+4*s12*s24+6*s14*s24+4*(s12*s12)+3*(s14*s14)+3*(s24*s24))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^1 of master 5
template<>
double bb2HgSC<5,1>(const double& s12, const double& s14, const double& s24)
{
    return pow(s14,-1)*(3*s12*s14*s24-s14*(s12*s12)+2*s12*(s14*s14)+5*s24*(s14*s14)+2*s12*(s24*s24)+2*s14*(s24*s24)+3*pow(s14,3))*pow(s12+s14,-1)*pow(s24,-1);
}

// Coefficient of master 5 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> bb2HgSC<5>(const double& s12, const double& s14, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        bb2HgSC<5,-1>(s12,s14,s24),
        bb2HgSC<5,0>(s12,s14,s24),
        bb2HgSC<5,1>(s12,s14,s24)
    });
}

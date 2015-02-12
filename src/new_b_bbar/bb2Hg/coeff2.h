/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 2: bubble(s14)

// Coefficient order epsilon^-1 of master 2
template<>
double bb2Hgbis<2,-1>(const double& s12, const double& s14, const double& s24)
{
    return 2*(QCD::CA-2*QCD::CF)*(2*s12*(s14+s24)+2*(s12*s12)+(s14+s24)*(s14+s24))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^0 of master 2
template<>
double bb2Hgbis<2,0>(const double& s12, const double& s14, const double& s24)
{
    return -2*(QCD::CA*(5*s14*s24+4*s12*(s14+s24)+4*(s12*s12)+3*(s14*s14)+3*(s24*s24))-2*QCD::CF*(4*s12*(s14+s24)+4*(s12*s12)+3*((s14+s24)*(s14+s24))))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^1 of master 2
template<>
double bb2Hgbis<2,1>(const double& s12, const double& s14, const double& s24)
{
    return 2*((s12+s24)*QCD::CA*(s24*(-s12+s24)+s14*s14)+QCD::CF*(s24*(s12*s12)-s12*(3*s14*s24+2*(s14*s14)+2*(s24*s24))-s24*(5*s14*s24+2*(s14*s14)+3*(s24*s24))))*pow(s14,-1)*pow(s24,-1)*pow(s12+s24,-1);
}

// Coefficient order epsilon^2 of master 2
template<>
double bb2Hgbis<2,2>(const double& s12, const double& s14, const double& s24)
{
    return 2*(s14+s24)*((s12+s24)*(2*s14+s24)*QCD::CA-(s24*(2*s14+s24)+s12*(4*s14+s24))*QCD::CF)*pow(s14,-1)*pow(s24,-1)*pow(s12+s24,-1);
}

// Coefficient of master 2 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> bb2Hgbis<2>(const double& s12, const double& s14, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        bb2Hgbis<2,-1>(s12,s14,s24),
        bb2Hgbis<2,0>(s12,s14,s24),
        bb2Hgbis<2,1>(s12,s14,s24),
        bb2Hgbis<2,2>(s12,s14,s24)
    });
}


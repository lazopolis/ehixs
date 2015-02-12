/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 1: bubble(s12)

// Coefficient order epsilon^-1 of master 1
template<>
double bb2Hgbis<1,-1>(const double& s12, const double& s14, const double& s24)
{
    return 2*(QCD::CA-2*QCD::CF)*(2*s12*(s14+s24)+2*(s12*s12)+(s14+s24)*(s14+s24))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^0 of master 1
template<>
double bb2Hgbis<1,0>(const double& s12, const double& s14, const double& s24)
{
    return -2*(QCD::CA-2*QCD::CF)*(4*s12*(s14+s24)+4*(s12*s12)+3*((s14+s24)*(s14+s24)))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^1 of master 1
template<>
double bb2Hgbis<1,1>(const double& s12, const double& s14, const double& s24)
{
    return 2*(QCD::CA-2*QCD::CF)*((s14+s24)*(s14+s24))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^2 of master 1
template<>
double bb2Hgbis<1,2>(const double& s12, const double& s14, const double& s24)
{
    return 4*(QCD::CA-2*QCD::CF)*((s14+s24)*(s14+s24))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient of master 1 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> bb2Hgbis<1>(const double& s12, const double& s14, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        bb2Hgbis<1,-1>(s12,s14,s24),
        bb2Hgbis<1,0>(s12,s14,s24),
        bb2Hgbis<1,1>(s12,s14,s24),
        bb2Hgbis<1,2>(s12,s14,s24)
    });
}


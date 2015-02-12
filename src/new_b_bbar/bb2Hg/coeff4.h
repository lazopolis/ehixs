/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 4: bubble(s12+s14+s24)

// Coefficient order epsilon^-1 of master 4
template<>
double bb2Hgbis<4,-1>(const double& s12, const double& s14, const double& s24)
{
    return -2*(QCD::CA-4*QCD::CF)*(2*s12*(s14+s24)+2*(s12*s12)+(s14+s24)*(s14+s24))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^0 of master 4
template<>
double bb2Hgbis<4,0>(const double& s12, const double& s14, const double& s24)
{
    return (2*QCD::CA*(5*s14*s24+4*s12*(s14+s24)+4*(s12*s12)+3*(s14*s14)+3*(s24*s24))-8*QCD::CF*(4*s12*(s14+s24)+4*(s12*s12)+3*((s14+s24)*(s14+s24))))*pow(s14,-1)*pow(s24,-1);
}

// Coefficient order epsilon^1 of master 4
template<>
double bb2Hgbis<4,1>(const double& s12, const double& s14, const double& s24)
{
    return -2*pow(s14,-1)*pow(s12+s14,-1)*pow(s24,-1)*((s12+s14)*(s12+s24)*QCD::CA*(s14*s14+s24*s24)-2*QCD::CF*(4*s14*s24*((s14+s24)*(s14+s24))+6*(s12*s12)*((s14+s24)*(s14+s24))+4*(s14+s24)*pow(s12,3)+2*pow(s12,4)+s12*(13*s24*(s14*s14)+13*s14*(s24*s24)+4*pow(s14,3)+4*pow(s24,3))))*pow(s12+s24,-1);
}

// Coefficient order epsilon^2 of master 4
template<>
double bb2Hgbis<4,2>(const double& s12, const double& s14, const double& s24)
{
    return -4*(s14+s24)*((s12+s14)*(s12+s24)*(s14+s24)*QCD::CA-s12*QCD::CF*(s12*(s14+s24)+s14*s14+s24*s24))*pow(s14,-1)*pow(s12+s14,-1)*pow(s24,-1)*pow(s12+s24,-1);
}

// Coefficient of master 4 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> bb2Hgbis<4>(const double& s12, const double& s14, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        bb2Hgbis<4,-1>(s12,s14,s24),
        bb2Hgbis<4,0>(s12,s14,s24),
        bb2Hgbis<4,1>(s12,s14,s24),
        bb2Hgbis<4,2>(s12,s14,s24)
    });
}


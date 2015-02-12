/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 7: box(s14,s24,s12+s14+s24)

// Coefficient order epsilon^0 of master 7
template<>
double bb2Hgbis<7,0>(const double& s12, const double& s14, const double& s24)
{
    return QCD::CA*(2*s12*(s14+s24)+2*(s12*s12)+(s14+s24)*(s14+s24));
}

// Coefficient order epsilon^1 of master 7
template<>
double bb2Hgbis<7,1>(const double& s12, const double& s14, const double& s24)
{
    return -(QCD::CA*(3*s14*s24+s14*s14+s24*s24));
}

// Coefficient order epsilon^2 of master 7
template<>
double bb2Hgbis<7,2>(const double& s12, const double& s14, const double& s24)
{
    return 0;
}

// Coefficient of master 7 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> bb2Hgbis<7>(const double& s12, const double& s14, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        bb2Hgbis<7,0>(s12,s14,s24),
        bb2Hgbis<7,1>(s12,s14,s24),
        bb2Hgbis<7,2>(s12,s14,s24)
    });
}


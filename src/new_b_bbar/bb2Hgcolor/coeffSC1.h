/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 1: box(s12,s14,s12+s14+s24)

// Coefficient order epsilon^0 of master 1
template<>
double bb2HgSC<1,0>(const double& s12, const double& s14, const double& s24)
{
    return s12*(-2*s12*(s14+s24)-2*(s12*s12)-(s14+s24)*(s14+s24))*pow(s24,-1);
}

// Coefficient order epsilon^1 of master 1
template<>
double bb2HgSC<1,1>(const double& s12, const double& s14, const double& s24)
{
    return s12*((s14+s24)*(s14+s24))*pow(s24,-1);
}

// Coefficient order epsilon^2 of master 1
template<>
double bb2HgSC<1,2>(const double& s12, const double& s14, const double& s24)
{
    return s12*s14*(s14+s24)*pow(s24,-1);
}

// Coefficient of master 1 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> bb2HgSC<1>(const double& s12, const double& s14, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        bb2HgSC<1,0>(s12,s14,s24),
        bb2HgSC<1,1>(s12,s14,s24),
        bb2HgSC<1,2>(s12,s14,s24)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 2: box(s12,s24,s12+s14+s24)

// Coefficient order epsilon^0 of master 2
template<>
double bb2HgSC<2,0>(const double& s12, const double& s14, const double& s24)
{
    return s12*(-2*s12*(s14+s24)-2*(s12*s12)-(s14+s24)*(s14+s24))*pow(s14,-1);
}

// Coefficient order epsilon^1 of master 2
template<>
double bb2HgSC<2,1>(const double& s12, const double& s14, const double& s24)
{
    return s12*((s14+s24)*(s14+s24))*pow(s14,-1);
}

// Coefficient order epsilon^2 of master 2
template<>
double bb2HgSC<2,2>(const double& s12, const double& s14, const double& s24)
{
    return s12*s24*(s14+s24)*pow(s14,-1);
}

// Coefficient of master 2 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> bb2HgSC<2>(const double& s12, const double& s14, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        bb2HgSC<2,0>(s12,s14,s24),
        bb2HgSC<2,1>(s12,s14,s24),
        bb2HgSC<2,2>(s12,s14,s24)
    });
}


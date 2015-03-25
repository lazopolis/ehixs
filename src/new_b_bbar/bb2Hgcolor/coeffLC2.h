/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 2: bubble(s14)

// Coefficient order epsilon^0 of master 2
template<>
double bb2HgLC<2,0>(const double& s12, const double& s14, const double& s24)
{
    return 2;
}

// Coefficient order epsilon^1 of master 2
template<>
double bb2HgLC<2,1>(const double& s12, const double& s14, const double& s24)
{
    return (-3*s12*s14-2*s12*s24-5*s14*s24-s12*s12-s24*s24)*pow(s14,-1)*pow(s12+s24,-1);
}

// Coefficient of master 2 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> bb2HgLC<2>(const double& s12, const double& s14, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        bb2HgLC<2,0>(s12,s14,s24),
        bb2HgLC<2,1>(s12,s14,s24)
    });
}


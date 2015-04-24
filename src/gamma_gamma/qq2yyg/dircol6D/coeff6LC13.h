/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 13: box6(s23,-s12-s23-s24,s14)

// Coefficient order epsilon^-1 of master 13
template<>
double qq2yyg6LC<13,-1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (5*pow(s12+s13+s14,-1))/2.-(9*s12*pow(s13,-1)*pow(s12+s13+s14,-1))/2.-s14*pow(s13,-1)*pow(s12+s13+s14,-1)-((s12-3*s13)*pow(s14,-1)*pow(s12+s13+s14,-1))/2.-2*(s12*s12)*pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)+(9*s12*pow(s12+s13+s14,-1)*pow(s23,-1))/2.-(5*s12-2*s13+s14+2*s23)*pow(s12+s13+s14,-1)*pow(s23,-1)+s12*(5*s12+s13+2*s23)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)-(15*(s12*s12)*pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s23,-1))/2.-(s13*(s13+4*s14+2*s23)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1))/2.-s12*s12*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1)+((-s12+s13)*(s12+s13+2*s14)*s24*pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1))/2.+((-2*s12+s13+4*s14+2*s23)*(s12*s12)*pow(s13,-1)*pow(s14,-1)*pow(s12+s13+s14,-1)*pow(s23,-1))/2.-(pow(s13,-1)*pow(s14,-1)*pow(s23,-1)*(s14*s23*(2*s13*s14+s13*s13+s14*s14+s23*s23)+s12*s12*(s13*(6*s14+4*s23)+3*(s13*s13)+3*((s14+s23)*(s14+s23)))+4*(s13+s14+s23)*pow(s12,3)+2*pow(s12,4)+s12*((3*s14+s23)*(s13*s13)+s13*(4*s14*s23+3*(s14*s14)+s23*s23)+pow(s13,3)+pow(s14+s23,3)))*pow(s12+s13+s14+s23,-1)*pow(s24,-1))/2.+(pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s14-s23,-1)*pow(s23,-1)*pow(s13*s14+(s12+s14)*s23,3)*pow(s12+s14+s24,-3))/2.+(pow(s13,-1)*((s12+s14)*(s12+s14)*(s23*s23)*(s12*(3*s14-2*s23)-3*s14*s23+2*(s14*s14)+2*(s23*s23))+s13*s14*s23*(3*s14*(s12*s12)+s14*(-8*s14*s23+4*(s14*s14)+7*(s23*s23))+s12*(-8*s14*s23+7*(s14*s14)+7*(s23*s23)))+(-2*s14+3*s23)*(s14*s14)*pow(s13,3)+s14*(s13*s13)*(-7*s23*(s14*s14)+3*s12*(s23*s23)+8*s14*(s23*s23)+2*pow(s14,3)))*pow(s12+s13+s14,-1)*pow(s14-s23,-2)*pow(s23,-1)*pow(s12+s14+s24,-2))/2.+(pow(s13,-1)*pow(s12+s13+s14,-1)*pow(s14-s23,-3)*pow(s23,-1)*(s14*(-3*s14*s23+s14*s14+3*(s23*s23))*pow(s13,3)+s14*(s13*s13)*(12*s23*(s14*s14)-12*s14*(s23*s23)+(3*s12+7*s23)*(s23*s23)-4*pow(s14,3))+s13*s14*(s14*s23*(6*s12*s23+3*(s12*s12)-13*(s23*s23))+19*(s14*s14)*(s23*s23)-10*s23*pow(s14,3)+2*pow(s14,4)+5*pow(s23,4))+s23*((-3*s14*s23+3*(s14*s14)+s23*s23)*pow(s12,3)+s12*s12*(-12*s23*(s14*s14)+12*s14*(s23*s23)+7*pow(s14,3)-4*pow(s23,3))+s12*(19*(s14*s14)*(s23*s23)-15*s23*pow(s14,3)+6*pow(s14,4)-8*s14*pow(s23,3)+pow(s23,4))+s14*(8*(s14*s14)*(s23*s23)-6*s23*pow(s14,3)+2*pow(s14,4)-4*s14*pow(s23,3)+pow(s23,4))))*pow(s12+s14+s24,-1))/2.-(pow(s13,-1)*(3*(s13+s14)*s23*(s12*s12)*(s14*s14)+pow(s12,3)*pow(s14,3)+(s13+s14)*s23*(2*s13*s14*(s23*s23)+s13*s13*(s23*s23)+2*(s14*s14)*(s23*s23)-2*s23*pow(s14,3)+pow(s14,4))+s12*s14*(6*s13*s14*(s23*s23)+3*(s13*s13)*(s23*s23)+4*(s14*s14)*(s23*s23)-2*s23*pow(s14,3)+pow(s14,4)))*pow(s12+s13+s14,-1)*pow(s14-s23,-3)*pow(s23,-1)*pow(s12+s23+s24,-1))/2.+((s12*s13+(s13+s14)*(s13+s23))*(2*s12*(s13+s14)+s12*s12+2*((s13+s14)*(s13+s14)))*pow(s13,-1)*pow(s14,-1)*pow(s23,-1)*pow(s12+s13+s14+s23,-1)*pow(s12+s13+s14+s23+s24,-1))/2.;
}

// Coefficient of master 13 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6LC<13>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6LC<13,-1>(s12,s13,s14,s23,s24)
    });
}


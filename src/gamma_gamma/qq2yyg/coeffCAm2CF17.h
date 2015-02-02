/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 17: box(s13,(-s12-s13-s14-s23-s24),(-s12-s23-s24))

// Coefficient order epsilon^0 of master 17
template<>
double qq2yygCAm2CF<17,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (2*s12*pow(s14,-2)*(-(s13*s14*(s23*s24+s14*(s23+s24)))+s24*(-s14+s24)*(s13*s13)+s12*(-((s14-2*s24)*(s13*s13))+s13*(s14*s24+2*s24*(s23+s24)-4*(s14*s14))+s14*(-(s14*s24)+s23*(s23+s24)-3*(s14*s14)))+s12*s12*(s14*(3*s23+2*s24)+s13*(s14+2*s23+4*s24)+s13*s13+(s23+s24)*(s23+s24))+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4)-s23*pow(s14,3))*pow(s12+s13+s14,-1)*pow(s23,-1)-(s23*(s23+s24)*(s14*s14)+s13*s14*(s24*s24)+s13*s13*(s24*s24)+s12*(s13*(2*s24*(s23+s24)+s14*(2*s23+3*s24))+2*s24*(s13*s13)+s14*(3*s23*s24+s14*(4*s23+s24)+2*(s23*s23)+s24*s24))+s12*s12*(2*s13*(s14+s23+2*s24)+s14*(4*s23+3*s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24))+2*(s13+s14+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s14,-2)*pow(s24,-1)+(-(s14*(2*s14-s23)*(s14+s23+s24))+2*(s13+s23+s24)*(s12*s12)+(-2*s14+s24)*(s13*s13)+s13*(-(s14*(s23+s24))+s24*(s23+s24)-4*(s14*s14))+s12*(s14*s23+s13*(2*s23+3*s24)+s13*s13-s14*s14+(s23+s24)*(s23+s24))+pow(s12,3))*pow(s14,-1)*pow(s12+s23+s24,-1)-s12*(-(s14*s23)+s13*s24+s12*(s13-s14+s23+s24)+s12*s12)*pow((-s12-s23-s24),-1)*pow((s12+s13+s23),-1)-2*s12*(s13+s14)*(s12+s23)*(-(s14*s23)+s13*s24+s12*(s13-s14+s23+s24)+s12*s12)*pow(s12+s13+s14,-1)*pow(s23,-1)*pow((-s12-s23-s24),-1)*pow((s12+s13+s23),-1)-s23*(s12+s23+s24)*(-(s14*s23)+s13*s24+s12*(s13-s14+s23+s24)+s12*s12)*pow(s24,-1)*pow((-s12-s23-s24),-1)*pow((s12+s13+s23),-1))/4.;
}

// Coefficient order epsilon^1 of master 17
template<>
double qq2yygCAm2CF<17,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (-(pow(s14,-2)*(s14*(s24*(s23+3*s24)+s14*(6*s23+7*s24))*(s13*s13)+s13*(s14*s14)*(s14*(5*s23-s24)+7*s23*s24-2*(s14*s14)+6*(s23*s23)+2*(s24*s24))+(2*s13*(4*s14+s23+s24)+s14*(5*s14+2*s23+4*s24)+2*(s13*s13))*pow(s12,3)+(s13+2*s14)*pow(s12,4)+(4*s14*s24+2*(s14*s14)+s24*s24)*pow(s13,3)+s12*s12*(2*(5*s14+s23+2*s24)*(s13*s13)+s14*(2*s24*(s23+s24)+s14*(5*s23+7*s24)+4*(s14*s14))+s13*(s14*(9*s23+13*s24)+13*(s14*s14)+(s23+s24)*(s23+s24))+pow(s13,3))+s12*(s14*s14*(2*s24*(2*s23+s24)+s14*(6*s23+5*s24)+5*(s14*s14))+s13*s13*(2*s24*(s23+s24)+s14*(7*s23+13*s24)+15*(s14*s14))+s13*s14*(10*s23*s24+s14*(16*s23+15*s24)+12*(s14*s14)+3*(s23*s23)+5*(s24*s24))+2*(2*s14+s24)*pow(s13,3))+s23*(3*s14+2*(s23+s24))*pow(s14,3))*pow(s12+s13+s14,-1)*pow(s23,-1))+((s13-3*s14+s23+s24)*(s12*s12)+s24*(s13*s13)+s13*(s14*(s23+s24)+s24*(3*s23+s24)+2*(s14*s14))+s14*(s14*(3*s23+2*s24)+s23*(s23+3*s24)+2*(s14*s14))+s12*(2*s14*(s23-s24)+2*s13*(s23+s24)+s13*s13+s14*s14+(s23+s24)*(s23+s24)))*pow(s14,-1)*pow(s24,-1)-(s13*s14*(3*s23+5*s24)+(s13-3*s14)*(s12*s12)+(2*s14+s24)*(s13*s13)+s12*(s13*(4*s14+s23+s24)-s14*(5*s14+s23+3*s24)+s13*s13)+s14*(2*s23*(s23+s24)-s14*(3*s23+2*s24)-2*(s14*s14)))*pow(s14,-1)*pow(s12+s23+s24,-1)+(-(s14*s23)+s13*s24+s12*(s13-s14+s23+s24)+s12*s12)*(s12*(-2*s13+s14-2*s23+s24)-(s13+s23)*(s13+s14+s23+s24)+s12*s12)*pow(s12,-1)*pow((-s12-s23-s24),-1)*pow((s12+s13+s23),-1)-(-(s14*s23)+s13*s24+s12*(s13-s14+s23+s24)+s12*s12)*pow(s12,-1)*(2*s13*(s13+s23)*(s23+s24)*(s13+s14+s23+s24)+s12*s12*(-2*s14*s23-s23*(2*s23+s24)+s13*(5*s14+4*(s23+s24))+6*(s13*s13)+s14*s14)+(s13-s14-s23)*pow(s12,3)+s12*((4*s14+9*s23+6*s24)*(s13*s13)-s23*(s14*s23+s23*(s23+s24)-s14*s14)+s13*(8*s23*s24+2*s14*(3*s23+s24)+s14*s14+5*(s23*s23)+2*(s24*s24))+3*pow(s13,3)))*pow(s12+s13+s14,-1)*pow(s23,-1)*pow((-s12-s23-s24),-1)*pow((s12+s13+s23),-1)-(-(s14*s23)+s13*s24+s12*(s13-s14+s23+s24)+s12*s12)*((s23+s24)*(2*s14+s23+2*s24)+s12*(3*s13+2*s14+4*s23+5*s24)+s13*(s14+3*(s23+s24))+3*(s12*s12)+s13*s13)*pow(s24,-1)*pow((-s12-s23-s24),-1)*pow((s12+s13+s23),-1))/4.;
}

// Coefficient order epsilon^2 of master 17
template<>
double qq2yygCAm2CF<17,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s14,-2)*(s14*(s13*s13)*(-7*s14*(4*s23+s24)+10*(s14*s14)-5*(s24*s24))+s13*(s14*s14)*(-17*s23*s24+s14*(-9*s23+13*s24)+18*(s14*s14)-18*(s23*s23)-3*(s24*s24))+(s13*(-5*s14+4*s24)-2*(s13*s13)+2*(s14*(3*s23+s24)+4*(s14*s14)+(s23+s24)*(s23+s24)))*pow(s12,3)+(2*s13+3*s14+4*(s23+s24))*pow(s12,4)+2*pow(s12,5)-2*(4*(s14*s14)+s24*s24)*pow(s13,3)-s12*s12*(4*(2*s14+s23+s24)*(s13*s13)+s13*(11*s14*(s23+s24)+8*(s14*s14)+2*(s23*s23-s24*s24))-s14*(2*s23*s24+s14*(13*s23+15*s24)+4*(s14*s14)+3*(s23*s23)-s24*s24)+2*pow(s13,3))-s12*(s13*s13*(2*s24*(2*s23+s24)+s14*(6*s23+13*s24)+11*(s14*s14))+s13*s14*(16*s23*s24+s14*(29*s23+8*s24)-6*(s14*s14)+6*(s23*s23)+6*(s24*s24))-s14*s14*(-2*s14*(s23-4*s24)+12*s23*s24+3*(s14*s14)+5*(s23*s23)+9*(s24*s24))+4*s24*pow(s13,3))-s23*(s14+4*s23+3*s24)*pow(s14,3))*pow(s12+s13+s14,-1)*pow(s23,-1)-(-2*(s23+s24)*(4*s14+3*s23+4*s24)*(s14*s14)-s13*s14*(12*s14*(s23+s24)+s24*(3*s23+2*s24)+6*(s14*s14))+s13*s13*(-6*(s14*s14)+s24*s24)+s12*(2*s24*(s13*s13)+s13*(s14*(s23-2*s24)+2*s24*(s23+s24)-12*(s14*s14))-s14*(s23*s24+5*s14*(3*s23+4*s24)+10*(s14*s14)-s23*s23+2*(s24*s24)))+s12*s12*(s14*(s23-2*s24)+2*s13*(s23+2*s24)+s13*s13-11*(s14*s14)+(s23+s24)*(s23+s24))+2*(s13+s23+s24)*pow(s12,3)+pow(s12,4))*pow(s14,-2)*pow(s24,-1)-((s13+13*s14+2*(s23+s24))*(s12*s12)+6*s14*(s13*s13)+s13*(14*s14*(s23+s24)+s24*(s23+s24)+8*(s14*s14))+s14*(17*s23*s24+12*s14*(s23+s24)+2*(s14*s14)+7*(s23*s23)+10*(s24*s24))+s12*(s13*(16*s14+s23+2*s24)+s14*(20*s23+23*s24)+16*(s14*s14)+(s23+s24)*(s23+s24))+pow(s12,3))*pow(s14,-1)*pow(s12+s23+s24,-1)-(-(s14*s23)+s13*s24+s12*(s13-s14+s23+s24)+s12*s12)*(3*(s13+s23)*(s13+s14+s23+s24)+s12*(6*s13+5*s14+6*s23+5*s24)+4*(s12*s12))*pow(s12,-1)*pow((-s12-s23-s24),-1)*pow((s12+s13+s23),-1)+(-(s14*s23)+s13*s24+s12*(s13-s14+s23+s24)+s12*s12)*pow(s12,-1)*(-7*s13*(s13+s23)*(s23+s24)*(s13+s14+s23+s24)+s12*s12*(22*s23*s24+s14*(9*s23+6*s24)-s13*(7*s14+12*s23+8*s24)-16*(s13*s13)+2*(s14*s14)+14*(s23*s23)+6*(s24*s24))+(-2*s13+5*s14+16*s23+12*s24)*pow(s12,3)+6*pow(s12,4)-s12*((9*s14+32*s23+21*s24)*(s13*s13)-2*s23*(5*s23*s24+s14*(2*s23+3*s24)+s14*s14+2*(s23*s23)+3*(s24*s24))+s13*(22*s23*s24+s14*(16*s23+7*s24)-2*(s14*s14)+17*(s23*s23)+7*(s24*s24))+11*pow(s13,3)))*pow(s12+s13+s14,-1)*pow(s23,-1)*pow((-s12-s23-s24),-1)*pow((s12+s13+s23),-1)+(-(s14*s23)+s13*s24+s12*(s13-s14+s23+s24)+s12*s12)*pow(s12,-1)*(3*(s13+s23)*(s23+s24)*(s13+s14+s23+s24)+(5*s13+4*s14+10*s23+9*s24)*(s12*s12)+s12*(14*s23*s24+s14*(7*s23+5*s24)+s13*(3*s14+11*s23+9*s24)+3*(s13*s13)+9*(s23*s23)+5*(s24*s24))+4*pow(s12,3))*pow(s24,-1)*pow((-s12-s23-s24),-1)*pow((s12+s13+s23),-1))/4.;
}

// Coefficient of master 17 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygCAm2CF<17>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygCAm2CF<17,0>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<17,1>(s12,s13,s14,s23,s24),
        qq2yygCAm2CF<17,2>(s12,s13,s14,s23,s24)
    });
}


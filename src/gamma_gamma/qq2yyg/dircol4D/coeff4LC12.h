/*This file was produced by Simone Lionetti using a Mathematica script*/

// Master n. 12: box(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24)

// Coefficient order epsilon^0 of master 12
template<>
double qq2yyg4LC<12,0>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s13,-1)*pow(s14,-1)*pow(s23,-1)*pow(s24,-1)*(s23*(s14*s14)*(-2*s23*s24*(s23+s24)-s14*s23*(s23+3*s24)+2*s24*(s14*s14))-2*(-(s13*s23*(s23+2*s24))+s14*(s14*(s23-s24)+s24*(2*s23+s24))+(s23-s24)*(s13*s13))*pow(s12,3)-(2*s14-s24)*s24*(3*s23+s24)*pow(s13,3)+s12*s12*((-2*s14*s23+s24*(7*s23+3*s24))*(s13*s13)+s13*(2*s24*(s14*s14)+3*s14*(s23*s23-s24*s24)+2*s23*(4*s23*s24+s23*s23+3*(s24*s24)))-s14*(s14*s23*(3*s23+7*s24)+2*s23*(s14*s14)+2*s24*(4*s23*s24+3*(s23*s23)+3*(s24*s24)))+(-4*s23+2*s24)*pow(s13,3))-2*s23*s24*pow(s13,4)+s13*s14*(2*s23*(s23+3*s24)*(s14*s14)-2*s24*pow(s23,3)-s14*(3*s24*(s23*s23)-6*s23*(s24*s24)+pow(s23,3)-2*pow(s24,3))+2*s23*pow(s24,3))+s13*s13*(2*(s14*s14)*(s23*s23-s24*s24)+2*s23*(s23+s24)*(s24*s24)+s14*(-6*s24*(s23*s23)+3*s23*(s24*s24)-2*pow(s23,3)+pow(s24,3)))-s12*(-(s13*s13*(2*(s23-s24)*(s14*s14)+s14*(-3*s23*s24+s23*s23-2*(s24*s24))+s24*(10*s23*s24+7*(s23*s23)+s24*s24)))+(s23*s24+2*s14*(s23+s24)+2*(s23*s23)-3*(s24*s24))*pow(s13,3)+2*s23*pow(s13,4)+s13*(s14*s14*(-3*s23*s24-2*(s23*s23)+s24*s24)-2*s23*s24*((s23+s24)*(s23+s24))-2*(s23-s24)*pow(s14,3)+s14*(3*s24*(s23*s23)-3*s23*(s24*s24)+pow(s23,3)+3*pow(s24,3)))+s14*(s14*s14*(3*s23*s24+3*(s23*s23)+2*(s24*s24))+2*s24*pow(s14,3)+2*s24*(2*s24*(s23*s23)+3*s23*(s24*s24)+pow(s23,3)+2*pow(s24,3))+s14*(10*s24*(s23*s23)+7*s23*(s24*s24)+pow(s23,3)+4*pow(s24,3)))))*pow(s12+s13+s14+s23+s24,-1))/8.;
}

// Coefficient order epsilon^1 of master 12
template<>
double qq2yyg4LC<12,1>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s12,-1)*pow(s13,-1)*pow(s14,-1)*pow(s23,-1)*pow(s24,-1)*((2*s23*(9*s23+10*s24)+s14*(39*s23+22*s24)+s13*(20*s14+3*(6*s23+s24))+18*(s14*s14))*pow(s12,5)+6*(s14+s23)*pow(s12,6)+pow(s12,4)*((22*s14+20*s23+3*s24)*(s13*s13)+(83*s23+48*s24)*(s14*s14)+s13*(58*s23*s24+s14*(97*s23+61*s24)+40*(s14*s14)+38*(s23*s23)+10*(s24*s24))+2*s23*(20*s23*s24+9*(s23*s23)+11*(s24*s24))+2*s14*(50*s23*s24+38*(s23*s23)+15*(s24*s24))+18*pow(s14,3))+pow(s12,3)*(s13*s13*(47*s23*s24+s14*(77*s23+50*s24)+22*(s14*s14)+25*(s23*s23)+11*(s24*s24))+s14*s14*(153*s23*s24+133*(s23*s23)+39*(s24*s24))+2*s23*(3*s23+4*s24)*((s23+s24)*(s23+s24))+(8*s14+10*s23+s24)*pow(s13,3)+(69*s23+38*s24)*pow(s14,3)+6*pow(s14,4)+s13*(3*(46*s23+27*s24)*(s14*s14)+79*s24*(s23*s23)+66*s23*(s24*s24)+s14*(192*s23*s24+131*(s23*s23)+69*(s24*s24))+20*pow(s14,3)+26*pow(s23,3)+11*pow(s24,3))+s14*(122*s24*(s23*s23)+79*s23*(s24*s24)+59*pow(s23,3)+22*pow(s24,3)))+2*s14*((s14+s23+s24)*(5*s23+6*s24)*(s14*s14)*(s23*s23)+s24*(-3*s23*s24-4*(s23*s23)+2*(s24*s24))*pow(s13,3)+s13*s14*s23*(s14*(4*s23*s24+9*(s23*s23)-8*(s24*s24))+2*(s24*(s23*s23)-5*s23*(s24*s24)+2*pow(s23,3)-4*pow(s24,3)))+s13*s13*(s24*(-7*s24*(s23*s23)-s23*(s24*s24)-4*pow(s23,3)+2*pow(s24,3))+s14*(-6*s24*(s23*s23)-11*s23*(s24*s24)+4*pow(s23,3)+2*pow(s24,3))))+s12*s12*((15*s23*s24+s14*(19*s23+15*s24)+5*(s23*s23)+2*(s24*s24))*pow(s13,3)+(2*s23+s24)*pow(s13,4)+s13*s13*((53*s23+33*s24)*(s14*s14)+37*s24*(s23*s23)+42*s23*(s24*s24)+5*s14*(19*s23*s24+12*(s23*s23)+9*(s24*s24))+9*pow(s23,3)+12*pow(s24,3))+s13*(44*(s23*s23)*(s24*s24)+s14*s14*(157*s23*s24+151*(s23*s23)+46*(s24*s24))+(55*s23+31*s24)*pow(s14,3)+28*s24*pow(s23,3)+6*pow(s23,4)+26*s23*pow(s24,3)+4*s14*(37*s24*(s23*s23)+26*s23*(s24*s24)+18*pow(s23,3)+9*pow(s24,3))+4*pow(s24,4))+s14*(42*(s23*s23)*(s24*s24)+s14*s14*(93*s23*s24+98*(s23*s23)+17*(s24*s24))+(19*s23+12*s24)*pow(s14,3)+44*s24*pow(s23,3)+16*pow(s23,4)+22*s23*pow(s24,3)+s14*(154*s24*(s23*s23)+69*s23*(s24*s24)+88*pow(s23,3)+13*pow(s24,3))+8*pow(s24,4)))+s12*(s23*(s14*s14)*((23*s23+16*s24)*(s14*s14)+4*s23*(11*s23*s24+5*(s23*s23)+6*(s24*s24))+3*s14*(28*s23*s24+19*(s23*s23)+7*(s24*s24)))+(s24*(4*s23*s24+3*(s23*s23)+s24*s24)+4*s14*(s23*s24+s23*s23+2*(s24*s24)))*pow(s13,3)+s24*(2*s23+s24)*pow(s13,4)+s13*s13*(s14*s14*(14*s23*s24+31*(s23*s23)+9*(s24*s24))+2*s24*(3*s23+2*s24)*((s23+s24)*(s23+s24))+s14*(18*s24*(s23*s23)+11*s23*(s24*s24)+19*pow(s23,3)+18*pow(s24,3)))+s13*s14*(2*(s14*s14)*(14*s23*s24+25*(s23*s23)+s24*s24)+s14*(75*s24*(s23*s23)-2*s23*(s24*s24)+70*pow(s23,3)+5*pow(s24,3))+2*(7*(s23*s23)*(s24*s24)+10*s24*pow(s23,3)+5*pow(s23,4)+6*s23*pow(s24,3)+4*pow(s24,4)))))*pow(s12+s13+s14+s23+s24,-1))/8.;
}

// Coefficient order epsilon^2 of master 12
template<>
double qq2yyg4LC<12,2>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return (pow(s12,-1)*pow(s13,-1)*pow(s14,-1)*pow(s23,-1)*pow(s24,-1)*((s13*(24*s14+23*s23-4*s24)+24*s23*(s23+s24)+5*s14*(12*s23+s24)+24*(s14*s14))*pow(s12,5)+8*(s14+s23)*pow(s12,6)+pow(s12,4)*((24*s14+24*s23-7*s24)*(s13*s13)+(119*s23+10*s24)*(s14*s14)+s13*(50*s23*s24+3*s14*(45*s23+s24)+48*(s14*s14)+56*(s23*s23)-12*(s24*s24))+2*s14*(44*s23*s24+62*(s23*s23)-11*(s24*s24))+24*s23*((s23+s24)*(s23+s24))+24*pow(s14,3))-4*s14*(-(s13*s14*(2*s23*(s23+s24)+s14*(4*s23+s24))*(s23*s23))-(s14+s23+s24)*(2*s23+s24)*(s14*s14)*(s23*s23)+s24*(2*s23*s24+2*(s23*s23)+s24*s24)*pow(s13,3)+s13*s13*(s14*(2*s24*(s23*s23)+2*s23*(s24*s24)-2*pow(s23,3)+pow(s24,3))+s24*(4*s24*(s23*s23)+3*s23*(s24*s24)+2*pow(s23,3)+pow(s24,3))))+s12*((s24*(4*s23*s24+9*(s23*s23)-3*(s24*s24))+16*s14*(s23*s23-s24*s24))*pow(s13,3)+(2*s23-s24)*s24*pow(s13,4)+s13*s14*(2*(s14*s14)*(31*(s23*s23)-7*(s24*s24))+2*(-3*s23*s24+8*(s23*s23)-5*(s24*s24))*((s23+s24)*(s23+s24))+s14*(71*s24*(s23*s23)-36*s23*(s24*s24)+92*pow(s23,3)-25*pow(s24,3)))+s23*(s14*s14)*((25*s23+4*s24)*(s14*s14)+48*s24*(s23*s23)+s14*(52*s23*s24+59*(s23*s23)-s24*s24)+14*s23*(s24*s24)+28*pow(s23,3)-6*pow(s24,3))+s13*s13*(s14*s14*(-6*s23*s24+53*(s23*s23)-29*(s24*s24))+s14*(20*s24*(s23*s23)-47*s23*(s24*s24)+33*pow(s23,3)-36*pow(s24,3))-2*s24*(-5*s24*(s23*s23)+s23*(s24*s24)-4*pow(s23,3)+2*pow(s24,3))))+s12*s12*((s14*(19*s23-7*s24)+14*s23*s24+11*(s23*s23)-7*(s24*s24))*pow(s13,3)+(2*s23-s24)*pow(s13,4)+s13*(3*(s14*s14)*(28*s23*s24+73*(s23*s23)-23*(s24*s24))+2*(5*s23*s24+4*(s23*s23)-2*(s24*s24))*((s23+s24)*(s23+s24))+(65*s23-9*s24)*pow(s14,3)+s14*(147*s24*(s23*s23)-23*s23*(s24*s24)+119*pow(s23,3)-55*pow(s24,3)))+s13*s13*((57*s23-11*s24)*(s14*s14)+48*s24*(s23*s23)+s14*(40*s23*s24+111*(s23*s23)-63*(s24*s24))+14*s23*(s24*s24)+17*pow(s23,3)-15*pow(s24,3))+s14*(s14*s14*(50*s23*s24+119*(s23*s23)-21*(s24*s24))+(25*s23-4*s24)*pow(s14,3)+s14*(142*s24*(s23*s23)-6*s23*(s24*s24)+127*pow(s23,3)-27*pow(s24,3))+2*(11*(s23*s23)*(s24*s24)+28*s24*pow(s23,3)+14*pow(s23,4)-8*s23*pow(s24,3)-5*pow(s24,4))))+pow(s12,3)*(s14*s14*(112*s23*s24+194*(s23*s23)-39*(s24*s24))+s13*s13*(s14*(94*s23-7*s24)+40*s23*s24+24*(s14*s14)+43*(s23*s23)-18*(s24*s24))+(8*s14+11*s23-4*s24)*pow(s13,3)+(92*s23+s24)*pow(s14,3)+8*pow(s14,4)+s14*(139*s24*(s23*s23)+14*s23*(s24*s24)+100*pow(s23,3)-29*pow(s24,3))+s13*((175*s23+2*s24)*(s14*s14)+82*s24*(s23*s23)+2*s14*(66*s23*s24+109*(s23*s23)-33*(s24*s24))+29*s23*(s24*s24)+24*pow(s14,3)+41*pow(s23,3)-12*pow(s24,3))+8*s23*pow(s23+s24,3)))*pow(s12+s13+s14+s23+s24,-1))/8.;
}

// Coefficient of master 12 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg4LC<12>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yyg4LC<12,0>(s12,s13,s14,s23,s24),
        qq2yyg4LC<12,1>(s12,s13,s14,s23,s24),
        qq2yyg4LC<12,2>(s12,s13,s14,s23,s24)
    });
}

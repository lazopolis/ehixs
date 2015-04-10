/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeff6ELC.h"

// Master n. 13: box6(s23,-s12-s23-s24,s14)

// Coefficient order epsilon^-1 of master 13
template<>
double qq2yyg6ELC<13,-1>(const double& is12, const double& is13, const double& is14, const double& is23, const double& is24)
{
    const cln::cl_RA s12 = cln::rational(is12);
    const cln::cl_RA s13 = cln::rational(is13);
    const cln::cl_RA s14 = cln::rational(is14);
    const cln::cl_RA s23 = cln::rational(is23);
    const cln::cl_RA s24 = cln::rational(is24);
    return cln::double_approx(
	-((-(s14*pow<3>(s13)*(3*s24*pow<2>(s14)*(2*s23*s24-pow<2>(s23)+3*pow<2>(s24))+s14*pow<2>(s24)*(5*s23*s24-3*pow<2>(s23)+12*pow<2>(s24))+(2*s23*s24-pow<2>(s23)+3*pow<2>(s24))*pow<3>(s14)+(5*s23*s24-pow<2>(s23)+6*pow<2>(s24))*pow<3>(s24)))-s24*(3*s24*(s23+s24)*pow<2>(s14)+s14*(3*s23+4*s24)*pow<2>(s24)+(s23+s24)*pow<3>(s14)+(s23+s24)*pow<3>(s24))*pow<4>(s13)+(63*pow<2>(s23)*pow<2>(s24)+3*pow<2>(s13)*(14*s23*s24+18*s14*(s23+2*s24)+21*pow<2>(s14)+2*pow<2>(s23)+15*pow<2>(s24))+3*pow<2>(s14)*(102*s23*s24+27*pow<2>(s23)+80*pow<2>(s24))+(16*s14+5*s23+14*s24)*pow<3>(s13)+2*(55*s23+81*s24)*pow<3>(s14)+16*s24*pow<3>(s23)+88*s23*pow<3>(s24)+s13*(3*(49*s23+81*s24)*pow<2>(s14)+48*s24*pow<2>(s23)+111*s23*pow<2>(s24)+3*s14*(83*s23*s24+18*pow<2>(s23)+72*pow<2>(s24))+88*pow<3>(s14)+5*pow<3>(s23)+68*pow<3>(s24))+s14*(147*s24*pow<2>(s23)+288*s23*pow<2>(s24)+20*pow<3>(s23)+162*pow<3>(s24))+pow<4>(s13)+40*pow<4>(s14)+pow<4>(s23)+40*pow<4>(s24))*pow<5>(s12)+pow<4>(s12)*(s24*(18*s23*s24+3*pow<2>(s23)+22*pow<2>(s24))*pow<2>(s23+s24)+(14*s23*s24+20*s14*(s23+2*s24)+24*pow<2>(s14)+pow<2>(s23)+16*pow<2>(s24))*pow<3>(s13)+(327*s23*s24+95*pow<2>(s23)+228*pow<2>(s24))*pow<3>(s14)+pow<2>(s13)*(3*(33*s23+53*s24)*pow<2>(s14)+18*s24*pow<2>(s23)+51*s23*pow<2>(s24)+3*s14*(47*s23*s24+8*pow<2>(s23)+38*pow<2>(s24))+61*pow<3>(s14)+pow<3>(s23)+29*pow<3>(s24))+pow<2>(s14)*(261*s24*pow<2>(s23)+456*s23*pow<2>(s24)+40*pow<3>(s23)+228*pow<3>(s24))+(3*s14+s23+3*s24)*pow<4>(s13)+2*(45*s23+59*s24)*pow<4>(s14)+s13*(75*pow<2>(s23)*pow<2>(s24)+pow<2>(s14)*(406*s23*s24+99*pow<2>(s23)+297*pow<2>(s24))+6*(28*s23+39*s24)*pow<3>(s14)+18*s24*pow<3>(s23)+102*s23*pow<3>(s24)+s14*(168*s24*pow<2>(s23)+333*s23*pow<2>(s24)+20*pow<3>(s23)+167*pow<3>(s24))+62*pow<4>(s14)+pow<4>(s23)+42*pow<4>(s24))+s14*(231*pow<2>(s23)*pow<2>(s24)+67*s24*pow<3>(s23)+289*s23*pow<3>(s24)+5*pow<4>(s23)+118*pow<4>(s24))+22*pow<5>(s14))+s23*pow<2>(s14)*(3*s14*(s23+2*s24)*pow<2>(s24)*pow<2>(s23+s24)+pow<3>(s14)*(3*s24*pow<2>(s23)+8*s23*pow<2>(s24)+pow<3>(s23)+8*pow<3>(s24))+s24*pow<2>(s14)*(9*s24*pow<2>(s23)+14*s23*pow<2>(s24)+3*pow<3>(s23)+8*pow<3>(s24))+2*pow<3>(s24)*pow<3>(s23+s24)+2*s24*(2*s23+3*s24)*pow<4>(s14)+(s23+2*s24)*pow<5>(s14))+s13*s14*(3*s14*(s23*s24+pow<2>(s23)-2*pow<2>(s24))*pow<2>(s24)*pow<2>(s23+s24)+(s23-2*s24)*pow<3>(s24)*pow<3>(s23+s24)+s24*(7*s23*s24+9*pow<2>(s23)-6*pow<2>(s24))*pow<4>(s14)+pow<3>(s14)*(9*pow<2>(s23)*pow<2>(s24)+2*s24*pow<3>(s23)+6*s23*pow<3>(s24)+pow<4>(s23)-8*pow<4>(s24))+s24*pow<2>(s14)*(9*pow<2>(s23)*pow<2>(s24)+6*s24*pow<3>(s23)-2*s23*pow<3>(s24)+3*pow<4>(s23)-8*pow<4>(s24))+(3*s23*s24+3*pow<2>(s23)-2*pow<2>(s24))*pow<5>(s14))-pow<2>(s13)*(3*s14*(s23+2*s24)*pow<2>(s23+s24)*pow<3>(s24)+3*pow<2>(s14)*pow<2>(s24)*(s24*pow<2>(s23)+4*s23*pow<2>(s24)+pow<3>(s23)+4*pow<3>(s24))+s24*pow<3>(s14)*(-6*s24*pow<2>(s23)+pow<3>(s23)+14*pow<3>(s24))+(-9*s24*pow<2>(s23)+11*pow<3>(s24))*pow<4>(s14)+pow<3>(s23+s24)*pow<4>(s24)+(-3*pow<2>(s23)+4*pow<2>(s24))*pow<5>(s14))+((33*s14+12*s23+29*s24)*pow<2>(s13)+(82*s23+135*s24)*pow<2>(s14)+33*s24*pow<2>(s23)+72*s23*pow<2>(s24)+s13*(62*s23*s24+7*s14*(10*s23+19*s24)+72*pow<2>(s14)+12*pow<2>(s23)+62*pow<2>(s24))+s14*(154*s23*s24+37*pow<2>(s23)+135*pow<2>(s24))+4*pow<3>(s13)+45*pow<3>(s14)+4*pow<3>(s23)+45*pow<3>(s24))*pow<6>(s12)+pow<3>(s12)*(pow<3>(s13)*(6*(5*s23+7*s24)*pow<2>(s14)+s24*(12*s23*s24+3*pow<2>(s23)+4*pow<2>(s24))+s14*(37*s23*s24+4*pow<2>(s23)+18*pow<2>(s24))+16*pow<3>(s14))+pow<3>(s14)*(235*s24*pow<2>(s23)+369*s23*pow<2>(s24)+40*pow<3>(s23)+164*pow<3>(s24))+(3*s23+7*s24)*pow<2>(s24)*pow<3>(s23+s24)+(s14+s23+s24)*(3*s14+2*s24)*pow<4>(s13)+(207*s23*s24+65*pow<2>(s23)+129*pow<2>(s24))*pow<4>(s14)+pow<2>(s13)*(pow<2>(s14)*(188*s23*s24+39*pow<2>(s23)+105*pow<2>(s24))+(93*s23+112*s24)*pow<3>(s14)+s24*(15*s24*pow<2>(s23)+18*s23*pow<2>(s24)+2*pow<3>(s23)+pow<3>(s24))+3*s14*(17*s24*pow<2>(s23)+34*s23*pow<2>(s24)+pow<3>(s23)+6*pow<3>(s24))+30*pow<4>(s14))+s14*s24*(184*pow<2>(s23)*pow<2>(s24)+81*s24*pow<3>(s23)+169*s23*pow<3>(s24)+12*pow<4>(s23)+54*pow<4>(s24))+pow<2>(s14)*(321*pow<2>(s23)*pow<2>(s24)+108*s24*pow<3>(s23)+357*s23*pow<3>(s24)+10*pow<4>(s23)+129*pow<4>(s24))+(46*s23+54*s24)*pow<5>(s14)+s13*(3*(115*s23*s24+32*pow<2>(s23)+67*pow<2>(s24))*pow<3>(s14)+pow<2>(s14)*(227*s24*pow<2>(s23)+383*s23*pow<2>(s24)+30*pow<3>(s23)+144*pow<3>(s24))+4*(28*s23+31*s24)*pow<4>(s14)+s24*(57*pow<2>(s23)*pow<2>(s24)+24*s24*pow<3>(s23)+50*s23*pow<3>(s24)+3*pow<4>(s23)+14*pow<4>(s24))+s14*(186*pow<2>(s23)*pow<2>(s24)+55*s24*pow<3>(s23)+197*s23*pow<3>(s24)+4*pow<4>(s23)+53*pow<4>(s24))+24*pow<5>(s14))+7*pow<6>(s14))+s12*((-3*s14*(s23+3*s24)*pow<2>(s24)-3*pow<2>(s14)*pow<2>(s24)+s23*pow<3>(s14)-(2*s23+3*s24)*pow<3>(s24))*pow<4>(s13)+pow<3>(s13)*(s14*(-13*s23*s24+6*pow<2>(s23)-26*pow<2>(s24))*pow<2>(s24)-s24*pow<2>(s14)*(s23*s24-9*pow<2>(s23)+30*pow<2>(s24))+4*(2*s23*s24+pow<2>(s23)-2*pow<2>(s24))*pow<3>(s14)+(-(s23*s24)+pow<2>(s23)-2*pow<2>(s24))*pow<3>(s24)+(5*s23+s24)*pow<4>(s14))+pow<2>(s13)*(pow<2>(s14)*(-19*s23*s24+14*pow<2>(s23)-48*pow<2>(s24))*pow<2>(s24)+pow<3>(s14)*(35*s24*pow<2>(s23)+29*s23*pow<2>(s24)+pow<3>(s23)-32*pow<3>(s24))-(2*s23+5*s24)*pow<2>(s23+s24)*pow<3>(s24)-3*s14*pow<2>(s24)*(7*s24*pow<2>(s23)+16*s23*pow<2>(s24)+pow<3>(s23)+10*pow<3>(s24))+(30*s23*s24+15*pow<2>(s23)-7*pow<2>(s24))*pow<4>(s14)+(9*s23+2*s24)*pow<5>(s14))+s14*(s14*pow<2>(s24)*(23*s23*s24+9*pow<2>(s23)+8*pow<2>(s24))*pow<2>(s23+s24)+(3*s23+2*s24)*pow<3>(s24)*pow<3>(s23+s24)+(30*s24*pow<2>(s23)+45*s23*pow<2>(s24)+4*pow<3>(s23)+14*pow<3>(s24))*pow<4>(s14)+s24*pow<2>(s14)*(89*pow<2>(s23)*pow<2>(s24)+51*s24*pow<3>(s23)+64*s23*pow<3>(s24)+12*pow<4>(s23)+14*pow<4>(s24))+pow<3>(s14)*(64*pow<2>(s23)*pow<2>(s24)+28*s24*pow<3>(s23)+62*s23*pow<3>(s24)+5*pow<4>(s23)+16*pow<4>(s24))+(19*s23*s24+7*pow<2>(s23)+8*pow<2>(s24))*pow<5>(s14)+2*(s23+s24)*pow<6>(s14))+s13*(3*s14*(2*s23*s24+2*pow<2>(s23)-3*pow<2>(s24))*pow<2>(s24)*pow<2>(s23+s24)+s23*pow<3>(s24)*pow<3>(s23+s24)+(53*s24*pow<2>(s23)+60*s23*pow<2>(s24)+5*pow<3>(s23)-6*pow<3>(s24))*pow<4>(s14)+pow<3>(s14)*(59*pow<2>(s23)*pow<2>(s24)+23*s24*pow<3>(s23)+36*s23*pow<3>(s24)+4*pow<4>(s23)-18*pow<4>(s24))+3*s24*pow<2>(s14)*(13*pow<2>(s23)*pow<2>(s24)+11*s24*pow<3>(s23)-2*s23*pow<3>(s24)+3*pow<4>(s23)-7*pow<4>(s24))+(38*s23*s24+18*pow<2>(s23)+3*pow<2>(s24))*pow<5>(s14)+(7*s23+3*s24)*pow<6>(s14)))+(32*s23*s24+2*s13*(16*s14+7*s23+15*s24)+s14*(34*s23+62*s24)+7*pow<2>(s13)+31*pow<2>(s14)+7*pow<2>(s23)+31*pow<2>(s24))*pow<7>(s12)+pow<2>(s12)*(3*s14*pow<2>(s24)*(9*s23*s24+3*pow<2>(s23)+5*pow<2>(s24))*pow<2>(s23+s24)+(3*s14*(s23-s24)*s24+3*(s23+s24)*pow<2>(s14)+pow<3>(s14)-2*pow<3>(s24))*pow<4>(s13)+(114*s24*pow<2>(s23)+168*s23*pow<2>(s24)+20*pow<3>(s23)+67*pow<3>(s24))*pow<4>(s14)+pow<3>(s13)*(2*s23*(3*s23+17*s24)*pow<2>(s14)+s14*s24*(9*s23*s24+9*pow<2>(s23)-26*pow<2>(s24))+(2*s23*s24+3*pow<2>(s23)-4*pow<2>(s24))*pow<2>(s24)+(20*s23+17*s24)*pow<3>(s14)+4*pow<4>(s14))+9*s24*pow<2>(s14)*(22*pow<2>(s23)*pow<2>(s24)+11*s24*pow<3>(s23)+18*s23*pow<3>(s24)+2*pow<4>(s23)+5*pow<4>(s24))+pow<3>(s14)*(209*pow<2>(s23)*pow<2>(s24)+82*s24*pow<3>(s23)+210*s23*pow<3>(s24)+10*pow<4>(s23)+67*pow<4>(s24))+pow<3>(s24)*pow<4>(s23+s24)+9*(9*s23*s24+3*pow<2>(s23)+5*pow<2>(s24))*pow<5>(s14)+pow<2>(s13)*((119*s23*s24+33*pow<2>(s23)+33*pow<2>(s24))*pow<3>(s14)+pow<2>(s14)*(59*s24*pow<2>(s23)+82*s23*pow<2>(s24)+3*pow<3>(s23)-27*pow<3>(s24))+3*s14*s24*(6*s24*pow<2>(s23)-6*s23*pow<2>(s24)+pow<3>(s23)-15*pow<3>(s24))-3*(4*s23*s24+pow<2>(s23)+3*pow<2>(s24))*pow<3>(s24)+(45*s23+34*s24)*pow<4>(s14)+6*pow<5>(s14))+(14*s23+15*s24)*pow<6>(s14)+s13*(pow<2>(s24)*(8*s23*s24+3*pow<2>(s23)+2*pow<2>(s24))*pow<2>(s23+s24)+pow<3>(s14)*(151*s24*pow<2>(s23)+214*s23*pow<2>(s24)+20*pow<3>(s23)+45*pow<3>(s24))+(161*s23*s24+54*pow<2>(s23)+63*pow<2>(s24))*pow<4>(s14)+3*s14*s24*(28*pow<2>(s23)*pow<2>(s24)+17*s24*pow<3>(s23)+12*s23*pow<3>(s24)+3*pow<4>(s23)-2*pow<4>(s24))+pow<2>(s14)*(161*pow<2>(s23)*pow<2>(s24)+58*s24*pow<3>(s23)+127*s23*pow<3>(s24)+6*pow<4>(s23)+3*pow<4>(s24))+(42*s23+33*s24)*pow<5>(s14)+4*pow<6>(s14))+pow<7>(s14))+6*(s13+2*s14+s23+2*s24)*pow<8>(s12)+2*pow<9>(s12))*pow<-1>(s13)*pow<-1>(s14)*pow<-1>(s12+s13+s14)*pow<-1>(s23)*pow<-1>(s24)*pow<-1>(s12+s23+s24)*pow<-1>(s12+s13+s14+s23+s24)*pow<-3>(s12+s14+s24))/2
    );
}

// Coefficient of master 13 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6ELC<13>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6ELC<13,-1>(s12,s13,s14,s23,s24)
    });
}


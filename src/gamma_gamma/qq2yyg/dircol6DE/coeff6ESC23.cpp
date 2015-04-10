/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeff6ESC.h"

// Master n. 23: box6(s13,s12+s13+s23,s24)

// Coefficient order epsilon^-1 of master 23
template<>
double qq2yyg6ESC<23,-1>(const double& is12, const double& is13, const double& is14, const double& is23, const double& is24)
{
    const cln::cl_RA s12 = cln::rational(is12);
    const cln::cl_RA s13 = cln::rational(is13);
    const cln::cl_RA s14 = cln::rational(is14);
    const cln::cl_RA s23 = cln::rational(is23);
    const cln::cl_RA s24 = cln::rational(is24);
    return cln::double_approx(
	(((s23+s24)*(s14*(s23-s24)+s23*s24+pow<2>(s14))*pow<2>(s23)*pow<3>(s14)+s14*pow<3>(s13)*(-(s23*(s23-5*s24)*pow<2>(s14))+s14*s24*(4*s23*s24-3*pow<2>(s23)+pow<2>(s24))+(s23+s24)*pow<3>(s24))+s13*s23*pow<2>(s14)*(s23*(3*s23+2*s24)*pow<2>(s14)-2*s23*(s23+s24)*pow<2>(s24)+(s23+2*s24)*pow<3>(s14)+s14*(pow<3>(s23)+pow<3>(s24)))+s14*pow<2>(s13)*(-(s14*s23*s24*pow<2>(s23+s24))+s23*(s23+5*s24)*pow<3>(s14)+s23*(s23+s24)*pow<3>(s24)+pow<2>(s14)*(-(s24*pow<2>(s23))+2*s23*pow<2>(s24)+2*pow<3>(s23)+pow<3>(s24)))-s14*s23*(s23-3*s24)*(s14+s24)*pow<4>(s13)+pow<4>(s12)*(pow<2>(s13)*(s23*(s23+2*s24)+3*s14*(5*s23+6*s24)+12*pow<2>(s14))+pow<2>(s14)*(32*s23*s24+12*pow<2>(s23)+15*pow<2>(s24))+(2*s14+3*s23)*pow<3>(s13)+(24*s23+22*s24)*pow<3>(s14)+(s23+2*s24)*pow<3>(s23)+s13*((36*s23+43*s24)*pow<2>(s14)+s14*(28*s23*s24+8*pow<2>(s23)+21*pow<2>(s24))+18*pow<3>(s14)+pow<3>(s23))+s14*(4*s24*pow<2>(s23)+9*s23*pow<2>(s24)+5*pow<3>(s23)+4*pow<3>(s24))+8*pow<4>(s14))+(6*s13*s14*(3*s14+2*s23+3*s24)+(6*s14+s23)*pow<2>(s13)+(16*s23+19*s24)*pow<2>(s14)+s14*(10*s23*s24+3*pow<2>(s23)+7*pow<2>(s24))+12*pow<3>(s14)+pow<3>(s23))*pow<5>(s12)+s23*s24*(s14+s24)*pow<5>(s13)+pow<2>(s12)*((s23*s24*(2*s23+3*s24)+(9*s23+5*s24)*pow<2>(s14)+s14*(21*s23*s24+4*pow<2>(s23)+7*pow<2>(s24)))*pow<3>(s13)+s14*pow<2>(s13)*(3*(5*s23+4*s24)*pow<2>(s14)+s14*(42*s23*s24+14*pow<2>(s23)+23*pow<2>(s24))+2*(5*s24*pow<2>(s23)+11*s23*pow<2>(s24)+pow<3>(s23)+6*pow<3>(s24)))+s23*(5*s14+s23+6*s24)*pow<4>(s13)+s14*(3*(6*s23*s24+4*pow<2>(s23)+pow<2>(s24))*pow<3>(s14)+s23*s24*(3*s24*pow<2>(s23)+3*pow<3>(s23)+pow<3>(s24))+pow<2>(s14)*(13*s24*pow<2>(s23)+17*s23*pow<2>(s24)+10*pow<3>(s23)+pow<3>(s24))+s14*s23*(8*s24*pow<2>(s23)-s23*pow<2>(s24)+6*pow<3>(s23)+4*pow<3>(s24))+2*(2*s23+s24)*pow<4>(s14))+s13*((46*s23*s24+25*pow<2>(s23)+18*pow<2>(s24))*pow<3>(s14)+pow<2>(s24)*pow<3>(s23)+pow<2>(s14)*(22*s24*pow<2>(s23)+31*s23*pow<2>(s24)+14*pow<3>(s23)+9*pow<3>(s24))+(14*s23+9*s24)*pow<4>(s14)+s14*(2*pow<2>(s23)*pow<2>(s24)+4*s24*pow<3>(s23)+9*s23*pow<3>(s24)+pow<4>(s23)+3*pow<4>(s24)))+s23*pow<5>(s13))+s12*(s14*(s14*s24*(15*s23+4*s24)+2*s23*pow<2>(s14)+s24*(11*s23*s24+2*pow<2>(s23)+4*pow<2>(s24)))*pow<3>(s13)+s23*pow<2>(s14)*(3*s24*(s23+s24)*pow<2>(s23)+pow<2>(s14)*(6*s23*s24+5*pow<2>(s23)+5*pow<2>(s24))+3*(s23+s24)*pow<3>(s14)+s14*(3*s24*pow<2>(s23)-2*s23*pow<2>(s24)+4*pow<3>(s23)+pow<3>(s24)))+s14*pow<2>(s13)*(pow<2>(s14)*(20*s23*s24+7*pow<2>(s23)+7*pow<2>(s24))+2*s23*pow<3>(s14)+s24*(2*s24*pow<2>(s23)+6*s23*pow<2>(s24)+pow<3>(s23)+3*pow<3>(s24))+2*s14*(2*s24*pow<2>(s23)+7*s23*pow<2>(s24)+2*pow<3>(s23)+3*pow<3>(s24)))+s23*(8*s14*s24+s24*(s23+3*s24)+2*pow<2>(s14))*pow<4>(s13)+s13*s14*(s23*pow<2>(s24)*(s23*s24-pow<2>(s23)+2*pow<2>(s24))+(14*s23*s24+10*pow<2>(s23)+3*pow<2>(s24))*pow<3>(s14)+pow<2>(s14)*(13*s24*pow<2>(s23)+13*s23*pow<2>(s24)+11*pow<3>(s23)+2*pow<3>(s24))+s14*s23*(2*s24*pow<2>(s23)-s23*pow<2>(s24)+2*pow<3>(s23)+3*pow<3>(s24))+s23*pow<4>(s14))+s23*(s14+2*s24)*pow<5>(s13))+pow<3>(s12)*((2*s23*(s23+3*s24)+s14*(11*s23+6*s24)+2*pow<2>(s14))*pow<3>(s13)+(37*s23*s24+18*pow<2>(s23)+11*pow<2>(s24))*pow<3>(s14)+pow<2>(s13)*(s23*s24*(s23+s24)+(27*s23+29*s24)*pow<2>(s14)+s14*(32*s23*s24+9*pow<2>(s23)+21*pow<2>(s24))+6*pow<3>(s14))+s24*(s23+s24)*pow<3>(s23)+pow<2>(s14)*(12*s24*pow<2>(s23)+21*s23*pow<2>(s24)+10*pow<3>(s23)+4*pow<3>(s24))+3*s23*pow<4>(s13)+(16*s23+11*s24)*pow<4>(s14)+s13*(pow<2>(s14)*(62*s23*s24+24*pow<2>(s23)+34*pow<2>(s24))+(37*s23+34*s24)*pow<3>(s14)+2*s24*pow<3>(s23)+s14*(11*s24*pow<2>(s23)+23*s23*pow<2>(s24)+7*pow<3>(s23)+12*pow<3>(s24))+6*pow<4>(s14))+s14*(7*s24*pow<3>(s23)+4*s23*pow<3>(s24)+4*pow<4>(s23)+pow<4>(s24))+2*pow<5>(s14))+2*s14*(3*s13+4*s14+2*s23+3*s24)*pow<6>(s12)+2*s14*pow<7>(s12))*pow<-1>(s14)*pow<-1>(s23)*pow<-1>(s12+s13+s23)*pow<-1>(s24)*pow<-1>(s12+s14+s24)*pow<-1>(s12+s23+s24)*pow<-3>(s12+s13+s14))/2
    );
}

// Coefficient of master 23 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6ESC<23>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6ESC<23,-1>(s12,s13,s14,s23,s24)
    });
}


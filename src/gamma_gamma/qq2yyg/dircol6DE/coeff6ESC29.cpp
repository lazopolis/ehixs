/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeff6ESC.h"

// Master n. 29: box6(-s12-s13-s14-s23-s24,s12+s14+s24,s12)

// Coefficient order epsilon^-1 of master 29
template<>
double qq2yyg6ESC<29,-1>(const double& is12, const double& is13, const double& is14, const double& is23, const double& is24)
{
    const cln::cl_RA s12 = cln::rational(is12);
    const cln::cl_RA s13 = cln::rational(is13);
    const cln::cl_RA s14 = cln::rational(is14);
    const cln::cl_RA s23 = cln::rational(is23);
    const cln::cl_RA s24 = cln::rational(is24);
    return cln::double_approx(
	((-((-(s14*s23*(2*s14+s23+2*s24))+s13*(-(s14*(s23-2*s24))+s24*(s23+2*s24))+s24*pow<2>(s13))*(pow<2>(s13)-pow<2>(s23)))+(s13+s23)*pow<2>(s12)*(-(s14*(s23-4*s24))+s13*(5*s14-s24)+5*s23*s24+2*pow<2>(s13)+2*pow<2>(s14)+2*pow<2>(s23)+2*pow<2>(s24))+(6*s23*s24+s14*(6*s23+4*s24)+s13*(6*s14+4*s23+6*s24)+3*pow<2>(s13)+2*pow<2>(s14)+3*pow<2>(s23)+2*pow<2>(s24))*pow<3>(s12)+s12*(s13+s23)*((3*s14+s23-4*s24)*pow<2>(s13)+s13*(s14*(s23-2*s24)+s23*s24+4*pow<2>(s14)+pow<2>(s23)-2*pow<2>(s24))+s23*(3*s23*s24-2*s14*(2*s23+s24)-2*pow<2>(s14)+pow<2>(s23)+4*pow<2>(s24))+pow<3>(s13))+4*(s13+s14+s23+s24)*pow<4>(s12)+2*pow<5>(s12))*pow<-1>(s14)*pow<-1>(s12+s13+s14)*pow<-1>(s24)*pow<-1>(s12+s23+s24)*pow<-2>(s12+s13+s23))/2
    );
}

// Coefficient of master 29 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yyg6ESC<29>(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yyg6ESC<29,-1>(s12,s13,s14,s23,s24)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6LC.h"

// Master type 6: box6(s15,s25,s34)

// Coefficient order epsilon^-1 of master type 6
template<>
double qq2yygstu6LC<6,-1>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (256+zb*(-512+zb*(384+384*pow<2>(t34)-96*pow<2>(t12)*pow<2>(u)+zb*(-128-384*pow<2>(t34)+zb*(16+pow<2>(t34)*(96+16*pow<2>(t34))-16*pow<4>(t12)))))-16*pow<4>(u))*pow<-1>(zb)*pow<-1>(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t12)*(16+(-8+pow<2>(u))*pow<2>(u))+zb*(32-8*pow<2>(u)+pow<2>(t12)*(-32+8*pow<2>(u))+zb*(-24+t12*(16*t34*u+t12*(32+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(8+2*pow<2>(u))+zb*(8-8*pow<2>(t34)+t12*(-8*t34*u+t12*(-16+t12*(8*t12+8*t34*u)+8*pow<2>(t34)))+zb*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))));
    return todouble<my_float>(foo);
}

// Coefficient of master type 6 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6LC<6>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygstu6LC<6,-1>(zb,t12,t34,u)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffz0LC.h"

// Master type 6: box6(s15,s25,s34)

// Coefficient order epsilon^-1 of master type 6
template<>
double qq2yygz0LC<6,-1>(const my_float& z, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (-16+(-96-16*pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(16*pow<2>(t12)+96*pow<2>(u))+z*(-64+pow<2>(t12)*(-64*pow<2>(t12)-192*pow<2>(u))+64*pow<4>(t34)+z*(-96+(192-96*pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(96*pow<2>(t12)+96*pow<2>(u))+z*(-64-64*pow<4>(t12)+z*(-16+(-96-16*pow<2>(t34))*pow<2>(t34)+16*pow<4>(t12))+64*pow<4>(t34))))+16*pow<4>(u))*pow<-1>(1+pow<2>(t34)*(-2+pow<2>(t34)-2*pow<2>(u))+(-2+pow<2>(u))*pow<2>(u)+z*(3+(-2-pow<2>(u))*pow<2>(u)+pow<2>(t34)*(2-5*pow<2>(t34)+6*pow<2>(u))+t12*(16*t34*u+t12*(-1+t12*(-16*t34*u+t12*(-7+5*pow<2>(t12)-10*pow<2>(t34)-6*pow<2>(u)))+pow<2>(t34)*(8+5*pow<2>(t34)-6*pow<2>(u))+pow<2>(u)*(8+pow<2>(u))))+z*(2+pow<2>(t34)*(4+10*pow<2>(t34)-6*pow<2>(u))+2*pow<2>(u)+pow<2>(t12)*(2-8*pow<2>(u)+pow<2>(t34)*(-24-10*pow<2>(t34)+6*pow<2>(u))+pow<2>(t12)*(6-10*pow<2>(t12)+20*pow<2>(t34)+6*pow<2>(u)))+z*(-2+t12*(-16*t34*u+t12*(-2+t12*(16*t34*u+t12*(-6+10*pow<2>(t12)-20*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(24+10*pow<2>(t34)-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(-4-10*pow<2>(t34)+2*pow<2>(u))+z*(-3+pow<2>(t34)*(-2+5*pow<2>(t34))+t12*(8*t34*u+t12*(1+(-8-5*pow<2>(t34))*pow<2>(t34)+t12*(-8*t34*u+t12*(7-5*pow<2>(t12)+10*pow<2>(t34)))))+z*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))))+t12*(-8*t34*u+t12*(-3+pow<2>(t34)*(-pow<2>(t34)+2*pow<2>(u))+t12*(8*t34*u+t12*(3-pow<2>(t12)+2*pow<2>(t34)+2*pow<2>(u)))-pow<4>(u))));
    return todouble<my_float>(foo);
}

// Coefficient of master type 6 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz0LC<6>(const my_float& z, const my_float& t12, const my_float& t34, const my_float& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz0LC<6,-1>(z,t12,t34,u)
    });
}


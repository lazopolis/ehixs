/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffz1SC.h"

// Master type 1: bubble(s12)

// Coefficient order epsilon^-1 of master type 1
template<>
double qq2yygz1SC<1,-1>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (-256+zb*(512+zb*(-384-384*pow<2>(t34)+96*pow<2>(t12)*pow<2>(u)+zb*(128+384*pow<2>(t34)+zb*(-144+(-96-16*pow<2>(t34))*pow<2>(t34)+144*pow<4>(t12)))))+16*pow<4>(u))*pow<-1>(zb)*pow<-1>(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t12)*(16+(-8+pow<2>(u))*pow<2>(u))+zb*(32-8*pow<2>(u)+pow<2>(t12)*(-32+8*pow<2>(u))+zb*(-24+t12*(16*t34*u+t12*(32+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(8+2*pow<2>(u))+zb*(8-8*pow<2>(t34)+t12*(-8*t34*u+t12*(-16+t12*(8*t12+8*t34*u)+8*pow<2>(t34)))+zb*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))));
    return todouble<my_float>(foo);
}

// Coefficient order epsilon^0 of master type 1
template<>
double qq2yygz1SC<1,0>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (1280+(-128-48*pow<2>(u))*pow<2>(u)+zb*(-2560+zb*(2944+zb*(-1664+t12*(896*t12+256*t34*u)-896*pow<2>(t34)+zb*(720+pow<2>(t12)*(-288-432*pow<2>(t12)-224*pow<2>(t34))+pow<2>(t34)*(480+80*pow<2>(t34))))+t12*(-512*t34*u+t12*(-896-288*pow<2>(u)))+pow<2>(t34)*(896-32*pow<2>(u))-224*pow<2>(u))+128*pow<2>(u)))*pow<-1>(zb)*pow<-1>(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t12)*(16+(-8+pow<2>(u))*pow<2>(u))+zb*(32-8*pow<2>(u)+pow<2>(t12)*(-32+8*pow<2>(u))+zb*(-24+t12*(16*t34*u+t12*(32+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(8+2*pow<2>(u))+zb*(8-8*pow<2>(t34)+t12*(-8*t34*u+t12*(-16+t12*(8*t12+8*t34*u)+8*pow<2>(t34)))+zb*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))));
    return todouble<my_float>(foo);
}

// Coefficient order epsilon^1 of master type 1
template<>
double qq2yygz1SC<1,1>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (-2048+pow<2>(u)*(384+32*pow<2>(u))+zb*(4096-384*pow<2>(u)+zb*(-5376+zb*(3328+t12*(-1664*t12-256*t34*u)+768*pow<2>(t34)+zb*(-1152+(-768-128*pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(864+288*pow<2>(t12)+416*pow<2>(t34))))+416*pow<2>(u)+pow<2>(t34)*(-768+96*pow<2>(u))+t12*(512*t34*u+t12*(1664+192*pow<2>(u))))))*pow<-1>(zb)*pow<-1>(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t12)*(16+(-8+pow<2>(u))*pow<2>(u))+zb*(32-8*pow<2>(u)+pow<2>(t12)*(-32+8*pow<2>(u))+zb*(-24+t12*(16*t34*u+t12*(32+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(8+2*pow<2>(u))+zb*(8-8*pow<2>(t34)+t12*(-8*t34*u+t12*(-16+t12*(8*t12+8*t34*u)+8*pow<2>(t34)))+zb*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))));
    return todouble<my_float>(foo);
}

// Coefficient of master type 1 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz1SC<1>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz1SC<1,-1>(zb,t12,t34,u),
        qq2yygz1SC<1,0>(zb,t12,t34,u),
        qq2yygz1SC<1,1>(zb,t12,t34,u)
    });
}


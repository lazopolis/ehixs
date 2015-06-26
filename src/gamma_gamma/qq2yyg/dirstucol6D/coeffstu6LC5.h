/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6LC.h"

// Master type 5: box6(s13,s34,s25)

// Coefficient order epsilon^-1 of master type 5
template<>
double qq2yygstu6LC<5,-1>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (-320+u*(-256+u*(-32-4*pow<2>(u)))+zb*(640+u*(384+32*u)+t34*u*(-64+16*pow<2>(u))+t12*(-128+32*pow<2>(u))+zb*(-288+(-128-24*u)*u+zb*(-32+t12*(-96+t34*(128+32*t34+96*u)+t12*(160-32*t12-16*t34*u))+t34*(-16*u+t34*(32+16*t34*u))+zb*(-4+t12*(16+t12*(-24+(16-4*t12)*t12-24*pow<2>(t34))-16*pow<2>(t34))+(8-4*pow<2>(t34))*pow<2>(t34)))+t34*(64*u+t34*(-32-24*pow<2>(u)))+t12*(192+t34*(-128-192*u)-16*pow<2>(u)+t12*(-160+8*pow<2>(u))))))*pow<-1>(zb)*pow<-1>(16+u*(16+(-4-u)*pow<2>(u))+pow<2>(t12)*(-16+u*(-16+(4+u)*pow<2>(u)))+zb*(-32+u*(-24+2*pow<2>(u))+t34*(-32+u*(-24+2*pow<2>(u)))+zb*(24+12*u+t34*(48+24*u+t34*(24+12*u))+zb*(-8+t34*(-24+t34*(-24+t34*(-8-2*u)-6*u)-6*u)-2*u+t12*(12+t34*(24+12*t34)+t12*(8+8*u+t34*(24+12*u+t34*(24+6*u+t34*(8+2*u)))+t12*(-16+(-24-12*t34)*t34-4*u+t12*(-6*u-6*t34*u+t12*(4+4*u)))))+(1+t34*(4+t34*(6+t34*(4+t34)))+t12*(-2+t34*(-6+(-6-2*t34)*t34)+t12*(-1+t34*(-4+t34*(-6+(-4-t34)*t34))+t12*(4+t12*(-1+t12*(-2+t12-2*t34))+t34*(8+t34*(6+2*t34))))))*zb)+t12*(-24+t12*(-24+t34*(-48+t34*(-24-12*u)-24*u)+(-24-6*u)*u+t12*(24+t12*u*(12+6*u)+t34*(24-6*pow<2>(u))-6*pow<2>(u)))+6*pow<2>(u)+t34*(-24+6*pow<2>(u))))+t12*(16+(-12-4*u)*pow<2>(u)+t12*(32+t34*(32+u*(24-2*pow<2>(u)))+u*(24-2*pow<2>(u))+t12*(-16+(12+4*u)*pow<2>(u))))));
    return todouble<my_float>(foo);
}

// Coefficient of master type 5 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6LC<5>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygstu6LC<5,-1>(zb,t12,t34,u)
    });
}


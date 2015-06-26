/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6SCnobar.h"

// Master n. 24: box6(s23,s35,s14)

// Coefficient order epsilon^-1 of master 24
template<>
double qq2yygstu6SCnobar<24,-1>(const double& z, const double& t12, const double& t34, const double& u)
{
    return (10+t34*(10+t34*(-10-10*t34-u)+(-10-8*u)*u)+u*(-9+(4-u)*u)+z*(12+(-2-2*u)*u+t34*(20+u*(-8+2*u)+t34*(-12-20*t34+10*u))+t12*(28+(4-6*u)*u+t34*(-7+t34*(-46+5*t34)+u*(-12+3*u))+t12*(-16+(4-6*u)*u+t34*(-20-20*u+t34*(-10+6*t34+2*u))+t12*(-32+(-4-2*u)*u+t34*(2+t34*(14-3*t34-4*u)+(4-u)*u)+t12*(4+t12*(4-3*t34)-2*u+t34*(-8+6*t34+4*u)))))+z*(2-u+t34*(-6+t34*(6-2*t34-u)+2*u)+t12*(6-4*u+t34*(-21+t34*(12+3*t34)+4*u)+t12*(4-6*u+t34*(-12+t34*(4+3*u))+t12*(-4+t12*(-6+t12*(-2+3*t34)+t34*(10-6*t34-2*u)-u)-4*u+t34*(10-4*u+t34*(-16+3*t34+2*u)))))+t12*(t34*(-1+(2-t34)*t34)+t12*(t34*(-4+(6-2*t34)*t34)+t12*(t34*(-6+(6-t34)*t34)+t12*(-(t12*t34)+t34*(-4+2*t34)))))*z))+t12*(6+t34*(-3+t34*(-16-7*t34-24*u)+(-8-3*u)*u)+u*(-8+(-4-2*u)*u)+t12*(-12+t34*(-20+t34*(-4*t34-5*u)+(-12-2*u)*u)+u*(-2-pow<2>(u))+t12*(-4+t12*(2+t12*(-2+t34)+t34*(2-2*t34-2*u)+3*u)+t34*(-6+t34*(-4+t34+2*u)+pow<2>(u))))))*pow<-1>(1-pow<2>(u)+t34*(-3+(3-t34)*t34+pow<2>(u))+t12*(2+(-2-2*u)*u+t34*(-6+(6-2*t34)*t34+u*(2+2*u))+t12*(-1-4*u+t34*(1+4*u)+t12*(-4+t12*(-1+u*(4+u)+t12*(2+t12*(1-t34)+t34*(-2-2*u)+2*u)+t34*(3+(-3+t34)*t34+(-4-u)*u))+t34*(8+t34*(-6+2*t34)-2*pow<2>(u))+2*pow<2>(u))))+z*(1+z*(-1+t34*(3+(1-3*t34)*t34)+t12*(-2-2*u+t34*(6+(2-6*t34)*t34+2*u)+t12*(-3-4*u+t34*(3+4*u)+t12*(-4+t12*(1+t34*(-3+t34*(-1+3*t34)-4*u)+4*u+t12*(6+t12*(3-3*t34)+t34*(-6-2*u)+2*u))+(-2+6*t34)*pow<2>(t34))))+z*(-1+t34*(-1+t34*(1+t34))+t12*(-2+t34*(-2+t34*(2+2*t34))+t12*(1-t34+t12*(4+t12*(1+t12*(-2+t12*(-1+t34)+2*t34)+t34*(1+(-1-t34)*t34))+(-2-2*t34)*pow<2>(t34))))))+t34*(1+t34*(-5+3*t34)-pow<2>(u))+pow<2>(u)+t12*(2+u*(4+2*u)+t34*(2+t34*(-10+6*t34)+(-4-2*u)*u)+t12*(3+t34*(-3-8*u)+8*u+t12*(4+t12*(-1+(-8-u)*u+t34*(-1+(5-3*t34)*t34+u*(8+u))+t12*(-6+t12*(-3+3*t34)-4*u+t34*(6+4*u)))-2*pow<2>(u)+t34*(-8+(10-6*t34)*t34+2*pow<2>(u)))))));
}

// Coefficient of master 24 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6SCnobar<24>(const double& z, const double& t12, const double& t34, const double& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygstu6SCnobar<24,-1>(z,t12,t34,u)
    });
}


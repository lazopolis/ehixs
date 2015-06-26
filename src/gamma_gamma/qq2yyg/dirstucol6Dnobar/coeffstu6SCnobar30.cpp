/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6SCnobar.h"

// Master n. 30: box6(s34,s45,s12)

// Coefficient order epsilon^-1 of master 30
template<>
double qq2yygstu6SCnobar<30,-1>(const double& z, const double& t12, const double& t34, const double& u)
{
    return (-4+t12*(t12*(-12+t34*(4+t34*(-12+4*t34)))+4*u+t34*(-52*u+t34*(12*u+4*t34*u)))-16*pow<2>(u)+t34*(t34*(8-4*pow<2>(t34)-8*pow<2>(u))+8*pow<2>(u))+z*(56+z*(-4+t34*(-16+t34*(-24+(-16-4*t34)*t34))+t12*(t12*(-60+t34*(-68+t34*(-28+12*t34)))+12*u+t34*(-4*u+t34*(-12*u+4*t34*u)))+t34*(12+(8-4*t34)*t34)*z*pow<2>(t12))+t12*(t12*(-56+t34*(52+(32-12*t34)*t34))+32*u+t34*(-24*u-8*u*pow<2>(t34)))-12*pow<2>(u)+t34*(-16-8*pow<2>(u)+t34*(-64+t34*(16+8*t34)+4*pow<2>(u)))))*pow<-1>(-1+t12*(-2*u+t34*(-4*u-2*t34*u)+t12*(2+t12*(t12*(-1+(-2-t34)*t34)+2*u+t34*(4*u+2*t34*u))+t34*(6+t34*(7+t34*(4+t34)-pow<2>(u))-2*pow<2>(u))-pow<2>(u)))+pow<2>(u)+t34*(-4+2*pow<2>(u)+t34*(-6+(-4-t34)*t34+pow<2>(u)))+z*(-1+z*(1+t34*(4+t34*(2+(-4-3*t34)*t34))+t12*(-2*u+t34*(-4*u-2*t34*u)+t12*(2+t34*(2+t34*(1+t34*(4+3*t34)))+t12*(t12*(-3+(-6-3*t34)*t34)+2*u+t34*(4*u+2*t34*u))))+z*(1+pow<2>(t12)*(-2+(1+t34*(2+t34))*pow<2>(t12)+t34*(-2+t34*(1-pow<2>(t34))))+(-2+pow<2>(t34))*pow<2>(t34)))+t34*(t34*(6+t34*(8+3*t34)-pow<2>(u))-2*pow<2>(u))-pow<2>(u)+t12*(4*u+t34*(8*u+4*t34*u)+t12*(-2+t12*(t12*(3+t34*(6+3*t34))-4*u+t34*(-8*u-4*t34*u))+pow<2>(u)+t34*(-6+2*pow<2>(u)+t34*(-9+(-8-3*t34)*t34+pow<2>(u)))))));
}

// Coefficient of master 30 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6SCnobar<30>(const double& z, const double& t12, const double& t34, const double& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygstu6SCnobar<30,-1>(z,t12,t34,u)
    });
}


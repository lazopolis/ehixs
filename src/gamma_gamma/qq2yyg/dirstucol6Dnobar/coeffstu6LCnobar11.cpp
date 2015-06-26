/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6LCnobar.h"

// Master n. 11: box6(s14,s34,s25)

// Coefficient order epsilon^-1 of master 11
template<>
double qq2yygstu6LCnobar<11,-1>(const double& z, const double& t12, const double& t34, const double& u)
{
    return (4+t12*(16+t34*(-16*t34+96*u)+t12*(24+t12*(16+4*t12)+t34*(24*t34+16*u)-8*pow<2>(u))-16*pow<2>(u))+pow<2>(u)*(24+4*pow<2>(u))+t34*(u*(16-16*pow<2>(u))+t34*(-8+t34*(4*t34-16*u)+24*pow<2>(u)))+z*(-48+(-128-16*u)*u+t34*(t34*(64+t34*(-16*t34+48*u)-48*pow<2>(u))+u*(16+16*pow<2>(u)))+t12*(32+t34*(-128+32*t34-96*u)+t12*(64+(-32-16*t12)*t12+t34*(-96*t34-48*u)+16*pow<2>(u)))+z*(408+u*(-128+24*u)+z*(-48+t12*(-32+t12*(64+(32-16*t12)*t12+t34*(-96*t34-16*u))+t34*(-128-32*t34+96*u))+t34*(-16*u+t34*(64+t34*(-16*t34+16*u)))+z*(4+pow<2>(t34)*(-8+4*pow<2>(t34))+t12*(-16+16*pow<2>(t34)+t12*(24+t12*(-16+4*t12)+24*pow<2>(t34)))))+t12*(t34*(256-96*u)+t12*(-176+t34*(144*t34+48*u)+24*pow<2>(t12)-8*pow<2>(u))+16*pow<2>(u))+t34*(-16*u+t34*(-112+t34*(24*t34-48*u)+24*pow<2>(u))))))*pow<-1>(-1+t34*(-4+t34*(-6+6*u+t34*(-4-t34+2*u))+u*(6-2*pow<2>(u)))+u*(2+(-2+u)*pow<2>(u))+t12*(-2+(6-4*u)*pow<2>(u)+t34*(-6+(-6-2*t34)*t34+6*pow<2>(u))+t12*(1+u*(-8+u*(6+(2-u)*u))+t12*(4+u*(-4+u*(-6+4*u))+t12*(1+6*t34*u+(6-6*u)*u+t12*(-2-t12-2*t34+4*u))+t34*(8+t34*(6+2*t34)-6*pow<2>(u)))+t34*(4+t34*(6+t34*(4+t34-2*u)-6*u)+u*(-12+2*pow<2>(u)))))+z*(-3+t12*(-2+t34*(6+t34*(18+10*t34)-18*pow<2>(u))+(-6+8*u)*pow<2>(u)+t12*(3+t34*(4+t34*(-6+12*u+t34*(-12-5*t34+8*u))+u*(24-4*pow<2>(u)))+u*(8+u*(-18+pow<2>(u)))+t12*(-4+u*(16+(6-8*u)*u)+t12*(-5+t12*(6+5*t12+10*t34-16*u)-24*t34*u+u*(-12+18*u))+t34*(-16+(-18-10*t34)*t34+18*pow<2>(u)))))+u*(4-pow<3>(u))+z*(-2+t34*(8+t34*(12+t34*(-8-10*t34+12*u))+u*(-12-2*pow<2>(u)))+t12*(4+(-6-4*u)*pow<2>(u)+t34*(12+(-12-20*t34)*t34+18*pow<2>(u))+t12*(2+t12*(u*(-24+u*(6+4*u))+t34*(8+t34*(12+20*t34)-18*pow<2>(u))+t12*(10+36*t34*u+t12*(-4-10*t12-20*t34+24*u)-18*pow<2>(u)))+(18-2*u)*pow<2>(u)+t34*(-8+t34*(-12+t34*(8+10*t34-12*u))+u*(-24+2*pow<2>(u)))))+z*(2-4*u+t34*(8+t34*(-12+t34*(-8+10*t34-8*u)+12*u))+z*(3-2*u+t34*(-4+6*u+t34*(-6-6*u+t34*(12-5*t34+2*u)))+t12*(-2+t34*(-6+(18-10*t34)*t34)+t12*(-3+8*u+t34*(4-12*u+t34*(6+t34*(-12+5*t34-2*u)+6*u))+t12*(-4+t34*(16+t34*(-18+10*t34))-4*u+t12*(5-6*u+6*t34*u+t12*(6-5*t12-10*t34+4*u)))))+(1+t34*(-4+t34*(6+(-4+t34)*t34))+t12*(-2+t34*(6+t34*(-6+2*t34))+t12*(-1+t34*(4+t34*(-6+(4-t34)*t34))+t12*(4+t34*(-8+(6-2*t34)*t34)+t12*(-1+t12*(-2+t12+2*t34))))))*z)+t12*(4+t34*(-12+t34*(-12+20*t34)-6*pow<2>(u))+6*pow<2>(u)+t12*(-2+(-8-6*u)*u+t34*(-8+24*u+t34*(12-12*u+t34*(8-10*t34+8*u)))+t12*((16-6*u)*u+t12*(-10+t12*(-4+10*t12+20*t34-16*u)-24*t34*u+u*(12+6*u))+t34*(-8+(12-20*t34)*t34+6*pow<2>(u))))))+2*pow<3>(u))+t34*(-4+t34*(6+t34*(12+5*t34-8*u)-12*u)+4*pow<3>(u))));
}

// Coefficient of master 11 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6LCnobar<11>(const double& z, const double& t12, const double& t34, const double& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygstu6LCnobar<11,-1>(z,t12,t34,u)
    });
}


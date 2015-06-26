/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6SCnobar.h"

// Master n. 31: box6(s35,s45,s12)

// Coefficient order epsilon^-1 of master 31
template<>
double qq2yygstu6SCnobar<31,-1>(const double& z, const double& t12, const double& t34, const double& u)
{
    return (t12*(64*t34*u+32*t12*pow<2>(t34))+32*pow<2>(u)+z*(-64+t12*(-224*t34*u+t12*(64-192*pow<2>(t34)))+64*pow<2>(t34)-96*pow<2>(u)+z*(256-192*pow<2>(t34)+t12*(256*t34*u+t12*(-192+480*pow<2>(t34)))+z*(-384+t12*(-64*t34*u+t12*(192-640*pow<2>(t34)))+192*pow<2>(t34)+z*(256-64*pow<2>(t34)+t12*(-64*t34*u+t12*(-64+480*pow<2>(t34)))+z*(-64+32*z*pow<2>(t12)*pow<2>(t34)+t12*(32*t34*u-192*t12*pow<2>(t34))))-32*pow<2>(u))+96*pow<2>(u))))*pow<-1>(1+t12*(-8*t34*u+t12*(-2+pow<2>(t12)-2*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(-2+pow<2>(t34)-2*pow<2>(u))+(-2+pow<2>(u))*pow<2>(u)+z*(4-4*pow<2>(u)+pow<2>(t34)*(-4*pow<2>(t34)+4*pow<2>(u))+t12*(8*t34*u+t12*(-4*pow<2>(t12)+8*pow<2>(t34)+4*pow<2>(u)))+z*(6+t12*(8*t34*u+t12*(4+6*pow<2>(t12)-12*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(4+6*pow<2>(t34)-2*pow<2>(u))-2*pow<2>(u)+z*(4+z*(1+pow<2>(t12)*(-2+pow<2>(t12)-2*pow<2>(t34))+(-2+pow<2>(t34))*pow<2>(t34))+t12*(-8*t34*u+t12*(-4*pow<2>(t12)+8*pow<2>(t34)))-4*pow<4>(t34)))))*pow<-2>(z);
}

// Coefficient of master 31 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6SCnobar<31>(const double& z, const double& t12, const double& t34, const double& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygstu6SCnobar<31,-1>(z,t12,t34,u)
    });
}


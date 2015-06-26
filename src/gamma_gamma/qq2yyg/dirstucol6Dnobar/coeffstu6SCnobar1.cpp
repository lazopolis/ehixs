/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6SCnobar.h"

// Master n. 1: bubble(s12)

// Coefficient order epsilon^-1 of master 1
template<>
double qq2yygstu6SCnobar<1,-1>(const double& z, const double& t12, const double& t34, const double& u)
{
    return (144+pow<2>(t34)*(96+16*pow<2>(t34))+pow<2>(t12)*(-144*pow<2>(t12)-96*pow<2>(u))+z*(-448+pow<2>(t12)*(576*pow<2>(t12)+192*pow<2>(u))+z*(864+pow<2>(t34)*(-192+96*pow<2>(t34))+pow<2>(t12)*(-864*pow<2>(t12)-96*pow<2>(u))+z*(-448+z*(144+pow<2>(t34)*(96+16*pow<2>(t34))-144*pow<4>(t12))+576*pow<4>(t12)-64*pow<4>(t34)))-64*pow<4>(t34))-16*pow<4>(u))*pow<-1>(1+pow<2>(t34)*(-2+pow<2>(t34)-2*pow<2>(u))+(-2+pow<2>(u))*pow<2>(u)+z*(3+(-2-pow<2>(u))*pow<2>(u)+pow<2>(t34)*(2-5*pow<2>(t34)+6*pow<2>(u))+t12*(16*t34*u+t12*(-1+t12*(-16*t34*u+t12*(-7+5*pow<2>(t12)-10*pow<2>(t34)-6*pow<2>(u)))+pow<2>(t34)*(8+5*pow<2>(t34)-6*pow<2>(u))+pow<2>(u)*(8+pow<2>(u))))+z*(2+pow<2>(t34)*(4+10*pow<2>(t34)-6*pow<2>(u))+2*pow<2>(u)+pow<2>(t12)*(2-8*pow<2>(u)+pow<2>(t34)*(-24-10*pow<2>(t34)+6*pow<2>(u))+pow<2>(t12)*(6-10*pow<2>(t12)+20*pow<2>(t34)+6*pow<2>(u)))+z*(-2+t12*(-16*t34*u+t12*(-2+t12*(16*t34*u+t12*(-6+10*pow<2>(t12)-20*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(24+10*pow<2>(t34)-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(-4-10*pow<2>(t34)+2*pow<2>(u))+z*(-3+pow<2>(t34)*(-2+5*pow<2>(t34))+t12*(8*t34*u+t12*(1+(-8-5*pow<2>(t34))*pow<2>(t34)+t12*(-8*t34*u+t12*(7-5*pow<2>(t12)+10*pow<2>(t34)))))+z*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))))+t12*(-8*t34*u+t12*(-3+pow<2>(t34)*(-pow<2>(t34)+2*pow<2>(u))+t12*(8*t34*u+t12*(3-pow<2>(t12)+2*pow<2>(t34)+2*pow<2>(u)))-pow<4>(u))));
}

// Coefficient order epsilon^0 of master 1
template<>
double qq2yygstu6SCnobar<1,0>(const double& z, const double& t12, const double& t34, const double& u)
{
    return (-720+pow<2>(t34)*(-480-80*pow<2>(t34)+32*pow<2>(u))+pow<2>(u)*(224+48*pow<2>(u))+t12*(256*t34*u+t12*(288+432*pow<2>(t12)+224*pow<2>(t34)+288*pow<2>(u)))+z*(1216+t12*(-256*t34*u+t12*(-256-1728*pow<2>(t12)-896*pow<2>(t34)-576*pow<2>(u)))+pow<2>(t34)*(1024+320*pow<2>(t34)-64*pow<2>(u))-320*pow<2>(u)+z*(-2272+z*(1216+t12*(256*t34*u+t12*(-256-1728*pow<2>(t12)-896*pow<2>(t34)))+pow<2>(t34)*(1024+320*pow<2>(t34))+z*(-720+(-480-80*pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(288+432*pow<2>(t12)+224*pow<2>(t34))))+224*pow<2>(u)+pow<2>(t34)*(-1088-480*pow<2>(t34)+32*pow<2>(u))+t12*(-256*t34*u+t12*(-64+2592*pow<2>(t12)+1344*pow<2>(t34)+288*pow<2>(u))))))*pow<-1>(1+pow<2>(t34)*(-2+pow<2>(t34)-2*pow<2>(u))+(-2+pow<2>(u))*pow<2>(u)+z*(3+(-2-pow<2>(u))*pow<2>(u)+pow<2>(t34)*(2-5*pow<2>(t34)+6*pow<2>(u))+t12*(16*t34*u+t12*(-1+t12*(-16*t34*u+t12*(-7+5*pow<2>(t12)-10*pow<2>(t34)-6*pow<2>(u)))+pow<2>(t34)*(8+5*pow<2>(t34)-6*pow<2>(u))+pow<2>(u)*(8+pow<2>(u))))+z*(2+pow<2>(t34)*(4+10*pow<2>(t34)-6*pow<2>(u))+2*pow<2>(u)+pow<2>(t12)*(2-8*pow<2>(u)+pow<2>(t34)*(-24-10*pow<2>(t34)+6*pow<2>(u))+pow<2>(t12)*(6-10*pow<2>(t12)+20*pow<2>(t34)+6*pow<2>(u)))+z*(-2+t12*(-16*t34*u+t12*(-2+t12*(16*t34*u+t12*(-6+10*pow<2>(t12)-20*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(24+10*pow<2>(t34)-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(-4-10*pow<2>(t34)+2*pow<2>(u))+z*(-3+pow<2>(t34)*(-2+5*pow<2>(t34))+t12*(8*t34*u+t12*(1+(-8-5*pow<2>(t34))*pow<2>(t34)+t12*(-8*t34*u+t12*(7-5*pow<2>(t12)+10*pow<2>(t34)))))+z*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))))+t12*(-8*t34*u+t12*(-3+pow<2>(t34)*(-pow<2>(t34)+2*pow<2>(u))+t12*(8*t34*u+t12*(3-pow<2>(t12)+2*pow<2>(t34)+2*pow<2>(u)))-pow<4>(u))));
}

// Coefficient order epsilon^1 of master 1
template<>
double qq2yygstu6SCnobar<1,1>(const double& z, const double& t12, const double& t34, const double& u)
{
    return (1152+t12*(-256*t34*u+t12*(-864-288*pow<2>(t12)-416*pow<2>(t34)-192*pow<2>(u)))+pow<2>(t34)*(768+128*pow<2>(t34)-96*pow<2>(u))+(-416-32*pow<2>(u))*pow<2>(u)+z*(-1280+z*(2304+z*(-1280+(-2304-512*pow<2>(t34))*pow<2>(t34)+z*(1152+pow<2>(t12)*(-864-288*pow<2>(t12)-416*pow<2>(t34))+pow<2>(t34)*(768+128*pow<2>(t34)))+t12*(-256*t34*u+t12*(1792+1152*pow<2>(t12)+1664*pow<2>(t34))))+t12*(256*t34*u+t12*(-1856-1728*pow<2>(t12)-2496*pow<2>(t34)-192*pow<2>(u)))+pow<2>(t34)*(3072+768*pow<2>(t34)-96*pow<2>(u))-416*pow<2>(u))+448*pow<2>(u)+pow<2>(t34)*(-2304-512*pow<2>(t34)+192*pow<2>(u))+t12*(256*t34*u+t12*(1792+1152*pow<2>(t12)+1664*pow<2>(t34)+384*pow<2>(u)))))*pow<-1>(1+pow<2>(t34)*(-2+pow<2>(t34)-2*pow<2>(u))+(-2+pow<2>(u))*pow<2>(u)+z*(3+(-2-pow<2>(u))*pow<2>(u)+pow<2>(t34)*(2-5*pow<2>(t34)+6*pow<2>(u))+t12*(16*t34*u+t12*(-1+t12*(-16*t34*u+t12*(-7+5*pow<2>(t12)-10*pow<2>(t34)-6*pow<2>(u)))+pow<2>(t34)*(8+5*pow<2>(t34)-6*pow<2>(u))+pow<2>(u)*(8+pow<2>(u))))+z*(2+pow<2>(t34)*(4+10*pow<2>(t34)-6*pow<2>(u))+2*pow<2>(u)+pow<2>(t12)*(2-8*pow<2>(u)+pow<2>(t34)*(-24-10*pow<2>(t34)+6*pow<2>(u))+pow<2>(t12)*(6-10*pow<2>(t12)+20*pow<2>(t34)+6*pow<2>(u)))+z*(-2+t12*(-16*t34*u+t12*(-2+t12*(16*t34*u+t12*(-6+10*pow<2>(t12)-20*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(24+10*pow<2>(t34)-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(-4-10*pow<2>(t34)+2*pow<2>(u))+z*(-3+pow<2>(t34)*(-2+5*pow<2>(t34))+t12*(8*t34*u+t12*(1+(-8-5*pow<2>(t34))*pow<2>(t34)+t12*(-8*t34*u+t12*(7-5*pow<2>(t12)+10*pow<2>(t34)))))+z*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))))+t12*(-8*t34*u+t12*(-3+pow<2>(t34)*(-pow<2>(t34)+2*pow<2>(u))+t12*(8*t34*u+t12*(3-pow<2>(t12)+2*pow<2>(t34)+2*pow<2>(u)))-pow<4>(u))));
}

// Coefficient of master 1 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6SCnobar<1>(const double& z, const double& t12, const double& t34, const double& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygstu6SCnobar<1,-1>(z,t12,t34,u),
        qq2yygstu6SCnobar<1,0>(z,t12,t34,u),
        qq2yygstu6SCnobar<1,1>(z,t12,t34,u)
    });
}


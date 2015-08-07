/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffz1Nf.h"

// Master n. 2: box6(s35,s45,s12)

// Coefficient order epsilon^-1 of master 2
template<>
double qq2yygz1Nf<2,-1>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = pow<3>(zb)*(pow<2>(u)*(-8192+2048*pow<2>(u))+pow<2>(t34)*(-32768+8192*pow<2>(u))+t12*(t34*u*(-16384+4096*pow<2>(u))+t12*(-32768+8192*pow<2>(u)))+zb*(16384+pow<2>(t34)*(114688-12288*pow<2>(u))+4096*pow<2>(u)+t12*(t12*(114688-12288*pow<2>(u))+t34*u*(32768+4096*pow<2>(u)))+zb*(-49152+10240*pow<2>(u)+pow<2>(t34)*(-155648+8192*pow<2>(t34)+10240*pow<2>(u))+t12*(t34*(-28672*u+28672*u*pow<2>(t34))+t12*(-155648+t12*(8192*t12+28672*t34*u)+10240*pow<2>(u)+pow<2>(t34)*(8192+18432*pow<2>(u))))+zb*(53248+t12*(t34*(4096*u-45056*u*pow<2>(t34))+t12*(98304+t12*(-20480*t12-45056*t34*u)+pow<2>(t34)*(-24576-8192*pow<2>(u))-8192*pow<2>(u)))+pow<2>(t34)*(98304-20480*pow<2>(t34)-8192*pow<2>(u))-8192*pow<2>(u)+zb*(-24576+2048*pow<2>(u)+pow<2>(t34)*(-24576+16384*pow<2>(t34)+2048*pow<2>(u))+t12*(t34*(12288*u+20480*u*pow<2>(t34))+t12*(-24576+t12*(t12*(16384+10240*pow<2>(t34))+t34*(20480*u+12288*u*pow<2>(t34)))+2048*pow<2>(u)+pow<2>(t34)*(18432+10240*pow<2>(t34)+2048*pow<2>(u))))+zb*(4096+zb*pow<2>(t12)*(pow<2>(t34)*(2048+2048*pow<2>(t34))+pow<2>(t12)*pow<2>(t34)*(2048+2048*pow<2>(t34)))+t12*(t34*(-4096*u-4096*u*pow<2>(t34))+t12*((-4096-12288*pow<2>(t34))*pow<2>(t34)+t12*(t12*(-4096-12288*pow<2>(t34))+t34*(-4096*u-4096*u*pow<2>(t34)))))-4096*pow<4>(t34)))))))*pow<-1>(16+(-8+pow<2>(u))*pow<2>(u)+zb*(-96+(40-4*pow<2>(u))*pow<2>(u)+zb*(248+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))+pow<2>(u)*(-82+6*pow<2>(u))+zb*(-360+(88-4*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(40+8*pow<2>(u))+t12*(72*t34*u+t12*(40+8*pow<2>(u)))+zb*(321+t12*(-128*t34*u+t12*(-82+pow<2>(t12)-2*pow<2>(t34)-12*pow<2>(u)))+pow<2>(t34)*(-82+pow<2>(t34)-12*pow<2>(u))+(-52+pow<2>(u))*pow<2>(u)+zb*(-180+zb*(62+zb*(-12+(16-4*pow<2>(t34))*pow<2>(t34)+zb*(1+pow<2>(t12)*(-2+pow<2>(t12)-2*pow<2>(t34))+(-2+pow<2>(t34))*pow<2>(t34))+t12*(8*t34*u+t12*(16-4*pow<2>(t12)+8*pow<2>(t34))))+t12*(-48*t34*u+t12*(-52+6*pow<2>(t12)-12*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(-52+6*pow<2>(t34)-2*pow<2>(u))-2*pow<2>(u))+16*pow<2>(u)+pow<2>(t34)*(88-4*pow<2>(t34)+8*pow<2>(u))+t12*(112*t34*u+t12*(88-4*pow<2>(t12)+8*pow<2>(t34)+8*pow<2>(u)))))))));
    return todouble<my_float>(foo);
}

// Coefficient of master 2 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz1Nf<2>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz1Nf<2,-1>(zb,t12,t34,u)
    });
}


/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffz0Nf.h"

// Master n. 2: box6(s35,s45,s12)

// Coefficient order epsilon^-1 of master 2
template<>
double qq2yygz0Nf<2,-1>(const my_float& z, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (z*(8192*pow<2>(t34)*pow<2>(u)+(-8192-6144*pow<2>(u))*pow<2>(u)+t12*(t34*(16384*u*pow<2>(t34)+u*(-16384-28672*pow<2>(u)))+t12*(t12*(t12*(8192-18432*pow<2>(t34))*pow<2>(t34)+t34*(16384*u-53248*u*pow<2>(t34)))+pow<2>(t34)*(-8192+8192*pow<2>(t34)-57344*pow<2>(u))+8192*pow<2>(u)))+z*(4096+pow<2>(t34)*(-8192+4096*pow<2>(t34)-26624*pow<2>(u))+pow<2>(u)*(22528+6144*pow<2>(u))+t12*(t34*(-73728*u*pow<2>(t34)+u*(65536+36864*pow<2>(u)))+t12*(-8192+t12*(t34*(-73728*u+143360*u*pow<2>(t34))+t12*(4096+pow<2>(t34)*(-55296+73728*pow<2>(t34))))-26624*pow<2>(u)+pow<2>(t34)*(59392-55296*pow<2>(t34)+104448*pow<2>(u))))+z*(-8192+(-18432-2048*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(24576-16384*pow<2>(t34)+30720*pow<2>(u))+t12*(t34*(126976*u*pow<2>(t34)+u*(-110592-20480*pow<2>(u)))+t12*(24576+t12*(t34*(126976*u-200704*u*pow<2>(t34))+t12*(-16384+(157696-172032*pow<2>(t34))*pow<2>(t34)))+pow<2>(t34)*(-178176+157696*pow<2>(t34)-92160*pow<2>(u))+30720*pow<2>(u)))+z*(-4096+pow<2>(t34)*(-49152+20480*pow<2>(t34)-12288*pow<2>(u))+4096*pow<2>(u)+t12*(t34*(-102400*u*pow<2>(t34)+u*(94208+4096*pow<2>(u)))+t12*(-49152+t12*(t34*(-102400*u+143360*u*pow<2>(t34))+t12*(20480+pow<2>(t34)*(-243712+258048*pow<2>(t34))))-12288*pow<2>(u)+pow<2>(t34)*(288768-243712*pow<2>(t34)+40960*pow<2>(u))))+z*(16384+t12*(t34*(-24576*u+40960*u*pow<2>(t34))+t12*(81920+t12*(t12*(215040-258048*pow<2>(t34))*pow<2>(t34)+t34*(40960*u-28672*u*pow<2>(t34)))+pow<2>(t34)*(-276480+215040*pow<2>(t34)-12288*pow<2>(u))-4096*pow<2>(u)))+pow<2>(t34)*(81920-4096*pow<2>(u))-4096*pow<2>(u)+z*(-4096+6144*pow<2>(u)+pow<2>(t34)*(-73728-20480*pow<2>(t34)+6144*pow<2>(u))+t12*(t34*(-24576*u-16384*u*pow<2>(t34))+t12*(-73728+t12*(t34*(-16384*u-28672*u*pow<2>(t34))+t12*(-20480+pow<2>(t34)*(-100352+172032*pow<2>(t34))))+6144*pow<2>(u)+pow<2>(t34)*(161792-100352*pow<2>(t34)+6144*pow<2>(u))))+z*(-8192+t12*(t34*(20480*u+12288*u*pow<2>(t34))+t12*(24576+t12*(t34*(12288*u+20480*u*pow<2>(t34))+t12*(16384+(14336-73728*pow<2>(t34))*pow<2>(t34)))+pow<2>(t34)*(-59392+14336*pow<2>(t34)-2048*pow<2>(u))-2048*pow<2>(u)))+pow<2>(t34)*(24576+16384*pow<2>(t34)-2048*pow<2>(u))-2048*pow<2>(u)+z*(4096+z*pow<2>(t12)*((-2048-2048*pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(-2048-2048*pow<2>(t34))*pow<2>(t34))+t12*(t34*(-4096*u-4096*u*pow<2>(t34))+t12*(pow<2>(t34)*(14336+6144*pow<2>(t34))+t12*(t34*(-4096*u-4096*u*pow<2>(t34))+t12*(-4096+pow<2>(t34)*(6144+18432*pow<2>(t34))))))-4096*pow<4>(t34)))))))))+t12*(8192*t34*pow<3>(u)+t12*(12288*pow<2>(t34)*pow<2>(u)+t12*(8192*u*pow<3>(t34)+2048*t12*pow<4>(t34))))+2048*pow<4>(u))*pow<-1>(1+t12*(-8*t34*u+t12*(-2+pow<2>(t12)-2*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(-2+pow<2>(t34)-2*pow<2>(u))+(-2+pow<2>(u))*pow<2>(u)+z*(4-4*pow<2>(u)+pow<2>(t34)*(-4*pow<2>(t34)+4*pow<2>(u))+t12*(8*t34*u+t12*(-4*pow<2>(t12)+8*pow<2>(t34)+4*pow<2>(u)))+z*(6+t12*(8*t34*u+t12*(4+6*pow<2>(t12)-12*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(4+6*pow<2>(t34)-2*pow<2>(u))-2*pow<2>(u)+z*(4+z*(1+pow<2>(t12)*(-2+pow<2>(t12)-2*pow<2>(t34))+(-2+pow<2>(t34))*pow<2>(t34))+t12*(-8*t34*u+t12*(-4*pow<2>(t12)+8*pow<2>(t34)))-4*pow<4>(t34)))))*pow<-4>(z);
    return todouble<my_float>(foo);
}

// Coefficient of master 2 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygz0Nf<2>(const my_float& z, const my_float& t12, const my_float& t34, const my_float& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygz0Nf<2,-1>(z,t12,t34,u)
    });
}


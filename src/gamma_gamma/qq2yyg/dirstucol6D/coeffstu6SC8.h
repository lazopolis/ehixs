/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6SC.h"

// Master type 8: box6(s13,s34,s25)

// Coefficient order epsilon^-1 of master type 8
template<>
double qq2yygstu6SC<8,-1>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (-160+u*(-48+u*(48+u*(8+(-2+u)*u)))+t12*(160+u*(128+u*(16+2*pow<2>(u))))+t34*(160+u*(128+u*(16+2*pow<2>(u))))+zb*(192+u*(-32+u*(-48+8*u))+t12*(-384+t12*(192+(64-16*u)*u)+t34*(-16+u*(96+u*(40+(-8-u)*u)))+u*(-160-8*pow<2>(u)))+t34*(-320+t34*(128+u*(96-8*pow<2>(u)))+u*(-224+(8-4*u)*pow<2>(u)))+zb*(112+u*(104+(-12-2*u)*u)+zb*(-144-24*u+t34*(16+u*(-8+4*u)+t34*(144+32*u+t34*(-16-8*t34*u+(8-4*u)*u)))+t12*(-32+24*u+t34*(8+(56-6*u)*u+t34*(-64-40*u+t34*(8+(-8-6*u)*u)))+t12*(256-8*u+t34*(-96-56*u+t34*(-16+8*u))+t12*(-96+16*t12+8*u+t34*(8+u*(8+2*u)))))+zb*(-2+u+t34*(2+t34*(4-2*u+t34*(-4+t34*(-2+2*t34+u))))+t12*(10-4*u+t34*(16+t34*(-12+t34*(-16+t34*(2+4*u))))+t12*(-20+6*u+t34*(-12+t34*(-4+20*t34+6*u))+t12*(20-4*u+t12*(-10+2*t12-6*t34+u)+(12-4*u)*pow<2>(t34))))+t12*zb*(t34*(-1+(2-pow<2>(t34))*pow<2>(t34))+t12*(t34*(4-4*pow<2>(t34))+t12*(t12*(4*t34-t12*t34)+t34*(-6+2*pow<2>(t34)))))))+t12*(240+u*(16+u*(4+4*u))+t34*(-192*u+t34*(80+u*(80+u*(12+4*u))))+t12*(-432+u*(-56+(12-2*u)*u)+t34*(112+u*(96+4*u))+t12*(80-4*pow<2>(u))))+t34*(144+(96-4*u)*u+t34*(-272+u*(-120+u*(-12+6*u))+t34*(16+12*pow<2>(u)))))))*pow<-1>(zb)*pow<-1>(-16+u*(-16+(4+u)*pow<2>(u))+t34*(16+u*(16+(-4-u)*pow<2>(u)))+pow<2>(t12)*(16+u*(16+(-4-u)*pow<2>(u))+t34*(-16+u*(-16+(4+u)*pow<2>(u))))+zb*(32+u*(24-2*pow<2>(u))+pow<2>(t34)*(-32+u*(-24+2*pow<2>(u)))+zb*(-24-12*u+t34*(-24-12*u+t34*(24+12*u+t34*(24+12*u)))+zb*(8+2*u+t34*(16+4*u+(-16+t34*(-8-2*u)-4*u)*pow<2>(t34))+zb*(-1+t34*(-3+t34*(-2+t34*(2+t34*(3+t34))))+t12*(2+t12*(1+t34*(3+t34*(2+t34*(-2+(-3-t34)*t34)))+t12*(-4+t34*(-4+t34*(2+t34*(4+2*t34)))+t12*(1-t34+t12*(2+t12*(-1+t34)-2*pow<2>(t34)))))+t34*(4+(-4-2*t34)*pow<2>(t34))))+t12*(-12+t34*(-12+t34*(12+12*t34))+t12*(-8-8*u+t34*(-16-4*u+t34*(6*u+t34*(16+4*u+t34*(8+2*u))))+t12*(16+t34*(8+(-12-12*t34)*t34-4*u)+4*u+t12*(6*u+t12*(-4-4*u+t34*(4+4*u))-6*u*pow<2>(t34))))))+t12*(24-6*pow<2>(u)+pow<2>(t34)*(-24+6*pow<2>(u))+t12*(24+u*(24+6*u)+t34*(24+t34*(-24+t34*(-24-12*u)-12*u)-6*pow<2>(u))+t12*(-24+t12*((-12-6*u)*u+t34*u*(12+6*u))+pow<2>(t34)*(24-6*pow<2>(u))+6*pow<2>(u)))))+t12*(-16+(12+4*u)*pow<2>(u)+t34*(16+(-12-4*u)*pow<2>(u))+t12*(-32+pow<2>(t34)*(32+u*(24-2*pow<2>(u)))+u*(-24+2*pow<2>(u))+t12*(16+(-12-4*u)*pow<2>(u)+t34*(-16+(12+4*u)*pow<2>(u)))))));
    return todouble<my_float>(foo);
}

// Coefficient of master type 8 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6SC<8>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygstu6SC<8,-1>(zb,t12,t34,u)
    });
}

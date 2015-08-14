/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "qq2yyg/qq2yyg1.h"

// Box n. type 4: box6(s13,s35,s24)

// Coefficient order epsilon^-1 of box type 4
template<>
template<>
double qq2yyg1<TT>::SC::box::c<4,-1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (-24+u*(-12+(-2-u)*u)+t34*(-24+u*(-16+6*u)+t34*(16+32*t34+8*u))+t12*(40+u*(8+u*(-10+2*u))+t34*(-32+16*u+t34*(-48+24*u))+t12*(24+t12*(-40+(8-2*u)*u)+u*(-4+(6-u)*u)+t34*(56+u*(-32+2*u))))+zb*(16+(4-2*u)*u+t34*(8+t34*(-24*t34-8*u)+u*(4+2*u))+t12*(-40+u*(-4+6*u)+t34*(52+(16-8*t34)*t34+(-4-3*u)*u)+t12*(-8+(8-6*u)*u+t34*(-56+t34*(16-8*u)+20*u)+t12*(40+u*(-12+2*u)+t12*(-8+4*u)+t34*(-4+(-4+u)*u))))+zb*(-2-u+t34*(6+t34*(-6+2*t34-u)+2*u)+t12*(6+t34*(-24+18*t34-4*u)+4*u+t12*(-4-6*u+t34*(24+t34*(-22+6*t34+3*u))+t12*(-4+t12*(6-2*t12+t34*(2-2*u)-u)+4*u+t34*(-8+t34*(2-2*u)+4*u))))+t12*(t34*(1+(-2+t34)*t34)+t12*(t34*(-4+(6-2*t34)*t34)+t12*(t34*(6+(-6+t34)*t34)+t12*(t12*t34+t34*(-4+2*t34)))))*zb)))*pow<-1>(zb)*pow<-1>(-4+zb*(4+t12*(-8+t34*(8*t34-2*u)+2*u+t12*(-4*u+4*t34*u+t12*(8+t12*(-4+t34*(4*t34-4*u)+4*u+t12*(-2*u+2*t34*u))-8*pow<2>(t34))))-4*pow<2>(t34)+zb*(-1+t34*(-1+t34*(1+t34))+t12*(2+t34*(2+(-2-2*t34)*t34)+t12*(1-t34+t12*(-4+t12*(1+t12*(2+t12*(-1+t34)-2*t34)+t34*(1+(-1-t34)*t34))+(2+2*t34)*pow<2>(t34))))))+t34*(4-pow<2>(u))+pow<2>(u)+t12*(8-2*pow<2>(u)+t34*(-8+2*pow<2>(u))+pow<2>(t12)*(-8+t34*(8-2*pow<2>(u))+t12*(4+t34*(-4+pow<2>(u))-pow<2>(u))+2*pow<2>(u))));
    return todouble(foo);
}

// Coefficient of box type 4 as a series in epsilon
template<>
template<>
EpsExp qq2yyg1<TT>::SC::box::c<4>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    return EpsExp(-1,{
        qq2yyg1<TT>::SC::box::c<4,-1>(zb,t12,t34,u)
    });
}


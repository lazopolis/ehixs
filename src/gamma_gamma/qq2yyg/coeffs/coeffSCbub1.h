/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "qq2yyg/qq2yyg.h"

// Bubble type 1: bubble(s12)

// Coefficient order epsilon^-1 of bubble type 1
template<>
template<>
double qq2yyg1<TT>::SC::bub::c<1,-1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (-256+zb*(512+zb*(-384-384*pow<2>(t34)+96*pow<2>(t12)*pow<2>(u)+zb*(128+384*pow<2>(t34)+zb*(-144+(-96-16*pow<2>(t34))*pow<2>(t34)+144*pow<4>(t12)))))+16*pow<4>(u))*pow<-1>(zb)*pow<-1>(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t12)*(16+(-8+pow<2>(u))*pow<2>(u))+zb*(32-8*pow<2>(u)+pow<2>(t12)*(-32+8*pow<2>(u))+zb*(-24+t12*(16*t34*u+t12*(32+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(8+2*pow<2>(u))+zb*(8-8*pow<2>(t34)+t12*(-8*t34*u+t12*(-16+t12*(8*t12+8*t34*u)+8*pow<2>(t34)))+zb*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))));
    return todouble(foo);
}

// Coefficient order epsilon^0 of bubble type 1
template<>
template<>
double qq2yyg1<TT>::SC::bub::c<1,0>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (1280+(-128-48*pow<2>(u))*pow<2>(u)+zb*(-2560+zb*(2944+zb*(-1664+t12*(896*t12+256*t34*u)-896*pow<2>(t34)+zb*(720+pow<2>(t12)*(-288-432*pow<2>(t12)-224*pow<2>(t34))+pow<2>(t34)*(480+80*pow<2>(t34))))+t12*(-512*t34*u+t12*(-896-288*pow<2>(u)))+pow<2>(t34)*(896-32*pow<2>(u))-224*pow<2>(u))+128*pow<2>(u)))*pow<-1>(zb)*pow<-1>(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t12)*(16+(-8+pow<2>(u))*pow<2>(u))+zb*(32-8*pow<2>(u)+pow<2>(t12)*(-32+8*pow<2>(u))+zb*(-24+t12*(16*t34*u+t12*(32+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(8+2*pow<2>(u))+zb*(8-8*pow<2>(t34)+t12*(-8*t34*u+t12*(-16+t12*(8*t12+8*t34*u)+8*pow<2>(t34)))+zb*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))));
    return todouble(foo);
}

// Coefficient order epsilon^1 of bubble type 1
template<>
template<>
double qq2yyg1<TT>::SC::bub::c<1,1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (-2048+pow<2>(u)*(384+32*pow<2>(u))+zb*(4096-384*pow<2>(u)+zb*(-5376+zb*(3328+t12*(-1664*t12-256*t34*u)+768*pow<2>(t34)+zb*(-1152+(-768-128*pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(864+288*pow<2>(t12)+416*pow<2>(t34))))+416*pow<2>(u)+pow<2>(t34)*(-768+96*pow<2>(u))+t12*(512*t34*u+t12*(1664+192*pow<2>(u))))))*pow<-1>(zb)*pow<-1>(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t12)*(16+(-8+pow<2>(u))*pow<2>(u))+zb*(32-8*pow<2>(u)+pow<2>(t12)*(-32+8*pow<2>(u))+zb*(-24+t12*(16*t34*u+t12*(32+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(8+2*pow<2>(u))+zb*(8-8*pow<2>(t34)+t12*(-8*t34*u+t12*(-16+t12*(8*t12+8*t34*u)+8*pow<2>(t34)))+zb*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))));
    return todouble(foo);
}

// Coefficient of bubble type 1 as a series in epsilon
template<>
template<>
EpsExp qq2yyg1<TT>::SC::bub::c<1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    return EpsExp(-1,{
        qq2yyg1<TT>::SC::bub::c<1,-1>(zb,t12,t34,u),
        qq2yyg1<TT>::SC::bub::c<1,0>(zb,t12,t34,u),
        qq2yyg1<TT>::SC::bub::c<1,1>(zb,t12,t34,u)
    });
}


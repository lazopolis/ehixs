/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "qq2yyg/qq2yyg.h"

// Box type 6: box6(s34,s35,s12)

// Coefficient order epsilon^-1 of box type 6
template<>
template<>
double qq2yyg1<TT>::SC::box::c<6,-1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (-48+t12*(128*t12+48*u+80*t34*u)+28*pow<2>(u)+t34*(-32+t34*(80+4*pow<2>(u)))+zb*(48+t12*(t12*(-176+48*t34)-56*u+t34*(-32*u+24*t34*u))+zb*(4+t34*(-16+t34*(24+t34*(-16+4*t34)))+t12*(t12*(60+t34*(-32+4*t34))+12*u+t34*(4*u+t34*(-12*u-4*t34*u)))+t34*(-12+t34*(8+4*t34))*zb*pow<2>(t12))-12*pow<2>(u)+t34*(48+8*pow<2>(u)+t34*(-112+16*t34+4*pow<2>(u)))))*pow<-1>(zb)*pow<-1>(4+zb*(-4+t34*(4+(4-4*t34)*t34)+t12*(-2*u+t34*(4*u-2*t34*u)+t12*(4+t34*(-4+t34*(-4+4*t34))+t12*(2*u+t34*(-4*u+2*t34*u))))+zb*(1+pow<2>(t12)*(-2+(1+(-2+t34)*t34)*pow<2>(t12)+t34*(2+t34*(1-pow<2>(t34))))+(-2+pow<2>(t34))*pow<2>(t34)))-pow<2>(u)+pow<2>(t12)*(-4+t34*(8+t34*(-4+pow<2>(u))-2*pow<2>(u))+pow<2>(u))+t34*(-8+t34*(4-pow<2>(u))+2*pow<2>(u)));
    return todouble(foo);
}

// Coefficient of box type 6 as a series in epsilon
template<>
template<>
EpsExp qq2yyg1<TT>::SC::box::c<6>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    return EpsExp(-1,{
        qq2yyg1<TT>::SC::box::c<6,-1>(zb,t12,t34,u)
    });
}


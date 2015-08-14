/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "qq2yyg/qq2yyg1.h"

// Box type 3: box6(s15,s25,s34)

// Coefficient order epsilon^-1 of box type 3
template<>
template<>
double qq2yyg1<TT>::LC::box::c<3,-1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (256+zb*(-512+zb*(384+384*pow<2>(t34)-96*pow<2>(t12)*pow<2>(u)+zb*(-128-384*pow<2>(t34)+zb*(16+pow<2>(t34)*(96+16*pow<2>(t34))-16*pow<4>(t12)))))-16*pow<4>(u))*pow<-1>(zb)*pow<-1>(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t12)*(16+(-8+pow<2>(u))*pow<2>(u))+zb*(32-8*pow<2>(u)+pow<2>(t12)*(-32+8*pow<2>(u))+zb*(-24+t12*(16*t34*u+t12*(32+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))))+2*pow<2>(u)+pow<2>(t34)*(8+2*pow<2>(u))+zb*(8-8*pow<2>(t34)+t12*(-8*t34*u+t12*(-16+t12*(8*t12+8*t34*u)+8*pow<2>(t34)))+zb*(-1+(2-pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(3+pow<2>(t12)*(-3+pow<2>(t12)-2*pow<2>(t34))+pow<4>(t34)))))));
    return todouble(foo);
}

// Coefficient of box type 3 as a series in epsilon
template<>
template<>
EpsExp qq2yyg1<TT>::LC::box::c<3>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    return EpsExp(-1,{
        qq2yyg1<TT>::LC::box::c<3,-1>(zb,t12,t34,u)
    });
}


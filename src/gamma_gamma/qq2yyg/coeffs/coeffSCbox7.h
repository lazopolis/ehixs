/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "qq2yyg/qq2yyg1.h"

// Box type 7: box6(s35,s45,s12)

// Coefficient order epsilon^-1 of box type 7
template<>
template<>
double qq2yyg1<TT>::SC::box::c<7,-1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (64*pow<2>(t12)+64*pow<2>(t34)+zb*(-64+t12*(-64*t12+96*t34*u)-64*pow<2>(t34)+zb*(64-32*t12*t34*u+32*zb*pow<2>(t12)*pow<2>(t34)))+32*pow<2>(u))*pow<3>(zb)*pow<-1>(16+(-8+pow<2>(u))*pow<2>(u)+zb*(-64+(24-2*pow<2>(u))*pow<2>(u)+zb*(104+t12*(-16*t34*u+t12*(-8-2*pow<2>(u)))+pow<2>(t34)*(-8-2*pow<2>(u))+(-26+pow<2>(u))*pow<2>(u)+zb*(-88+zb*(41+zb*(-10+(12-2*pow<2>(t34))*pow<2>(t34)+zb*(1+pow<2>(t12)*(-2+pow<2>(t12)-2*pow<2>(t34))+(-2+pow<2>(t34))*pow<2>(t34))+t12*(8*t34*u+t12*(12-2*pow<2>(t12)+4*pow<2>(t34))))+t12*(-32*t34*u+t12*(-26+pow<2>(t12)-2*pow<2>(t34)-2*pow<2>(u)))+pow<2>(t34)*(-26+pow<2>(t34)-2*pow<2>(u))-2*pow<2>(u))+12*pow<2>(u)+pow<2>(t34)*(24+4*pow<2>(u))+t12*(40*t34*u+t12*(24+4*pow<2>(u)))))));
    return todouble(foo);
}

// Coefficient of box type 7 as a series in epsilon
template<>
template<>
EpsExp qq2yyg1<TT>::SC::box::c<7>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    return EpsExp(-1,{
        qq2yyg1<TT>::SC::box::c<7,-1>(zb,t12,t34,u)
    });
}


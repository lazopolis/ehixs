/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "qq2yyg/qq2yyg.h"

// Box type 2: box6(s12,s15,s34)

// Coefficient order epsilon^-1 of box type 2
template<>
template<>
double qq2yyg1<TT>::SC::box::c<2,-1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (-64+(96-20*pow<2>(u))*pow<2>(u)+t34*(u*(96+(-16-2*pow<2>(u))*pow<2>(u))+t34*(64+t34*u*(64-16*pow<2>(u))+pow<2>(u)*(-64+12*pow<2>(u))))+t12*(256+t12*(320+t34*u*(64-16*pow<2>(u))+(-64-4*pow<2>(u))*pow<2>(u))+t34*(u*(224+(-48-2*pow<2>(u))*pow<2>(u))+t34*(64+pow<2>(u)*(-32+4*pow<2>(u))))-16*pow<4>(u))+zb*(192+pow<2>(u)*(-128+4*pow<2>(u))+pow<2>(t34)*(-256+(112-4*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(64+16*pow<2>(u)))+zb*(-128+t34*(-128*u+t34*(448+t34*(112*u+t34*(-320+16*t34*u-8*pow<2>(u)))-24*pow<2>(u)))+32*pow<2>(u)+t12*(320-16*pow<2>(u)+t34*(u*(112-4*pow<2>(u))+t34*(-160+8*pow<2>(u)+t34*(u*(-112+4*pow<2>(u))+t34*(-160+8*pow<2>(u)))))+t12*(576-88*pow<2>(u)+t34*(u*(-48-24*pow<2>(u))+t34*(-544+40*pow<2>(u)+t34*u*(-48+4*pow<2>(u))))+t12*(64+t12*(-64-48*t34*u-24*pow<2>(u))-96*pow<2>(u)+t34*(u*(-144-20*pow<2>(u))+t34*(-96+24*pow<2>(u))))))+zb*(16+zb*(4+t34*(-6*u+t34*(20+t34*(4*u+t34*(-20+t34*(-4*t34+2*u)))))+t12*(16+t34*(-10*u+t34*(4+t34*(t34*(-8+t34*(-12*t34-2*u))+12*u)))+t12*(20+t34*(-4*u+t34*(-16+t34*(8*u+t34*(-4-4*t34*u))))+t12*(t34*(-12*u+t34*(-24+t34*(24*t34+12*u)))+t12*(-20+t12*(-16-4*t12+t34*(4*t34-10*u))+t34*(-22*u+t34*(12+12*t34*u))))))+t12*zb*(pow<2>(t34)*(6+(-4-2*pow<2>(t34))*pow<2>(t34))+t12*(pow<2>(t34)*(14+(-12-2*pow<2>(t34))*pow<2>(t34))+t12*((8-8*pow<2>(t34))*pow<2>(t34)+pow<2>(t12)*(2*pow<2>(t34)+2*t12*pow<2>(t34))))))-4*pow<2>(u)+t34*(64*u+t34*(-176+t34*(-64*u+t34*(176-16*pow<2>(t34)+4*pow<2>(u)))))+t12*(-112-4*pow<2>(u)+t34*(48*u+t34*(-16+t34*(-48*u+t34*(128+4*pow<2>(u)))))+t12*(-224+t34*(16*u+t34*(128+t34*(96*t34-16*u)-12*pow<2>(u)))+12*pow<2>(u)+t12*(-32+(64+32*t34*u)*pow<2>(t34)+20*pow<2>(u)+t12*(80+t12*(16+16*t34*u)+8*pow<2>(u)+t34*(48*u+t34*(-32+12*pow<2>(u)))))))))+t12*(-448+t34*(u*(-256+32*pow<2>(u))+t34*(288+192*t34*u+(96-2*pow<2>(u))*pow<2>(u)))+t12*(-704+pow<2>(u)*(112+8*pow<2>(u))+t12*(-64+48*pow<2>(u)+t34*u*(128+16*pow<2>(u)))+t34*(u*(64+48*pow<2>(u))+t34*(480+pow<2>(u)*(32+2*pow<2>(u)))))+12*pow<4>(u))))*pow<-1>(zb)*pow<-1>(16+(-8+pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-16+(8-pow<2>(u))*pow<2>(u))+t12*(16+(-8+pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-16+(8-pow<2>(u))*pow<2>(u))+t12*(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t34)*(16+(-8+pow<2>(u))*pow<2>(u))+t12*(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t34)*(16+(-8+pow<2>(u))*pow<2>(u)))))+zb*(-32+pow<2>(t34)*(32-8*pow<2>(u))+8*pow<2>(u)+t12*(-32+pow<2>(t34)*(32-8*pow<2>(u))+8*pow<2>(u)+t12*(32-8*pow<2>(u)+pow<2>(t34)*(-32+8*pow<2>(u))+t12*(32-8*pow<2>(u)+pow<2>(t34)*(-32+8*pow<2>(u)))))+zb*(24+zb*(-8+(16-8*pow<2>(t34))*pow<2>(t34)+t12*(-8+t34*(t34*(16+t34*(-8*t34-8*u))+8*u)+t12*(16+t34*(t34*(-24+t34*(8*t34-8*u))+8*u)+t12*(16+t34*(-8*u+t34*(-24+t34*(8*t34+8*u)))+t12*(-8+t34*(-8*u+t34*(8+8*t34*u))+t12*(-8+8*pow<2>(t34))))))+zb*(1+pow<2>(t34)*(-3+(3-pow<2>(t34))*pow<2>(t34))+t12*(1+pow<2>(t34)*(-3+(3-pow<2>(t34))*pow<2>(t34))+t12*(-3+pow<2>(t34)*(3+(-1+pow<2>(t34))*pow<2>(t34))+t12*(-3+pow<2>(t34)*(3+(-1+pow<2>(t34))*pow<2>(t34))+t12*(3+(-1-2*pow<2>(t34))*pow<2>(t34)+t12*(3+(-1-2*pow<2>(t34))*pow<2>(t34)+t12*(-1+t12*(-1+pow<2>(t34))+pow<2>(t34)))))))))-2*pow<2>(u)+pow<2>(t34)*(-32+pow<2>(t34)*(8+2*pow<2>(u)))+t12*(24-2*pow<2>(u)+t34*(-16*u+t34*(-32+t34*(16*u+t34*(8+2*pow<2>(u)))))+t12*(-32+t34*(-16*u+t34*(40+t34*(16*u+t34*(-8-2*pow<2>(u)))+2*pow<2>(u)))+t12*(-32+t34*(16*u+t34*(40+t34*(-16*u+t34*(-8-2*pow<2>(u)))+2*pow<2>(u)))+t12*(8+t34*(16*u+t34*(-8-16*t34*u-2*pow<2>(u)))+2*pow<2>(u)+t12*(8+pow<2>(t34)*(-8-2*pow<2>(u))+2*pow<2>(u)))))))));
    return todouble(foo);
}

// Coefficient of box type 2 as a series in epsilon
template<>
template<>
EpsExp qq2yyg1<TT>::SC::box::c<2>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    return EpsExp(-1,{
        qq2yyg1<TT>::SC::box::c<2,-1>(zb,t12,t34,u)
    });
}


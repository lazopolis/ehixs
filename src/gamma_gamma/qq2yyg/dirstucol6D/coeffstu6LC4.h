/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6LC.h"

// Master type 4: box6(s13,s15,s24)

// Coefficient order epsilon^-1 of master type 4
template<>
double qq2yygstu6LC<4,-1>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (32+u*(16+u*(-48+u*(-24+u*(10+5*u))))+t12*(32+u*(64+u*(48+(-16-14*u)*u))+t34*(256+u*(-64+u*(-96+u*(16+8*u)))+t34*(224+u*(-128+u*(-48+(32-2*u)*u))))+t12*(-192+u*(-96+u*(48+24*u))+t34*(-192+u*(96+(48-24*u)*u))+t12*(128-32*pow<2>(u))))+t34*(-96+u*(-32+u*(-16+(10+2*u)*pow<2>(u)))+t34*(-288+t34*(-160+u*(32+u*(32+u*(-8+2*u))))+u*(-16+u*(64+(2+u)*pow<2>(u)))))+zb*(-128+u*(-16+u*(128+u*(40+(-16-5*u)*u)))+t12*(-96+u*(-256+u*(-144+u*(64+34*u)))+t12*(704+u*(304+(-160-84*u)*u)+t12*(-480-64*t12*u+u*(96+136*u)+t34*(-160+(96-40*u)*u))+t34*(896+u*(-288+u*(-96+56*u))+t34*(192+u*(-208+(64-4*u)*u))))+t34*(-912+u*(96+u*(184+(8-5*u)*u))+t34*(-1056+u*(544+u*(168+u*(-48+4*u)))+t34*(-240+u*(192+u*(-16+(8-u)*u))))))+zb*(192+u*(-32+u*(-120+u*(-16+6*u)))+t12*(128+u*(376+u*(136+(-66-20*u)*u))+t12*(-1040+u*(-368+u*(216+90*u))+t12*(704+(-272-208*u)*u+t12*(48-32*t12+t34*(48-8*u)+200*u)+t34*(448+u*(-240+72*u)+t34*(32+(32-8*u)*u)))+t34*(-1616+u*(328+(104-40*u)*u)+t34*(-608+u*(488+u*(-96+2*u))+t34*(-32+u*(32+(16-4*u)*u)))))+t34*(1264+u*(16+u*(-120+(-24-3*u)*u))+t34*(1808+u*(-832+u*(-172+(36-2*u)*u))+t34*(720+t34*(48+u*(-24+(12-2*u)*u))+u*(-496+u*(48+(-8+u)*u))))))+zb*(-136+u*(56+46*u)+t34*(232+u*(280+58*u)+t34*(1568+(396-40*u)*u+t34*(2096+(144-72*u)*u+t34*(1112+(-64-22*u)*u+t34*(232+t34*(16-4*u)+(-40-2*u)*u)))))+t12*(-96+u*(-248+u*(-40+18*u))+t34*(-848+u*(-80+32*u)+t34*(-1400+u*(568+(52-20*u)*u)+t34*(-712+(440-32*u)*u+t34*(-40+t34*(24-8*u)+u*(32+u*(-12+2*u))))))+t12*(776+u*(196+(-122-30*u)*u)+t12*(-504+u*(280+124*u)+t12*(-128+t12*(88+8*t34)-220*u+t34*(-128+8*u-12*t34*u))+t34*(-432+(224-54*u)*u+t34*(-64-40*u+t34*(-24+(16-6*u)*u))))+t34*(1392+u*(-192+u*(-44+8*u))+t34*(688+u*(-372+u*(56+2*u))+t34*(96+t34*(24+(8-6*u)*u)+u*(-24+u*(-28+4*u)))))))+zb*(-18+(-61-6*u)*u+t34*(-254+(-222-14*u)*u+t34*(-794+(-299-4*u)*u+t34*(-1062+u*(-172+12*u)+t34*(-694+u*(-27+10*u)+t34*(-218+u*(10+2*u)+t34*(-30-2*t34+3*u))))))+t12*(38+70*u+t34*(264+40*u+t34*(442-162*u+t34*(192-152*u+t34*(-86-6*u+t34*(-72+16*u+t34*(-10+2*u))))))+t12*(-292+u*(-36+18*u)+t34*(-552+(56-12*u)*u+t34*(-300+(92-24*u)*u+t34*(-92+u*(-8+12*u)+t34*(-64-12*t34+u*(-8+6*u)))))+t12*(172+(-110-20*u)*u+t34*(152+u*(-64+22*u)+t34*(36+u*(20+8*u)+t34*(48+t34*(8-6*u)+u*(-32+6*u))))+t12*(182+t12*(-82+(-16-6*t34)*t34)+121*u+t34*(310+78*u+t34*(198+101*u+t34*(70+28*u)))))))+zb*(58+21*u+t34*(198+70*u+t34*(282+87*u+t34*(246+52*u+t34*(158+19*u+t34*(66+6*u+t34*(14+2*t34+u))))))+t12*(-6-6*u+t34*(-19-8*u+t34*(4+10*u+t34*(79+t34*(114+t34*(67+t34*(16+t34-2*u)-8*u)-2*u)+16*u)))+t12*(44+t34*(66+t34*(14+t34*(24+t34*(52+t34*(22+2*t34)))))+t12*(-20+t12*(-102+t12*(26+t34*(3+(4-t34)*t34))+t34*(-224+t34*(-196+t34*(-76-2*t34-12*u)-41*u)-30*u)-21*u)+6*u+t34*(-8-16*u+t34*(-4-12*u+t34*(-24+16*u+t34*(-8+6*u)))))))+t12*zb*(t34*(-21+t34*(-70+t34*(-87+t34*(-52+t34*(-19+(-6-t34)*t34)))))+t12*(t34*(6+t34*(14+t34*(4+t34*(-12+(-10-2*t34)*t34))))+(t34*(-6+t34*(-2+t34*(6+2*t34)))+t12*t34*(21+t34*(50+t34*(49+16*t34))))*pow<2>(t12))))))+t34*(-400+t34*(-1984+u*(-344+u*(156+(4-6*u)*u))+t34*(-2080+t34*(-768+t34*(-80+24*u)+u*(120+(12-4*u)*u))+u*(24+u*(140+(-16-2*u)*u))))+u*(-304+u*(-92+2*pow<2>(u)))))+t34*(320+u*(160+u*(64+(-12-2*u)*pow<2>(u)))+t34*(1216+u*(128+u*(-176+u*(-4+(4-u)*u)))+t34*(960+u*(-96+u*(-112+24*u))+t34*(192+u*(-48+4*pow<2>(u))))))))*pow<-1>(zb)*pow<-1>(16+(-8+pow<2>(u))*pow<2>(u)+t34*(48+pow<2>(u)*(-24+3*pow<2>(u))+t34*(48+pow<2>(u)*(-24+3*pow<2>(u))+t34*(16+(-8+pow<2>(u))*pow<2>(u))))+pow<2>(t12)*(-16+(8-pow<2>(u))*pow<2>(u)+t34*(-48+(24-3*pow<2>(u))*pow<2>(u)+t34*(-48+(24-3*pow<2>(u))*pow<2>(u)+t34*(-16+(8-pow<2>(u))*pow<2>(u)))))+zb*(-48+(16-pow<2>(u))*pow<2>(u)+t34*(-144+(48-3*pow<2>(u))*pow<2>(u)+t34*(-144+(48-3*pow<2>(u))*pow<2>(u)+t34*(-48+(16-pow<2>(u))*pow<2>(u))))+pow<2>(t12)*(48+(-16+pow<2>(u))*pow<2>(u)+t34*(144+pow<2>(u)*(-48+3*pow<2>(u))+t34*(144+pow<2>(u)*(-48+3*pow<2>(u))+t34*(48+(-16+pow<2>(u))*pow<2>(u)))))+zb*(56+t34*(168+t34*(160+t34*(32+t34*(-24+t34*(-8-2*pow<2>(u))-6*pow<2>(u))-16*pow<2>(u))-32*pow<2>(u))-30*pow<2>(u))-10*pow<2>(u)+zb*(-32+zb*(9+t34*(27+t34*(17+t34*(-21+t34*(-29+t34*(-7+t34*(3+t34))))))+zb*(-1+t34*(-3+t34*(-1+t34*(5+t34*(5+t34*(-1+(-3-t34)*t34)))))+pow<2>(t12)*(3+t34*(9+t34*(9+t34*(3+t34*(1+t34*(3+t34*(3+t34))))))+pow<2>(t12)*(-3+t34*(-9+t34*(-11+t34*(-9+(-6-2*t34)*t34)))+(1+t34*(3+t34*(3+t34)))*pow<2>(t12))))+t12*(t34*(-8*u+t34*(-24*u+t34*(-24*u-8*t34*u)))+t12*(-19+t34*(-57+t34*(-49+t34*(5+t34*(23+t34*(5+(-3-t34)*t34)))))+t12*(t34*(8*u+t34*(24*u+t34*(24*u+8*t34*u)))+t12*(11+t34*(33+t34*(35+t34*(17+t34*(6+2*t34))))+(-1+t34*(-3+(-3-t34)*t34))*pow<2>(t12))))))+t12*(t34*(24*u+t34*(72*u+t34*(72*u+24*t34*u)))+t12*(48+t12*(t34*(-24*u+t34*(-72*u+t34*(-72*u-24*t34*u)))+t12*(-16+t34*(-48+t34*(-48+t34*(-16-2*pow<2>(u))-6*pow<2>(u))-6*pow<2>(u))-2*pow<2>(u)))+t34*(144+t34*(128+t34*(t34*(-48+t34*(-16-2*pow<2>(u))-6*pow<2>(u))-6*pow<2>(u))-2*pow<2>(u)))))+2*pow<2>(u)+t34*(-96+6*pow<2>(u)+t34*(-80+8*pow<2>(u)+t34*(16+8*pow<2>(u)+t34*(48+6*pow<2>(u)+t34*(16+2*pow<2>(u)))))))+t12*(t34*(-16*u+t34*(-48*u+t34*(-48*u-16*t34*u)))+t12*(-64+8*pow<2>(u)+t12*(t34*(16*u+t34*(48*u+t34*(48*u+16*t34*u)))+t12*(8+2*pow<2>(u)+t34*(24+6*pow<2>(u)+t34*(24+6*pow<2>(u)+t34*(8+2*pow<2>(u))))))+t34*(-192+24*pow<2>(u)+t34*(-184+26*pow<2>(u)+t34*(-40+14*pow<2>(u)+t34*(24+6*pow<2>(u)+t34*(8+2*pow<2>(u)))))))))));
    return todouble<my_float>(foo);
}

// Coefficient of master type 4 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6LC<4>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    return Expansion<Parameter::epsilon, double>(-1,{
        qq2yygstu6LC<4,-1>(zb,t12,t34,u)
    });
}


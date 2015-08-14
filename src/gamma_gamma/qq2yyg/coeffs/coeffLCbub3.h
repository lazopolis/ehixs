/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "qq2yyg/qq2yyg.h"

// Bubble type 3: bubble(s34)

// Coefficient order epsilon^0 of bubble type 3
template<>
template<>
double qq2yyg1<TT>::LC::bub::c<3,0>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (16384+pow<2>(u)*(-16384+pow<2>(u)*(6144+pow<2>(u)*(-1024+64*pow<2>(u))))+zb*(-81920+pow<2>(u)*(65536+pow<2>(u)*(-18432+(2048-64*pow<2>(u))*pow<2>(u)))+zb*(163840+pow<2>(t34)*(20480+pow<2>(u)*(-13312+(2816-192*pow<2>(u))*pow<2>(u)))+pow<2>(u)*(-99328+pow<2>(u)*(19200+pow<2>(u)*(-1216+16*pow<2>(u))))+t12*(t12*(8192+pow<2>(u)*(-4096+512*pow<2>(u)))+t34*u*(8192+pow<2>(u)*(-4096+512*pow<2>(u))))+zb*(-163840+pow<2>(u)*(68608+pow<2>(u)*(-7680+192*pow<2>(u)))+t12*(t12*(-32768+(12288-1024*pow<2>(u))*pow<2>(u))+t34*u*(-28672+(10240-768*pow<2>(u))*pow<2>(u)))+pow<2>(t34)*(-81920+pow<2>(u)*(39936+pow<2>(u)*(-5632+192*pow<2>(u))))+zb*(75776+pow<2>(u)*(-16128+pow<2>(u)*(256+16*pow<2>(u)))+pow<2>(t34)*(130048+pow<2>(u)*(-46336+(4160-48*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(3072+pow<2>(u)*(512+192*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(8192+2048*pow<2>(u))+u*(30720+pow<2>(u)*(-7168+384*pow<2>(u))))+t12*(59392+pow<2>(u)*(-21760+(2816-144*pow<2>(u))*pow<2>(u))+t12*(t12*(-4096+(7168-1024*pow<2>(u))*pow<2>(u))+t34*u*(10240+(4096-640*pow<2>(u))*pow<2>(u)))+pow<2>(t34)*(6144+pow<2>(u)*(12032+(-256-16*pow<2>(u))*pow<2>(u)))))+zb*(2048+pow<2>(u)*(-5632+512*pow<2>(u))+pow<2>(t34)*(-103424+(26112-1344*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-9216+(-1024-192*pow<2>(u))*pow<2>(u)))+t12*(t34*(u*pow<2>(t34)*(-20480-3072*pow<2>(u))+u*(-5120+(512-64*pow<2>(u))*pow<2>(u)))+t12*(-63488+(23040-2304*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-18432+pow<2>(u)*(-24064+256*pow<2>(u)))+t12*(t34*u*(-25600+pow<2>(u)*(-6144+320*pow<2>(u)))+t12*(12288+pow<2>(u)*(-14336+1024*pow<2>(u))))))+zb*(-20480+(4160-80*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(41728+pow<2>(u)*(-6592+160*pow<2>(u))+pow<2>(t34)*(13824+pow<2>(t34)*(-2304-64*pow<2>(u))+pow<2>(u)*(-64+48*pow<2>(u))))+t12*(t34*(u*(-8704+1024*pow<2>(u))+pow<2>(t34)*(-2560*u*pow<2>(t34)+u*(19456+1536*pow<2>(u))))+t12*(42496+pow<2>(u)*(-11392+352*pow<2>(u))+pow<2>(t34)*(15616+pow<2>(u)*(17024+144*pow<2>(u))+pow<2>(t34)*(2816+pow<2>(u)*(512+48*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(8192+512*pow<2>(u))+u*(17408+3328*pow<2>(u)))+t12*(-13312+(7488-16*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(4096+(4416-176*pow<2>(u))*pow<2>(u))+t12*(t34*u*(3584+256*pow<2>(u))+t12*(-512+768*pow<2>(u)))))))+zb*(10240-832*pow<2>(u)+t12*(t34*(pow<2>(t34)*(3840*u*pow<2>(t34)+u*(-8704-256*pow<2>(u)))+u*(3840-256*pow<2>(u)))+t12*(-17408+t12*(t34*(u*(-512-640*pow<2>(u))+u*pow<2>(t34)*(-12288-256*pow<2>(u)))+t12*(6144+t12*(t12*(1024-768*pow<2>(u))+t34*u*(-5376-128*pow<2>(u)))+pow<2>(t34)*(-8192-4416*pow<2>(u))-320*pow<2>(u)))+pow<2>(t34)*(-512+pow<2>(t34)*(-5632-512*pow<2>(u))-4992*pow<2>(u))+1920*pow<2>(u)))+pow<2>(t34)*(-6656+192*pow<2>(u)+pow<2>(t34)*(-12288+576*pow<2>(u)+pow<2>(t34)*(4608+64*pow<2>(u))))+zb*(-2240+zb*(192+pow<2>(t34)*(192+pow<2>(t34)*(-960+576*pow<2>(t34)))+zb*pow<2>(t12)*(pow<2>(t34)*(144+pow<2>(t34)*(-16+pow<2>(t34)*(-144+16*pow<2>(t34))))+pow<2>(t12)*(pow<2>(t12)*(48*pow<2>(t12)*pow<2>(t34)+(48-80*pow<2>(t34))*pow<2>(t34))+pow<2>(t34)*(-240+pow<2>(t34)*(352+16*pow<2>(t34)))))+t12*(t34*(-192*u+pow<2>(t34)*(-128*u+320*u*pow<2>(t34)))+t12*(-384+pow<2>(t34)*(512+pow<2>(t34)*(-384+256*pow<2>(t34)))+t12*(t34*(192*u+pow<2>(t34)*(-640*u-64*u*pow<2>(t34)))+t12*((-576-960*pow<2>(t34))*pow<2>(t34)+t12*(t12*(384+t12*(-192*t12-192*t34*u)-128*pow<2>(t34))+t34*(192*u-256*u*pow<2>(t34))))))))+48*pow<2>(u)+pow<2>(t34)*(-448+pow<2>(t34)*(5568+pow<2>(t34)*(-2880-16*pow<2>(u))-176*pow<2>(u))+144*pow<2>(u))+t12*(t34*(128*u+pow<2>(t34)*(1792*u-1920*u*pow<2>(t34)))+t12*(3968+48*pow<2>(u)+pow<2>(t34)*(-3328+624*pow<2>(u)+pow<2>(t34)*(3200+pow<2>(t34)*(-256-48*pow<2>(u))+144*pow<2>(u)))+t12*(t34*(-1920*u+pow<2>(t34)*(5376*u+128*u*pow<2>(t34)))+t12*(-1024-240*pow<2>(u)+pow<2>(t34)*(4672+624*pow<2>(u)+pow<2>(t34)*(960+160*pow<2>(u)))+t12*(t34*(1408*u+512*u*pow<2>(t34))+t12*(-896+t12*(192*t12+384*t34*u)+144*pow<2>(u)+pow<2>(t34)*(128+144*pow<2>(u)))))))))))))))))*pow<-1>(zb)*pow<-1>(1024+pow<2>(u)*(-1024+pow<2>(u)*(384+pow<2>(u)*(-64+4*pow<2>(u))))+pow<2>(t12)*(-1024+pow<2>(u)*(1024+pow<2>(u)*(-384+(64-4*pow<2>(u))*pow<2>(u))))+zb*(-5120+pow<2>(u)*(4096+pow<2>(u)*(-1152+(128-4*pow<2>(u))*pow<2>(u)))+pow<2>(t12)*(5120+pow<2>(u)*(-4096+pow<2>(u)*(1152+pow<2>(u)*(-128+4*pow<2>(u)))))+zb*(11520+pow<2>(t34)*(-1024+pow<2>(u)*(256+(64-16*pow<2>(u))*pow<2>(u)))+pow<2>(u)*(-7168+pow<2>(u)*(1440+(-96+pow<2>(u))*pow<2>(u)))+t12*(t34*u*(-2048+(1024-128*pow<2>(u))*pow<2>(u))+t12*(-12800+pow<2>(u)*(7680+pow<2>(u)*(-1472+(96-2*pow<2>(u))*pow<2>(u)))+pow<2>(t34)*(1024+pow<2>(u)*(-256+pow<2>(u)*(-64+16*pow<2>(u))))+t12*(t34*u*(2048+pow<2>(u)*(-1024+128*pow<2>(u)))+t12*(1280+pow<2>(u)*(-512+pow<2>(u)*(32+pow<4>(u)))))))+zb*(-15360+pow<2>(u)*(7168+pow<2>(u)*(-960+32*pow<2>(u)))+pow<2>(t34)*(4096+pow<2>(u)*(-768+pow<2>(u)*(-128+16*pow<2>(u))))+t12*(t34*u*(7168+pow<2>(u)*(-2560+192*pow<2>(u)))+t12*(20480+pow<2>(u)*(-8704+(1024-32*pow<2>(u))*pow<2>(u))+t12*(t34*u*(-7168+(2560-192*pow<2>(u))*pow<2>(u))+t12*(-5120+(1536-64*pow<2>(u))*pow<2>(u)))+pow<2>(t34)*(-4096+pow<2>(u)*(768+(128-16*pow<2>(u))*pow<2>(u)))))+zb*(13440+pow<2>(u)*(-4480+(360-4*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(-7168+pow<2>(u)*(960+(96-4*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(384+pow<2>(u)*(64+24*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(1024+256*pow<2>(u))+u*(-10752+(2560-96*pow<2>(u))*pow<2>(u)))+t12*(-22400+pow<2>(u)*(6400+pow<2>(u)*(-408+4*pow<2>(u)))+pow<2>(t34)*(7680+pow<2>(t34)*(-384+(-64-24*pow<2>(u))*pow<2>(u))+pow<2>(u)*(384+pow<2>(u)*(-96+8*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(-1024-256*pow<2>(u))+u*(12288+pow<2>(u)*(-2560+128*pow<2>(u))))+t12*(9600+pow<2>(u)*(-1920+pow<2>(u)*(56+4*pow<2>(u)))+pow<2>(t34)*(-512+pow<2>(u)*(-1344-4*pow<4>(u)))+t12*(t34*u*(-1536-32*pow<4>(u))+t12*(-640+(-8-4*pow<2>(u))*pow<4>(u)))))))+zb*(-8064+(1792-72*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(7168+(-640-32*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-1152+(-128-24*pow<2>(u))*pow<2>(u)))+t12*(t34*(u*pow<2>(t34)*(-2560-384*pow<2>(u))+u*(8960+pow<2>(u)*(-1280+16*pow<2>(u))))+t12*(17024+pow<2>(u)*(-3072+88*pow<2>(u))+pow<2>(t34)*(-8704+pow<2>(u)*(-2048+32*pow<2>(u))+pow<2>(t34)*(1152+pow<2>(u)*(128+24*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(2560+384*pow<2>(u))+u*(-12800+(1280-32*pow<2>(u))*pow<2>(u)))+t12*(-10880+(1280-24*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(1536+2688*pow<2>(u))+t12*(t12*(1920+8*pow<4>(u))+t34*u*(3840+16*pow<4>(u)))))))+zb*(3360+pow<2>(u)*(-448+6*pow<2>(u))+pow<2>(t34)*(-4480+pow<2>(u)*(240+4*pow<2>(u))+pow<2>(t34)*(1440+pow<2>(t34)*(-64-16*pow<2>(u))+pow<2>(u)*(96+6*pow<2>(u))))+zb*(-960+64*pow<2>(u)+pow<2>(t34)*(1792-48*pow<2>(u)+pow<2>(t34)*(-960-32*pow<2>(u)+pow<2>(t34)*(128+16*pow<2>(u))))+zb*(180+zb*(-20+pow<2>(t34)*(64+pow<2>(t34)*(-72+(32-4*pow<2>(t34))*pow<2>(t34)))+zb*(1+pow<2>(t34)*(-4+pow<2>(t34)*(6+(-4+pow<2>(t34))*pow<2>(t34)))+pow<2>(t12)*(-6+pow<2>(t34)*(12+pow<2>(t34)*(-8+(4-2*pow<2>(t34))*pow<2>(t34)))+pow<2>(t12)*(15+pow<2>(t34)*(-8+pow<2>(t34)*(4+pow<2>(t34)*(4+pow<2>(t34))))+pow<2>(t12)*(-20+pow<2>(t34)*(-8+(-8-4*pow<2>(t34))*pow<2>(t34))+pow<2>(t12)*(15+pow<2>(t12)*(-6+pow<2>(t12)-4*pow<2>(t34))+pow<2>(t34)*(12+6*pow<2>(t34)))))))+t12*(t34*(16*u+pow<2>(t34)*(-32*u+16*u*pow<2>(t34)))+t12*(100+pow<2>(t34)*(-160+pow<2>(t34)*(88+pow<2>(t34)*(-32+4*pow<2>(t34))))+t12*(t34*(-64*u+pow<2>(t34)*(32*u-32*u*pow<2>(t34)))+t12*(-200+(96-24*pow<2>(t34))*pow<2>(t34)+t12*(t34*(96*u+pow<2>(t34)*(32*u+16*u*pow<2>(t34)))+t12*(200+pow<2>(t34)*(32+8*pow<2>(t34))+t12*(t12*(-100+t12*(20*t12+16*t34*u)-32*pow<2>(t34))+t34*(-64*u-32*u*pow<2>(t34))))))))))-4*pow<2>(u)+pow<2>(t34)*(-448+4*pow<2>(u)+pow<2>(t34)*(360+pow<2>(t34)*(-96+4*pow<2>(t34)-4*pow<2>(u))+4*pow<2>(u)))+t12*(t34*(-224*u+pow<2>(t34)*(320*u-96*u*pow<2>(t34)))+t12*(-740+12*pow<2>(u)+pow<2>(t34)*(928+80*pow<2>(u)+pow<2>(t34)*(-408-4*pow<2>(u)+pow<2>(t34)*(96-4*pow<2>(t34)+8*pow<2>(u))))+t12*(t34*(704*u+pow<2>(t34)*(-320*u+128*u*pow<2>(t34)))+t12*(1160-8*pow<2>(u)+pow<2>(t34)*(-480-168*pow<2>(u)+pow<2>(t34)*(56-4*pow<2>(u)-4*pow<2>(t34)*pow<2>(u)))+t12*(t34*(-768*u+pow<2>(t34)*(-64*u-32*u*pow<2>(t34)))+t12*(-840-8*pow<2>(u)+pow<2>(t34)*(-32+80*pow<2>(u)+pow<2>(t34)*(-8+4*pow<2>(u)))+t12*(t34*(320*u+64*u*pow<2>(t34))+t12*(260+t12*(-32*t34*u+t12*(-20-4*pow<2>(u)))+12*pow<2>(u)+pow<2>(t34)*(32+4*pow<2>(u)))))))))))+t12*(t34*(pow<2>(t34)*(192*u*pow<2>(t34)+u*(-1280-32*pow<2>(u)))+u*(1344-32*pow<2>(u)))+t12*(3200-160*pow<2>(u)+pow<2>(t34)*(-3072-624*pow<2>(u)+pow<2>(t34)*(1024+pow<2>(t34)*(-128-16*pow<2>(u))+32*pow<2>(u)))+t12*(t34*(u*(-3264+32*pow<2>(u))+pow<2>(t34)*(-192*u*pow<2>(t34)+u*(1280+64*pow<2>(u))))+t12*(-3840+96*pow<2>(u)+pow<2>(t34)*(1280-64*pow<2>(t34)+1008*pow<2>(u))+t12*(t12*(1920+t12*(t34*u*(-576-32*pow<2>(u))+t12*(-320-32*pow<2>(u)))+32*pow<2>(u)-336*pow<2>(t34)*pow<2>(u))+t34*(u*(2496+32*pow<2>(u))-32*pow<2>(t34)*pow<3>(u))))))))+t12*(t34*(u*(-4480+320*pow<2>(u))+pow<2>(t34)*(-128*u*pow<2>(t34)+u*(2560+192*pow<2>(u))))+t12*(-8960+(928-8*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(6400+(1776-4*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-1472+(-96-12*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(64+16*pow<2>(u))))+t12*(t34*(pow<2>(t34)*(128*u*pow<2>(t34)+u*(-2560-256*pow<2>(u)))+u*(8320-320*pow<2>(u)))+t12*(8000+pow<2>(u)*(-480+4*pow<2>(u))+t12*(t12*(-2560+(-32-8*pow<2>(u))*pow<2>(u)+pow<2>(t34)*pow<2>(u)*(336+4*pow<2>(u))+t12*(t34*u*(384+64*pow<2>(u))+t12*(160+pow<2>(u)*(32+6*pow<2>(u)))))+t34*(u*(-4224-64*pow<2>(u))+64*pow<2>(t34)*pow<3>(u)))+pow<2>(t34)*(-1920+(-2352-4*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(32+6*pow<4>(u))))))))))))));
    return todouble(foo);
}

// Coefficient order epsilon^1 of bubble type 3
template<>
template<>
double qq2yyg1<TT>::LC::bub::c<3,1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (32768+pow<2>(u)*(-32768+pow<2>(u)*(12288+pow<2>(u)*(-2048+128*pow<2>(u))))+zb*(-196608+pow<2>(u)*(163840+pow<2>(u)*(-49152+(6144-256*pow<2>(u))*pow<2>(u)))+zb*(540672+pow<2>(t34)*pow<2>(u)*(-4096+(2048-256*pow<2>(u))*pow<2>(u))+pow<2>(u)*(-368640+pow<2>(u)*(86016+pow<2>(u)*(-7680+192*pow<2>(u))))+t12*(t34*u*(-20480+pow<2>(u)*(11264+pow<2>(u)*(-1792+64*pow<2>(u))))+t12*(-24576+pow<2>(u)*(14336+pow<2>(u)*(-2560+128*pow<2>(u)))))+zb*(-913408+pow<2>(u)*(501760+pow<2>(u)*(-89088+(5504-80*pow<2>(u))*pow<2>(u)))+pow<2>(t34)*(12288+pow<2>(u)*(9216+pow<2>(u)*(-4864+448*pow<2>(u))))+t12*(t12*(135168+pow<2>(u)*(-64512+(8960-320*pow<2>(u))*pow<2>(u)))+t34*u*(104448+pow<2>(u)*(-46592+(5760-160*pow<2>(u))*pow<2>(u))))+zb*(1067008+pow<2>(t34)*(-90112+pow<2>(t34)*(16384+4096*pow<2>(u))+pow<2>(u)*(3072+(5120-320*pow<2>(u))*pow<2>(u)))+pow<2>(u)*(-466432+pow<2>(u)*(62080+pow<2>(u)*(-2592+16*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(50176+(4608-192*pow<2>(u))*pow<2>(u))+u*(-289792+pow<2>(u)*(98816+pow<2>(u)*(-8128+128*pow<2>(u)))))+t12*(-337920+pow<2>(u)*(115712+pow<2>(u)*(-9344+128*pow<2>(u)))+pow<2>(t34)*(32768+pow<2>(u)*(48640+(-1792-32*pow<2>(u))*pow<2>(u)))+t12*(t34*u*(58368+(4608-704*pow<2>(u))*pow<2>(u))+t12*(8192+pow<2>(u)*(11776+(-1280-32*pow<2>(u))*pow<2>(u))))))+zb*(-910336+pow<2>(u)*(311552+pow<2>(u)*(-30144+752*pow<2>(u)))+pow<2>(t34)*(251904+pow<2>(u)*(-26880+pow<2>(u)*(-4096+176*pow<2>(u)))+pow<2>(t34)*(-70656+pow<2>(u)*(-13824+192*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(-193024+pow<2>(u)*(-12032+480*pow<2>(u)))+u*(510464+pow<2>(u)*(-125696+(6048-32*pow<2>(u))*pow<2>(u))))+t12*(509952+pow<2>(u)*(-109824+pow<2>(u)*(2560+48*pow<2>(u)))+pow<2>(t34)*(-142336+pow<2>(u)*(-161024+pow<2>(u)*(5056+16*pow<2>(u))))+t12*(t34*u*(-225792+pow<2>(u)*(-9984+992*pow<2>(u)))+t12*(-41984+pow<2>(u)*(-34816+pow<2>(u)*(2496+32*pow<2>(u)))))))+zb*(577024+pow<2>(u)*(-149248+(9568-96*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(-365056+pow<2>(u)*(33024+(2528-48*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(132096+(16384-192*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-4096+256*pow<2>(u))))+t12*(t34*(u*(-556800+(93376-2240*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(u*pow<2>(t34)*(-2816+192*pow<2>(u))+u*(302592+(12928-384*pow<2>(u))*pow<2>(u))))+t12*(-513024+pow<2>(u)*(65408+pow<2>(u)*(352+48*pow<2>(u)))+pow<2>(t34)*(241152+pow<2>(u)*(217856+pow<2>(u)*(-4960+16*pow<2>(u)))+pow<2>(t34)*(9216+pow<2>(u)*(2176+96*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(17920+640*pow<2>(u))+u*(351744+(6784-128*pow<2>(u))*pow<2>(u)))+t12*(93696+pow<2>(t34)*(10240+(4096-192*pow<2>(u))*pow<2>(u))+pow<2>(u)*(35072+(-1248-16*pow<2>(u))*pow<2>(u))+t12*(t34*u*(2304+(192-64*pow<2>(u))*pow<2>(u))+t12*(-2048+pow<2>(u)*(640+32*pow<2>(u))))))))+zb*(-269056+(48896-1680*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(305408+(-19904-832*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-139008+pow<2>(t34)*(12544-704*pow<2>(u))+(-7808-48*pow<2>(u))*pow<2>(u)))+t12*(t34*(u*(366208+pow<2>(u)*(-38496+320*pow<2>(u)))+pow<2>(t34)*(u*pow<2>(t34)*(6272-480*pow<2>(u))+u*(-248064+pow<2>(u)*(-7616+96*pow<2>(u)))))+t12*(354048+pow<2>(u)*(-29376+64*pow<2>(u))+pow<2>(t34)*(-201472+pow<2>(u)*(-155008+2128*pow<2>(u))+pow<2>(t34)*(-29184+(-5568-48*pow<2>(u))*pow<2>(u)))+t12*(t34*(u*pow<2>(t34)*(-46336-320*pow<2>(u))+u*(-288000+(-448-96*pow<2>(u))*pow<2>(u)))+t12*(-118016+pow<2>(u)*(-12416+112*pow<2>(u))+pow<2>(t34)*(-31232+pow<2>(u)*(-4288+176*pow<2>(u)))+t12*(t12*(8448+(-960-32*pow<2>(u))*pow<2>(u))+t34*u*(-384+pow<2>(u)*(32+32*pow<2>(u))))))))+zb*(89344+pow<2>(u)*(-9952+112*pow<2>(u))+pow<2>(t34)*(-152320+pow<2>(u)*(6528+96*pow<2>(u))+pow<2>(t34)*(87168+pow<2>(u)*(1056+48*pow<2>(u))+pow<2>(t34)*(-13824-128*pow<2>(t34)+576*pow<2>(u))))+t12*(t34*(u*(-140608+7936*pow<2>(u))+pow<2>(t34)*(u*(112704+2432*pow<2>(u))+pow<2>(t34)*(-64*u*pow<2>(t34)+u*(-4800+384*pow<2>(u)))))+t12*(-165504+10048*pow<2>(u)+pow<2>(t34)*(86528+(61280-448*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(35200+pow<2>(t34)*(-512-96*pow<2>(u))+(5312-48*pow<2>(u))*pow<2>(u)))+t12*(t34*(pow<2>(t34)*(64*u*pow<2>(t34)+u*(44928-640*pow<2>(u)))+u*(137280-1280*pow<2>(u)))+t12*(89984+(-192-144*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(36352+pow<2>(u)*(-3520+48*pow<2>(u))+pow<2>(t34)*(1280+224*pow<2>(u)))+t12*(t34*(64*u*pow<2>(t34)+u*(-9408-640*pow<2>(u)))+t12*(-14208+pow<2>(u)*(64+32*pow<2>(u))+pow<2>(t34)*(-256+(224-16*pow<2>(u))*pow<2>(u))+t12*(t12*(384+32*pow<2>(u))+t34*u*(448+128*pow<2>(u)))))))))+zb*(-19904+1040*pow<2>(u)+pow<2>(t34)*(44288-1040*pow<2>(u)+pow<2>(t34)*(-31104+pow<2>(t34)*(6400+320*pow<2>(t34)-112*pow<2>(u))+112*pow<2>(u)))+t12*(t34*(pow<2>(t34)*(pow<2>(t34)*(160*u*pow<2>(t34)+u*(1568-96*pow<2>(u)))+u*(-27424-320*pow<2>(u)))+u*(29280-608*pow<2>(u)))+t12*(49920-1936*pow<2>(u)+pow<2>(t34)*(-18368-12656*pow<2>(u)+pow<2>(t34)*(-20096-2352*pow<2>(u)+pow<2>(t34)*(832+48*pow<2>(u))))+t12*(t34*(u*(-39456+384*pow<2>(u))+pow<2>(t34)*(-672*u*pow<2>(t34)+u*(-20800+320*pow<2>(u))))+t12*(-41088+720*pow<2>(u)+pow<2>(t34)*(-21120+pow<2>(t34)*(-2560-128*pow<2>(u))+4624*pow<2>(u))+t12*(t12*(12032+t12*(t34*u*(-864-64*pow<2>(u))+t12*(-960-32*pow<2>(u)))+pow<2>(t34)*(1344-144*pow<2>(u))+208*pow<2>(u))+t34*(1120*u*pow<2>(t34)+u*(11040+288*pow<2>(u))))))))+zb*(2656-32*pow<2>(u)+pow<2>(t34)*(-6816+pow<2>(t34)*(5408+pow<2>(t34)*(-992-256*pow<2>(t34)-16*pow<2>(u)))+48*pow<2>(u))+t12*(t34*(-2752*u+pow<2>(t34)*(3072*u+pow<2>(t34)*(-192*u-128*u*pow<2>(t34))))+t12*(-8736+112*pow<2>(u)+pow<2>(t34)*(1824+976*pow<2>(u)+pow<2>(t34)*(5568+400*pow<2>(u)+pow<2>(t34)*(-480+32*pow<2>(t34)+48*pow<2>(u))))+t12*(t34*(6016*u+pow<2>(t34)*(4992*u+768*u*pow<2>(t34)))+t12*(10304+pow<2>(t34)*(6816+pow<2>(t34)*(1952-80*pow<2>(u))-912*pow<2>(u))-144*pow<2>(u)+t12*(t34*(-3840*u+pow<2>(t34)*(-1920*u+64*u*pow<2>(t34)))+t12*(-5056+pow<2>(t34)*(-1824-128*pow<2>(t34)-144*pow<2>(u))+80*pow<2>(u)+t12*(640*t34*u+t12*(864+t12*(-32*t12-64*t34*u)-16*pow<2>(u)+32*pow<2>(t34)*pow<2>(u)))))))))+zb*(-160+pow<2>(t34)*(416+pow<2>(t34)*(-288+pow<2>(t34)*(-32+64*pow<2>(t34))))+zb*pow<2>(t12)*(pow<2>(t12)*(pow<2>(t34)*(112+pow<2>(t34)*(96+48*pow<2>(t34)))+pow<2>(t12)*(pow<2>(t12)*(80*pow<2>(t34)-16*pow<2>(t12)*pow<2>(t34))+pow<2>(t34)*(-144+pow<2>(t34)*(-144+16*pow<2>(t34)))))+pow<2>(t34)*(-32+pow<2>(t34)*(48-16*pow<4>(t34))))+t12*(t12*(672+pow<2>(t34)*(-48+pow<2>(t34)*(-784+(176-16*pow<2>(t34))*pow<2>(t34)))+t12*(t34*(-224*u+pow<2>(t34)*(-576*u-224*u*pow<2>(t34)))+t12*(-1088+pow<2>(t34)*(-1200+(-704-80*pow<2>(t34))*pow<2>(t34))+t12*(t12*(832+t12*(-160*t34*u+t12*(-288+t12*(32*t12+32*t34*u)-48*pow<2>(t34)))+pow<2>(t34)*(880+240*pow<2>(t34)))+t34*(288*u+pow<2>(t34)*(672*u-32*u*pow<2>(t34)))))))+t34*(64*u+pow<2>(t34)*(-96*u+32*u*pow<4>(t34))))))))))))))))*pow<-1>(zb)*pow<-1>(-1024+pow<2>(u)*(1024+pow<2>(u)*(-384+(64-4*pow<2>(u))*pow<2>(u)))+pow<2>(t12)*(1024+pow<2>(u)*(-1024+pow<2>(u)*(384+pow<2>(u)*(-64+4*pow<2>(u)))))+zb*(6144+pow<2>(u)*(-5120+pow<2>(u)*(1536+pow<2>(u)*(-192+8*pow<2>(u))))+pow<2>(t12)*(-6144+pow<2>(u)*(5120+pow<2>(u)*(-1536+(192-8*pow<2>(u))*pow<2>(u))))+zb*(-16640+pow<2>(u)*(11264+pow<2>(u)*(-2592+(224-5*pow<2>(u))*pow<2>(u)))+pow<2>(t34)*(1024+pow<2>(u)*(-256+pow<2>(u)*(-64+16*pow<2>(u))))+t12*(t34*u*(2048+pow<2>(u)*(-1024+128*pow<2>(u)))+t12*(17920+pow<2>(t34)*(-1024+pow<2>(u)*(256+(64-16*pow<2>(u))*pow<2>(u)))+pow<2>(u)*(-11776+pow<2>(u)*(2624+pow<2>(u)*(-224+6*pow<2>(u))))+t12*(t34*u*(-2048+(1024-128*pow<2>(u))*pow<2>(u))+t12*(-1280+pow<2>(u)*(512+pow<2>(u)*(-32-pow<4>(u)))))))+zb*(26880+pow<2>(t34)*(-5120+pow<2>(u)*(1024+(192-32*pow<2>(u))*pow<2>(u)))+pow<2>(u)*(-14336+pow<2>(u)*(2400+(-128+pow<2>(u))*pow<2>(u)))+t12*(t34*u*(-9216+(3584-320*pow<2>(u))*pow<2>(u))+t12*(-33280+pow<2>(u)*(16384+pow<2>(u)*(-2496+(128-2*pow<2>(u))*pow<2>(u)))+pow<2>(t34)*(5120+pow<2>(u)*(-1024+pow<2>(u)*(-192+32*pow<2>(u))))+t12*(t34*u*(9216+pow<2>(u)*(-3584+320*pow<2>(u)))+t12*(6400+pow<2>(u)*(-2048+pow<2>(u)*(96+pow<4>(u)))))))+zb*(-28800+pow<2>(u)*(11648+pow<2>(u)*(-1320+36*pow<2>(u)))+pow<2>(t34)*(11264+pow<2>(t34)*(-384+(-64-24*pow<2>(u))*pow<2>(u))+pow<2>(u)*(-1728+pow<2>(u)*(-224+20*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(-1024-256*pow<2>(u))+u*(17920+pow<2>(u)*(-5120+288*pow<2>(u))))+t12*(42880+pow<2>(u)*(-15104+(1432-36*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(-11776+pow<2>(u)*(384+(224-24*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(384+pow<2>(u)*(64+24*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(1024+256*pow<2>(u))+u*(-19456+(5120-320*pow<2>(u))*pow<2>(u)))+t12*(-14720+pow<2>(u)*(3456+(-120-4*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(512+pow<2>(u)*(1344+4*pow<4>(u)))+t12*(t34*u*(1536+32*pow<4>(u))+t12*(640+(8+4*pow<2>(u))*pow<4>(u)))))))+zb*(21504+pow<2>(u)*(-6272+(432-4*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(-14336+pow<2>(u)*(1600+(128-4*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(1536+pow<2>(u)*(192+48*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(3584+640*pow<2>(u))+u*(-19712+(3840-112*pow<2>(u))*pow<2>(u)))+t12*(-39424+pow<2>(u)*(9472+pow<2>(u)*(-496+4*pow<2>(u)))+pow<2>(t34)*(16384+pow<2>(t34)*(-1536+(-192-48*pow<2>(u))*pow<2>(u))+pow<2>(u)*(2432+pow<2>(u)*(-128+8*pow<2>(u))))+t12*(t34*(u*pow<2>(t34)*(-3584-640*pow<2>(u))+u*(25088+pow<2>(u)*(-3840+160*pow<2>(u))))+t12*(20480+pow<2>(u)*(-3200+pow<2>(u)*(80+4*pow<2>(u)))+pow<2>(t34)*(-2048+pow<2>(u)*(-4032-4*pow<4>(u)))+t12*(t34*u*(-5376-48*pow<4>(u))+t12*(-2560+(-16-4*pow<2>(u))*pow<4>(u)))))))+zb*(-11424+(2240-78*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(11648+(-880-36*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-2592+(-224-30*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(64+16*pow<2>(u))))+t12*(t34*(pow<2>(t34)*(128*u*pow<2>(t34)+u*(-5120-576*pow<2>(u)))+u*(13440+pow<2>(u)*(-1600+16*pow<2>(u))))+t12*(25984+pow<2>(u)*(-4000+96*pow<2>(u))+pow<2>(t34)*(-15104+pow<2>(u)*(-3824+36*pow<2>(u))+pow<2>(t34)*(2624+pow<2>(t34)*(-64-16*pow<2>(u))+pow<2>(u)*(224+36*pow<2>(u))))+t12*(t34*(u*(-21120+(1600-32*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(-128*u*pow<2>(t34)+u*(5120+640*pow<2>(u))))+t12*(-18880+(1760-28*pow<2>(u))*pow<2>(u)+t12*(t12*(4480+pow<2>(t34)*(-336-4*pow<2>(u))*pow<2>(u)+pow<2>(u)*(32+16*pow<2>(u))+t12*(t34*u*(-384-64*pow<2>(u))+t12*(-160+(-32-6*pow<2>(u))*pow<2>(u))))+t34*(u*(8064+pow<2>(u)*(64+16*pow<2>(u)))-64*pow<2>(t34)*pow<3>(u)))+pow<2>(t34)*(3456+pow<2>(u)*(5040+4*pow<2>(u))+pow<2>(t34)*(-32-6*pow<4>(u)))))))+zb*(4320+pow<2>(u)*(-512+6*pow<2>(u))+pow<2>(t34)*(-6272+pow<2>(u)*(288+4*pow<2>(u))+pow<2>(t34)*(2400+pow<2>(t34)*(-192-32*pow<2>(u))+pow<2>(u)*(128+6*pow<2>(u))))+zb*(-1140+68*pow<2>(u)+pow<2>(t34)*(2240-52*pow<2>(u)+pow<2>(t34)*(-1320-36*pow<2>(u)+pow<2>(t34)*(224-4*pow<2>(t34)+20*pow<2>(u))))+t12*(t34*(pow<2>(t34)*(288*u*pow<2>(t34)+u*(-1600-32*pow<2>(u)))+u*(1568-32*pow<2>(u)))+t12*(3940-172*pow<2>(u)+pow<2>(t34)*(-4000-704*pow<2>(u)+pow<2>(t34)*(1432+pow<2>(t34)*(-224+4*pow<2>(t34)-24*pow<2>(u))+36*pow<2>(u)))+t12*(t34*(u*(-3968+32*pow<2>(u))+pow<2>(t34)*(-320*u*pow<2>(t34)+u*(1600+64*pow<2>(u))))+t12*(-5000+104*pow<2>(u)+pow<2>(t34)*(1760+1176*pow<2>(u)+pow<2>(t34)*(-120+4*pow<2>(u)+4*pow<2>(t34)*pow<2>(u)))+t12*(t34*(pow<2>(t34)*(32*u*pow<2>(t34)+u*(64-32*pow<2>(u)))+u*(3264+32*pow<2>(u)))+t12*(2760+pow<2>(t34)*(32+pow<2>(t34)*(8-4*pow<2>(u))-416*pow<2>(u))+40*pow<2>(u)+t12*(t34*(-64*u*pow<2>(t34)+u*(-896-32*pow<2>(u)))+t12*(-580+pow<2>(t34)*(-32-4*pow<2>(u))-44*pow<2>(u)+t12*(32*t34*u+t12*(20+4*pow<2>(u)))))))))))+zb*(200+zb*(-21+pow<2>(t34)*(68+pow<2>(t34)*(-78+(36-5*pow<2>(t34))*pow<2>(t34)))+zb*(1+pow<2>(t34)*(-4+pow<2>(t34)*(6+(-4+pow<2>(t34))*pow<2>(t34)))+pow<2>(t12)*(-6+pow<2>(t34)*(12+pow<2>(t34)*(-8+(4-2*pow<2>(t34))*pow<2>(t34)))+pow<2>(t12)*(15+pow<2>(t34)*(-8+pow<2>(t34)*(4+pow<2>(t34)*(4+pow<2>(t34))))+pow<2>(t12)*(-20+pow<2>(t34)*(-8+(-8-4*pow<2>(t34))*pow<2>(t34))+pow<2>(t12)*(15+pow<2>(t12)*(-6+pow<2>(t12)-4*pow<2>(t34))+pow<2>(t34)*(12+6*pow<2>(t34)))))))+t12*(t34*(16*u+pow<2>(t34)*(-32*u+16*u*pow<2>(t34)))+t12*(106+pow<2>(t34)*(-172+pow<2>(t34)*(96+pow<2>(t34)*(-36+6*pow<2>(t34))))+t12*(t34*(-64*u+pow<2>(t34)*(32*u-32*u*pow<2>(t34)))+t12*(-215+pow<2>(t34)*(104+pow<2>(t34)*(-28+(-4-pow<2>(t34))*pow<2>(t34)))+t12*(t34*(96*u+pow<2>(t34)*(32*u+16*u*pow<2>(t34)))+t12*(220+pow<2>(t34)*(40+pow<2>(t34)*(16+4*pow<2>(t34)))+t12*(t34*(-64*u-32*u*pow<2>(t34))+t12*(-115+(-44-6*pow<2>(t34))*pow<2>(t34)+t12*(16*t34*u+t12*(26-pow<2>(t12)+4*pow<2>(t34))))))))))))-4*pow<2>(u)+pow<2>(t34)*(-512+4*pow<2>(u)+pow<2>(t34)*(432+pow<2>(t34)*(-128+8*pow<2>(t34)-4*pow<2>(u))+4*pow<2>(u)))+t12*(t34*(-240*u+pow<2>(t34)*(352*u-112*u*pow<2>(t34)))+t12*(-840+12*pow<2>(u)+pow<2>(t34)*(1088+80*pow<2>(u)+pow<2>(t34)*(-496-4*pow<2>(u)+pow<2>(t34)*(128-8*pow<2>(t34)+8*pow<2>(u))))+t12*(t34*(768*u+pow<2>(t34)*(-352*u+160*u*pow<2>(t34)))+t12*(1360-8*pow<2>(u)+pow<2>(t34)*(-576-168*pow<2>(u)+pow<2>(t34)*(80-4*pow<2>(u)-4*pow<2>(t34)*pow<2>(u)))+t12*(t34*(-864*u+pow<2>(t34)*(-96*u-48*u*pow<2>(t34)))+t12*(-1040-8*pow<2>(u)+pow<2>(t34)*(-64+80*pow<2>(u)+pow<2>(t34)*(-16+4*pow<2>(u)))+t12*(t34*(384*u+96*u*pow<2>(t34))+t12*(360+t12*(-48*t34*u+t12*(-40-4*pow<2>(u)))+12*pow<2>(u)+pow<2>(t34)*(64+4*pow<2>(u))))))))))))+t12*(t34*(u*(-5824+352*pow<2>(u))+pow<2>(t34)*(-320*u*pow<2>(t34)+u*(3840+224*pow<2>(u))))+t12*(-12160+(1088-8*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(9472+(2400-4*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-2496+(-128-12*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(192+32*pow<2>(u))))+t12*(t34*(pow<2>(t34)*(320*u*pow<2>(t34)+u*(-3840-320*pow<2>(u)))+u*(11584-352*pow<2>(u)))+t12*(11840+pow<2>(u)*(-576+4*pow<2>(u))+t12*(t12*(-4480+(-64-8*pow<2>(u))*pow<2>(u)+pow<2>(t34)*pow<2>(u)*(672+4*pow<2>(u))+t12*(t34*u*(960+96*pow<2>(u))+t12*(480+pow<2>(u)*(64+6*pow<2>(u)))))+t34*(u*(-6720-96*pow<2>(u))+96*pow<2>(t34)*pow<3>(u)))+pow<2>(t34)*(-3200+(-3360-4*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(96+6*pow<4>(u)))))))))))))));
    return todouble(foo);
}

// Coefficient of bubble type 3 as a series in epsilon
template<>
template<>
EpsExp qq2yyg1<TT>::LC::bub::c<3>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    return EpsExp(0,{
        qq2yyg1<TT>::LC::bub::c<3,0>(zb,t12,t34,u),
        qq2yyg1<TT>::LC::bub::c<3,1>(zb,t12,t34,u)
    });
}

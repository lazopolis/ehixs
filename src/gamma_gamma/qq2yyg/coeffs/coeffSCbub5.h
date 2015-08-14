/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "qq2yyg/qq2yyg1.h"

// Bubble type 5: bubble(s35)

// Coefficient order epsilon^0 of bubble type 5
template<>
template<>
double qq2yyg1<TT>::SC::bub::c<5,0>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (2048+pow<2>(u)*(-1536+(384-32*pow<2>(u))*pow<2>(u))+t34*(-2048+t34*(-4096+(2048-256*pow<2>(u))*pow<2>(u))+pow<2>(u)*(512+(128-32*pow<2>(u))*pow<2>(u)))+t12*(t34*(u*(-4096+(2048-256*pow<2>(u))*pow<2>(u))+t34*u*(-2048+(1024-128*pow<2>(u))*pow<2>(u)))+t12*(-2048+pow<2>(u)*(512+(128-32*pow<2>(u))*pow<2>(u))+t34*(2048+pow<2>(u)*(-2560+(896-96*pow<2>(u))*pow<2>(u))))+(-512+(256-32*pow<2>(u))*pow<2>(u))*pow<3>(u))+zb*(-6144+pow<2>(u)*(2560+(-128-32*pow<2>(u))*pow<2>(u))+t34*(8192-2048*pow<2>(u)+t34*(4096+pow<2>(u)*(-2048+256*pow<2>(u))+t34*(-10240+(3072-128*pow<2>(u))*pow<2>(u))))+t12*(t34*(u*(4096+pow<2>(u)*(-2048+256*pow<2>(u)))+t34*(t34*u*(-4096+1024*pow<2>(u))+u*(-8192+2048*pow<2>(u))))+(1024-256*pow<2>(u))*pow<3>(u)+t12*(8192+pow<2>(u)*(-2560+pow<2>(u)*(-256+96*pow<2>(u)))+t34*(-8192+pow<2>(u)*(512+pow<2>(u)*(256+32*pow<2>(u)))+t34*(4096+pow<2>(u)*(-6144+1280*pow<2>(u))))+t12*(t12*(-2048+(1024-128*pow<2>(u))*pow<2>(u))+t34*u*(2048+pow<2>(u)*(-2048+384*pow<2>(u)))+(-1024+256*pow<2>(u))*pow<3>(u))))+zb*(5632+(-256-288*pow<2>(u))*pow<2>(u)+t34*(-14848+pow<2>(u)*(2304+96*pow<2>(u))+t34*(4608+pow<2>(u)*(-256+32*pow<2>(u))+t34*(14848+pow<2>(u)*(-3328+160*pow<2>(u))+t34*(-10240+1536*pow<2>(u)))))+t12*(u*(-512+pow<2>(u)*(-1024+160*pow<2>(u)))+t34*(u*(-1024+(1536-64*pow<2>(u))*pow<2>(u))+t34*(u*(7680+pow<2>(u)*(-1280+96*pow<2>(u)))+t34*(t34*u*(-3072+256*pow<2>(u))+u*(-6144+512*pow<2>(u)))))+t12*(-11264+pow<2>(u)*(1024+704*pow<2>(u))+t34*(20480+pow<2>(u)*(-5120+512*pow<2>(u))+t34*(-5632+(1280-480*pow<2>(u))*pow<2>(u)+t34*(3584+pow<2>(u)*(-4864+224*pow<2>(u)))))+t12*(t12*(5632+(256-672*pow<2>(u))*pow<2>(u)+t12*u*(-2048+512*pow<2>(u))+t34*(-5632+(2816-608*pow<2>(u))*pow<2>(u)))+t34*(t34*u*(3072-2304*pow<2>(u))+u*(3072+(-1536-64*pow<2>(u))*pow<2>(u)))+u*(2560-32*pow<4>(u)))))+zb*(1024+(-1024-64*pow<2>(u))*pow<2>(u)+t12*(t34*(t34*(1024*u+t34*(t34*(-2048*u-1024*t34*u)+u*(6144-768*pow<2>(u))))+u*(-8192-1024*pow<2>(u)))+u*(-2048+1280*pow<2>(u))+t12*(1024+pow<2>(u)*(-1280+128*pow<2>(u))+t34*(-15872+(2560-160*pow<2>(u))*pow<2>(u)+t34*(19968+(-2048-96*pow<2>(u))*pow<2>(u)+t34*(512+t34*(2048-1536*pow<2>(u))+(1792-96*pow<2>(u))*pow<2>(u))))+t12*(u*(1024-1280*pow<2>(u))+t34*(t34*(7168*u+t34*u*(2048-512*pow<2>(u)))+u*(10240+256*pow<2>(u)))+t12*(-1024+(3584-320*pow<2>(u))*pow<2>(u)+t12*(t12*(-1024-2304*pow<2>(u))+t34*u*(-8192-256*pow<2>(u))+u*(1024+1024*pow<2>(u)))+t34*(7680+(3072-96*pow<2>(u))*pow<2>(u)+t34*(-9728+(1024-32*pow<2>(u))*pow<2>(u)))))))+zb*(-4224+160*pow<2>(u)+t34*(640-1888*pow<2>(u)+t34*(9728+1600*pow<2>(u)+t34*(-8960+64*pow<2>(u)+t34*(-6272+t34*(6272-1280*t34-224*pow<2>(u))+288*pow<2>(u)))))+t12*(u*(256-96*pow<2>(u))+t34*(u*(2816+640*pow<2>(u))+t34*(u*(-10880-64*pow<2>(u))+t34*(t34*(t34*(-256*u-128*t34*u)+u*(2560-96*pow<2>(u)))+u*(512+128*pow<2>(u)))))+t12*(8064+96*pow<2>(u)+t34*(3200-864*pow<2>(u)+t34*(-10752+384*pow<2>(u)+t34*(8192-768*pow<2>(u)+t34*(640+t34*(896-160*pow<2>(u))+800*pow<2>(u)))))+t12*(u*(2560-192*pow<2>(u))+t34*(u*(-1536-512*pow<2>(u))+t34*(u*(5888+192*pow<2>(u))+t34*(768*t34*u+u*(3584+128*pow<2>(u)))))+t12*(-5504-160*pow<2>(u)+t34*(-4224+736*pow<2>(u)+t34*(6144+2624*pow<2>(u)+t34*(-7424+704*pow<2>(u))))+t12*(t12*(1664+1536*t12*u+t34*(384-1056*pow<2>(u))-96*pow<2>(u))+u*(-4352+288*pow<2>(u))+t34*(t34*u*(-6784+128*pow<2>(u))+u*(768+896*pow<2>(u))))))))+zb*(128+zb*(672+t34*(288+t34*(-1632+t34*(-480+t34*(1248+t34*(96+t34*(-288+96*t34))))))+t12*(-288*u+t34*(192*u+t34*(-672*u+t34*(896*u+t34*(-96*u+t34*(-64*u+32*t34*u)))))+t12*(-768+t34*(-1344+t34*(4192+t34*(-992+t34*(-1088+t34*(256+t34*(-288+32*t34))))))+t12*(864*u+t34*(-192*u+t34*(576*u+t34*(-512*u+t34*(-160*u-64*t34*u))))+t12*(-1728+t34*(1536+t34*(-2976+t34*(352+(1632-352*t34)*t34)))+t12*(-864*u+t34*(-192*u+t34*(-288*u+t34*(640*u-128*t34*u)))+t12*(3072+t34*(-192+t34*(416+96*t34))+t12*(t12*(-1248-288*t34)+288*u+t34*(192*u+384*t34*u))))))))+zb*pow<2>(t12)*(t34*(288+t34*(-288+t34*(288+t34*(-320+t34*(-32+(96-32*t34)*t34)))))+pow<2>(t12)*(t34*(-864+t34*(672+t34*(-192+t34*(128+(160-32*t34)*t34))))+pow<2>(t12)*(t34*(864+t34*(-480+(-96-64*t34)*t34))+t34*(-288+96*t34)*pow<2>(t12)))))+96*pow<2>(u)+t34*(-3584+384*pow<2>(u)+t34*(-576*pow<2>(u)+t34*(6016+t34*(-1408+t34*(-2304+(1280-128*t34)*t34)-32*pow<2>(u))+128*pow<2>(u))))+t12*(t34*(-1280*u+t34*(4352*u+t34*(-3328*u+t34*(-256*u+512*t34*u))))+t12*(256-480*pow<2>(u)+t34*(9472-288*pow<2>(u)+t34*(-256-192*pow<2>(u)+t34*(-4864+192*pow<2>(u)+t34*(1280-96*pow<2>(u)+t34*(-512+256*t34+96*pow<2>(u))))))+t12*(768*u+t34*(3456*u+t34*(-2304*u+t34*(256*u+t34*(256*u+128*t34*u))))+t12*(-512+t12*(-1536*u+t34*(-4096*u+t34*(1024*u-2048*t34*u))+t12*(-256+t12*(384*t12+768*u+1920*t34*u)+t34*(3840+t34*(1792-576*pow<2>(u))-672*pow<2>(u))-288*pow<2>(u)))+672*pow<2>(u)+t34*(-9728+576*pow<2>(u)+t34*(-2560-192*pow<2>(u)+t34*(4480-64*pow<2>(u)+t34*(-2432+64*pow<2>(u)))))))))))+t34*(8192+(1024-128*pow<2>(u))*pow<2>(u)+t34*(-17408+t34*(-5120+768*pow<2>(u)+t34*(14336-1536*pow<2>(u)+t34*(-5120+256*pow<2>(u))))+64*pow<4>(u)))))))*pow<-1>(256+pow<2>(u)*(-256+pow<2>(u)*(96+(-16+pow<2>(u))*pow<2>(u)))+pow<2>(t12)*(-512+pow<2>(u)*(512+pow<2>(u)*(-192+(32-2*pow<2>(u))*pow<2>(u)))+pow<2>(t12)*(256+pow<2>(u)*(-256+pow<2>(u)*(96+(-16+pow<2>(u))*pow<2>(u)))))+zb*(-512+pow<2>(u)*(384+pow<2>(u)*(-96+8*pow<2>(u)))+t34*(512+pow<2>(u)*(-384+(96-8*pow<2>(u))*pow<2>(u)))+pow<2>(t12)*(1024+pow<2>(u)*(-768+(192-16*pow<2>(u))*pow<2>(u))+t34*(-1024+pow<2>(u)*(768+pow<2>(u)*(-192+16*pow<2>(u))))+pow<2>(t12)*(-512+pow<2>(u)*(384+pow<2>(u)*(-96+8*pow<2>(u)))+t34*(512+pow<2>(u)*(-384+(96-8*pow<2>(u))*pow<2>(u)))))+zb*(256+pow<2>(u)*(-192+(48-4*pow<2>(u))*pow<2>(u))+t34*(-1024+(512-64*pow<2>(u))*pow<2>(u)+t34*(256+pow<2>(u)*(-192+(48-4*pow<2>(u))*pow<2>(u))))+t12*(t34*u*(-256+(128-16*pow<2>(u))*pow<2>(u))+u*(256+pow<2>(u)*(-128+16*pow<2>(u)))+t12*(-768+pow<2>(u)*(448+pow<2>(u)*(-80+4*pow<2>(u)))+t34*(2048+pow<2>(u)*(-1024+128*pow<2>(u))+t34*(-512+pow<2>(u)*(384+pow<2>(u)*(-96+8*pow<2>(u)))))+t12*(u*(-512+(256-32*pow<2>(u))*pow<2>(u))+t34*u*(512+pow<2>(u)*(-256+32*pow<2>(u)))+t12*(768+pow<2>(u)*(-320+pow<2>(u)*(16+4*pow<2>(u)))+t12*(t34*u*(-256+(128-16*pow<2>(u))*pow<2>(u))+u*(256+pow<2>(u)*(-128+16*pow<2>(u)))+t12*(-256+pow<2>(u)*(64+(16-4*pow<2>(u))*pow<2>(u))))+t34*(-1024+(512-64*pow<2>(u))*pow<2>(u)+t34*(256+pow<2>(u)*(-192+(48-4*pow<2>(u))*pow<2>(u))))))))+zb*(128+(64-24*pow<2>(u))*pow<2>(u)+t34*(640+pow<2>(u)*(-192+8*pow<2>(u))+t34*(-640+(192-8*pow<2>(u))*pow<2>(u)+t34*(-128+pow<2>(u)*(-64+24*pow<2>(u)))))+t12*(u*(-512+128*pow<2>(u))+t34*(t34*u*(-512+128*pow<2>(u))+u*(256+pow<2>(u)*(-128+16*pow<2>(u))))+t12*(128+pow<2>(u)*(-192+40*pow<2>(u))+t34*(-1664+(448-8*pow<2>(u))*pow<2>(u)+t34*(1280+pow<2>(u)*(-384+16*pow<2>(u))+t34*(256+(128-48*pow<2>(u))*pow<2>(u))))+t12*(u*(1024-256*pow<2>(u))+t34*(t34*u*(1024-256*pow<2>(u))+u*(-512+(256-32*pow<2>(u))*pow<2>(u)))+t12*(-640+(192-8*pow<2>(u))*pow<2>(u)+t12*(u*(-512+128*pow<2>(u))+t12*(384+(-64-8*pow<2>(u))*pow<2>(u)+t34*(-384+pow<2>(u)*(64+8*pow<2>(u))))+t34*(t34*u*(-512+128*pow<2>(u))+u*(256+pow<2>(u)*(-128+16*pow<2>(u)))))+t34*(1408+(-320-8*pow<2>(u))*pow<2>(u)+t34*(-640+(192-8*pow<2>(u))*pow<2>(u)+t34*(-128+pow<2>(u)*(-64+24*pow<2>(u)))))))))+zb*(-160+pow<2>(u)*(-48+6*pow<2>(u))+pow<2>(t34)*(576+pow<2>(u)*(-32+4*pow<2>(u))+pow<2>(t34)*(-160+pow<2>(u)*(-48+6*pow<2>(u))))+zb*(32+24*pow<2>(u)+t34*(-160+8*pow<2>(u)+t34*(-192-16*pow<2>(u)+t34*(192+t34*(160+t34*(-32-24*pow<2>(u))-8*pow<2>(u))+16*pow<2>(u))))+t12*(-128*u+t34*(t34*(256*u+t34*(-128*t34*u+u*(128-32*pow<2>(u))))+u*(128-32*pow<2>(u)))+t12*(-64*pow<2>(u)+t34*(128+160*pow<2>(u)+t34*(576-144*pow<2>(u)+t34*(-448-16*pow<2>(u)+t34*(-320+16*pow<2>(u)+t34*(64+48*pow<2>(u))))))+t12*(384*u+t34*(u*(-384+32*pow<2>(u))+t34*(-384*u+t34*(256*t34*u+u*(-256+64*pow<2>(u)))))+t12*(-192+48*pow<2>(u)+t34*(320-336*pow<2>(u)+t34*(-576+t34*(320+t34*(160+t34*(-32-24*pow<2>(u))-8*pow<2>(u))-16*pow<2>(u))+336*pow<2>(u)))+t12*(-384*u+t34*(pow<2>(t34)*(-128*t34*u+u*(128-32*pow<2>(u)))+u*(384+32*pow<2>(u)))+t12*(256+t12*(128*u+t34*(128*t34*u+u*(-128-32*pow<2>(u)))+t12*(-96-8*pow<2>(u)+t34*(96+8*pow<2>(u))))+t34*(-384+160*pow<2>(u)+t34*(192-176*pow<2>(u)+t34*(-64+16*pow<2>(u))))))))))+zb*(16+zb*(-8+t34*(-8+t34*(24+t34*(24+t34*(-24+t34*(-24+t34*(8+8*t34))))))+zb*(1+pow<2>(t34)*(-4+pow<2>(t34)*(6+(-4+pow<2>(t34))*pow<2>(t34)))+pow<2>(t12)*(-6+pow<2>(t34)*(12+pow<2>(t34)*(-8+(4-2*pow<2>(t34))*pow<2>(t34)))+pow<2>(t12)*(15+pow<2>(t34)*(-8+pow<2>(t34)*(4+pow<2>(t34)*(4+pow<2>(t34))))+pow<2>(t12)*(-20+pow<2>(t34)*(-8+(-8-4*pow<2>(t34))*pow<2>(t34))+pow<2>(t12)*(15+pow<2>(t12)*(-6+pow<2>(t12)-4*pow<2>(t34))+pow<2>(t34)*(12+6*pow<2>(t34)))))))+t12*(t34*(16*u+pow<2>(t34)*(-32*u+16*u*pow<2>(t34)))+t12*(40+t34*(24+t34*(-64+t34*(-32+t34*(40+t34*(24+(-16-16*t34)*t34)))))+t12*(t34*(-64*u+pow<2>(t34)*(32*u-32*u*pow<2>(t34)))+t12*(-80+t34*(-16+t34*(48+t34*(16+t34*(-8+t34*(24+t34*(8+8*t34))))))+t12*(t34*(96*u+pow<2>(t34)*(32*u+16*u*pow<2>(t34)))+t12*(80+t34*(-16+(-32+(-8-24*t34)*t34)*pow<2>(t34))+t12*(t12*(-40+t34*(24+t34*(-8+24*t34))+t12*(t12*(8-8*t34)+16*t34*u))+t34*(-64*u-32*u*pow<2>(t34))))))))))-4*pow<2>(u)+t34*(64+t34*(-16+4*pow<2>(u)+t34*(-128+t34*(-16+t34*(64+t34*(16-4*pow<2>(u)))+4*pow<2>(u)))))+t12*(16*u+t34*(-80*u+t34*(-96*u+t34*(96*u+t34*(80*u-16*t34*u))))+t12*(-80+12*pow<2>(u)+t34*(-128+t34*(80*pow<2>(u)+t34*(256+t34*(-16-4*pow<2>(u)+t34*(-128+t34*(-32+8*pow<2>(u)))))))+t12*(-64*u+t34*(256*u+t34*(96*u+t34*(-160*u+t34*(-160*u+32*t34*u))))+t12*(160+pow<2>(t34)*(96+t34*(-128+t34*(80+t34*(64+t34*(16-4*pow<2>(u)))-4*pow<2>(u)))-168*pow<2>(u))-8*pow<2>(u)+t12*(96*u+t34*(-288*u+t34*(96*u+t34*(32*u+t34*(80*u-16*t34*u))))+t12*(-160-8*pow<2>(u)+t34*(128+t34*(-128+80*pow<2>(u)+pow<2>(t34)*(-48+4*pow<2>(u))))+t12*(-64*u+t34*(128*u+t34*(-96*u+32*t34*u))+t12*(80+t12*(16*u-16*t34*u+t12*(-16-4*pow<2>(u)))+12*pow<2>(u)+t34*(-64+t34*(48+4*pow<2>(u)))))))))))))+t12*(u*(384-32*pow<2>(u))+t34*(u*(-128+96*pow<2>(u))+t34*(u*(128-96*pow<2>(u))+t34*u*(-384+32*pow<2>(u))))+t12*(128+(128-8*pow<2>(u))*pow<2>(u)+t34*(512-384*pow<2>(u)+t34*(-1344+(96-4*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(320+(96-12*pow<2>(u))*pow<2>(u))))+t12*(u*(-896+32*pow<2>(u))+t34*(u*(384-160*pow<2>(u))+t34*(t34*u*(768-64*pow<2>(u))+u*(-256+192*pow<2>(u))))+t12*(320+pow<2>(u)*(-96+4*pow<2>(u))+t34*(-1024+768*pow<2>(u)+t34*(960+(-96-4*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-160+pow<2>(u)*(-48+6*pow<2>(u)))))+t12*(u*(640+32*pow<2>(u))+t34*(u*(-384+32*pow<2>(u))+t34*(u*(128-96*pow<2>(u))+t34*u*(-384+32*pow<2>(u))))+t12*(-384+t34*(512-384*pow<2>(u)+t34*(-192+pow<2>(u)*(32+4*pow<2>(u))))+t12*(u*(-128-32*pow<2>(u))+t34*u*(128+32*pow<2>(u))+t12*(96+pow<2>(u)*(16+6*pow<2>(u))))-8*pow<4>(u))))))))))));
    return todouble(foo);
}

// Coefficient order epsilon^1 of bubble type 5
template<>
template<>
double qq2yyg1<TT>::SC::bub::c<5,1>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    const TT foo = (-2048+pow<2>(u)*(1536+pow<2>(u)*(-384+32*pow<2>(u)))+t34*(4096+pow<2>(u)*(-1024+pow<2>(u)*(-256+64*pow<2>(u)))+t34*(12288+pow<2>(u)*(-5120+pow<2>(u)*(256+64*pow<2>(u)))+t34*(6144+pow<2>(u)*(-3072+384*pow<2>(u)))))+zb*(14336+pow<2>(u)*(-6144+pow<2>(u)*(384+64*pow<2>(u)))+t34*(pow<2>(u)*(-2560+pow<2>(u)*(256+96*pow<2>(u)))+t34*(-21504+pow<2>(u)*(6144+(-64-32*pow<2>(u))*pow<2>(u))+t34*(8192+(-1024-256*pow<2>(u))*pow<2>(u)+t34*(15360+pow<2>(u)*(-4608+192*pow<2>(u))))))+zb*(-16384+pow<2>(u)*(128+(1088-24*pow<2>(u))*pow<2>(u))+t34*(17920+pow<2>(u)*(-6144+(1248-80*pow<2>(u))*pow<2>(u))+t34*(30208+pow<2>(u)*(-7296+(32-24*pow<2>(u))*pow<2>(u))+t34*(-37376+(5632-224*pow<2>(u))*pow<2>(u)+t34*(-17920+t34*(15360-2304*pow<2>(u))+(4864-352*pow<2>(u))*pow<2>(u)))))+t12*(u*(6912+(640-336*pow<2>(u))*pow<2>(u))+t34*(u*(6912+pow<2>(u)*(-2944+48*pow<2>(u)))+t34*(u*(-26880+(3072-304*pow<2>(u))*pow<2>(u))+t34*(t34*(u*(13824-1152*pow<2>(u))+t34*u*(4608-384*pow<2>(u)))+u*(-13568+(2816-176*pow<2>(u))*pow<2>(u)))))+t12*(32256+pow<2>(u)*(-3072+pow<2>(u)*(-1952+48*pow<2>(u)))+t34*(-25600+pow<2>(u)*(11520+pow<2>(u)*(-3584+96*pow<2>(u)))+t34*(-50176+pow<2>(u)*(7680+pow<2>(u)*(640+48*pow<2>(u)))+t34*(6144+pow<2>(u)*(6656+256*pow<2>(u))+t34*(5120+(4864-128*pow<2>(u))*pow<2>(u)))))+t12*(u*(-15872+2688*pow<2>(u))+t34*(u*(-20480+pow<2>(u)*(5248+288*pow<2>(u)))+t34*(t34*u*(4096+pow<2>(u)*(2048+128*pow<2>(u)))+u*(6656+pow<2>(u)*(2304+416*pow<2>(u)))))+t12*(-15360+pow<2>(u)*(1408+pow<2>(u)*(1216+8*pow<2>(u)))+t12*(t34*u*(9472+(-2560-16*pow<2>(u))*pow<2>(u))+u*(8960+pow<2>(u)*(-2048+16*pow<2>(u)))+t12*(-512+pow<2>(u)*(-512+32*pow<2>(u))))+t34*(7680+pow<2>(u)*(-5120+pow<2>(u)*(2080+32*pow<2>(u)))+t34*(22016+pow<2>(u)*(-5504+(800-8*pow<2>(u))*pow<2>(u))))))))+zb*(2304+(3712-176*pow<2>(u))*pow<2>(u)+t34*(-25088+pow<2>(u)*(256+352*pow<2>(u))+t34*(8704+pow<2>(u)*(-4480+768*pow<2>(u))+t34*(58880+pow<2>(u)*(-5376+416*pow<2>(u))+t34*(-10496+pow<2>(u)*(-896+176*pow<2>(u))+t34*(-25600+t34*(7680-384*pow<2>(u))+3072*pow<2>(u))))))+zb*(3968+pow<2>(u)*(-1312+112*pow<2>(u))+t34*(9728+pow<2>(u)*(4224+192*pow<2>(u))+t34*(-20224+pow<2>(u)*(1856+96*pow<2>(u))+t34*(-11904+pow<2>(u)*(-4480+64*pow<2>(u))+t34*(39296+pow<2>(u)*(-2848+48*pow<2>(u))+t34*(10496-1536*pow<2>(u)+t34*(-12800+1920*t34+512*pow<2>(u)))))))+t12*(u*(6144+256*pow<2>(u))+t34*(u*(-12288-1024*pow<2>(u))+t34*(u*(9024-2048*pow<2>(u))+t34*(u*(34240-1792*pow<2>(u))+t34*(u*(-3456-768*pow<2>(u))+t34*(t34*(576*u+192*t34*u)+u*(-9856+256*pow<2>(u)))))))+t12*(-7552+(-320-216*pow<2>(u))*pow<2>(u)+t34*(-24064+(1568-768*pow<2>(u))*pow<2>(u)+t34*(13184+(1440-344*pow<2>(u))*pow<2>(u)+t34*(15104+(1408-368*pow<2>(u))*pow<2>(u)+t34*(-29184+(960-96*pow<2>(u))*pow<2>(u)+t34*(-11264+t34*(3840-32*pow<2>(u))+608*pow<2>(u))))))+t12*(u*(-16128+304*pow<2>(u))+t34*(u*(4096+1744*pow<2>(u))+t34*(u*(6912-192*pow<2>(u))+t34*(u*(-28672+1536*pow<2>(u))+t34*(t34*u*(4608-208*pow<2>(u))+u*(-5632+400*pow<2>(u))))))+t12*(5504+pow<2>(u)*(2304+96*pow<2>(u))+t12*(t34*(t34*(u*(1216-1472*pow<2>(u))+t34*u*(9024-128*pow<2>(u)))+u*(7680-1824*pow<2>(u)))+u*(13824-480*pow<2>(u))+t12*(-2176+pow<2>(u)*(-704+8*pow<2>(u))+t12*(u*(-3840-80*pow<2>(u))+t12*(256+32*pow<2>(u))+t34*u*(-3584+80*pow<2>(u)))+t34*(-6656+(800-64*pow<2>(u))*pow<2>(u)+t34*(-4480+pow<2>(u)*(1440+8*pow<2>(u))))))+t34*(20992+pow<2>(u)*(-1216+320*pow<2>(u))+t34*(1280+pow<2>(u)*(-4736+400*pow<2>(u))+t34*(1664+pow<2>(u)*(-5120+224*pow<2>(u))+t34*(12544+pow<2>(u)*(-96+48*pow<2>(u))))))))))+zb*(736*pow<2>(u)+t34*(1536-96*pow<2>(u)+t34*(5440+192*pow<2>(u)+t34*(-8448+t34*(-13248+t34*(9728+t34*(7616+t34*(-2816+192*t34)-256*pow<2>(u))-480*pow<2>(u))-672*pow<2>(u))+576*pow<2>(u))))+t12*(t34*(t34*(t34*(t34*(t34*(-1984*t34*u+u*(3392-96*pow<2>(u)))+u*(13120-224*pow<2>(u)))+u*(-5248-320*pow<2>(u)))+u*(-4672-576*pow<2>(u)))+u*(5440-608*pow<2>(u)))+u*(-2880-224*pow<2>(u))+t12*(1792-688*pow<2>(u)+t34*(-6912-1280*pow<2>(u)+t34*(-11328+1520*pow<2>(u)+t34*(2880+3648*pow<2>(u)+t34*(4480+2032*pow<2>(u)+t34*(-7040+704*pow<2>(u)+t34*(-5184+832*t34+208*pow<2>(u)))))))+t12*(u*(4096+432*pow<2>(u))+t34*(u*(-10944+1200*pow<2>(u))+t34*(u*(-8256+1824*pow<2>(u))+t34*(u*(5376+640*pow<2>(u))+t34*(u*(-6656+368*pow<2>(u))+t34*(1088*t34*u+u*(-3136+144*pow<2>(u)))))))+t12*(-3840-816*pow<2>(u)+t34*(9216+t34*(18880+t34*(2048+t34*(2368+t34*(4608-240*pow<2>(u))-1104*pow<2>(u))-896*pow<2>(u))-1312*pow<2>(u))+144*pow<2>(u))+t12*(t34*(t34*(t34*(u*(640-320*pow<2>(u))+t34*u*(1856-64*pow<2>(u)))+u*(10560-640*pow<2>(u)))+u*(10176-576*pow<2>(u)))+u*(384-192*pow<2>(u))+t12*(2304+752*pow<2>(u)+t12*(u*(-1536-16*pow<2>(u))+t12*(-256-64*t12*u+16*pow<2>(u)-16*t34*pow<2>(u))+t34*(u*(-4672-16*pow<2>(u))+t34*u*(-3008+32*pow<2>(u))))+t34*(-3840+1248*pow<2>(u)+t34*(-10944+1136*pow<2>(u)+t34*(-4416+640*pow<2>(u))))))))))+zb*(64+zb*(-368+t34*(32+t34*(992+t34*(-96+t34*(-768+t34*(96+t34*(32+t34*(-32+112*t34)))))))+t12*(t34*(176*u+t34*(-32*u+t34*(-304*u+t34*(64*u+t34*(80*u+t34*(-32*u+48*t34*u))))))+t12*(1488+t34*(-16+t34*(-1840+t34*(-48+t34*(1040+t34*(528+t34*(-592+(-464-96*t34)*t34))))))+t12*(t12*(-2272+t34*(-128+t34*(1104+t34*(-512+t34*(928+t34*(480+t34*(496+416*t34))))))+t12*(t12*(1568+t34*(160+t34*(-656+t34*(656+(-944-656*t34)*t34)))+t12*(t34*(-224*u+t34*(-640*u+t34*(-608*u-192*t34*u)))+t12*(-432+t34*(-32+400*t34)+t12*(t12*(16-16*t34)+t34*(16*u-16*t34*u)))))+t34*(576*u+t34*(1296*u+t34*(912*u+t34*(768*u+t34*(464*u+80*t34*u)))))))+t34*(-544*u+t34*(-608*u+(-320*u+t34*(-416*u+t34*(-96*u-64*t34*u)))*pow<2>(t34))))))+zb*pow<2>(t12)*((-88+t34*(16+t34*(152+t34*(-32+t34*(-40+(16-24*t34)*t34)))))*pow<2>(t34)+pow<2>(t12)*(pow<2>(t12)*((-288+t34*(-432+t34*(-72+(-96-136*t34)*t34)))*pow<2>(t34)+pow<2>(t12)*((112+t34*(224+112*t34))*pow<2>(t34)-8*pow<2>(t12)*pow<2>(t34)))+pow<2>(t34)*(272+t34*(192+t34*(-192+t34*(64+t34*(144+32*pow<2>(t34)))))))))-88*pow<2>(u)+t34*(224+16*pow<2>(u)+t34*(-96+152*pow<2>(u)+t34*(1248-32*pow<2>(u)+t34*(-224-40*pow<2>(u)+t34*(-3168+t34*(480+(1696-224*t34)*t34-24*pow<2>(u))+16*pow<2>(u))))))+t12*(-80*u+t34*(-2896*u+t34*(432*u+t34*(1840*u+t34*(-1392*u+t34*(1168*u+t34*(1040*u-112*t34*u))))))+t12*(-2464+272*pow<2>(u)+t34*(896+640*pow<2>(u)+t34*(704+688*pow<2>(u)+t34*(-192+640*pow<2>(u)+t34*(1696+496*pow<2>(u)+t34*(-1408+256*pow<2>(u)+t34*(-1792+t34*(-1088+64*t34)+80*pow<2>(u)))))))+t12*(192*u+t34*(5664*u+t34*(1344*u+t34*(-5856*u+t34*(-2240*u+t34*(-1440*u+t34*(-832*u+96*t34*u))))))+t12*(7040+t34*(576+t34*(-2688+t34*(3296+t34*(-608+t34*(1760+t34*(1632-72*pow<2>(u))-256*pow<2>(u))-712*pow<2>(u))-1824*pow<2>(u))-1824*pow<2>(u))-1296*pow<2>(u))-288*pow<2>(u)+t12*(-96*u+t34*(-2688*u+t34*(-1296*u+t34*(3504*u+t34*(1520*u+336*t34*u))))+t12*(-6976+112*pow<2>(u)+t12*(-64*u+t34*(-32*u+t34*(-480*u-512*t34*u))+t12*(2368+t12*(-32*t12+48*u-48*t34*u)-8*pow<2>(u)+t34*(3040+32*pow<2>(u)+t34*(736+8*pow<2>(u)))))+t34*(-4736+608*pow<2>(u)+t34*(1344+976*pow<2>(u)+t34*(-2560+576*pow<2>(u)+t34*(-1248+96*pow<2>(u))))))))))))))+t12*(u*(-5632+pow<2>(u)*(-2304+96*pow<2>(u)))+t34*(u*(21248+pow<2>(u)*(-896+112*pow<2>(u)))+t34*(u*(27648+pow<2>(u)*(-384+256*pow<2>(u)))+t34*(u*(-23296+pow<2>(u)*(1408+48*pow<2>(u)))+t34*(t34*(4608*u+1536*t34*u)+u*(-19968+2176*pow<2>(u))))))+t12*(-10496+pow<2>(u)*(3072+80*pow<2>(u))+t34*(41728+pow<2>(u)*(-4992+752*pow<2>(u))+t34*(4352+(-2176-816*pow<2>(u))*pow<2>(u)+t34*(-60160+(512-16*pow<2>(u))*pow<2>(u)+t34*(-7680+(1664-128*pow<2>(u))*pow<2>(u)+t34*(7680+896*pow<2>(u))))))+t12*(u*(13312+(1664-64*pow<2>(u))*pow<2>(u))+t34*(u*(-18432+(512-224*pow<2>(u))*pow<2>(u))+t34*(u*(-50176+(2560-256*pow<2>(u))*pow<2>(u))+t34*(t34*u*(8192-384*pow<2>(u))+u*(-1024+(-384-96*pow<2>(u))*pow<2>(u)))))+t12*(9984+pow<2>(u)*(-10880+432*pow<2>(u))+t34*(-14336+pow<2>(u)*(-11392+544*pow<2>(u))+t34*(1280+(-2304-16*pow<2>(u))*pow<2>(u)+t34*(22528+(1024-128*pow<2>(u))*pow<2>(u))))+t12*(u*(-7680+pow<2>(u)*(-1280+32*pow<2>(u)))+t34*(u*(9472+(-384-16*pow<2>(u))*pow<2>(u))+t34*u*(17664+(640-16*pow<2>(u))*pow<2>(u)))+t12*(-1792+(6144-80*pow<2>(u))*pow<2>(u)+t34*(-2304+pow<2>(u)*(5632+80*pow<2>(u)))+128*t12*pow<3>(u)))))))))+t12*(u*(-1024+pow<2>(u)*(-1536+448*pow<2>(u)))+t34*(u*(-11264+(3584-192*pow<2>(u))*pow<2>(u))+t34*(t34*(u*(18432-4608*pow<2>(u))+t34*u*(6144-1536*pow<2>(u)))+u*(7168+(-1024-192*pow<2>(u))*pow<2>(u))))+t12*(-24576+pow<2>(u)*(8448+(384-240*pow<2>(u))*pow<2>(u))+t34*(-10240+pow<2>(u)*(6656+(-128-224*pow<2>(u))*pow<2>(u))+t34*(5120+t34*(-3072+(7680-1728*pow<2>(u))*pow<2>(u))+pow<2>(u)*(11008+pow<2>(u)*(-3136+16*pow<2>(u)))))+t12*(t34*(t34*u*(-3072+(3584-704*pow<2>(u))*pow<2>(u))+u*(-1024+pow<2>(u)*(4352+pow<2>(u)*(-1088+16*pow<2>(u)))))+(1280+(-256-16*pow<2>(u))*pow<2>(u))*pow<3>(u)+t12*(10240+pow<2>(u)*(-4352+pow<2>(u)*(256+48*pow<2>(u)))+t34*(10240+pow<2>(u)*(-3840+(512-48*pow<2>(u))*pow<2>(u)))+t12*u*(1024-64*pow<4>(u)))))))+t12*(u*(2048+pow<2>(u)*(-512+pow<2>(u)*(-128+32*pow<2>(u))))+t34*(u*(10240+pow<2>(u)*(-4608+pow<2>(u)*(384+32*pow<2>(u))))+t34*(t34*u*(3072+pow<2>(u)*(-1536+192*pow<2>(u)))+u*(9216+pow<2>(u)*(-4608+576*pow<2>(u)))))+t12*(2048+pow<2>(u)*(1024+pow<2>(u)*(-1024+(192-8*pow<2>(u))*pow<2>(u)))+t34*(-4096+pow<2>(u)*(5632+pow<2>(u)*(-2048+224*pow<2>(u)))+t34*(-4096+pow<2>(u)*(3584+pow<2>(u)*(-1024+96*pow<2>(u)))))+t12*(t12*pow<2>(u)*(-512+(256-32*pow<2>(u))*pow<2>(u))+u*(-2048+pow<2>(u)*(1280+pow<2>(u)*(-256+16*pow<2>(u))))+t34*u*(-2048+pow<2>(u)*(768-16*pow<4>(u)))))))*pow<-1>(256+pow<2>(u)*(-256+pow<2>(u)*(96+(-16+pow<2>(u))*pow<2>(u)))+t34*(256+pow<2>(u)*(-256+pow<2>(u)*(96+(-16+pow<2>(u))*pow<2>(u))))+pow<2>(t12)*(-512+pow<2>(u)*(512+pow<2>(u)*(-192+(32-2*pow<2>(u))*pow<2>(u)))+t34*(-512+pow<2>(u)*(512+pow<2>(u)*(-192+(32-2*pow<2>(u))*pow<2>(u))))+pow<2>(t12)*(256+pow<2>(u)*(-256+pow<2>(u)*(96+(-16+pow<2>(u))*pow<2>(u)))+t34*(256+pow<2>(u)*(-256+pow<2>(u)*(96+(-16+pow<2>(u))*pow<2>(u))))))+zb*(-512+pow<2>(u)*(384+pow<2>(u)*(-96+8*pow<2>(u)))+pow<2>(t34)*(512+pow<2>(u)*(-384+(96-8*pow<2>(u))*pow<2>(u)))+pow<2>(t12)*(1024+pow<2>(u)*(-768+(192-16*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(-1024+pow<2>(u)*(768+pow<2>(u)*(-192+16*pow<2>(u))))+pow<2>(t12)*(-512+pow<2>(u)*(384+pow<2>(u)*(-96+8*pow<2>(u)))+pow<2>(t34)*(512+pow<2>(u)*(-384+(96-8*pow<2>(u))*pow<2>(u)))))+zb*(256+pow<2>(u)*(-192+(48-4*pow<2>(u))*pow<2>(u))+t34*(-768+pow<2>(u)*(320+(-16-4*pow<2>(u))*pow<2>(u))+t34*(-768+pow<2>(u)*(320+(-16-4*pow<2>(u))*pow<2>(u))+t34*(256+pow<2>(u)*(-192+(48-4*pow<2>(u))*pow<2>(u)))))+t12*(u*pow<2>(t34)*(-256+(128-16*pow<2>(u))*pow<2>(u))+u*(256+pow<2>(u)*(-128+16*pow<2>(u)))+t12*(-768+pow<2>(u)*(448+pow<2>(u)*(-80+4*pow<2>(u)))+t34*(1280+pow<2>(u)*(-576+pow<2>(u)*(48+4*pow<2>(u)))+t34*(1536+pow<2>(u)*(-640+pow<2>(u)*(32+8*pow<2>(u)))+t34*(-512+pow<2>(u)*(384+pow<2>(u)*(-96+8*pow<2>(u))))))+t12*(u*(-512+(256-32*pow<2>(u))*pow<2>(u))+u*pow<2>(t34)*(512+pow<2>(u)*(-256+32*pow<2>(u)))+t12*(768+pow<2>(u)*(-320+pow<2>(u)*(16+4*pow<2>(u)))+t12*(u*pow<2>(t34)*(-256+(128-16*pow<2>(u))*pow<2>(u))+u*(256+pow<2>(u)*(-128+16*pow<2>(u)))+t12*(-256+pow<2>(u)*(64+(16-4*pow<2>(u))*pow<2>(u))+t34*(-256+pow<2>(u)*(64+(16-4*pow<2>(u))*pow<2>(u)))))+t34*(-256+pow<2>(u)*(192+pow<2>(u)*(-48+4*pow<2>(u)))+t34*(-768+pow<2>(u)*(320+(-16-4*pow<2>(u))*pow<2>(u))+t34*(256+pow<2>(u)*(-192+(48-4*pow<2>(u))*pow<2>(u)))))))))+zb*(128+(64-24*pow<2>(u))*pow<2>(u)+t34*(768+(-128-16*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-768+pow<2>(u)*(128+16*pow<2>(u))+t34*(-128+pow<2>(u)*(-64+24*pow<2>(u)))))+zb*(-160+pow<2>(u)*(-48+6*pow<2>(u))+t34*(-160+pow<2>(u)*(-48+6*pow<2>(u))+t34*(576+pow<2>(u)*(-32+4*pow<2>(u))+t34*(576+pow<2>(u)*(-32+4*pow<2>(u))+t34*(-160+pow<2>(u)*(-48+6*pow<2>(u))+t34*(-160+pow<2>(u)*(-48+6*pow<2>(u)))))))+zb*(32+24*pow<2>(u)+t34*(-128+32*pow<2>(u)+t34*(-352-8*pow<2>(u)+pow<2>(t34)*(352+t34*(128+t34*(-32-24*pow<2>(u))-32*pow<2>(u))+8*pow<2>(u))))+zb*(16+zb*(-8+zb*(1+t34*(1+t34*(-4+t34*(-4+t34*(6+t34*(6+t34*(-4+t34*(-4+t34*(1+t34))))))))+pow<2>(t12)*(-6+t34*(-6+t34*(12+t34*(12+t34*(-8+t34*(-8+t34*(4+t34*(4+(-2-2*t34)*t34)))))))+pow<2>(t12)*(15+t34*(15+t34*(-8+t34*(-8+t34*(4+t34*(4+t34*(4+t34*(4+t34*(1+t34))))))))+pow<2>(t12)*(-20+t34*(-20+t34*(-8+t34*(-8+t34*(-8+t34*(-8+(-4-4*t34)*t34)))))+pow<2>(t12)*(15+t34*(15+t34*(12+t34*(12+t34*(6+6*t34))))+pow<2>(t12)*(-6+t34*(-6+(-4-4*t34)*t34)+(1+t34)*pow<2>(t12)))))))+t12*(t34*(16*u+t34*(16*u+t34*(-32*u+t34*(-32*u+t34*(16*u+16*t34*u)))))+t12*(40+t34*(64+t34*(-40+t34*(-96+t34*(8+t34*(64+t34*(8+(-32-16*t34)*t34))))))+t12*(t34*(-64*u+t34*(-64*u+t34*(32*u+t34*(32*u+t34*(-32*u-32*t34*u)))))+t12*(-80+t34*(-96+t34*(32+t34*(64+t34*(8+t34*(16+t34*(32+t34*(16+8*t34)))))))+t12*(t34*(96*u+t34*(96*u+t34*(32*u+t34*(32*u+t34*(16*u+16*t34*u)))))+t12*(80+t34*(64+t34*(-16+t34*(-32+t34*(-40+(-32-24*t34)*t34))))+t12*(t34*(-64*u+t34*(-64*u+t34*(-32*u-32*t34*u)))+t12*(-40+t34*(-16+t34*(16+t34*(16+24*t34)))+t12*(t34*(16*u+16*t34*u)+t12*(8-8*pow<2>(t34)))))))))))+t34*(-16+t34*(16+t34*(48+(-48+t34*(-16+t34*(16+8*t34)))*pow<2>(t34)))))-4*pow<2>(u)+t34*(80-4*pow<2>(u)+t34*(48+4*pow<2>(u)+t34*(-144+4*pow<2>(u)+t34*(-144+4*pow<2>(u)+t34*(48+t34*(80+t34*(16-4*pow<2>(u))-4*pow<2>(u))+4*pow<2>(u))))))+t12*(16*u+t34*(-64*u+t34*(-176*u+(176*u+t34*(64*u-16*t34*u))*pow<2>(t34)))+t12*(-80+12*pow<2>(u)+t34*(-208+12*pow<2>(u)+t34*(-128+80*pow<2>(u)+t34*(256+80*pow<2>(u)+t34*(240-4*pow<2>(u)+t34*(-144-4*pow<2>(u)+t34*(-160+8*pow<2>(u)+t34*(-32+8*pow<2>(u))))))))+t12*(-64*u+t34*(192*u+t34*(352*u+t34*(-64*u+t34*(-320*u+t34*(-128*u+32*t34*u)))))+t12*(160+t34*(160+t34*(96+t34*(-32+t34*(-48+t34*(144+t34*(80+t34*(16-4*pow<2>(u))-4*pow<2>(u))-4*pow<2>(u))-4*pow<2>(u))-168*pow<2>(u))-168*pow<2>(u))-8*pow<2>(u))-8*pow<2>(u)+t12*(96*u+t34*(-192*u+t34*(-192*u+t34*(128*u+t34*(112*u+t34*(64*u-16*t34*u)))))+t12*(-160-8*pow<2>(u)+t34*(-32-8*pow<2>(u)+t34*(80*pow<2>(u)+t34*(-128+80*pow<2>(u)+t34*(-48+4*pow<2>(u)+t34*(-48+4*pow<2>(u))))))+t12*(-64*u+t34*(64*u+t34*(32*u+t34*(-64*u+32*t34*u)))+t12*(80+t12*(16*u-16*u*pow<2>(t34)+t12*(-16+t34*(-16-4*pow<2>(u))-4*pow<2>(u)))+12*pow<2>(u)+t34*(16+12*pow<2>(u)+t34*(-16+4*pow<2>(u)+t34*(48+4*pow<2>(u)))))))))))))+t12*(-128*u+t34*(t34*(u*(384-32*pow<2>(u))+t34*(u*(384-32*pow<2>(u))+t34*(-128*t34*u-32*pow<3>(u))))-32*pow<3>(u))+t12*(-64*pow<2>(u)+t34*(128+96*pow<2>(u)+t34*(704+16*pow<2>(u)+t34*(128-160*pow<2>(u)+t34*(-768+t34*(-256+64*pow<2>(u)+t34*(64+48*pow<2>(u)))))))+t12*(384*u+t12*(-192+48*pow<2>(u)+t34*(128-288*pow<2>(u)+t34*(-256+t34*(-256+t34*(480+t34*(128+t34*(-32-24*pow<2>(u))-32*pow<2>(u))-24*pow<2>(u))+320*pow<2>(u))))+t12*(-384*u+t12*(256+t34*(-128+160*pow<2>(u)+t34*(-192-16*pow<2>(u)+t34*(128-160*pow<2>(u)+t34*(-64+16*pow<2>(u)))))+t12*(128*u+t12*(-96-8*pow<2>(u)+pow<2>(t34)*(96+8*pow<2>(u)))+t34*(t34*(128*t34*u-32*pow<3>(u))-32*pow<3>(u))))+t34*(t34*(u*(384+32*pow<2>(u))+t34*(u*(128-32*pow<2>(u))+t34*(-128*t34*u-32*pow<3>(u))))+32*pow<3>(u))))+t34*(32*pow<3>(u)+t34*(u*(-768+32*pow<2>(u))+t34*(u*(-640+64*pow<2>(u))+t34*(256*t34*u+64*pow<3>(u)))))))))+t12*(u*(384-32*pow<2>(u))+t34*(u*(256+64*pow<2>(u))+pow<2>(t34)*(u*(-256-64*pow<2>(u))+t34*u*(-384+32*pow<2>(u))))+t12*(128+(128-8*pow<2>(u))*pow<2>(u)+t34*(640+(-256-8*pow<2>(u))*pow<2>(u)+t34*(-832+(-288-4*pow<2>(u))*pow<2>(u)+t34*(-1344+(96-4*pow<2>(u))*pow<2>(u)+t34*(320+(96-12*pow<2>(u))*pow<2>(u)+t34*(320+(96-12*pow<2>(u))*pow<2>(u))))))+t12*(u*(-896+32*pow<2>(u))+t34*(u*(-512-128*pow<2>(u))+t34*(u*(128+32*pow<2>(u))+t34*(t34*u*(768-64*pow<2>(u))+u*(512+128*pow<2>(u)))))+t12*(320+pow<2>(u)*(-96+4*pow<2>(u))+t34*(-704+pow<2>(u)*(672+4*pow<2>(u))+t34*(-64+(672-4*pow<2>(u))*pow<2>(u)+t34*(960+(-96-4*pow<2>(u))*pow<2>(u)+t34*(-160+pow<2>(u)*(-48+6*pow<2>(u))+t34*(-160+pow<2>(u)*(-48+6*pow<2>(u)))))))+t12*(u*(640+32*pow<2>(u))+t34*(u*(256+64*pow<2>(u))+t34*(u*(-256-64*pow<2>(u))+t34*(u*(-256-64*pow<2>(u))+t34*u*(-384+32*pow<2>(u)))))+t12*(-384+t34*(128+(-384-8*pow<2>(u))*pow<2>(u)+t34*(320+pow<2>(u)*(-352+4*pow<2>(u))+t34*(-192+pow<2>(u)*(32+4*pow<2>(u)))))+t12*(u*(-128-32*pow<2>(u))+u*pow<2>(t34)*(128+32*pow<2>(u))+t12*(96+pow<2>(u)*(16+6*pow<2>(u))+t34*(96+pow<2>(u)*(16+6*pow<2>(u)))))-8*pow<4>(u))))))))+t12*(u*(-512+128*pow<2>(u))+t34*(u*(-256+16*pow<4>(u))+t34*(t34*u*(-512+128*pow<2>(u))+u*(-256+16*pow<4>(u))))+t12*(128+pow<2>(u)*(-192+40*pow<2>(u))+t34*(-1536+pow<2>(u)*(256+32*pow<2>(u))+t34*(-384+pow<2>(u)*(64+8*pow<2>(u))+t34*(1536+(-256-32*pow<2>(u))*pow<2>(u)+t34*(256+(128-48*pow<2>(u))*pow<2>(u)))))+t12*(u*(1024-256*pow<2>(u))+t34*(t34*(t34*u*(1024-256*pow<2>(u))+u*(512-32*pow<4>(u)))+u*(512-32*pow<4>(u)))+t12*(-640+(192-8*pow<2>(u))*pow<2>(u)+t34*(768+(-128-16*pow<2>(u))*pow<2>(u)+t34*(768+(-128-16*pow<2>(u))*pow<2>(u)+t34*(-768+pow<2>(u)*(128+16*pow<2>(u))+t34*(-128+pow<2>(u)*(-64+24*pow<2>(u))))))+t12*(u*(-512+128*pow<2>(u))+t12*(384+(-64-8*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-384+pow<2>(u)*(64+8*pow<2>(u))))+t34*(u*(-256+16*pow<4>(u))+t34*(t34*u*(-512+128*pow<2>(u))+u*(-256+16*pow<4>(u)))))))))))));
    return todouble(foo);
}

// Coefficient of bubble type 5 as a series in epsilon
template<>
template<>
EpsExp qq2yyg1<TT>::SC::bub::c<5>(const TT& zb, const TT& t12, const TT& t34, const TT& u)
{
    return EpsExp(0,{
        qq2yyg1<TT>::SC::bub::c<5,0>(zb,t12,t34,u),
        qq2yyg1<TT>::SC::bub::c<5,1>(zb,t12,t34,u)
    });
}


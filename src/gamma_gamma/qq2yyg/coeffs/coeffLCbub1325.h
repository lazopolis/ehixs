#include "qq2yyg/qq2yyg1.h"
#include "counterforge.h"

// Expansion order epsilon^-1 of c13*bubble(13)+c25*bubble(25) 
template<>
template<>
double qq2yyg1<TT>::LC::bub::c1325<-1>(const TT& zb, const TT& t12, const TT& t34)
{
    const TT foo = ((t34*(zb*(2816+zb*(-20800+zb*(64800+zb*(-110288+zb*(110632+zb*(-65304+(20952-2808*zb)*zb))))))+t34*(-2560+zb*(14592+zb*(-31680+zb*(25856+zb*(17184+zb*(-60392+zb*(59392+zb*(-27210+4815*zb)))))))+t34*(-1280+zb*(16256+zb*(-81088+zb*(218688+zb*(-358144+zb*(370024+zb*(-236752+(85354-13047*zb)*zb))))))+t34*(6400+zb*(-48640+zb*(178944+zb*(-414720+zb*(635984+zb*(-630960+zb*(383056+zb*(-127894+17823*zb)))))))+t34*(-3840+zb*(27392+zb*(-93504+zb*(207712+zb*(-320976+zb*(330368+zb*(-207952+(70518-9735*zb)*zb))))))+t34*(-2816+zb*(16896+zb*(-43584+zb*(63808+zb*(-55632+zb*(23336+zb*(2704+zb*(-6278+1589*zb)))))))+t34*(2048+zb*(-13696+zb*(40256+zb*(-70592+zb*(82800+zb*(-64840+zb*(30432+zb*(-6810+403*zb)))))))+t34*(zb*(768+zb*(-5376+zb*(15808+zb*(-25184+zb*(23152+zb*(-11872+(2918-227*zb)*zb))))))+t34*(512+zb*(-2816+zb*(6128+zb*(-6600+zb*(3544+zb*(-798+35*zb))))))*pow<2>(zb)))))))))+flog<TT>(((1+t12)*zb)/2)*(pow<2>(t34)*(-2048+zb*(15360+zb*(-49152+zb*(86784+zb*(-90880+zb*(56064+zb*(-18720+2592*zb))))))+t34*(1024+zb*(-9216+zb*(35072+zb*(-72960+zb*(89280+zb*(-63936+(24624-3888*zb)*zb)))))+t34*(4096+zb*(-30720+zb*(96768+zb*(-165120+zb*(163712+zb*(-93504+(28224-3456*zb)*zb)))))+t34*(-2048+zb*(18432+zb*(-69888+zb*(144384+zb*(-174976+zb*(123840+zb*(-47088+7344*zb))))))+t34*(-2048+zb*(15360+zb*(-46080+zb*(69888+zb*(-54784+zb*(18816+(-288-864*zb)*zb)))))+t34*(1024+zb*(-9216+zb*(34560+zb*(-69888+zb*(82112+zb*(-55872+(20304-3024*zb)*zb)))))+t34*(t34*(256+zb*(-1536+zb*(3584+zb*(-4032+(2160-432*zb)*zb))))*pow<2>(zb)+(-1536+zb*(8448+zb*(-18048+zb*(18624+zb*(-9216+1728*zb)))))*pow<2>(zb))))))))+t12*(t34*(-4096+zb*(30720+zb*(-98304+zb*(173568+zb*(-181760+zb*(112128+zb*(-37440+5184*zb))))))+t34*(1024+zb*(-8192+zb*(29952+zb*(-63232+zb*(81472+zb*(-63104+(26832-4752*zb)*zb)))))+t34*(8192+zb*(-63488+zb*(208384+zb*(-373760+zb*(392704+zb*(-238848+(76320-9504*zb)*zb)))))+t34*(-2048+zb*(16384+zb*(-60160+zb*(128768+zb*(-168832+zb*(131968+zb*(-55440+9360*zb))))))+t34*(-4096+zb*(34816+zb*(-121856+zb*(226560+zb*(-239104+zb*(139968+zb*(-39744+3456*zb))))))+t34*(1024+zb*(-8192+zb*(30464+zb*(-67840+zb*(93248+zb*(-74624+(30384-4464*zb)*zb)))))+t34*(zb*(-2048+zb*(11776+zb*(-26112+zb*(27136+zb*(-11904+zb*(288+864*zb))))))+t34*((-256+zb*(2304+zb*(-5888+zb*(5760+(-1776-144*zb)*zb))))*pow<2>(zb)+t34*(-256+zb*(1024+zb*(-1344+576*zb)))*pow<3>(zb)))))))))+t12*(-2048+zb*(15360+zb*(-49152+zb*(86784+zb*(-90880+zb*(56064+zb*(-18720+2592*zb))))))+t34*(-1024+zb*(11264+zb*(-45312+zb*(92416+zb*(-104896+zb*(65600+zb*(-20208+2160*zb))))))+t34*(4096+zb*(-32768+zb*(112128+zb*(-211968+zb*(237312+zb*(-155584+(54272-7488*zb)*zb)))))+t34*(2048+zb*(-22528+zb*(91392+zb*(-186624+zb*(208896+zb*(-125440+(34704-2448*zb)*zb)))))+t34*(-2048+zb*(19456+zb*(-76800+zb*(163840+zb*(-202624+zb*(142592+zb*(-50656+6240*zb))))))+t34*(-1024+zb*(11264+zb*(-46848+zb*(96000+zb*(-103104+zb*(54272+(-9264-1296*zb)*zb)))))+t34*(zb*(-2048+zb*(13824+zb*(-38912+zb*(56832+zb*(-42688+(13376-384*zb)*zb)))))+t34*((768+zb*(-1792+zb*(-896+zb*(5376+zb*(-4752+1296*zb)))))*pow<2>(zb)+t34*((256+zb*(-640+zb*(-384+(1728-960*zb)*zb)))*pow<3>(zb)+t34*(192+zb*(-480+288*zb))*pow<5>(zb)))))))))+t12*(-1024+zb*(10240+zb*(-40192+zb*(82688+zb*(-97088+zb*(64768+zb*(-22416+3024*zb))))))+t34*(zb*(2048+zb*(-13824+zb*(36864+zb*(-48640+zb*(31360+(-7520-288*zb)*zb)))))+t34*(2048+zb*(-20480+zb*(82176+zb*(-174336+zb*(210944+zb*(-143232+(48496-5616*zb)*zb)))))+t34*(zb*(-4096+zb*(27648+zb*(-73216+zb*(94720+zb*(-58368+zb*(11776+1536*zb))))))+t34*(-1024+zb*(10240+zb*(-43776+zb*(100608+zb*(-130624+zb*(91776+zb*(-28880+1680*zb))))))+t34*(zb*(2048+zb*(-13824+zb*(35840+zb*(-43520+zb*(22720+(-1184-2080*zb)*zb)))))+t34*((1792+zb*(-8960+zb*(16768+zb*(-12928+zb*(1936+1392*zb)))))*pow<2>(zb)+t34*((512+zb*(-2560+zb*(4224+zb*(-2880+704*zb))))*pow<3>(zb)+t34*((-384+(864-480*zb)*zb)*pow<5>(zb)+t34*(64+zb*(-192+128*zb))*pow<5>(zb)))))))))+t12*(zb*(2048+zb*(-14336+zb*(40192+zb*(-56960+zb*(41600+zb*(-13696+1152*zb))))))+t12*((-1536+zb*(7680+zb*(-13440+zb*(8192+(1120-2016*zb)*zb))))*pow<2>(zb)+t34*((256+(-2752+(4800-2304*zb)*zb)*pow<2>(zb))*pow<3>(zb)+t34*((3072+zb*(-15360+zb*(26432+zb*(-14336+zb*(-5328+5520*zb)))))*pow<2>(zb)+t34*((-512+(5504+zb*(-9568+4576*zb))*pow<2>(zb))*pow<3>(zb)+t34*((-1536+zb*(7680+zb*(-12544+zb*(4096+(7312-5008*zb)*zb))))*pow<2>(zb)+t34*((256+(-2752+(4736-2240*zb)*zb)*pow<2>(zb))*pow<3>(zb)+t34*((-448+zb*(2048+zb*(-3120+1520*zb)))*pow<4>(zb)+t34*((32-32*zb)*pow<6>(zb)+t34*(16-16*zb)*pow<6>(zb))))))))+t12*((512+zb*(-896+zb*(-1920+(4896-2592*zb)*zb)))*pow<3>(zb)+t34*((64+zb*(-896+(1936-1104*zb)*zb))*pow<4>(zb)+t34*((-1024+zb*(1792+zb*(4032+zb*(-10304+5504*zb))))*pow<3>(zb)+t34*((-128+zb*(1792+zb*(-3856+2192*zb)))*pow<4>(zb)+t34*((512+zb*(-896+zb*(-2304+(5920-3232*zb)*zb)))*pow<3>(zb)+t34*((64+zb*(-896+(1904-1072*zb)*zb))*pow<4>(zb)+t34*((192+zb*(-512+320*zb))*pow<5>(zb)+t34*(16-16*zb)*pow<6>(zb)))))))+t12*((192+zb*(-1536+(2928-1584*zb)*zb))*pow<4>(zb)+t12*((-256+(768-512*zb)*zb)*pow<5>(zb)+t12*((64-64*zb)*pow<6>(zb)+pow<2>(t34)*((-128+128*zb)*pow<6>(zb)+(64-64*zb)*pow<2>(t34)*pow<6>(zb)))+t34*((64-64*zb)*pow<6>(zb)+t34*((512+zb*(-1536+1024*zb))*pow<5>(zb)+t34*((-128+128*zb)*pow<6>(zb)+t34*((-256+(768-512*zb)*zb)*pow<5>(zb)+t34*(64-64*zb)*pow<6>(zb))))))+t34*((-192+(608-416*zb)*zb)*pow<5>(zb)+t34*((-384+zb*(3072+zb*(-5872+3184*zb)))*pow<4>(zb)+t34*((384+zb*(-1216+832*zb))*pow<5>(zb)+t34*((192+zb*(-1536+(2960-1616*zb)*zb))*pow<4>(zb)+t34*((-192+(608-416*zb)*zb)*pow<5>(zb)+t34*(-16+16*zb)*pow<6>(zb)))))))))+t34*((-1024+zb*(4352+zb*(-5248+zb*(-1472+(6560-3168*zb)*zb))))*pow<2>(zb)+t34*(zb*(-4096+zb*(28672+zb*(-80640+zb*(113920+zb*(-80640+(22880-96*zb)*zb)))))+t34*((2048+zb*(-8704+zb*(10176+zb*(3776+zb*(-13872+6576*zb)))))*pow<2>(zb)+t34*(zb*(2048+zb*(-14336+zb*(40704+zb*(-56960+zb*(36416+(-4480-3392*zb)*zb)))))+t34*((-1024+zb*(4352+zb*(-4608+zb*(-3136+(8048-3632*zb)*zb))))*pow<2>(zb)+t34*((-256+(2688+zb*(-4896+2464*zb))*pow<2>(zb))*pow<3>(zb)+t34*((-320+zb*(832+zb*(-720+208*zb)))*pow<4>(zb)+t34*((-64+(192-128*zb)*zb)*pow<5>(zb)+t34*(-16+16*zb)*pow<6>(zb))))))))))))))+t12*(zb*(2816+zb*(-21952+zb*(72288+zb*(-130160+zb*(138136+zb*(-86184+(29160-4104*zb)*zb))))))+t34*(-5120+zb*(34304+zb*(-104832+zb*(190112+zb*(-212352+zb*(132472+zb*(-30216+zb*(-8316+3942*zb)))))))+t34*(-6400+zb*(68992+zb*(-300288+zb*(713792+zb*(-1030912+zb*(936568+zb*(-528664+(172018-25098*zb)*zb))))))+t34*(12800+zb*(-97152+zb*(348544+zb*(-771328+zb*(1115904+zb*(-1038008+zb*(589392+zb*(-186098+25984*zb)))))))+t34*(-10752+zb*(77568+zb*(-250816+zb*(470752+zb*(-560544+zb*(439136+zb*(-225488+(70742-10666*zb)*zb))))))+t34*(-4608+zb*(16896+zb*(5632+zb*(-109472+zb*(205440+zb*(-182704+zb*(98008+zb*(-35998+6772*zb)))))))+t34*(3840+zb*(-21376+zb*(40448+zb*(-10112+zb*(-69312+zb*(106312+zb*(-70376+(25038-4350*zb)*zb))))))+t34*(zb*(1920+zb*(-12800+zb*(29120+zb*(-24352+zb*(-1544+zb*(12496+zb*(-5902+1040*zb)))))))+t34*((1152+zb*(-5888+zb*(11024+zb*(-9144+zb*(3320+(-510-6*zb)*zb)))))*pow<2>(zb)+t34*(-192+zb*(608+zb*(-520+zb*(-112+(282-42*zb)*zb))))*pow<3>(zb)))))))))+t12*(-2560+zb*(19712+zb*(-76224+zb*(185952+zb*(-291168+zb*(282768+zb*(-160264+(46974-5193*zb)*zb))))))+t34*(-8960+zb*(85376+zb*(-342976+zb*(768064+zb*(-1042608+zb*(865168+zb*(-422632+(111814-13263*zb)*zb))))))+t34*(5120+zb*(-21632+zb*(20160+zb*(32896+zb*(-79392+zb*(57280+zb*(-14368+zb*(-1722+1728*zb)))))))+t34*(-14848+zb*(112384+zb*(-363200+zb*(628224+zb*(-616992+zb*(352128+zb*(-123408+(27434-1708*zb)*zb))))))+t34*(-512+zb*(-24064+zb*(180160+zb*(-517728+zb*(770496+zb*(-642248+zb*(298168+zb*(-68414+3942*zb)))))))+t34*(1280+zb*(-128+zb*(-26688+zb*(106752+zb*(-207712+zb*(218008+zb*(-112120+zb*(20198+478*zb)))))))+t34*(zb*(2432+zb*(-20672+zb*(64000+zb*(-89344+zb*(51984+zb*(-5296+zb*(-3214+312*zb)))))))+t34*((-320+zb*(-1600+zb*(10368+zb*(-14832+zb*(4864+(2270-860*zb)*zb)))))*pow<2>(zb)+t34*((-384+zb*(192+zb*(1896+zb*(-2016+zb*(8+235*zb)))))*pow<3>(zb)+t34*(-80+zb*(328+zb*(-528+(332-7*zb)*zb)))*pow<4>(zb)))))))))+t12*(-3840+zb*(32640+zb*(-123776+zb*(277824+zb*(-394704+zb*(346512+zb*(-172960+(41086-2796*zb)*zb))))))+t34*(-2560+zb*(42880+zb*(-227584+zb*(599424+zb*(-897536+zb*(788832+zb*(-396840+(106298-12916*zb)*zb))))))+t34*(-13312+zb*(108288+zb*(-382208+zb*(734464+zb*(-812672+zb*(512128+zb*(-166424+zb*(14258+5612*zb)))))))+t34*(512+zb*(-13568+zb*(81664+zb*(-236352+zb*(381952+zb*(-338464+zb*(142544+(-15746-2648*zb)*zb))))))+t34*(-1280+zb*(12672+zb*(-28288+zb*(8768+zb*(26848+zb*(-17656+zb*(-3952+(5646-3004*zb)*zb))))))+t34*(zb*(1408+zb*(-11776+zb*(41856+zb*(-77984+zb*(72488+zb*(-25816+zb*(-1438+1520*zb)))))))+t34*((-4352+zb*(18048+zb*(-20640+zb*(-4560+zb*(19048+zb*(-9002+1604*zb))))))*pow<2>(zb)+t34*((-704+zb*(2208+zb*(-112+zb*(-4448+(3674-808*zb)*zb))))*pow<3>(zb)+t34*((-496+zb*(1720+zb*(-1344+zb*(-20+120*zb))))*pow<4>(zb)+t34*(8+zb*(-112+zb*(140+4*zb)))*pow<5>(zb)))))))))+t12*(-1280+zb*(16000+zb*(-72000+zb*(171264+zb*(-240688+zb*(195560+zb*(-78088+zb*(6712+2493*zb)))))))+t34*(-5888+zb*(51968+zb*(-206592+zb*(461408+zb*(-609840+zb*(465168+zb*(-181216+zb*(22760+2271*zb)))))))+t34*(-1792+zb*(22528+zb*(-106752+zb*(249856+zb*(-313552+zb*(217160+zb*(-83744+zb*(15322+1086*zb)))))))+t34*(-1280+zb*(11520+zb*(-26752+zb*(1280+zb*(80304+zb*(-121032+zb*(66480+(-8186-2522*zb)*zb))))))+t34*(zb*(384+zb*(576+zb*(-5312+zb*(-9040+zb*(47616+zb*(-51496+(20802-3648*zb)*zb))))))+t34*((-3200+zb*(15776+zb*(-30608+zb*(26112+zb*(-7624+zb*(-626+444*zb))))))*pow<2>(zb)+t34*((-832+zb*(8144+zb*(-20184+zb*(17776+zb*(-5458+562*zb)))))*pow<3>(zb)+t34*((-112+zb*(440+zb*(288+zb*(-942+186*zb))))*pow<4>(zb)+t34*((168+zb*(-288+(254-109*zb)*zb))*pow<5>(zb)+t34*(-40+zb*(50+5*zb))*pow<6>(zb)))))))))+t12*(-512+zb*(5888+zb*(-26432+zb*(60256+zb*(-76096+zb*(46736+zb*(280+zb*(-15152+5002*zb)))))))+t34*(-1024+zb*(12544+zb*(-57728+zb*(133024+zb*(-158336+zb*(75520+zb*(23944+zb*(-38692+10798*zb)))))))+t34*(-512+zb*(7424+zb*(-34560+zb*(74688+zb*(-71488+zb*(11016+zb*(26664+zb*(-17010+3840*zb)))))))+t34*(zb*(768+zb*(-4736+zb*(11200+zb*(-18016+zb*(27416+zb*(-31904+(19538-4400*zb)*zb))))))+t34*((576+zb*(-4640+zb*(4128+zb*(13152+zb*(-22992+(11910-2156*zb)*zb)))))*pow<2>(zb)+t34*((416+zb*(-160+zb*(-3600+zb*(6152+zb*(-3038+348*zb)))))*pow<3>(zb)+t34*((1760+zb*(-6392+zb*(7080+zb*(-2582+112*zb))))*pow<4>(zb)+t34*((-8+zb*(528+zb*(-586+32*zb)))*pow<5>(zb)+t34*((-24+(50-14*zb)*zb)*pow<6>(zb)+t34*(-6+6*zb)*pow<7>(zb)))))))))+t12*(zb*(512+zb*(-3328+zb*(6752+zb*(-2592+zb*(-10392+zb*(19464+zb*(-13770+3329*zb)))))))+t12*((-768+zb*(4224+zb*(-8904+zb*(9792+zb*(-5450+1088*zb)))))*pow<3>(zb)+t34*((-1664+zb*(9504+zb*(-22120+zb*(25720+zb*(-14518+3096*zb)))))*pow<3>(zb)+t34*((-1024+zb*(6176+zb*(-15344+zb*(17816+zb*(-10106+2532*zb)))))*pow<3>(zb)+t34*((-128+zb*(736+zb*(-432+zb*(-1328+(1130-24*zb)*zb))))*pow<3>(zb)+t34*((-160+zb*(1208+zb*(-2096+(1618-620*zb)*zb)))*pow<4>(zb)+t34*((-488+zb*(1208+zb*(-690+8*zb)))*pow<5>(zb)+t34*((88+zb*(-142+76*zb))*pow<6>(zb)+t34*((-2-8*zb)*pow<7>(zb)-4*t34*pow<8>(zb))))))))+t12*((-128+zb*(784+zb*(-1760+zb*(1960+zb*(-1036+171*zb)))))*pow<3>(zb)+t34*((-256+zb*(1648+zb*(-4016+zb*(4744+zb*(-2676+569*zb)))))*pow<3>(zb)+t34*((-128+zb*(944+zb*(-2672+zb*(3424+zb*(-2168+619*zb)))))*pow<3>(zb)+t34*((80+zb*(-336+zb*(480+zb*(-424+169*zb))))*pow<4>(zb)+t34*((80+zb*(-136+(132-87*zb)*zb))*pow<5>(zb)+t34*((24+(28-29*zb)*zb)*pow<6>(zb)+t34*(pow<8>(zb)-5*t34*pow<8>(zb)))))))+t12*((32+zb*(-112+zb*(144+zb*(-76+10*zb))))*pow<4>(zb)+t34*((64+zb*(-240+zb*(336+zb*(-196+40*zb))))*pow<4>(zb)+t34*((32+zb*(-144+zb*(240+zb*(-184+58*zb))))*pow<4>(zb)+t34*((-16+zb*(48+zb*(-72+32*zb)))*pow<5>(zb)+t34*((4-2*zb)*pow<7>(zb)+t34*((12-8*zb)*pow<7>(zb)-2*t34*pow<8>(zb)))))))))+t34*(zb*(1024+zb*(-6144+zb*(11840+zb*(2272+zb*(-40984+zb*(61176+zb*(-37426+8271*zb)))))))+t34*(zb*(512+zb*(-3840+zb*(8448+zb*(1952+zb*(-31024+zb*(41312+zb*(-22110+4808*zb)))))))+t34*((-832+zb*(-352+zb*(9392+zb*(-17200+(10766-1836*zb)*zb))))*pow<3>(zb)+t34*((-96+zb*(-1408+zb*(7592+zb*(-11160+(6606-1582*zb)*zb))))*pow<3>(zb)+t34*((1152+zb*(-4760+zb*(6584+zb*(-3142+202*zb))))*pow<4>(zb)+t34*((-480+zb*(1072+zb*(-666+96*zb)))*pow<5>(zb)+t34*((128+zb*(-150+20*zb))*pow<6>(zb)+t34*((-12+5*zb)*pow<7>(zb)-t34*pow<8>(zb))))))))))))))))*pow<-1>(-1+t12)*pow<-1>(2+(-1+t12)*zb)*pow<-1>(-2+(2+t12-t34)*zb)*pow<-2>(1+t12)*pow<-2>(1+t34)*pow<-2>(t12+t34)*pow<-2>(-1+zb)*pow<-2>(zb)*pow<-2>(-2+(3+t12)*zb)*pow<-3>(-1+t34))/8;
    return todouble(foo);
}

// Expansion order epsilon^0 of c13*bubble(13)+c25*bubble(25) 
template<>
template<>
double qq2yyg1<TT>::LC::bub::c1325<0>(const TT& zb, const TT& t12, const TT& t34)
{
    const TT foo = ((t34*(zb*(-4608+zb*(37376+zb*(-129344+zb*(248512+zb*(-288544+zb*(205968+zb*(-87468+(19944-1836*zb)*zb)))))))+t34*(-5120+zb*(49792+zb*(-208768+zb*(492512+zb*(-717008+zb*(666440+zb*(-397176+zb*(149326+zb*(-33795+3798*zb))))))))+t34*(1280+zb*(-16896+zb*(87232+zb*(-242528+zb*(409488+zb*(-442696+zb*(312464+zb*(-142968+(40075-5454*zb)*zb)))))))+t34*(6400+zb*(-55296+zb*(181824+zb*(-252288+zb*(1776+zb*(470040+zb*(-658968+zb*(423410+zb*(-133223+16326*zb))))))))+t34*(256+zb*(7680+zb*(-48896+zb*(84800+zb*(48000+zb*(-362336+zb*(532240+zb*(-372604+(127567-16702*zb)*zb)))))))+t34*(-4352+zb*(44672+zb*(-180608+zb*(380640+zb*(-461600+zb*(341144+zb*(-172680+zb*(75954+zb*(-28249+5074*zb))))))))+t34*(-512+zb*(-3584+zb*(38976+zb*(-119264+zb*(169344+zb*(-111016+zb*(14992+zb*(17360+zb*(-6319+22*zb))))))))+t34*(zb*(-3328+zb*(20160+zb*(-48448+zb*(58560+zb*(-37560+zb*(13208+zb*(-3442+(899-46*zb)*zb)))))))+t34*(-1216+zb*(6256+zb*(-12624+zb*(12480+zb*(-6064+(1197-30*zb)*zb)))))*pow<3>(zb)))))))))+pow<2>(flog<TT>(((1+t12)*zb)/2))*(pow<2>(t34)*(-2048+zb*(17408+zb*(-64512+zb*(135936+zb*(-177664+zb*(146944+zb*(-74784+(21312-2592*zb)*zb))))))+t34*(1024+zb*(-10240+zb*(44288+zb*(-108032+zb*(162240+zb*(-153216+zb*(88560+zb*(-28512+3888*zb)))))))+t34*(4096+zb*(-34816+zb*(127488+zb*(-261888+zb*(328832+zb*(-257216+zb*(121728+zb*(-31680+3456*zb)))))))+t34*(-2048+zb*(20480+zb*(-88320+zb*(214272+zb*(-319360+zb*(298816+zb*(-170928+(54432-7344*zb)*zb))))))+t34*(-2048+zb*(17408+zb*(-61440+zb*(115968+zb*(-124672+zb*(73600+zb*(-19104+zb*(-576+864*zb)))))))+t34*(1024+zb*(-10240+zb*(43776+zb*(-104448+zb*(152000+zb*(-137984+zb*(76176+zb*(-23328+3024*zb)))))))+t34*((-1536+zb*(9984+zb*(-26496+zb*(36672+zb*(-27840+(10944-1728*zb)*zb)))))*pow<2>(zb)+t34*(256+zb*(-1792+zb*(5120+zb*(-7616+zb*(6192+zb*(-2592+432*zb))))))*pow<2>(zb))))))))+t12*(t34*(-4096+zb*(34816+zb*(-129024+zb*(271872+zb*(-355328+zb*(293888+zb*(-149568+(42624-5184*zb)*zb))))))+t34*(-1024+zb*(8192+zb*(-26368+zb*(42752+zb*(-32960+zb*(2368+zb*(15152+zb*(-10272+2160*zb)))))))+t34*(9216+zb*(-81920+zb*(316160+zb*(-690176+zb*(928704+zb*(-784768+zb*(403728+zb*(-114336+13392*zb)))))))+t34*(2048+zb*(-16384+zb*(50944+zb*(-72960+zb*(31232+zb*(43584+zb*(-65680+(33120-5904*zb)*zb))))))+t34*(-6144+zb*(59392+zb*(-244992+zb*(562688+zb*(-785024+zb*(677888+zb*(-350640+(97632-10800*zb)*zb))))))+t34*(-1024+zb*(8192+zb*(-22784+zb*(17664+zb*(36416+zb*(-94272+zb*(85904+zb*(-35424+5328*zb)))))))+t34*(1024+zb*(-12288+zb*(57600+zb*(-142336+zb*(205248+zb*(-177024+zb*(88368+zb*(-22752+2160*zb)))))))+t34*((-1792+zb*(12544+zb*(-34688+zb*(48320+zb*(-35376+(12576-1584*zb)*zb)))))*pow<2>(zb)+t34*(256+zb*(-2048+zb*(6400+zb*(-9984+zb*(8112+zb*(-3168+432*zb))))))*pow<2>(zb)))))))))+t12*(-2048+zb*(17408+zb*(-64512+zb*(135936+zb*(-177664+zb*(146944+zb*(-74784+(21312-2592*zb)*zb))))))+t34*(-5120+zb*(47104+zb*(-185600+zb*(409600+zb*(-552640+zb*(464384+zb*(-235376+(64992-7344*zb)*zb))))))+t34*(5120+zb*(-46080+zb*(183040+zb*(-417280+zb*(593984+zb*(-537472+zb*(299792+zb*(-93344+12240*zb)))))))+t34*(10240+zb*(-96256+zb*(385792+zb*(-860160+zb*(1161984+zb*(-965888+zb*(475312+zb*(-122976+11952*zb)))))))+t34*(-4096+zb*(39936+zb*(-172800+zb*(429568+zb*(-664064+zb*(646016+zb*(-380656+(121696-15600*zb)*zb))))))+t34*(-5120+zb*(51200+zb*(-214784+zb*(491264+zb*(-664768+zb*(536448+zb*(-243248+(51168-2160*zb)*zb))))))+t34*(1024+zb*(-11264+zb*(54528+zb*(-151040+zb*(256832+zb*(-267392+zb*(161072+zb*(-48608+4848*zb)))))))+t34*(zb*(-2048+zb*(14592+zb*(-40448+zb*(54144+zb*(-32768+zb*(2064+(6624-2160*zb)*zb))))))+t34*((-256+zb*(2816+zb*(-9088+zb*(11904+zb*(-5424+zb*(-1056+1104*zb))))))*pow<2>(zb)+t34*(-256+zb*(1280+zb*(-2176+zb*(1248+(192-288*zb)*zb))))*pow<3>(zb)))))))))+t12*(-3072+zb*(28672+zb*(-114944+zb*(258816+zb*(-357440+zb*(308800+zb*(-161968+(46752-5616*zb)*zb))))))+t34*(-1024+zb*(14336+zb*(-72448+zb*(188416+zb*(-282816+zb*(250496+zb*(-124688+(29600-1872*zb)*zb))))))+t34*(6144+zb*(-59392+zb*(247552+zb*(-580608+zb*(834560+zb*(-747072+zb*(401584+zb*(-115872+13104*zb)))))))+t34*(2048+zb*(-28672+zb*(145664+zb*(-378880+zb*(563456+zb*(-487424+zb*(230288+zb*(-47392+912*zb)))))))+t34*(-3072+zb*(32768+zb*(-150272+zb*(385024+zb*(-597696+zb*(567616+zb*(-313904+(87456-7920*zb)*zb))))))+t34*(-1024+zb*(14336+zb*(-73984+zb*(192512+zb*(-278464+zb*(223616+zb*(-87440+zb*(7072+3376*zb)))))))+t34*(zb*(-2048+zb*(17664+zb*(-63488+zb*(121472+zb*(-129216+zb*(70928+(-14304-1008*zb)*zb))))))+t34*((768+zb*(-2048+zb*(-2176+zb*(13056+zb*(-17232+(9632-2000*zb)*zb)))))*pow<2>(zb)+t34*((256+zb*(-896+zb*(-128+zb*(3360+zb*(-4032+1440*zb)))))*pow<3>(zb)+t34*(256+zb*(-928+(1088-416*zb)*zb))*pow<5>(zb)))))))))+t12*(-1024+zb*(13312+zb*(-66816+zb*(177408+zb*(-276928+zb*(260416+zb*(-142480+(40288-4176*zb)*zb))))))+t34*(zb*(2048+zb*(-16896+zb*(56064+zb*(-95104+zb*(83776+zb*(-30848+zb*(-2496+3456*zb)))))))+t34*(2048+zb*(-26624+zb*(135424+zb*(-365824+zb*(579840+zb*(-548736+zb*(295248+zb*(-77088+5712*zb)))))))+t34*(zb*(-4096+zb*(33792+zb*(-111616+zb*(186816+zb*(-159488+zb*(52496+(10208-8112*zb)*zb))))))+t34*(-1024+zb*(13312+zb*(-70400+zb*(199424+zb*(-328896+zb*(315776+zb*(-161552+zb*(31648+1712*zb)))))))+t34*(zb*(2048+zb*(-16896+zb*(55040+zb*(-88320+zb*(67712+zb*(-12720+zb*(-12576+5712*zb)))))))+t34*((1792+zb*(-11008+zb*(25984+zb*(-27008+zb*(7280+(6816-3856*zb)*zb)))))*pow<2>(zb)+t34*((512+zb*(-3392+zb*(7936+zb*(-8656+(4512-912*zb)*zb))))*pow<3>(zb)+t34*(t34*(64+zb*(-272+(352-144*zb)*zb))*pow<5>(zb)+(-448+zb*(1504+zb*(-1664+608*zb)))*pow<5>(zb)))))))))+t12*(zb*(2048+zb*(-17920+zb*(63744+zb*(-118272+zb*(120192+zb*(-62368+zb*(11712+864*zb)))))))+t34*((-1024+zb*(5632+zb*(-9856+zb*(1024+zb*(15584+zb*(-16832+5472*zb))))))*pow<2>(zb)+t34*(zb*(-4096+zb*(35840+zb*(-127744+zb*(236352+zb*(-235328+zb*(112528+(-12128-5424*zb)*zb))))))+t34*((2048+zb*(-11264+zb*(19392+zb*(-896+zb*(-32720+(34592-11152*zb)*zb)))))*pow<2>(zb)+t34*(zb*(2048+zb*(-17920+zb*(64256+zb*(-117888+zb*(110016+zb*(-37680+zb*(-11232+8400*zb)))))))+t34*((-1024+zb*(5632+zb*(-9216+zb*(-1280+zb*(18672+zb*(-18656+5872*zb))))))*pow<2>(zb)+t34*((-256+zb*(-192+zb*(5184+zb*(-12752+(12000-3984*zb)*zb))))*pow<3>(zb)+t34*((-320+zb*(1152+zb*(-1520+(864-176*zb)*zb)))*pow<4>(zb)+t34*((-64+zb*(272+zb*(-352+144*zb)))*pow<5>(zb)+t34*(-16+(32-16*zb)*zb)*pow<6>(zb)))))))))+t12*((-1536+zb*(9728+zb*(-22528+zb*(20608+zb*(-256+zb*(-10624+4608*zb))))))*pow<2>(zb)+t34*((256+zb*(-192+zb*(-3712+zb*(10384+zb*(-10144+3408*zb)))))*pow<3>(zb)+t34*((3072+zb*(-19456+zb*(44608+zb*(-38528+zb*(-5328+(26656-11024*zb)*zb)))))*pow<2>(zb)+t34*((-512+zb*(384+zb*(7424+zb*(-20720+(20192-6768*zb)*zb))))*pow<3>(zb)+t34*((-1536+zb*(9728+zb*(-21632+zb*(15232+zb*(11440+zb*(-21472+8240*zb))))))*pow<2>(zb)+t34*((256+zb*(-192+zb*(-3712+zb*(10288+zb*(-9952+3312*zb)))))*pow<3>(zb)+t34*((-448+zb*(2688+zb*(-5872+(5472-1840*zb)*zb)))*pow<4>(zb)+t34*(t34*(16+zb*(-32+16*zb))*pow<6>(zb)+(48+zb*(-96+48*zb))*pow<6>(zb))))))))+t12*((512+zb*(-1216+zb*(-2752+zb*(11280+zb*(-12000+4176*zb)))))*pow<3>(zb)+t34*((64+zb*(-1152+zb*(3632+zb*(-4064+1520*zb))))*pow<4>(zb)+t34*((-1024+zb*(2432+zb*(5696+zb*(-23280+(24864-8688*zb)*zb))))*pow<3>(zb)+t34*((-128+zb*(2304+zb*(-7248+(8096-3024*zb)*zb)))*pow<4>(zb)+t34*((512+zb*(-1216+zb*(-3136+zb*(12720+zb*(-13728+4848*zb)))))*pow<3>(zb)+t34*((64+zb*(-1152+zb*(3600+zb*(-4000+1488*zb))))*pow<4>(zb)+t34*((192+zb*(-720+(864-336*zb)*zb))*pow<5>(zb)+t34*(16+zb*(-32+16*zb))*pow<6>(zb)))))))+t12*((192+zb*(-1984+zb*(5488+zb*(-5792+2096*zb))))*pow<4>(zb)+t34*((-192+zb*(864+zb*(-1152+480*zb)))*pow<5>(zb)+t34*((-384+zb*(3968+zb*(-10992+(11616-4208*zb)*zb)))*pow<4>(zb)+t34*((384+zb*(-1728+(2304-960*zb)*zb))*pow<5>(zb)+t34*((192+zb*(-1984+zb*(5520+zb*(-5856+2128*zb))))*pow<4>(zb)+t34*((-192+zb*(864+zb*(-1152+480*zb)))*pow<5>(zb)+t34*(-16+(32-16*zb)*zb)*pow<6>(zb))))))+t12*((-256+zb*(1088+zb*(-1408+576*zb)))*pow<5>(zb)+t12*((64+zb*(-128+64*zb))*pow<6>(zb)+pow<2>(t34)*((-128+(256-128*zb)*zb)*pow<6>(zb)+(64+zb*(-128+64*zb))*pow<2>(t34)*pow<6>(zb)))+t34*((64+zb*(-128+64*zb))*pow<6>(zb)+t34*((512+zb*(-2176+(2816-1152*zb)*zb))*pow<5>(zb)+t34*((-128+(256-128*zb)*zb)*pow<6>(zb)+t34*((-256+zb*(1088+zb*(-1408+576*zb)))*pow<5>(zb)+t34*(64+zb*(-128+64*zb))*pow<6>(zb)))))))))))))))+flog<TT>(((1+t12)*zb)/2)*(t34*(zb*(5632+zb*(-47232+zb*(171200+zb*(-350176+zb*(441840+zb*(-351872+zb*(172512+zb*(-47520+5616*zb))))))))+t34*(-5120+zb*(34304+zb*(-92544+zb*(115072+zb*(-17344+zb*(-155152+zb*(239568+zb*(-173204+(64050-9630*zb)*zb)))))))+t34*(-2560+zb*(35072+zb*(-194688+zb*(599552+zb*(-1153664+zb*(1456336+zb*(-1213552+zb*(644212+zb*(-196802+26094*zb))))))))+t34*(12800+zb*(-110080+zb*(455168+zb*(-1187328+zb*(2101408+zb*(-2533888+zb*(2028032+zb*(-1021900+(291434-35646*zb)*zb)))))))+t34*(-7680+zb*(62464+zb*(-241792+zb*(602432+zb*(-1057376+zb*(1302688+zb*(-1076640+zb*(556940+zb*(-160506+19470*zb))))))))+t34*(-5632+zb*(39424+zb*(-120960+zb*(214784+zb*(-238880+zb*(157936+zb*(-41264+zb*(-17964+(15734-3178*zb)*zb)))))))+t34*(4096+zb*(-31488+zb*(107904+zb*(-221696+zb*(306784+zb*(-295280+zb*(190544+zb*(-74484+(14426-806*zb)*zb)))))))+t34*(zb*(1536+zb*(-12288+zb*(42368+zb*(-81984+zb*(96672+zb*(-70048+zb*(29580+zb*(-6290+454*zb))))))))+t34*(1024+zb*(-6656+zb*(17888+zb*(-25456+zb*(20288+zb*(-8684+(1666-70*zb)*zb))))))*pow<2>(zb)))))))))+t12*(zb*(5632+zb*(-49536+zb*(188480+zb*(-404896+zb*(536592+zb*(-448640+zb*(230688+zb*(-66528+8208*zb))))))))+t34*(-10240+zb*(84480+zb*(-325504+zb*(761088+zb*(-1155104+zb*(1131488+zb*(-677248+zb*(216312+(-23004-2268*zb)*zb)))))))+t34*(-17920+zb*(185088+zb*(-831104+zb*(2143232+zb*(-3506752+zb*(3779808+zb*(-2690896+zb*(1228160+zb*(-330182+40566*zb))))))))+t34*(23040+zb*(-184832+zb*(696704+zb*(-1640192+zb*(2620800+zb*(-2851488+zb*(2041248+zb*(-906768+(227362-25874*zb)*zb)))))))+t34*(-8704+zb*(66560+zb*(-201600+zb*(255808+zb*(38816+zb*(-534528+zb*(698784+zb*(-429440+(128618-14314*zb)*zb)))))))+t34*(-16896+zb*(105472+zb*(-264320+zb*(372224+zb*(-427552+zb*(526400+zb*(-515216+zb*(288928+zb*(-74966+5926*zb))))))))+t34*(2048+zb*(-11008+zb*(2688+zb*(113664+zb*(-357280+zb*(509184+zb*(-394640+zb*(172864+zb*(-43042+5522*zb))))))))+t34*(4096+zb*(-27648+zb*(78464+zb*(-137856+zb*(199840+zb*(-249664+zb*(218624+zb*(-111280+(28310-2886*zb)*zb)))))))+t34*(zb*(1536+zb*(-9984+zb*(28288+zb*(-48160+zb*(56336+zb*(-45120+zb*(21920+zb*(-5282+466*zb))))))))+t34*(1024+zb*(-7040+zb*(19488+zb*(-27712+zb*(21104+zb*(-7896+zb*(1018+14*zb)))))))*pow<2>(zb)))))))))+t12*(-5120+zb*(50176+zb*(-241408+zb*(712832+zb*(-1359136+zb*(1684464+zb*(-1334704+zb*(645164+zb*(-170862+18594*zb))))))))+t34*(-28160+zb*(267520+zb*(-1134976+zb*(2811968+zb*(-4426272+zb*(4505200+zb*(-2900976+zb*(1112692+zb*(-225638+18642*zb))))))))+t34*(-2560+zb*(97280+zb*(-654976+zb*(2053632+zb*(-3713984+zb*(4208304+zb*(-3073760+zb*(1426656+zb*(-387332+46740*zb))))))))+t34*(-4096+zb*(34560+zb*(-59776+zb*(-256896+zb*(1284032+zb*(-2369584+zb*(2303728+zb*(-1249296+(365880-48552*zb)*zb)))))))+t34*(-22528+zb*(129536+zb*(-248320+zb*(47360+zb*(513856+zb*(-826128+zb*(551584+zb*(-140704+zb*(-18104+13448*zb))))))))+t34*(-6656+zb*(40192+zb*(-75648+zb*(36672+zb*(896+zb*(75152+zb*(-98832+zb*(-3376+(46100-14500*zb)*zb)))))))+t34*(7680+zb*(-45568+zb*(77440+zb*(68224+zb*(-425088+zb*(633904+zb*(-467936+zb*(194992+zb*(-51724+8076*zb))))))))+t34*(zb*(3840+zb*(-30080+zb*(81280+zb*(-83008+zb*(-4784+zb*(67472+zb*(-41984+(7624-360*zb)*zb)))))))+t34*((2304+zb*(-14848+zb*(34976+zb*(-36928+zb*(17104+zb*(-3612+(1462-458*zb)*zb))))))*pow<2>(zb)+t34*(-384+zb*(1440+zb*(-1440+zb*(-896+zb*(2508+zb*(-1326+98*zb))))))*pow<3>(zb)))))))))+t12*(-12800+zb*(117504+zb*(-504704+zb*(1327552+zb*(-2299296+zb*(2630304+zb*(-1925008+zb*(842568+zb*(-192098+15978*zb))))))))+t34*(-23040+zb*(279552+zb*(-1397632+zb*(3876096+zb*(-6615264+zb*(7188288+zb*(-4946944+zb*(2075168+zb*(-488582+52358*zb))))))))+t34*(-16384+zb*(189696+zb*(-897408+zb*(2258816+zb*(-3318848+zb*(2922944+zb*(-1500400+zb*(386656+(-10392-14680*zb)*zb)))))))+t34*(-28672+zb*(226304+zb*(-760704+zb*(1346816+zb*(-1253824+zb*(497408+zb*(10944+zb*(-14896+zb*(-32088+8712*zb))))))))+t34*(-3584+zb*(-19200+zb*(326528+zb*(-1321664+zb*(2612608+zb*(-2914496+zb*(1908240+zb*(-713968+(127412-1876*zb)*zb)))))))+t34*(2560+(-79488+zb*(374144+zb*(-868608+zb*(1152384+zb*(-856864+zb*(313392+(-33524-3996*zb)*zb))))))*pow<2>(zb)+t34*(zb*(4864+zb*(-54912+zb*(214144+zb*(-384064+zb*(314816+zb*(-67344+zb*(-51936+(28264-3832*zb)*zb)))))))+t34*((-640+zb*(-3968+zb*(29760+zb*(-55040+zb*(30720+zb*(11056+zb*(-15224+3336*zb)))))))*pow<2>(zb)+t34*((-768+zb*(160+zb*(7840+zb*(-13952+zb*(6696+(734-710*zb)*zb)))))*pow<3>(zb)+t34*(-160+zb*(832+zb*(-1952+zb*(2224+zb*(-950+6*zb)))))*pow<4>(zb)))))))))+t12*(-10240+zb*(107520+zb*(-488832+zb*(1289728+zb*(-2168960+zb*(2354928+zb*(-1586240+zb*(597692+zb*(-96202+606*zb))))))))+t34*(-16896+zb*(206592+zb*(-1058048+zb*(2990016+zb*(-5136416+zb*(5522752+zb*(-3664112+zb*(1414228+zb*(-279406+21290*zb))))))))+t34*(-30208+zb*(291840+zb*(-1239552+zb*(2946560+zb*(-4221088+zb*(3711024+zb*(-1958912+zb*(559496+(-45764-13396*zb)*zb)))))))+t34*(-1536+zb*(-2560+zb*(113920+zb*(-579968+zb*(1394656+zb*(-1843504+zb*(1337040+zb*(-465912+zb*(37524+10340*zb))))))))+t34*(-2560+zb*(28672+zb*(-81536+zb*(62336+zb*(28704+zb*(24304+zb*(-170816+zb*(163792+zb*(-66200+13304*zb))))))))+t34*(zb*(2816+zb*(-32768+zb*(145216+zb*(-332448+zb*(414384+zb*(-264080+zb*(62752+(8056-3928*zb)*zb)))))))+t34*((-8704+zb*(43136+zb*(-59424+zb*(-24496+zb*(123136+zb*(-102568+(33252-4332*zb)*zb))))))*pow<2>(zb)+t34*((-1408+zb*(5600+zb*(-3536+zb*(-8976+zb*(13784+zb*(-6708+1244*zb))))))*pow<3>(zb)+t34*((-992+zb*(4768+zb*(-7040+zb*(3732+(-446-22*zb)*zb))))*pow<4>(zb)+t34*(16+zb*(-320+zb*(684+(-362-18*zb)*zb)))*pow<5>(zb)))))))))+t12*(-3584+zb*(47360+zb*(-240640+zb*(659904+zb*(-1096608+zb*(1118160+zb*(-640208+zb*(138736+(31870-14990*zb)*zb)))))))+t34*(-13824+zb*(142848+zb*(-657664+zb*(1717504+zb*(-2725216+zb*(2617728+zb*(-1395920+zb*(282680+(58002-26138*zb)*zb)))))))+t34*(-4608+zb*(64512+zb*(-342528+zb*(931712+zb*(-1419168+zb*(1226432+zb*(-570512+zb*(110784+(13228-9852*zb)*zb)))))))+t34*(-2560+zb*(27136+zb*(-87552+zb*(87936+zb*(99616+zb*(-311808+zb*(256384+zb*(-46448+zb*(-36548+13844*zb))))))))+t34*(zb*(768+zb*(1536+zb*(-22208+zb*(10080+zb*(131360+zb*(-270512+zb*(214400+zb*(-77032+11608*zb))))))))+t34*((-6400+zb*(38784+zb*(-93920+zb*(106560+zb*(-47968+zb*(-4384+(8912-1584*zb)*zb))))))*pow<2>(zb)+t34*((-1664+zb*(21472+zb*(-72960+zb*(102864+zb*(-65792+(17428-1348*zb)*zb)))))*pow<3>(zb)+t34*((-224+zb*(1088+zb*(768+zb*(-4688+(3492-436*zb)*zb))))*pow<4>(zb)+t34*((336+zb*(-960+zb*(1232+zb*(-854+246*zb))))*pow<5>(zb)+t34*(-80+zb*(168+(-66-22*zb)*zb))*pow<6>(zb)))))))))+t12*(-1024+zb*(13824+zb*(-72320+zb*(193536+zb*(-291392+zb*(230064+zb*(-33200+zb*(-97332+(74506-16662*zb)*zb)))))))+t34*(-2048+zb*(29184+zb*(-154880+zb*(417472+zb*(-601856+zb*(381200+zb*(101168+zb*(-322476+(190374-38138*zb)*zb)))))))+t34*(-1024+zb*(16896+zb*(-92672+zb*(243072+zb*(-305344+zb*(99056+zb*(175968+zb*(-214192+(95536-17296*zb)*zb)))))))+t34*(zb*(1536+zb*(-11008+zb*(30208+zb*(-57472+zb*(110352+zb*(-171824+zb*(158816+zb*(-73080+12472*zb))))))))+t34*((1152+zb*(-10624+zb*(14912+zb*(36048+zb*(-109792+zb*(105336+zb*(-44508+7476*zb)))))))*pow<2>(zb)+t34*((832+zb*(1152+zb*(-18704+zb*(42192+zb*(-37832+(13460-1100*zb)*zb)))))*pow<3>(zb)+t34*((3520+zb*(-17264+zb*(30048+zb*(-22800+(6912-416*zb)*zb))))*pow<4>(zb)+t34*((-16+zb*(1328+zb*(-2784+(1576-104*zb)*zb)))*pow<5>(zb)+t34*((-48+zb*(124+zb*(-94+18*zb)))*pow<6>(zb)+t34*(-12+(22-10*zb)*zb)*pow<7>(zb)))))))))+t12*(zb*(1024+zb*(-7680+zb*(18624+zb*(-8704+zb*(-41856+zb*(97104+zb*(-96952+(47274-8834*zb)*zb)))))))+t34*(zb*(2048+zb*(-14336+zb*(32640+zb*(3200+zb*(-149760+zb*(300000+zb*(-277680+(126622-22734*zb)*zb)))))))+t34*(zb*(1024+zb*(-8704+zb*(22528+zb*(1408+zb*(-108992+zb*(210992+zb*(-182688+(79112-14680*zb)*zb)))))))+t34*((-1920+zb*(2688+zb*(17152+zb*(-54976+zb*(60848+zb*(-27512+3720*zb))))))*pow<3>(zb)+t34*((-192+zb*(-2944+zb*(20736+zb*(-44112+zb*(42960+zb*(-20852+4404*zb))))))*pow<3>(zb)+t34*((2304+zb*(-12800+zb*(26080+zb*(-23248+(8084-420*zb)*zb))))*pow<4>(zb)+t34*((-960+zb*(3280+zb*(-3936+(1960-344*zb)*zb)))*pow<5>(zb)+t34*((256+zb*(-560+(328-24*zb)*zb))*pow<6>(zb)+t34*((-24+(26-2*zb)*zb)*pow<7>(zb)+t34*(-2+2*zb)*pow<8>(zb)))))))))+t12*((-1792+zb*(11808+zb*(-31344+zb*(44832+zb*(-36476+(15490-2518*zb)*zb)))))*pow<3>(zb)+t34*((-3840+zb*(26144+zb*(-74576+zb*(113200+zb*(-95316+(41718-7330*zb)*zb)))))*pow<3>(zb)+t34*((-2304+zb*(16544+zb*(-50272+zb*(78512+zb*(-67028+(30850-6302*zb)*zb)))))*pow<3>(zb)+t34*((-256+zb*(1888+zb*(-3168+zb*(-160+zb*(3108+(-1122-290*zb)*zb)))))*pow<3>(zb)+t34*((-320+zb*(2896+zb*(-7040+zb*(7964+zb*(-4914+1414*zb)))))*pow<4>(zb)+t34*((-976+zb*(3440+zb*(-3788+zb*(1282+42*zb))))*pow<5>(zb)+t34*((176+zb*(-460+(438-154*zb)*zb))*pow<6>(zb)+t34*((-4+zb*(-22+26*zb))*pow<7>(zb)+t34*(-8+8*zb)*pow<8>(zb))))))))+t12*((-256+zb*(1888+zb*(-5376+zb*(7952+zb*(-6432+(2586-362*zb)*zb)))))*pow<3>(zb)+t12*((64+zb*(-288+zb*(512+zb*(-440+(172-20*zb)*zb))))*pow<4>(zb)+t34*((128+zb*(-608+zb*(1152+zb*(-1064+(472-80*zb)*zb))))*pow<4>(zb)+t34*((64+zb*(-352+zb*(768+zb*(-848+(484-116*zb)*zb))))*pow<4>(zb)+t34*((-32+zb*(128+zb*(-240+(208-64*zb)*zb)))*pow<5>(zb)+t34*((8+zb*(-12+4*zb))*pow<7>(zb)+t34*((24+zb*(-40+16*zb))*pow<7>(zb)+t34*(-4+4*zb)*pow<8>(zb)))))))+t34*((-512+zb*(3936+zb*(-11936+zb*(18672+zb*(-15904+(6962-1218*zb)*zb)))))*pow<3>(zb)+t34*((-256+zb*(2208+zb*(-7584+zb*(12960+zb*(-12032+(6058-1354*zb)*zb)))))*pow<3>(zb)+t34*((160+zb*(-864+zb*(1760+zb*(-2048+(1394-402*zb)*zb))))*pow<4>(zb)+t34*((160+zb*(-432+zb*(544+zb*(-450+178*zb))))*pow<5>(zb)+t34*((48+zb*(32+zb*(-154+74*zb)))*pow<6>(zb)+t34*((-2+2*zb)*pow<8>(zb)+t34*(-10+10*zb)*pow<8>(zb)))))))))))))))))+t12*(zb*(-4608+zb*(40448+zb*(-152256+zb*(320576+zb*(-412320+zb*(331184+zb*(-161940+(43992-5076*zb)*zb)))))))+t34*(-10240+zb*(87296+zb*(-320512+zb*(667456+zb*(-882080+zb*(797104+zb*(-525632+zb*(257836+zb*(-84150+12924*zb))))))))+t34*(-6400+zb*(48512+zb*(-158400+zb*(278272+zb*(-247632+zb*(24400+zb*(176848+zb*(-176774+(72241-11067*zb)*zb)))))))+t34*(7424+zb*(-67328+zb*(230720+zb*(-331136+zb*(-2064+zb*(665248+zb*(-945536+zb*(606684+zb*(-183879+19851*zb))))))))+t34*(18688+zb*(-145920+zb*(476416+zb*(-811136+zb*(702656+zb*(-174200+zb*(-186128+zb*(154270+zb*(-35543+913*zb))))))))+t34*(-8960+zb*(126720+zb*(-632704+zb*(1557440+zb*(-2056192+zb*(1359120+zb*(-225536+zb*(-219040+(110381-11209*zb)*zb)))))))+t34*(-12288+zb*(93568+zb*(-279488+zb*(377920+zb*(-132736+zb*(-227216+zb*(267536+zb*(-89786+zb*(395+2063*zb))))))))+t34*(-512+zb*(-22784+zb*(177472+zb*(-525376+zb*(786368+zb*(-620880+zb*(232352+zb*(-23268+(-2013-1359*zb)*zb)))))))+t34*(zb*(-2816+zb*(14208+zb*(-18688+zb*(-14128+zb*(53848+zb*(-45184+zb*(13878+zb*(-1725+623*zb))))))))+t34*(256+zb*(-3648+zb*(14832+zb*(-26896+zb*(24096+zb*(-10116+zb*(1453+17*zb)))))))*pow<2>(zb)))))))))+t12*(-5120+zb*(37504+zb*(-100480+zb*(86624+zb*(124720+zb*(-384472+zb*(406200+zb*(-214370+(54093-4698*zb)*zb)))))))+t34*(-16640+zb*(147712+zb*(-578496+zb*(1315296+zb*(-1924368+zb*(1901656+zb*(-1293968+zb*(595952+zb*(-168999+21864*zb))))))))+t34*(-11008+zb*(74112+zb*(-208576+zb*(339200+zb*(-397696+zb*(393224+zb*(-310016+zb*(161788+zb*(-46958+5909*zb))))))))+t34*(30720+zb*(-247040+zb*(834496+zb*(-1467776+zb*(1276288+zb*(-188472+zb*(-604656+zb*(488700+zb*(-126052+3767*zb))))))))+t34*(4608+zb*(35712+zb*(-440768+zb*(1613088+zb*(-2991968+zb*(3121800+zb*(-1800336+zb*(484784+(-16160-10691*zb)*zb)))))))+t34*(-25344+zb*(232192+zb*(-918720+zb*(1965728+zb*(-2353984+zb*(1445704+zb*(-280272+zb*(-80364+zb*(-4014+19083*zb))))))))+t34*(-7936+zb*(25728+zb*(100672+zb*(-715264+zb*(1691552+zb*(-2068248+zb*(1393344+zb*(-505836+(99254-13345*zb)*zb)))))))+t34*(zb*(-17152+zb*(130240+zb*(-379008+zb*(523776+zb*(-320696+zb*(21616+zb*(55748+(-12892-1611*zb)*zb)))))))+t34*((-1088+zb*(12672+zb*(-38064+zb*(39968+zb*(-3944+zb*(-16094+zb*(6091+489*zb)))))))*pow<2>(zb)+t34*(-640+zb*(3600+zb*(-6384+zb*(3424+zb*(1212+zb*(-1323+97*zb))))))*pow<3>(zb)))))))))+t12*(-8960+zb*(82304+zb*(-324672+zb*(717312+zb*(-978608+zb*(872704+zb*(-542224+zb*(251670+zb*(-83389+13869*zb))))))))+t34*(-18688+zb*(153600+zb*(-546880+zb*(1118336+zb*(-1459472+zb*(1264624+zb*(-726688+zb*(267148+zb*(-57763+5793*zb))))))))+t34*(3072+zb*(-2944+zb*(-62912+zb*(317824+zb*(-816256+zb*(1303872+zb*(-1298160+zb*(762804+zb*(-236140+28776*zb))))))))+t34*(18432+zb*(-124928+zb*(310592+zb*(-248832+zb*(-346880+zb*(979680+zb*(-884928+zb*(293412+(35900-32432*zb)*zb)))))))+t34*(-18688+zb*(194688+zb*(-946880+zb*(2607104+zb*(-4254592+zb*(4092288+zb*(-2182160+zb*(535136+(-23534-3238*zb)*zb)))))))+t34*(-16128+zb*(117760+zb*(-337088+zb*(393792+zb*(121984+zb*(-867776+zb*(932608+zb*(-360692+zb*(-15234+30698*zb))))))))+t34*(zb*(-21120+zb*(215232+zb*(-848320+zb*(1704704+zb*(-1882304+zb*(1129648+zb*(-343028+(54996-9888*zb)*zb)))))))+t34*((8128+zb*(-24256+zb*(-10752+zb*(104992+zb*(-120000+zb*(33932+(14492-6472*zb)*zb))))))*pow<2>(zb)+t34*((2624+zb*(-9296+zb*(5376+zb*(10848+zb*(-13110+zb*(2883+689*zb))))))*pow<3>(zb)+t34*(400+zb*(-816+zb*(-224+zb*(1208+(-435-147*zb)*zb))))*pow<4>(zb)))))))))+t12*(-6656+zb*(67456+zb*(-305792+zb*(808832+zb*(-1363856+zb*(1516208+zb*(-1122824+zb*(548090+zb*(-165397+23953*zb))))))))+t34*(-12544+zb*(128512+zb*(-529344+zb*(1181888+zb*(-1599616+zb*(1369512+zb*(-744208+zb*(254780+zb*(-57995+9001*zb))))))))+t34*(8448+zb*(-50432+zb*(114112+zb*(-107296+zb*(-37504+zb*(238200+zb*(-286864+zb*(129000+(9470-17204*zb)*zb)))))))+t34*(-8960+zb*(82944+zb*(-416000+zb*(1289888+zb*(-2487008+zb*(2956824+zb*(-2073136+zb*(736556+(-56174-24836*zb)*zb)))))))+t34*(-11008+zb*(86400+zb*(-326784+zb*(756096+zb*(-1036416+zb*(675352+zb*(19232+zb*(-250364+zb*(82724+4838*zb))))))))+t34*(zb*(2560+zb*(36032+zb*(-306880+zb*(908384+zb*(-1347240+zb*(1043120+zb*(-357732+zb*(1676+19926*zb))))))))+t34*((30016+zb*(-180704+zb*(402592+zb*(-378264+zb*(76624+zb*(98456+zb*(-52494+3788*zb)))))))*pow<2>(zb)+t34*((5984+zb*(-32640+zb*(55912+zb*(-24656+zb*(-24044+(25598-6084*zb)*zb)))))*pow<3>(zb)+t34*((880+zb*(-6440+zb*(13608+zb*(-12414+(5249-911*zb)*zb))))*pow<4>(zb)+t34*(256+zb*(-1056+zb*(1512+zb*(-785+73*zb))))*pow<5>(zb)))))))))+t12*(-3328+zb*(38016+zb*(-183872+zb*(507520+zb*(-876736+zb*(964744+zb*(-667792+zb*(286562+zb*(-77165+12065*zb))))))))+t34*(-1280+zb*(38912+zb*(-246976+zb*(718784+zb*(-1174592+zb*(1170928+zb*(-739936+zb*(303904+zb*(-82527+12741*zb))))))))+t34*(-4864+zb*(34048+zb*(-114560+zb*(223936+zb*(-257184+zb*(160976+zb*(-4400+zb*(-116856+(118574-39670*zb)*zb)))))))+t34*(-2816+zb*(17408+zb*(-64128+zb*(212416+zb*(-486624+zb*(618928+zb*(-352288+zb*(-15892+(106790-33682*zb)*zb)))))))+t34*(zb*(12928+zb*(-77376+zb*(192576+zb*(-209952+zb*(-19184+zb*(266352+zb*(-208540+zb*(27036+16076*zb))))))))+t34*((20288+zb*(-147328+zb*(419552+zb*(-583888+zb*(385504+zb*(-67052+zb*(-42704+15544*zb)))))))*pow<2>(zb)+t34*((-1920+zb*(-16000+zb*(100784+zb*(-189264+zb*(155032+zb*(-53550+5030*zb))))))*pow<3>(zb)+t34*((-448+zb*(-4304+zb*(17120+zb*(-21484+(10890-1774*zb)*zb))))*pow<4>(zb)+t34*((-536+zb*(1696+zb*(-2534+(2001-669*zb)*zb)))*pow<5>(zb)+t34*(-64+zb*(204+zb*(-129+3*zb)))*pow<6>(zb)))))))))+t12*(-512+zb*(10752+zb*(-63872+zb*(185504+zb*(-303680+zb*(274888+zb*(-106744+zb*(-12902+(17541-975*zb)*zb)))))))+t34*(-1024+zb*(11264+zb*(-67456+zb*(213664+zb*(-371552+zb*(369384+zb*(-213008+zb*(69888+zb*(-11369+181*zb))))))))+t34*(-512+zb*(4608+zb*(-17536+zb*(36608+zb*(-42784+zb*(28392+zb*(1984+zb*(-46924+(59496-23262*zb)*zb)))))))+t34*(zb*(4096+zb*(-14464+zb*(7168+zb*(55680+zb*(-190136+zb*(328688+zb*(-316188+(154052-28882*zb)*zb)))))))+t34*((-1536+zb*(2144+zb*(22272+zb*(-93000+zb*(142480+zb*(-89704+zb*(9682+7508*zb)))))))*pow<2>(zb)+t34*((-7840+zb*(26720+zb*(-4264+zb*(-86832+zb*(133828+zb*(-76414+14872*zb))))))*pow<3>(zb)+t34*((-4896+zb*(26136+zb*(-51328+zb*(47692+zb*(-20976+3470*zb)))))*pow<4>(zb)+t34*((-1032+zb*(2640+zb*(-1972+zb*(-108+402*zb))))*pow<5>(zb)+t34*((40+zb*(-306+(433-181*zb)*zb))*pow<6>(zb)+t34*(12+(15-13*zb)*zb)*pow<7>(zb)))))))))+t12*(zb*(512+zb*(-5888+zb*(19456+zb*(-22112+zb*(-16288+zb*(72240+zb*(-76710+(32657-3881*zb)*zb)))))))+t34*(zb*(1024+zb*(-7936+zb*(24384+zb*(-24224+zb*(-27776+zb*(85312+zb*(-78372+(34143-6541*zb)*zb)))))))+t34*(zb*(512+zb*(-1280+zb*(-1984+zb*(13440+zb*(-25248+zb*(27568+zb*(-27796+(24036-9184*zb)*zb)))))))+t34*((-256+zb*(-3904+zb*(22272+zb*(-61056+zb*(108864+zb*(-113844+(58844-11000*zb)*zb))))))*pow<2>(zb)+t34*((-2112+zb*(12768+zb*(-32352+zb*(41360+zb*(-26176+zb*(5742+694*zb))))))*pow<3>(zb)+t34*((-3168+zb*(23296+zb*(-60352+zb*(71316+zb*(-38414+7446*zb)))))*pow<4>(zb)+t34*((1184+zb*(-5488+zb*(10388+zb*(-8716+2648*zb))))*pow<5>(zb)+t34*((-192+zb*(580+zb*(-820+368*zb)))*pow<6>(zb)+t34*((-26+(41-5*zb)*zb)*pow<7>(zb)+t34*(7-zb)*pow<8>(zb)))))))))+t12*((-1600+zb*(10752+zb*(-29840+zb*(45192+zb*(-37994+(15457-1981*zb)*zb)))))*pow<3>(zb)+t34*((-832+zb*(9840+zb*(-37016+zb*(62176+zb*(-52232+(21991-3897*zb)*zb)))))*pow<3>(zb)+t34*((-448+zb*(2512+zb*(-6208+zb*(8776+zb*(-9366+(7741-2986*zb)*zb)))))*pow<3>(zb)+t34*((-1216+zb*(4176+zb*(-4112+zb*(3936+zb*(-8548+(7643-1958*zb)*zb)))))*pow<3>(zb)+t34*((752+zb*(-3312+zb*(4792+zb*(-2774+zb*(371+180*zb)))))*pow<4>(zb)+t34*((1768+zb*(-7872+zb*(12768+zb*(-8571+1976*zb))))*pow<5>(zb)+t34*((-520+zb*(2134+zb*(-2641+1002*zb)))*pow<6>(zb)+t34*((12+zb*(-135+102*zb))*pow<7>(zb)+t34*(9*pow<9>(zb)+t34*pow<9>(zb)))))))))+t12*((-128+zb*(1552+zb*(-5384+zb*(9280+zb*(-8510+(3649-465*zb)*zb)))))*pow<3>(zb)+t12*((32+zb*(-208+zb*(576+zb*(-712+(354-43*zb)*zb))))*pow<4>(zb)+t34*((64+zb*(-368+zb*(832+zb*(-888+(460-97*zb)*zb))))*pow<4>(zb)+t34*((32+zb*(-112+(176+(-50-47*zb)*zb)*pow<2>(zb)))*pow<4>(zb)+t34*((48+zb*(-192+zb*(208+zb*(-88+19*zb))))*pow<5>(zb)+t34*((64+zb*(-104+zb*(30+15*zb)))*pow<6>(zb)+t34*((40+zb*(-52+13*zb))*pow<7>(zb)+t34*((-14+11*zb)*pow<8>(zb)+t34*pow<9>(zb))))))))+t34*((-256+zb*(1808+zb*(-6736+zb*(12544+zb*(-11700+(5337-981*zb)*zb)))))*pow<3>(zb)+t34*((-128+zb*(496+zb*(-704+zb*(160+zb*(-34+(809-599*zb)*zb)))))*pow<3>(zb)+t34*((240+zb*(336+zb*(-2016+zb*(1736+(-247-81*zb)*zb))))*pow<4>(zb)+t34*((-312+zb*(800+zb*(-546+zb*(-69+147*zb))))*pow<5>(zb)+t34*((-288+zb*(1004+zb*(-973+273*zb)))*pow<6>(zb)+t34*((130+zb*(-293+147*zb))*pow<7>(zb)+t34*((-21+21*zb)*pow<8>(zb)+2*t34*pow<9>(zb))))))))))))))))))*pow<-1>(-1+t12)*pow<-1>(2+(-1+t12)*zb)*pow<-1>(-2+(2+t12-t34)*zb)*pow<-2>(1+t34)*pow<-2>(t12+t34)*pow<-2>(zb)*pow<-2>(-2+(3+t12)*zb)*pow<-3>(1+t12)*pow<-3>(-1+t34)*pow<-3>(-1+zb))/16;
    return todouble(foo);
}

// Series in epsilon of c13*bubble(13)+c25*bubble(245)
template<>
EpsExp qq2yyg1<TT>::LC::bub::c1325(const TT& zb, const TT& t12, const TT& t34)
{
    return times(
                 CounterForge::cGamma(),
                 EpsExp(-1,{
                    qq2yyg1<TT>::LC::bub::c1325<-1>(zb,t12,t34),
                    qq2yyg1<TT>::LC::bub::c1325<0>(zb,t12,t34)
                 }),
                 2
                 );
}

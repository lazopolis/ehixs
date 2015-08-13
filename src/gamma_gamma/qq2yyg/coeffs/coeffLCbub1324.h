#include "qq2yyg/qq2yyg.h"
#include "counterforge.h"

// Expansion order epsilon^-1 of c13*bubble(13)+c24*bubble(24) 
template<>
template<>
double qq2yyg1<TT>::LC::bub::c1324<-1>(const TT& zb, const TT& t12, const TT& u)
{
    const TT foo = (-20480+zb*(180224+zb*(-668672+zb*(1334272+zb*(-1472000+zb*(709120+zb*(237696+zb*(-537088+zb*(309936+zb*(-80352+zb*(7020+324*zb))))))))))+u*(8192+zb*(-32768+zb*(-16384+zb*(312320+zb*(-734208+zb*(835328+zb*(-534272+zb*(223168+zb*(-95072+zb*(46224+zb*(-14256+1728*zb))))))))))+u*(21504+zb*(-160768+zb*(465920+zb*(-617984+zb*(225920+zb*(388992+zb*(-572864+zb*(342176+zb*(-114492+(24588-3024*zb)*zb))))))))+u*(-8192+zb*(24576+zb*(20480+zb*(-158464+zb*(213248+zb*(-83072+zb*(-37120+zb*(34640+zb*(-6576+288*zb))))))))+u*(-8704+zb*(52224+zb*(-103808+zb*(54784+zb*(77280+zb*(-123808+zb*(66216+zb*(-17096+2464*zb)))))))+u*(3072+zb*(-6144+zb*(-6144+zb*(17088+zb*(1856+zb*(-20048+(11696-1888*zb)*zb)))))+u*(1664+zb*(-7040+zb*(6080+zb*(6112+zb*(-10824+(3944-224*zb)*zb))))+u*(-512+zb*(512+zb*(256+zb*(1136+zb*(-2192+736*zb))))+u*(-144+zb*(256+zb*(364+zb*(-508+32*zb)))+u*(32+u*(4+(12-16*zb)*zb)+(64-96*zb)*pow<2>(zb))))))))))+pow<2>(t12)*(40960+zb*(-311296+zb*(886784+zb*(-720896+zb*(-2195456+zb*(7538176+zb*(-11006976+zb*(9378176+zb*(-4880800+zb*(1488864+zb*(-224520+zb*(5256+1728*zb)))))))))))+u*(-16384+zb*(16384+zb*(454656+zb*(-2173952+zb*(4573184+zb*(-5291264+zb*(3499264+zb*(-1321024+zb*(370240+zb*(-179184+(83744-15600*zb)*zb)))))))))+u*(-43008+zb*(284672+zb*(-604160+zb*(-134656+zb*(2874880+zb*(-5729536+zb*(5727232+zb*(-3276768+zb*(1069416+zb*(-179928+12560*zb)))))))))+u*(16384+u*(17408+zb*(-98304+zb*(125696+zb*(258688+zb*(-922368+zb*(1084128+zb*(-621504+(179968-20768*zb)*zb))))))+u*(-6144+zb*(-6144+zb*(117760+zb*(-252736+zb*(193792+zb*(-27536+zb*(-24112+7360*zb))))))+u*(-3328+u*(1024+zb*(2048+zb*(-14720+zb*(17136+(-3664-1632*zb)*zb)))+u*(288+zb*(-1088+zb*(-504+(3000-1696*zb)*zb))+u*(-64+zb*(-192+(592-336*zb)*zb)+u*(-8+(24-16*zb)*zb))))+zb*(15616+zb*(-5248+zb*(-56992+zb*(90464+zb*(-47232+7680*zb))))))))+(-387072+zb*(1301760+zb*(-1847552+zb*(1231744+zb*(-280960+zb*(-66224+zb*(33552+480*zb)))))))*pow<2>(zb))))+pow<2>(t12)*(-20480+zb*(98304+zb*(93184+zb*(-2225152+zb*(9058816+zb*(-20201984+zb*(28450944+zb*(-26247936+zb*(15799728+zb*(-5954144+zb*(1241804+(-80540-12576*zb)*zb))))))))))+u*(8192+zb*(81920+zb*(-1069056+zb*(4570112+zb*(-10556416+zb*(14936320+zb*(-13675264+zb*(8460608+zb*(-3780192+zb*(1331632+zb*(-351520+42832*zb))))))))))+u*(21504+zb*(-148480+zb*(281600+zb*(701952+zb*(-4386176+zb*(9240448+zb*(-10641344+zb*(7274464+zb*(-2862652+(551548-36512*zb)*zb))))))))+u*(-8192+zb*(-57344+zb*(712704+zb*(-2545920+zb*(4592384+zb*(-4833152+zb*(3200000+zb*(-1371152+(349488-44640*zb)*zb)))))))+u*(-8704+u*(3072+u*(1664+u*(-512+zb*(-512+zb*(10752+zb*(-21808+(13200-1312*zb)*zb)))+u*(-144+zb*(1216+zb*(-2804+(2148-416*zb)*zb))+u*(32+u*(4-4*zb)+zb*(-64+(48-16*zb)*zb))))+zb*(-15744+zb*(42944+zb*(-31584+zb*(-14536+zb*(15752+352*zb))))))+zb*(12288+zb*(-155136+zb*(438336+zb*(-540608+zb*(336336+zb*(-126928+28992*zb)))))))+zb*(74752+zb*(-203136+zb*(55552+zb*(594144+zb*(-1038048+zb*(769320+zb*(-303560+53504*zb)))))))))))+pow<2>(t12)*((-16384+zb*(571392+zb*(-3718144+zb*(11747840+zb*(-21900032+zb*(25781632+zb*(-19498304+zb*(9232736+zb*(-2455120+zb*(224144+30560*zb))))))))))*pow<2>(zb)+u*((155648+zb*(-1479680+zb*(5922816+zb*(-13111040+zb*(18039040+zb*(-16336576+zb*(9931648+zb*(-3968144+(921392-72352*zb)*zb))))))))*pow<2>(zb)+u*((-86016+zb*(386560+zb*(-434432+zb*(-610304+zb*(2256000+zb*(-2717088+zb*(1533616+zb*(-341296+16992*zb))))))))*pow<2>(zb)+u*((-67584+zb*(501504+zb*(-1581824+zb*(2850432+zb*(-3305344+zb*(2384368+zb*(-917264+141536*zb)))))))*pow<2>(zb)+u*((46080+zb*(-185728+zb*(346048+zb*(-516064+zb*(550032+zb*(-284496+49440*zb))))))*pow<2>(zb)+u*((4608+zb*(-30656+zb*(20864+zb*(56528+zb*(-58480+9760*zb)))))*pow<2>(zb)+u*(u*(640+zb*(-560+(80-96*zb)*zb))*pow<2>(zb)+(-5888+zb*(13344+zb*(784+zb*(-11024+3360*zb))))*pow<2>(zb)))))))+pow<2>(t12)*((120832+zb*(-1239040+zb*(4480512+zb*(-8362496+zb*(9043072+zb*(-5717888+zb*(1903264+(-191200-37824*zb)*zb)))))))*pow<4>(zb)+u*((-303104+zb*(1926144+zb*(-5371904+zb*(8330752+zb*(-7601664+zb*(3984960+zb*(-1032832+67840*zb)))))))*pow<4>(zb)+u*((89600+zb*(-620032+zb*(1618176+zb*(-2092288+zb*(1521504+zb*(-617824+104608*zb))))))*pow<4>(zb)+u*((-4096+zb*(107008+zb*(-187392+zb*(77440+(10496-4224*zb)*zb))))*pow<4>(zb)+u*((22656+zb*(-65408+(45920-4768*zb)*zb))*pow<4>(zb)+u*(u*(-864+zb*(736+32*zb))*pow<4>(zb)+(-4608+zb*(6464+zb*(-5760+3200*zb)))*pow<4>(zb))))))+pow<2>(t12)*((-102400+zb*(582656+zb*(-1184768+zb*(1086464+zb*(-401920+zb*(-5952+25088*zb))))))*pow<6>(zb)+u*((150528+zb*(-737280+zb*(1243648+zb*(-902912+zb*(226752+9536*zb)))))*pow<6>(zb)+u*((-79872+zb*(289792+zb*(-386304+(211584-44032*zb)*zb)))*pow<6>(zb)+u*((7680+zb*(-20736+(18048-7040*zb)*zb))*pow<6>(zb)+u*((3840-3904*zb)*pow<6>(zb)+u*(-64+64*zb)*pow<6>(zb)))))+pow<2>(t12)*(pow<2>(t12)*(u*(-512*u*pow<10>(zb)+(-6144+2048*zb)*pow<10>(zb))+1536*pow<12>(zb)*pow<2>(t12)+pow<10>(zb)*(2048-6656*pow<2>(zb)))+(14848+zb*(-16384+zb*(-22272+zb*(27648+544*zb))))*pow<8>(zb)+u*((-2048+zb*(-10752+(43520-19072*zb)*zb))*pow<8>(zb)+u*((256+zb*(3072+1216*zb))*pow<8>(zb)+u*(32*u*pow<8>(zb)+(1536-640*zb)*pow<8>(zb)))))))))))*pow<-1>(-1+zb)*pow<-1>(zb)*pow<-2>(-2+u+zb)*pow<-2>(-2+u+(3-2*t12)*zb)*pow<-2>(2+u+(-1+2*t12)*zb)*pow<-2>(2+u-(1+2*t12)*zb)*pow<-2>(-2+u+(3+2*t12)*zb)*pow<-3>(-1+pow<2>(t12));
    return todouble(foo);
}

// Expansion order epsilon^0 of c13*bubble(13)+c24*bubble(24) 
template<>
template<>
double qq2yyg1<TT>::LC::bub::c1324<0>(const TT& zb, const TT& t12, const TT& u)
{
    const TT foo = (-16384+zb*(192512+zb*(-1124352+zb*(4118528+zb*(-10051584+zb*(16646656+zb*(-18746112+zb*(14178304+zb*(-6959552+zb*(2045808+zb*(-284904+zb*(-2808+3888*zb)))))))))))+u*(32768+zb*(-329728+zb*(1609728+zb*(-4784128+zb*(9119744+zb*(-11187968+zb*(8578048+zb*(-3723648+zb*(555520+zb*(238648+zb*(-130512+(22824-1296*zb)*zb))))))))))+u*(-4096+u*(-28672+u*(14336+zb*(-84480+zb*(185088+zb*(-35072+zb*(-607104+zb*(1173984+zb*(-947664+zb*(345440+(-43320-696*zb)*zb)))))))+u*(8192+zb*(-79104+zb*(328192+zb*(-743168+zb*(935168+zb*(-611280+zb*(169280+(-4968-2184*zb)*zb))))))+u*(-6656+zb*(37504+zb*(-86976+zb*(80192+zb*(19168+zb*(-78664+(39260-4148*zb)*zb)))))+u*(-512+zb*(8512+zb*(-35904+zb*(69440+zb*(-61952+(20444-412*zb)*zb))))+u*(1216+u*(-128+u*(-80+u*(16+zb*(-30+14*zb))+zb*(324+zb*(-462+186*zb)))+zb*(-40+zb*(1168+zb*(-2064+936*zb))))+zb*(-5968+zb*(11640+zb*(-8776+zb*(-136+1768*zb)))))))))+zb*(272896+zb*(-1219072+zb*(3189760+zb*(-5076992+zb*(4822848+zb*(-2545216+zb*(587904+zb*(24560+zb*(-30062+2430*zb))))))))))+zb*(-11264+zb*(305664+zb*(-1658368+zb*(4618752+zb*(-7527808+zb*(7392320+zb*(-4286272+zb*(1341104+zb*(-159772+zb*(-13566+3402*zb))))))))))))+flog<TT>(64)*((512+zb*(-3840+zb*(12160+zb*(-21184+zb*(22176+zb*(-14288+zb*(5544+zb*(-1188+zb*(108+512*zb*pow<16>(t12))))))))))*pow<4>(zb)+u*((-256+zb*(1280+zb*(-2368+zb*(1856+zb*(-304+zb*(-400+zb*(228+zb*(-36+512*zb*pow<16>(t12)))))))))*pow<4>(zb)+u*((-128+zb*(832+zb*(-2048+zb*(2464+zb*(-1544+(484-60*zb)*zb)))))*pow<4>(zb)+u*(u*(3072+zb*(-10752+(11520-3456*zb)*zb)+u*(-1536+zb*(1536+1152*zb)+u*(-768+384*u+1920*zb)))+(64+zb*(-256+zb*(384+zb*(-272+(92-12*zb)*zb))))*pow<4>(zb)))))+flog<TT>(16)*pow<3>(u)*(6144+zb*(-36864+zb*(76800+zb*(-64512+17280*zb)))+u*(-3072+zb*(10752+(-5376-5760*zb)*zb)+u*(-1536+zb*(7680+zb*(-9600+32*pow<10>(t12)*pow<6>(zb)))+u*(768+u*(u*(16+zb*(-72+zb*(116+zb*(-78+18*zb)))+u*(-8+zb*(16+(-2-6*zb)*zb)+u*(-4+(14-10*zb)*zb+u*(2+zb*(-2-2*pow<4>(t12))))))+pow<4>(zb)*(3024*pow<2>(t12)*pow<2>(zb)+32*pow<6>(t12)))+zb*(-1920+32*pow<10>(t12)*pow<6>(zb))))))+pow<2>(t12)*(32768+zb*(-385024+zb*(2455552+zb*(-10079232+zb*(27405312+zb*(-50065408+zb*(61820160+zb*(-51103872+zb*(27221888+zb*(-8381856+zb*(953080+zb*(169268+(-41340-1296*zb)*zb)))))))))))+u*(-65536+u*(8192+zb*(-43008+zb*(40448+zb*(976384+zb*(-5229568+zb*(11625984+zb*(-13551936+zb*(8785280+zb*(-3161248+zb*(564104+(3398-19854*zb)*zb)))))))))+u*(57344+zb*(-521216+zb*(2475008+zb*(-7299584+zb*(13224448+zb*(-14289664+zb*(8945664+zb*(-3131104+zb*(549888+(13668-28260*zb)*zb))))))))+u*(-28672+zb*(201728+zb*(-740608+zb*(1395584+zb*(-973952+zb*(-375424+zb*(729328+zb*(-204104+(7504-13656*zb)*zb)))))))+u*(-16384+zb*(129536+zb*(-503552+zb*(1227200+zb*(-1712704+zb*(1167872+zb*(-307984+(40252-22924*zb)*zb))))))+u*(13312+zb*(-75008+zb*(213952+zb*(-316416+zb*(166080+zb*(31136+(-15836-14308*zb)*zb)))))+u*(1024+zb*(-9856+zb*(32384+zb*(-68640+zb*(66912+(-10352-9072*zb)*zb))))+u*(-2432+zb*(9888+zb*(-18600+zb*(15988+(412-4136*zb)*zb)))+u*(256+zb*(-304+zb*(-24+(986-626*zb)*zb))+u*(160+zb*(-392+zb*(294+2*zb))+u*(-32+zb*(28+4*zb)))))))))))+zb*(692224+zb*(-3913728+zb*(13703680+zb*(-30435840+zb*(43452416+zb*(-39887616+zb*(22698944+zb*(-6692160+zb*(-185808+zb*(851944+zb*(-234534+15822*zb))))))))))))+pow<2>(t12)*(-16384+zb*(159744+zb*(-1234944+zb*(6557696+zb*(-22055936+zb*(47994368+zb*(-68867328+zb*(64630528+zb*(-37476928+zb*(10853232+zb*(372840+zb*(-1107424+zb*(178212+12420*zb))))))))))))+u*(32768+zb*(-280576+zb*(2031616+zb*(-9540608+zb*(26797056+zb*(-46668544+zb*(51474432+zb*(-34378496+zb*(9989120+zb*(3433784+zb*(-3906304+(1089268-71020*zb)*zb))))))))))+u*(-4096+zb*(-3072+zb*(-139776+zb*(599552+zb*(-97792+zb*(-2188672+zb*(3478976+zb*(-3025472+zb*(2732080+zb*(-1761660+zb*(389814+30326*zb))))))))))+u*(-28672+u*(14336+u*(8192+u*(-6656+u*(-512+u*(1216+u*(-128+u*(-80+u*(16+(-14-2*zb)*zb)+zb*(228+zb*(-234+54*zb)))+zb*(-104+zb*(1088+zb*(-2020+972*zb))))+zb*(-4560+zb*(5896+zb*(-1296+zb*(-7620+4924*zb)))))+zb*(6976+zb*(-17472+zb*(34432+zb*(-46048+zb*(10988+7412*zb))))))+zb*(30336+zb*(-83264+zb*(121152+zb*(41440+zb*(-220616+zb*(97220+11236*zb)))))))+zb*(-64768+zb*(215040+zb*(-516736+zb*(724224+zb*(-225424+zb*(-307520+zb*(117632+40400*zb))))))))+zb*(-72192+zb*(365824+zb*(-1027072+zb*(1041664+zb*(-60576+zb*(-46384+zb*(-383152+zb*(104976+62896*zb)))))))))+zb*(227840+zb*(-1157632+zb*(4030464+zb*(-8286720+zb*(9216576+zb*(-5449792+zb*(2126144+zb*(-766512+zb*(-16238+115422*zb))))))))))))+pow<2>(t12)*((215040+zb*(-1784832+zb*(7486464+zb*(-19762176+zb*(34067200+zb*(-37310592+zb*(23342080+zb*(-4665280+zb*(-3588536+zb*(2346980+(-298668-48736*zb)*zb))))))))))*pow<2>(zb)+u*((-382976+zb*(2607616+zb*(-8665600+zb*(17623552+zb*(-22200064+zb*(14388928+zb*(2305472+zb*(-12342240+zb*(8967112+zb*(-2478438+165598*zb))))))))))*pow<2>(zb)+u*((100864+zb*(-224768+zb*(-483328+zb*(1994496+zb*(-2904000+zb*(4872192+zb*(-7727040+zb*(6143376+zb*(-1902622+108366*zb)))))))))*pow<2>(zb)+u*((167936+zb*(-1128960+zb*(2864640+zb*(-2944128+zb*(1250688+zb*(-1421024+zb*(1836448+(-551448-85608*zb)*zb)))))))*pow<2>(zb)+u*((-99072+zb*(368000+zb*(-104320+zb*(-1015232+zb*(1062544+zb*(-114184+(9024-92424*zb)*zb))))))*pow<2>(zb)+u*((-11008+zb*(132544+zb*(-423744+zb*(432992+zb*(-133968+(95436-74588*zb)*zb)))))*pow<2>(zb)+u*((17216+zb*(-59008+zb*(22784+zb*(66000+(-16116-20188*zb)*zb))))*pow<2>(zb)+u*((-2048+zb*(-3232+zb*(22432+(-13112-1288*zb)*zb)))*pow<2>(zb)+u*((-536+zb*(2244+(-900-264*zb)*zb))*pow<2>(zb)+u*(u*(2-2*zb)*pow<2>(zb)+(72+(-22-18*zb)*zb)*pow<2>(zb))))))))))+pow<2>(t12)*((-470016+zb*(2843648+zb*(-7766016+zb*(11472384+zb*(-8643456+zb*(1049088+zb*(3157120+zb*(-1817056+zb*(70724+107708*zb)))))))))*pow<4>(zb)+u*((575488+zb*(-2652160+zb*(4572672+zb*(-1947648+zb*(-6289024+zb*(12109440+zb*(-8691808+(2490304-146528*zb)*zb)))))))*pow<4>(zb)+u*((-57344+zb*(-67072+zb*(1598208+zb*(-5807104+zb*(9854336+zb*(-7988320+(2741488-250512*zb)*zb))))))*pow<4>(zb)+u*((-87552+zb*(446464+zb*(-1761024+zb*(4025088+zb*(-4112480+(1609664-126304*zb)*zb)))))*pow<4>(zb)+u*((-16256+zb*(-77312+zb*(763264+zb*(-1283488+(660696-70776*zb)*zb))))*pow<4>(zb)+u*((3456+zb*(81536+zb*(-234016+(148800-13856*zb)*zb)))*pow<4>(zb)+u*((14464+zb*(-44064+(28016-2384*zb)*zb))*pow<4>(zb)+u*(u*(132-100*zb)*pow<4>(zb)+(-4384+(4928-1056*zb)*zb)*pow<4>(zb))))))))+pow<2>(t12)*((374784+zb*(-1351680+zb*(1934848+zb*(-831488+zb*(-505216+zb*(237056+(262688-128000*zb)*zb))))))*pow<6>(zb)+u*((-157696+zb*(230400+zb*(1131008+zb*(-3247616+zb*(2862208+(-827584-9632*zb)*zb)))))*pow<6>(zb)+u*((-94720+zb*(818176+zb*(-2337024+zb*(2518528+zb*(-1003680+90048*zb)))))*pow<6>(zb)+u*((166400+zb*(-757248+zb*(1088256+zb*(-528512+50464*zb))))*pow<6>(zb)+u*((-116096+zb*(279552+zb*(-179488+27776*zb)))*pow<6>(zb)+u*((27776+zb*(-30912+6816*zb))*pow<6>(zb)+u*(-32*u*pow<6>(zb)+(-608+448*zb)*pow<6>(zb)))))))+pow<2>(t12)*(pow<2>(t12)*((10240+zb*(-41984+(48128-17920*zb)*zb))*pow<10>(zb)+u*((-16384+(29184-14336*zb)*zb)*pow<10>(zb)+u*(-512*u*pow<10>(zb)+(8704-5120*zb)*pow<10>(zb)))+(-1024+1024*zb)*pow<12>(zb)*pow<2>(t12))+(-112640+zb*(167936+zb*(-83968+zb*(170496+zb*(-210048+73600*zb)))))*pow<8>(zb)+u*((-45056+zb*(242688+zb*(-241664+zb*(768+51712*zb))))*pow<8>(zb)+u*((115712+zb*(-196096+zb*(66560+9216*zb)))*pow<8>(zb)+u*((-30720+zb*(18176+3840*zb))*pow<8>(zb)+u*(256*u*pow<8>(zb)+(-2432+1664*zb)*pow<8>(zb)))))))))))+flog<TT>(4)*(40960+zb*(-380928+zb*(1517568+zb*(-3337216+zb*(4276736+zb*(-2878720+zb*(197248+zb*(1375424+zb*(-1223488+zb*(513504+zb*(-111024+9936*zb))))))))))+u*(-36864+zb*(253952+zb*(-668672+zb*(693248+zb*(309504+zb*(-1699584+zb*(2142528+zb*(-1489472+zb*(650432+zb*(-185264+(33072-2880*zb)*zb)))))))))+u*(-34816+zb*(310272+zb*(-1108992+zb*(2014208+zb*(-1803648+zb*(280768+zb*(1009664+zb*(-1050656+zb*(488400+zb*(-118896+(14448-720*zb)*zb)))))))))+u*(25600+zb*(-144384+zb*(295936+zb*(-151552+zb*(-393792+zb*(769152+zb*(-581312+zb*(229680+zb*(-57376+(12288-1776*zb)*zb))))))))+u*(6144+u*(-7168+zb*(47616+zb*(-81920+zb*(14464+zb*(90656+zb*(-81856+zb*(22776+zb*(-1624+576*zb)))))))+u*(512+zb*(7680+zb*(-25344+zb*(10944+zb*(29616+zb*(-38760+(16088-2112*zb)*zb)))))+u*(1536+zb*(-8448+zb*(5632+zb*(4576+zb*(-5448+zb*(280+512*zb)))))+u*(-256+u*(-192+u*(32+u*(16-16*zb)*zb+zb*(-32+(128-112*zb)*zb))+zb*(224+zb*(320+(-240-64*zb)*zb)))+zb*(-64+zb*(-384+zb*(2720+zb*(-2800+768*zb))))))))+zb*(-77824+zb*(256512+zb*(-349952+zb*(113472+zb*(241824+zb*(-294128+zb*(136584+zb*(-29560+2944*zb))))))))))))+pow<2>(t12)*(-81920+zb*(663552+zb*(-2084864+zb*(2328576+zb*(3670016+zb*(-17271808+zb*(29552128+zb*(-29763328+zb*(19139776+zb*(-7863136+zb*(1954032+zb*(-254616+11592*zb)))))))))))+u*(73728+zb*(-360448+zb*(-6144+zb*(4081664+zb*(-13515776+zb*(22693888+zb*(-23296768+zb*(15519488+zb*(-6942304+zb*(2217472+zb*(-571192+(120200-13872*zb)*zb))))))))))+u*(69632+zb*(-595968+zb*(1947648+zb*(-2508800+zb*(-1311232+zb*(9042688+zb*(-13684736+zb*(10959744+zb*(-5045360+zb*(1250088+(-121304-3040*zb)*zb)))))))))+u*(-75776+zb*(301056+zb*(169984+zb*(-3125248+zb*(7871744+zb*(-10040576+zb*(7520896+zb*(-3425280+zb*(936088+zb*(-147336+13040*zb)))))))))+u*(-18432+zb*(214016+zb*(-736768+zb*(910080+zb*(255872+zb*(-1858880+zb*(2046176+zb*(-1047664+(255056-20288*zb)*zb)))))))+u*(29696+zb*(-92160+zb*(-115968+zb*(881920+zb*(-1562688+zb*(1332992+zb*(-600816+(141136-13408*zb)*zb))))))+u*(512+u*(-5376+zb*(12544+zb*(26240+zb*(-105984+(114928-47632*zb)*zb)))+u*(448+zb*(4512+zb*(-14800+zb*(10632+(2728-3328*zb)*zb)))+u*(416+zb*(-768+zb*(-1880+(4264-2032*zb)*zb))+u*(-48+u*(-8+8*zb)+zb*(-248+(648-352*zb)*zb)))))+zb*(-40704+zb*(143872+zb*(-144000+zb*(-44128+zb*(157392+zb*(-86704+15040*zb))))))))))))+pow<2>(t12)*(40960+zb*(-217088+zb*(-88064+zb*(4543488+zb*(-20342784+zb*(49462784+zb*(-77103872+zb*(80946816+zb*(-57847392+zb*(27708016+zb*(-8437752+zb*(1402884+(-55388-12576*zb)*zb)))))))))))+u*(-36864+zb*(-57344+zb*(2313216+zb*(-12434432+zb*(34741760+zb*(-60631040+zb*(70737792+zb*(-56844416+zb*(31820720+zb*(-12397600+zb*(3276476+zb*(-517724+30256*zb)))))))))))+u*(-34816+zb*(400384+zb*(-1780736+zb*(3447808+zb*(-1082112+zb*(-7930752+zb*(16847872+zb*(-16729664+zb*(9219576+zb*(-2634116+zb*(273052+6320*zb))))))))))+u*(37888+zb*(-41984+zb*(-1201152+zb*(6506496+zb*(-16116864+zb*(23499136+zb*(-21874496+zb*(13216768+zb*(-4932780+(990316-81152*zb)*zb))))))))+u*(9216+zb*(-215552+zb*(1193728+zb*(-2860160+zb*(3459648+zb*(-2162912+zb*(623312+zb*(5288+zb*(-61080+8864*zb))))))))+u*(-14848+u*(-256+u*(2688+zb*(-15232+zb*(20928+zb*(22784+zb*(-62744+(31576-960*zb)*zb))))+u*(-224+zb*(-3088+zb*(17576+zb*(-28908+(16180-1728*zb)*zb)))+u*(-208+zb*(1376+zb*(-2964+2228*zb))+u*(24+4*u+zb*(-52+(44-16*zb)*zb)))))+zb*(45440+zb*(-256768+zb*(544448+zb*(-543120+zb*(290296+zb*(-111880+29344*zb)))))))+zb*(53248+zb*(119424+zb*(-976256+zb*(2113696+zb*(-2251328+zb*(1359512+zb*(-488472+82496*zb))))))))))))+pow<2>(t12)*((32768+zb*(-1159168+zb*(8007680+zb*(-27213824+zb*(55547904+zb*(-73463296+zb*(64778240+zb*(-37963776+zb*(14142976+zb*(-2903408+zb*(163024+30560*zb)))))))))))*pow<2>(zb)+u*((-327680+zb*(3686400+zb*(-17043456+zb*(43892736+zb*(-71089152+zb*(76493824+zb*(-55698176+zb*(27100672+zb*(-8266048+(1290240-41792*zb)*zb)))))))))*pow<2>(zb)+u*((327680+zb*(-2338816+zb*(7178240+zb*(-12324864+zb*(12916736+zb*(-8646400+zb*(4147328+zb*(-1751936+(546112-55360*zb)*zb))))))))*pow<2>(zb)+u*((49152+zb*(-684032+zb*(3230720+zb*(-7892992+zb*(11717120+zb*(-10791168+zb*(5752512+zb*(-1541632+158528*zb))))))))*pow<2>(zb)+u*((-159744+zb*(919040+zb*(-2459648+zb*(4228608+zb*(-4921472+zb*(3503392+zb*(-1300640+190976*zb)))))))*pow<2>(zb)+u*((36864+zb*(-119808+zb*(273664+zb*(-608256+zb*(723520+zb*(-362496+59200*zb))))))*pow<2>(zb)+u*((16384+zb*(-63232+zb*(32640+zb*(79360+zb*(-76224+13120*zb)))))*pow<2>(zb)+u*(u*(640-560*zb)*pow<2>(zb)+pow<2>(zb)*(-7168+zb*(15104+(-10752+3264*zb)*pow<2>(zb))))))))))+pow<2>(t12)*((-241664+zb*(2598912+zb*(-10200064+zb*(21205504+zb*(-26448640+zb*(20478848+zb*(-9524416+zb*(2285664+(-115552-37824*zb)*zb))))))))*pow<4>(zb)+u*((727040+zb*(-5394432+zb*(17150464+zb*(-30395904+zb*(32577152+zb*(-21289472+zb*(7953888+zb*(-1359712+30016*zb))))))))*pow<4>(zb)+u*((-482304+zb*(3255808+zb*(-9228288+zb*(14133504+zb*(-12736960+zb*(6742112+zb*(-1859872+172448*zb)))))))*pow<4>(zb)+u*((97792+zb*(-838144+zb*(2099968+zb*(-2434560+zb*(1577952+zb*(-598880+100384*zb))))))*pow<4>(zb)+u*((-49408+zb*(260480+zb*(-344640+zb*(132896+(5728-4224*zb)*zb))))*pow<4>(zb)+u*((31872+zb*(-82944+zb*(63904+zb*(-16928+3200*zb))))*pow<4>(zb)+u*(u*(-864+zb*(736+32*zb))*pow<4>(zb)+(-2880+zb*(4128+zb*(-5088+3232*zb)))*pow<4>(zb)))))))+pow<2>(t12)*((204800+zb*(-1267712+zb*(2952192+zb*(-3357696+zb*(1890304+zb*(-390016+zb*(-56128+25088*zb)))))))*pow<6>(zb)+u*((-403456+zb*(2207744+zb*(-4409344+zb*(4135936+zb*(-1758336+zb*(201728+34624*zb))))))*pow<6>(zb)+u*((310272+zb*(-1396736+zb*(2306048+zb*(-1712384+(526400-34496*zb)*zb))))*pow<6>(zb)+u*((-95232+zb*(338944+zb*(-443136+(243712-51072*zb)*zb)))*pow<6>(zb)+u*(u*(-64*u*pow<6>(zb)+(3968-4096*zb)*pow<6>(zb))+(-9088+(14144-7040*zb)*zb)*pow<7>(zb)))))+pow<2>(t12)*(pow<2>(t12)*((-4096+zb*(2048+(13312-6656*zb)*zb))*pow<10>(zb)+u*((14336+(-10240-4608*zb)*zb)*pow<10>(zb)+u*(-512*u*pow<10>(zb)+(-5120+1536*zb)*pow<10>(zb)))-3072*pow<12>(zb)*pow<2>(t12))+(-29696+zb*(47616+zb*(28160+zb*(-77568+zb*(26560+544*zb)))))*pow<8>(zb)+u*((18944+zb*(3072+zb*(-120064+(109312-18528*zb)*zb)))*pow<8>(zb)+u*((-2560+zb*(-16640+(44160-17856*zb)*zb))*pow<8>(zb)+u*((-2816+zb*(5888+576*zb))*pow<8>(zb)+u*(32*u*pow<8>(zb)+(1472-608*zb)*pow<8>(zb))))))))))))+flog<TT>(2-u-zb)*(-40960+zb*(380928+zb*(-1517568+zb*(3337216+zb*(-4278272+zb*(2890240+zb*(-233728+zb*(-1311872+zb*(1156960+zb*(-470640+zb*(94392+(-6372-324*zb)*zb))))))))))+u*(36864+zb*(-253952+zb*(668672+zb*(-693248+zb*(-308736+zb*(1695744+zb*(-2141568+zb*(1517696+zb*(-723248+zb*(267872+zb*(-81756+(17388-1728*zb)*zb))))))))))+u*(34816+zb*(-310272+zb*(1108992+zb*(-2014208+zb*(1804032+zb*(-283264+zb*(-1000448+zb*(1034048+zb*(-476088+zb*(117444+zb*(-16380+1296*zb))))))))))+u*(-37888+u*(-9216+zb*(88576+zb*(-280320+zb*(371840+zb*(-113472+zb*(-241824+zb*(293360+zb*(-135048+(28600-2752*zb)*zb)))))))+u*(14848+zb*(-67584+zb*(97664+zb*(-14464+zb*(-90656+zb*(81856+zb*(-22776+(1624-576*zb)*zb))))))+u*(256+u*(-2688+zb*(8576+zb*(-6080+zb*(-4096+zb*(5304+(-280-512*zb)*zb))))+u*(224+zb*(144+zb*(216+zb*(-2516+(2764-768*zb)*zb)))+u*(208+zb*(-288+zb*(-236+zb*(252+64*zb)))+u*(-24+u*(-4+zb*(-12+16*zb))+zb*(20+zb*(-108+112*zb))))))+zb*(-9600+zb*(25344+zb*(-10944+zb*(-29616+zb*(38760+zb*(-16088+2112*zb)))))))))+zb*(218112+zb*(-449536+zb*(280576+zb*(359040+zb*(-768384+zb*(581696+zb*(-235776+zb*(66700+zb*(-17436+2736*zb))))))))))))+pow<2>(t12)*(81920+zb*(-663552+zb*(2084864+zb*(-2328576+zb*(-3670016+zb*(17271808+zb*(-29552128+zb*(29763328+zb*(-19139776+zb*(7858528+zb*(-1937904+zb*(235032+(-1800-1728*zb)*zb)))))))))))+u*(-73728+zb*(360448+zb*(6144+zb*(-4081664+zb*(13515776+zb*(-22693888+zb*(23296768+zb*(-15519488+zb*(6942304+zb*(-2217472+zb*(571192+zb*(-120200+13872*zb)))))))))))+u*(-69632+zb*(595968+zb*(-1947648+zb*(2508800+zb*(1311232+zb*(-9042688+zb*(13684736+zb*(-10959744+zb*(5045360+zb*(-1250088+zb*(121304+3040*zb))))))))))+u*(75776+zb*(-301056+zb*(-169984+zb*(3125248+zb*(-7871744+zb*(10040576+zb*(-7520896+zb*(3425280+zb*(-936088+(147336-13040*zb)*zb))))))))+u*(18432+u*(-29696+u*(-512+zb*(40704+zb*(-143872+zb*(144000+zb*(44128+zb*(-157392+(86704-15040*zb)*zb)))))+u*(5376+zb*(-12544+zb*(-26240+zb*(105984+zb*(-114928+(47632-6048*zb)*zb))))+u*(-448+u*(-416+u*(48+u*(8+zb*(-24+16*zb))+zb*(248+zb*(-648+352*zb)))+zb*(768+zb*(1880+zb*(-4264+2032*zb))))+zb*(-4512+zb*(14800+zb*(-10632+zb*(-2728+3328*zb)))))))+zb*(92160+zb*(115968+zb*(-881920+zb*(1562688+zb*(-1332992+zb*(600816+zb*(-141136+13408*zb))))))))+zb*(-214016+zb*(736768+zb*(-910080+zb*(-255872+zb*(1858880+zb*(-2046176+zb*(1047664+zb*(-255056+20288*zb))))))))))))+pow<2>(t12)*(-40960+zb*(217088+zb*(88064+zb*(-4543488+zb*(20342784+zb*(-49462784+zb*(77103872+zb*(-80946816+zb*(57847392+zb*(-27708016+zb*(8437752+zb*(-1402884+zb*(55388+12576*zb))))))))))))+u*(36864+zb*(57344+zb*(-2313216+zb*(12434432+zb*(-34741760+zb*(60631040+zb*(-70737792+zb*(56844416+zb*(-31820720+zb*(12397600+zb*(-3276476+(517724-30256*zb)*zb))))))))))+u*(34816+zb*(-400384+zb*(1780736+zb*(-3447808+zb*(1082112+zb*(7930752+zb*(-16847872+zb*(16729664+zb*(-9219576+zb*(2634116+(-273052-6320*zb)*zb)))))))))+u*(-37888+u*(-9216+zb*(215552+zb*(-1193728+zb*(2860160+zb*(-3459648+zb*(2162912+zb*(-623312+zb*(-5288+(61080-8864*zb)*zb)))))))+u*(14848+zb*(-53248+zb*(-119424+zb*(976256+zb*(-2113696+zb*(2251328+zb*(-1359512+(488472-82496*zb)*zb))))))+u*(256+zb*(-45440+zb*(256768+zb*(-544448+zb*(543120+zb*(-290296+(111880-29344*zb)*zb)))))+u*(-2688+zb*(15232+zb*(-20928+zb*(-22784+zb*(62744+zb*(-31576+960*zb)))))+u*(224+u*(208+u*(-24+u*(-4+4*zb)+zb*(52+zb*(-44+16*zb)))+zb*(-1376+zb*(2964+zb*(-2228+432*zb))))+zb*(3088+zb*(-17576+zb*(28908+zb*(-16180+1728*zb)))))))))+zb*(41984+zb*(1201152+zb*(-6506496+zb*(16116864+zb*(-23499136+zb*(21874496+zb*(-13216768+zb*(4932780+zb*(-990316+81152*zb))))))))))))+pow<2>(t12)*((-32768+zb*(1159168+zb*(-8007680+zb*(27213824+zb*(-55547904+zb*(73463296+zb*(-64778240+zb*(37963776+zb*(-14142976+zb*(2903408+(-163024-30560*zb)*zb))))))))))*pow<2>(zb)+u*((327680+zb*(-3686400+zb*(17043456+zb*(-43892736+zb*(71089152+zb*(-76493824+zb*(55698176+zb*(-27100672+zb*(8266048+zb*(-1290240+41792*zb))))))))))*pow<2>(zb)+u*((-327680+zb*(2338816+zb*(-7178240+zb*(12324864+zb*(-12916736+zb*(8646400+zb*(-4147328+zb*(1751936+zb*(-546112+55360*zb)))))))))*pow<2>(zb)+u*((-49152+zb*(684032+zb*(-3230720+zb*(7892992+zb*(-11717120+zb*(10791168+zb*(-5752512+(1541632-158528*zb)*zb)))))))*pow<2>(zb)+u*((159744+zb*(-919040+zb*(2459648+zb*(-4228608+zb*(4921472+zb*(-3503392+(1300640-190976*zb)*zb))))))*pow<2>(zb)+u*((-36864+zb*(119808+zb*(-273664+zb*(608256+zb*(-723520+(362496-59200*zb)*zb)))))*pow<2>(zb)+u*((-16384+zb*(63232+zb*(-32640+zb*(-79360+(76224-13120*zb)*zb))))*pow<2>(zb)+u*(u*(-640+zb*(560+zb*(-80+96*zb)))*pow<2>(zb)+(7168+zb*(-15104+zb*(-64+(10752-3264*zb)*zb)))*pow<2>(zb))))))))+pow<2>(t12)*((241664+zb*(-2598912+zb*(10200064+zb*(-21205504+zb*(26448640+zb*(-20478848+zb*(9524416+zb*(-2285664+zb*(115552+37824*zb)))))))))*pow<4>(zb)+u*((-727040+zb*(5394432+zb*(-17150464+zb*(30395904+zb*(-32577152+zb*(21289472+zb*(-7953888+(1359712-30016*zb)*zb)))))))*pow<4>(zb)+u*((482304+zb*(-3255808+zb*(9228288+zb*(-14133504+zb*(12736960+zb*(-6742112+(1859872-172448*zb)*zb))))))*pow<4>(zb)+u*((-97792+zb*(838144+zb*(-2099968+zb*(2434560+zb*(-1577952+(598880-100384*zb)*zb)))))*pow<4>(zb)+u*((49408+zb*(-260480+zb*(344640+zb*(-132896+zb*(-5728+4224*zb)))))*pow<4>(zb)+u*((-31872+zb*(82944+zb*(-63904+(16928-3200*zb)*zb)))*pow<4>(zb)+u*(u*(864+(-736-32*zb)*zb)*pow<4>(zb)+(2880+zb*(-4128+(5088-3232*zb)*zb))*pow<4>(zb)))))))+pow<2>(t12)*((-204800+zb*(1267712+zb*(-2952192+zb*(3357696+zb*(-1890304+zb*(390016+(56128-25088*zb)*zb))))))*pow<6>(zb)+u*((403456+zb*(-2207744+zb*(4409344+zb*(-4135936+zb*(1758336+(-201728-34624*zb)*zb)))))*pow<6>(zb)+u*((-310272+zb*(1396736+zb*(-2306048+zb*(1712384+zb*(-526400+34496*zb)))))*pow<6>(zb)+u*((95232+zb*(-338944+zb*(443136+zb*(-243712+51072*zb))))*pow<6>(zb)+u*(u*(u*(64-64*zb)*pow<6>(zb)+(-3968+(4096-64*zb)*zb)*pow<6>(zb))+(9088+zb*(-14144+7040*zb))*pow<7>(zb)))))+pow<2>(t12)*(pow<2>(t12)*((4096+zb*(-2048+zb*(-13312+6656*zb)))*pow<10>(zb)+u*((-14336+zb*(10240+4608*zb))*pow<10>(zb)+u*(512*u*pow<10>(zb)+(5120-1536*zb)*pow<10>(zb)))+(-1536*u*pow<12>(zb)+(3072-1536*zb)*pow<12>(zb))*pow<2>(t12))+(29696+zb*(-47616+zb*(-28160+zb*(77568+(-26560-544*zb)*zb))))*pow<8>(zb)+u*((-18944+zb*(-3072+zb*(120064+zb*(-109312+18528*zb))))*pow<8>(zb)+u*((2560+zb*(16640+zb*(-44160+17856*zb)))*pow<8>(zb)+u*((2816+(-5888-576*zb)*zb)*pow<8>(zb)+u*(-32*u*pow<8>(zb)+(-1472+608*zb)*pow<8>(zb))))))))))))+flog<TT>(256)*(u*((1536+zb*(-8448+zb*(18432+zb*(-20352+zb*(12000+zb*(-3600+432*zb))))))*pow<6>(zb)+u*((-768+zb*(2304+zb*(-1920+(528-144*zb)*pow<2>(zb))))*pow<6>(zb)+u*((-384+zb*(1728+zb*(-2400+(1296-240*zb)*zb)))*pow<6>(zb)+u*(pow<3>(u)*(zb*(-32+zb*(112+zb*(-120+36*zb)))+u*(u*(u*(-4*zb+u*zb*(4*pow<2>(t12)-4*zb*pow<2>(t12)))+zb*(8+zb*(-20-108*pow<2>(zb)*pow<4>(t12))))+zb*(16+zb*(-16+zb*(-12+zb*(20*pow<6>(t12)-24*zb*pow<6>(t12)))))))+(192+zb*(-384+(240-48*zb)*zb))*pow<6>(zb)))))+(1152*pow<2>(t12)+zb*(-4032*pow<2>(t12)+zb*(4896*pow<2>(t12)+zb*(-2448*pow<2>(t12)+432*zb*pow<2>(t12)))))*pow<9>(zb)))*pow<-1>(-1+zb)*pow<-1>(zb)*pow<-2>(-2+u+(3-2*t12)*zb)*pow<-2>(2+u+(-1+2*t12)*zb)*pow<-2>(2+u-(1+2*t12)*zb)*pow<-2>(-2+u+(3+2*t12)*zb)*pow<-3>(-1+t12)*pow<-3>(1+t12)*pow<-3>(-2+u+zb);
    return todouble(foo);
}

// Series in epsilon of c13*bubble(13)+c24*bubble(24)
template<>
EpsExp qq2yyg1<TT>::LC::bub::c1324(const TT& zb, const TT& t12, const TT& u)
{
    return times(
                 CounterForge::cGamma,
                 EpsExp(-1,{
                    qq2yyg1<TT>::LC::bub::c1324<-1>(zb,t12,u),
                    qq2yyg1<TT>::LC::bub::c1324<0>(zb,t12,u)
                 }),
                 2
                 );
}

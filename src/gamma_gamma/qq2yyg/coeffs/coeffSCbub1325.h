#include "qq2yyg/qq2yyg.h"
#include "counterforge.h"

// Expansion order epsilon^-1 of c13*bubble(13)+c25*bubble(25) 
template<>
template<>
double qq2yyg1<TT>::SC::bub::c1325<-1>(const TT& zb, const TT& t12, const TT& t34)
{
    const TT foo = ((t34*(zb*(7168+zb*(-62208+zb*(233600+zb*(-495424+zb*(648480+zb*(-535808+zb*(272448+zb*(-77760+9504*zb))))))))+t34*(6144+zb*(-59392+zb*(263296+zb*(-703680+zb*(1244128+zb*(-1496240+zb*(1211248+zb*(-629592+(188604-24516*zb)*zb)))))))+t34*(-9216+zb*(103424+zb*(-483456+zb*(1261760+zb*(-2059552+zb*(2214560+zb*(-1593336+zb*(749764+zb*(-210622+26664*zb))))))))+t34*(-512+zb*(-21760+zb*(198912+zb*(-719488+zb*(1421728+zb*(-1696960+zb*(1260936+zb*(-574868+(149084-17037*zb)*zb)))))))+t34*(-1536+zb*(5376+zb*(-14848+zb*(84736+zb*(-288192+zb*(506368+zb*(-492576+zb*(267384+zb*(-75210+8463*zb))))))))+t34*(2048+zb*(-10752+zb*(11904+zb*(29504+zb*(-81856+zb*(51632+zb*(34024+zb*(-59320+(26828-4022*zb)*zb)))))))+t34*(zb*(1024+zb*(-2176+zb*(-10944+zb*(47168+zb*(-68000+zb*(40032+zb*(-2836+zb*(-5786+1558*zb))))))))+t34*((1408+zb*(-7584+zb*(15312+zb*(-13792+zb*(4612+(380-361*zb)*zb)))))*pow<3>(zb)+t34*(256+zb*(-1040+zb*(1464+zb*(-776+zb*(66+35*zb)))))*pow<4>(zb)))))))))+t12*(zb*(7168+zb*(-57600+zb*(199040+zb*(-385984+zb*(458976+zb*(-342272+zb*(156096+zb*(-39744+4320*zb))))))))+t34*(12288+zb*(-108544+zb*(405504+zb*(-829568+zb*(999552+zb*(-700640+zb*(251872+zb*(-22064+zb*(-10056+1656*zb))))))))+t34*(-21504+zb*(224768+zb*(-988672+zb*(2391744+zb*(-3474688+zb*(3091520+zb*(-1634616+zb*(468236+zb*(-59438+2640*zb))))))))+t34*(8192+zb*(-102912+zb*(537344+zb*(-1564608+zb*(2805696+zb*(-3192800+zb*(2284432+zb*(-984436+(234360-25276*zb)*zb)))))))+t34*(-18432+zb*(147712+zb*(-509440+zb*(1030272+zb*(-1445088+zb*(1559680+zb*(-1281592+zb*(712720+zb*(-226366+30647*zb))))))))+t34*(7168+zb*(-65280+zb*(249344+zb*(-506112+zb*(597888+zb*(-465760+zb*(319376+zb*(-214864+(94696-16609*zb)*zb)))))))+t34*((-8192+zb*(42432+zb*(-78016+zb*(58688+zb*(-19688+zb*(18876+zb*(-19762+5654*zb)))))))*pow<2>(zb)+t34*(zb*(1024+zb*(-7680+zb*(18880+zb*(-17408+zb*(2528+zb*(3584+zb*(-3460+(4560-1886*zb)*zb)))))))+t34*((512+zb*(-2560+zb*(3424+zb*(848+zb*(-4584+zb*(3336+zb*(-1634+563*zb)))))))*pow<2>(zb)+t34*(64+zb*(-400+zb*(704+zb*(-488+(216-77*zb)*zb))))*pow<4>(zb)))))))))+t12*(6144+zb*(-49152+zb*(154496+zb*(-229568+zb*(118688+zb*(109968+zb*(-207120+zb*(129064+zb*(-36228+3708*zb))))))))+t34*(-15360+zb*(123904+zb*(-435584+zb*(875072+zb*(-1101216+zb*(884608+zb*(-429576+zb*(98764+(4318-4920*zb)*zb)))))))+t34*(15360+zb*(-101888+zb*(223616+zb*(-69824+zb*(-497408+zb*(939312+zb*(-806416+zb*(407244+zb*(-134168+24094*zb))))))))+t34*(-29696+zb*(233984+zb*(-732544+zb*(1092800+zb*(-581184+zb*(-482112+zb*(934856+zb*(-596356+(188368-27994*zb)*zb)))))))+t34*(13824+zb*(-101120+zb*(332416+zb*(-628672+zb*(688992+zb*(-346992+zb*(-50440+zb*(133492+zb*(-44704+3275*zb))))))))+t34*(-5632+zb*(39680+zb*(-117376+zb*(219456+zb*(-337920+zb*(414112+zb*(-329296+zb*(152860+zb*(-44720+8563*zb))))))))+t34*(zb*(-5632+zb*(36736+zb*(-105536+zb*(194304+zb*(-262288+zb*(240072+zb*(-126836+(33056-3764*zb)*zb)))))))+t34*((-3456+zb*(21696+zb*(-58368+zb*(91328+zb*(-89408+zb*(49692+zb*(-11728+364*zb)))))))*pow<2>(zb)+t34*((-1408+zb*(7648+zb*(-17104+zb*(20448+zb*(-13012+(3388-65*zb)*zb)))))*pow<3>(zb)+t34*(-320+zb*(1360+zb*(-2224+zb*(1696+zb*(-526+35*zb)))))*pow<4>(zb)))))))))+t12*(-3072+zb*(2560+zb*(69632+zb*(-286656+zb*(519616+zb*(-529888+zb*(331832+zb*(-135036+(35534-4512*zb)*zb)))))))+t34*(4096+zb*(-6656+zb*(-78592+zb*(379584+zb*(-767168+zb*(845184+zb*(-520688+zb*(157092+(-8168-4676*zb)*zb)))))))+t34*(-6144+zb*(7680+zb*(106752+zb*(-429632+zb*(728448+zb*(-674880+zb*(339800+zb*(-47036+zb*(-42616+17454*zb))))))))+t34*(14336+zb*(-47616+zb*(-82432+zb*(644672+zb*(-1336384+zb*(1445504+zb*(-942624+zb*(389092+zb*(-90536+6334*zb))))))))+t34*(-9216+zb*(64256+zb*(-172544+zb*(186048+zb*(45024+zb*(-334720+zb*(397992+zb*(-263892+(106324-19421*zb)*zb)))))))+t34*(zb*(-2304+zb*(32768+zb*(-146496+zb*(304384+zb*(-337856+zb*(210640+zb*(-66548+zb*(688+4495*zb))))))))+t34*((-512+zb*(18496+zb*(-84352+zb*(161792+zb*(-165096+zb*(96364+zb*(-29616+3212*zb)))))))*pow<2>(zb)+t34*((64+zb*(3584+zb*(-16448+zb*(28736+zb*(-24596+(9760-1220*zb)*zb)))))*pow<3>(zb)+t34*((608+zb*(-1552+zb*(368+zb*(1664+zb*(-1082+19*zb)))))*pow<4>(zb)+t34*(80+zb*(-64+zb*(-160+zb*(128+11*zb))))*pow<5>(zb)))))))))+t12*(-2560+zb*(14080+zb*(11648+zb*(-216512+zb*(588896+zb*(-783504+zb*(592808+zb*(-264072+(66700-7441*zb)*zb)))))))+t34*(10752+zb*(-113408+zb*(473984+zb*(-1058752+zb*(1411712+zb*(-1181056+zb*(635352+zb*(-222852+(51194-7013*zb)*zb)))))))+t34*(7168+zb*(-34816+zb*(54016+zb*(28288+zb*(-228736+zb*(347312+zb*(-229576+zb*(53524+zb*(2584+164*zb))))))))+t34*(zb*(-8704+zb*(-5376+zb*(204288+zb*(-618688+zb*(883104+zb*(-748080+zb*(414188+zb*(-141564+21148*zb))))))))+t34*(zb*(17408+zb*(-106880+zb*(243776+zb*(-231008+zb*(23088+zb*(125864+zb*(-104644+(39440-7328*zb)*zb)))))))+t34*((12928+zb*(-87616+zb*(233920+zb*(-323168+zb*(261824+zb*(-135252+(42880-5488*zb)*zb))))))*pow<2>(zb)+t34*((7936+zb*(-48960+zb*(113808+zb*(-130056+zb*(77516+zb*(-21720+1664*zb))))))*pow<3>(zb)+t34*((2496+zb*(-12576+zb*(22816+zb*(-17212+zb*(3516+728*zb)))))*pow<4>(zb)+t34*((448+zb*(-2176+zb*(2860+(-796-211*zb)*zb)))*pow<5>(zb)+t34*(120+zb*(-280+zb*(134+zb)))*pow<6>(zb)))))))))+t12*(4096+zb*(-29440+zb*(113920+zb*(-314432+zb*(582880+zb*(-658816+zb*(421216+zb*(-138084+(20982-2261*zb)*zb)))))))+t34*(3072+zb*(-50944+zb*(291840+zb*(-833344+zb*(1367232+zb*(-1380320+zb*(884048+zb*(-351796+(74664-4645*zb)*zb)))))))+t34*(5120+zb*(-46080+zb*(142336+zb*(-161920+zb*(-69312+zb*(353152+zb*(-318024+zb*(78764+(27812-11636*zb)*zb)))))))+t34*(zb*(8192+zb*(-66304+zb*(240128+zb*(-461504+zb*(489952+zb*(-300800+zb*(128556+zb*(-47776+9508*zb))))))))+t34*((-4608+zb*(3776+zb*(52640+zb*(-133856+zb*(107304+zb*(-9268+zb*(-22320+6144*zb)))))))*pow<2>(zb)+t34*((-5312+zb*(19456+zb*(-19584+zb*(3440+zb*(-4788+(9856-2752*zb)*zb)))))*pow<3>(zb)+t34*((-3520+zb*(10944+zb*(-8632+zb*(-1740+(4524-1736*zb)*zb))))*pow<4>(zb)+t34*((-928+zb*(2496+zb*(-1548+zb*(-752+672*zb))))*pow<5>(zb)+t34*((-264+zb*(504+(-150-15*zb)*zb))*pow<6>(zb)+t34*(8+(-24+zb)*zb)*pow<7>(zb)))))))))+t12*(1536+zb*(-17152+zb*(72704+zb*(-182784+zb*(306144+zb*(-313904+zb*(153768+zb*(-2288+zb*(-20252+2239*zb))))))))+t34*(1536+zb*(-15616+zb*(85504+zb*(-257920+zb*(432640+zb*(-417120+zb*(245792+zb*(-96084+zb*(19238+1947*zb))))))))+t34*(zb*(-6656+zb*(41216+zb*(-87808+zb*(58880+zb*(38960+zb*(-59944+zb*(-3012+(27312-8696*zb)*zb)))))))+t34*((-9984+zb*(60032+zb*(-133568+zb*(119296+zb*(-10768+zb*(-43892+(22672-4104*zb)*zb))))))*pow<2>(zb)+t34*((-5120+zb*(34784+zb*(-78448+zb*(69928+zb*(-13972+zb*(-12120+4968*zb))))))*pow<3>(zb)+t34*((-3200+zb*(21824+zb*(-48336+zb*(43988+zb*(-15644+1696*zb)))))*pow<4>(zb)+t34*((-1456+zb*(7512+zb*(-11164+(5984-1164*zb)*zb)))*pow<5>(zb)+t34*((-64+zb*(52+(160-76*zb)*zb))*pow<6>(zb)+t34*((36+zb*(-92+61*zb))*pow<7>(zb)+t34*(6-7*zb)*pow<8>(zb)))))))))+t12*(zb*(-1792+zb*(11264+zb*(-29440+zb*(50208+zb*(-54400+zb*(16008+zb*(25612+zb*(-20006+2491*zb))))))))+t12*((256+zb*(1664+zb*(-8192+zb*(10272+zb*(-7976+zb*(9816+zb*(-7036+1141*zb)))))))*pow<2>(zb)+t34*((256+zb*(-384+zb*(-9280+zb*(46416+zb*(-82728+zb*(64212+zb*(-21058+2729*zb)))))))*pow<2>(zb)+t34*((-4960+zb*(20032+zb*(-29832+zb*(21156+zb*(-7620+1120*zb)))))*pow<4>(zb)+t34*((3552+zb*(-21248+zb*(43688+zb*(-37600+(12938-1444*zb)*zb))))*pow<4>(zb)+t34*((-16+zb*(1456+zb*(-4888+(4500-891*zb)*zb)))*pow<5>(zb)+t34*((-1152+zb*(3052+zb*(-2166+217*zb)))*pow<6>(zb)+t34*((-4+zb*(-116+118*zb))*pow<7>(zb)+t34*(14-14*zb)*pow<8>(zb)))))))+t12*((384+zb*(-2624+zb*(3888+zb*(-816+zb*(-556+zb*(-574+281*zb))))))*pow<3>(zb)+t34*((384+zb*(-2560+zb*(10160+zb*(-20464+zb*(18740+zb*(-7040+833*zb))))))*pow<3>(zb)+t34*((-704+zb*(5568+zb*(-12768+zb*(12248+zb*(-5094+708*zb)))))*pow<4>(zb)+t34*((-2240+zb*(7856+zb*(-8360+(2760-40*zb)*zb)))*pow<5>(zb)+t34*((208+zb*(-1164+(1262-255*zb)*zb))*pow<6>(zb)+t34*((148+(-136-39*zb)*zb)*pow<7>(zb)+t34*((-10+18*zb)*pow<8>(zb)-2*t34*pow<9>(zb)))))))+t12*((-160+zb*(208+zb*(872+zb*(-1400+zb*(448+33*zb)))))*pow<4>(zb)+t34*((-160+zb*(816+zb*(-2120+zb*(2424+zb*(-1084+121*zb)))))*pow<4>(zb)+t34*((608+zb*(-2536+zb*(3208+zb*(-1432+154*zb))))*pow<5>(zb)+t34*((456+zb*(-728+zb*(208+66*zb)))*pow<6>(zb)+t34*((-112+(120-11*zb)*zb)*pow<7>(zb)+t34*(12-11*zb)*pow<8>(zb)))))+t12*((16+zb*(144+zb*(-320+zb*(160+zb))))*pow<5>(zb)+t12*((-16+16*zb)*pow<7>(zb)+(16-16*zb)*pow<2>(t34)*pow<7>(zb))+t34*((16+zb*(-64+zb*(96+zb*(-56+5*zb))))*pow<5>(zb)+t34*((-208+zb*(416+zb*(-216+10*zb)))*pow<6>(zb)+t34*((-8+10*zb)*pow<8>(zb)+t34*((-8+5*zb)*pow<8>(zb)+t34*pow<9>(zb)))))))))+t34*(zb*(-1792+zb*(12544+zb*(-37504+zb*(38592+zb*(35136+zb*(-106128+zb*(79020+zb*(-24192+4459*zb))))))))+t34*((2816+zb*(-9728+zb*(3840+zb*(18624+zb*(-21800+zb*(3468+(4288-1496*zb)*zb))))))*pow<2>(zb)+t34*((1408+zb*(3904+zb*(-40896+zb*(86752+zb*(-75764+(29200-4872*zb)*zb)))))*pow<3>(zb)+t34*((2272+zb*(-9632+zb*(15512+zb*(-12668+(4788-96*zb)*zb))))*pow<4>(zb)+t34*((3360+zb*(-14032+zb*(19556+zb*(-10256+1464*zb))))*pow<5>(zb)+t34*((904+zb*(-1884+zb*(832+20*zb)))*pow<6>(zb)+t34*((-92+(256-124*zb)*zb)*pow<7>(zb)+t34*((-14+9*zb)*pow<8>(zb)+t34*pow<9>(zb)))))))))))))))))*pow<-1>(2+t12-t34)*pow<-1>(1+t34)*pow<-1>(2+(-1+t12)*zb)*pow<-2>(-1+t12)*pow<-2>(1+t12)*pow<-2>(-1+t34)*pow<-2>(t12+t34)*pow<-2>(-1+zb)*pow<-2>(zb)*pow<-2>(-2+(3+t12)*zb)*pow<-2>(-2+(2+t12-t34)*zb))/8;
    return todouble(foo);
}

// Expansion order epsilon^0 of c13*bubble(13)+c25*bubble(25) 
template<>
template<>
double qq2yyg1<TT>::SC::bub::c1325<0>(const TT& zb, const TT& t12, const TT& t34)
{
    const TT foo = ((t34*(zb*(-6144+zb*(60416+zb*(-265984+zb*(687360+zb*(-1146368+zb*(1275328+zb*(-942576+zb*(444240+zb*(-120528+14256*zb)))))))))+t34*(4096+zb*(-52736+zb*(209920+zb*(-233472+zb*(-648768+zb*(2641824+zb*(-4216064+zb*(3775488+zb*(-1977436+(564756-67608*zb)*zb))))))))+t34*(-7168+zb*(170752+zb*(-1062400+zb*(3064384+zb*(-4581344+zb*(2966544+zb*(987184+zb*(-3230324+zb*(2393238+zb*(-805448+104580*zb)))))))))+t34*(6656+zb*(-156416+zb*(1112320+zb*(-3888000+zb*(7748000+zb*(-9154512+zb*(6106392+zb*(-1709628+zb*(-350246+(346949-61506*zb)*zb))))))))+t34*(-23552+zb*(185344+zb*(-752768+zb*(2176320+zb*(-4646560+zb*(6898512+zb*(-6705040+zb*(4013456+zb*(-1334364+(190658-2020*zb)*zb))))))))+t34*(25088+zb*(-189696+zb*(608896+zb*(-1166592+zb*(1756448+zb*(-2492032+zb*(2948808+zb*(-2355576+zb*(1097672+zb*(-250969+17958*zb)))))))))+t34*(-7168+zb*(77568+zb*(-328320+zb*(734528+zb*(-1006752+zb*(1005072+zb*(-898712+zb*(701932+zb*(-366018+(94628-6748*zb)*zb))))))))+t34*(zb*(-9216+zb*(73856+zb*(-244864+zb*(437248+zb*(-463712+zb*(313808+zb*(-151532+zb*(56490+zb*(-12397+306*zb)))))))))+t34*((-3840+zb*(25408+zb*(-67104+zb*(89840+zb*(-63208+zb*(20904+zb*(-1352+zb*(-878+236*zb))))))))*pow<2>(zb)+t34*(-512+zb*(2368+zb*(-3984+zb*(2608+zb*(96+zb*(-880+(333-30*zb)*zb))))))*pow<3>(zb))))))))))+flog<TT>(((1+t12)*zb)/2)*(t34*(zb*(-14336+zb*(138752+zb*(-591616+zb*(1458048+zb*(-2287808+zb*(2368576+zb*(-1616512+zb*(700416+zb*(-174528+19008*zb)))))))))+t34*(-12288+zb*(145408+zb*(-784128+zb*(2525568+zb*(-5353664+zb*(7768544+zb*(-7783552+zb*(5298192+zb*(-2336808+(600768-68040*zb)*zb))))))))+t34*(30720+zb*(-356352+zb*(1819136+zb*(-5424384+zb*(10538240+zb*(-14028960+zb*(13030768+zb*(-8367880+zb*(3557164+zb*(-900812+102360*zb)))))))))+t34*(-17408+zb*(267776+zb*(-1615104+zb*(5327232+zb*(-10925056+zb*(14785600+zb*(-13531584+zb*(8357808+zb*(-3368676+(806814-87402*zb)*zb))))))))+t34*(2048+zb*(-56320+zb*(481792+zb*(-2035968+zb*(5028288+zb*(-7826496+zb*(7913680+zb*(-5191528+zb*(2133092+zb*(-499588+51000*zb)))))))))+t34*(-7168+zb*(39424+zb*(-85760+zb*(163968+zb*(-523136+zb*(1322144+zb*(-1962672+zb*(1706608+zb*(-857484+(229046-24970*zb)*zb))))))))+t34*(4096+zb*(-27648+zb*(51712+zb*(52736+zb*(-338944+zb*(497312+zb*(-251280+zb*(-100952+zb*(178196+zb*(-76388+11160*zb)))))))))+t34*(zb*(2048+zb*(-6400+zb*(-20352+zb*(134208+zb*(-276128+zb*(274272+zb*(-122544+zb*(2564+(16170-3838*zb)*zb))))))))+t34*((2816+zb*(-18496+zb*(48384+zb*(-63216+zb*(41288+zb*(-10148+zb*(-1420+792*zb)))))))*pow<3>(zb)+t34*(512+zb*(-2592+zb*(5008+zb*(-4480+zb*(1684+(-62-70*zb)*zb)))))*pow<4>(zb))))))))))+t12*(zb*(-14336+zb*(129536+zb*(-513280+zb*(1170048+zb*(-1689920+zb*(1602496+zb*(-996736+zb*(391680+zb*(-88128+8640*zb)))))))))+t34*(-24576+zb*(241664+zb*(-1018880+zb*(2391808+zb*(-3370240+zb*(2802496+zb*(-1138944+zb*(-71904+zb*(284720+zb*(-109824+13680*zb)))))))))+t34*(55296+zb*(-588800+zb*(2670848+zb*(-6705408+zb*(10037440+zb*(-8764256+zb*(3573744+zb*(544616+zb*(-1257444+(500036-66072*zb)*zb))))))))+t34*(-28672+zb*(358400+zb*(-1888256+zb*(5540352+zb*(-9935232+zb*(11100448+zb*(-7375968+zb*(2375560+zb*(64224+zb*(-257384+46528*zb)))))))))+t34*(35840+zb*(-286720+zb*(979712+zb*(-1956096+zb*(2766272+zb*(-3220928+zb*(3105424+zb*(-2168552+zb*(947088+zb*(-226484+24444*zb)))))))))+t34*(-49152+zb*(420864+zb*(-1461760+zb*(2554368+zb*(-2130432+zb*(310336+zb*(660864+zb*(-134424+zb*(-364200+(237048-43512*zb)*zb))))))))+t34*(7168+zb*(-105472+zb*(559872+zb*(-1448192+zb*(1925760+zb*(-1078560+zb*(-235648+zb*(561000+zb*(-161088+zb*(-44396+19556*zb)))))))))+t34*(4096+zb*(-29696+zb*(52736+zb*(100864+zb*(-507264+zb*(730848+zb*(-410144+zb*(-9736+zb*(84880+(-12664-3920*zb)*zb))))))))+t34*(zb*(4096+zb*(-24832+zb*(38912+zb*(49664+zb*(-231104+zb*(287248+zb*(-152472+zb*(28544+zb*(-1116+1060*zb)))))))))+t34*((1024+zb*(-3328+zb*(-6656+zb*(44160+zb*(-76288+zb*(59512+zb*(-21496+(3560-488*zb)*zb)))))))*pow<2>(zb)+t34*(640+zb*(-3520+zb*(7216+zb*(-6864+zb*(3092+zb*(-648+84*zb))))))*pow<4>(zb))))))))))+t12*(-12288+zb*(96256+zb*(-277760+zb*(254848+zb*(473536+zb*(-1672480+zb*(2236672+zb*(-1669104+zb*(722264+zb*(-168000+16056*zb)))))))))+t34*(18432+zb*(-133120+zb*(368640+zb*(-406016+zb*(-179200+zb*(1101216+zb*(-1513328+zb*(1160296+zb*(-557388+(163052-22584*zb)*zb))))))))+t34*(6144+zb*(-221184+zb*(1684992+zb*(-6022784+zb*(12293696+zb*(-15434592+zb*(12220384+zb*(-6124216+zb*(1973296+zb*(-435732+59996*zb)))))))))+t34*(30720+zb*(-47104+zb*(-1123328+zb*(6727168+zb*(-17980672+zb*(27804704+zb*(-26732128+zb*(16233184+zb*(-6145212+(1392676-160008*zb)*zb))))))))+t34*(-33792+zb*(202752+zb*(-205312+zb*(-1710464+zb*(7708032+zb*(-15736416+zb*(18877840+zb*(-13956648+zb*(6241604+zb*(-1561980+174384*zb)))))))))+t34*(-12288+zb*(156672+zb*(-762368+zb*(1994496+zb*(-3408640+zb*(4560800+zb*(-5172896+zb*(4460656+zb*(-2458524+(726028-83936*zb)*zb))))))))+t34*(3072+zb*(-43008+zb*(246784+zb*(-653952+zb*(734464+zb*(16544+zb*(-764512+zb*(552520+zb*(-18548+zb*(-93236+19872*zb)))))))))+t34*(zb*(-13312+zb*(92672+zb*(-286720+zb*(591488+zb*(-979040+zb*(1207328+zb*(-920800+zb*(349308+(-34100-6824*zb)*zb))))))))+t34*(zb*(2048+zb*(-25344+zb*(112384+zb*(-262784+zb*(393920+zb*(-423600+zb*(315192+zb*(-129660+zb*(13804+4040*zb)))))))))+t34*((1024+zb*(-8960+zb*(30592+zb*(-57088+zb*(69200+zb*(-56536+zb*(25896+(-3048-1080*zb)*zb)))))))*pow<2>(zb)+t34*(-512+zb*(2432+zb*(-4960+zb*(5456+zb*(-3036+zb*(536+84*zb))))))*pow<4>(zb))))))))))+t12*(-6144+zb*(99328+zb*(-541440+zb*(1480704+zb*(-2309056+zb*(2116448+zb*(-1089264+zb*(261368+zb*(-10556+(220-1608*zb)*zb))))))))+t34*(28672+zb*(-356352+zb*(1804288+zb*(-5018368+zb*(8555136+zb*(-9312800+zb*(6449376+zb*(-2673608+zb*(529968+(11272-17584*zb)*zb))))))))+t34*(-40960+zb*(463872+zb*(-2112000+zb*(5197312+zb*(-7707072+zb*(7129568+zb*(-3898016+zb*(758592+zb*(554572+zb*(-448156+102288*zb)))))))))+t34*(49152+zb*(-610304+zb*(2851840+zb*(-6764544+zb*(8771072+zb*(-5695136+zb*(480224+zb*(2052640+zb*(-1684176+(675648-126416*zb)*zb))))))))+t34*(-39936+zb*(486400+zb*(-2396160+zb*(6309888+zb*(-9663360+zb*(8593376+zb*(-4000848+zb*(556912+zb*(226152+zb*(-83452+11028*zb)))))))))+t34*(20480+zb*(-168960+zb*(637440+zb*(-1520128+zb*(2566272+zb*(-3051040+zb*(2448352+zb*(-1365840+zb*(644728+zb*(-269712+58408*zb)))))))))+t34*(-11264+zb*(97280+zb*(-327680+zb*(561664+zb*(-606976+zb*(640480+zb*(-740768+zb*(620832+zb*(-328512+(122164-27220*zb)*zb))))))))+t34*(zb*(-11264+zb*(90624+zb*(-296960+zb*(547072+zb*(-680224+zb*(622048+zb*(-382432+zb*(121952+(-10208-608*zb)*zb))))))))+t34*((-6912+zb*(53248+zb*(-172416+zb*(313152+zb*(-350048+zb*(235864+zb*(-81436+zb*(6928+1620*zb))))))))*pow<2>(zb)+t34*((-2816+zb*(19968+zb*(-57344+zb*(86400+zb*(-71976+zb*(31176+zb*(-5592+184*zb)))))))*pow<3>(zb)+t34*(-640+zb*(3520+zb*(-7456+zb*(7648+zb*(-3868+(888-92*zb)*zb)))))*pow<4>(zb))))))))))+t12*(11264+zb*(-44544+zb*(-129280+zb*(1168896+zb*(-3223360+zb*(4843808+zb*(-4476064+zb*(2647496+zb*(-1002684+(228374-23906*zb)*zb))))))))+t34*(-40960+zb*(314368+zb*(-901632+zb*(980224+zb*(575936+zb*(-2882976+zb*(3574992+zb*(-2286648+zb*(785112+zb*(-118944+528*zb)))))))))+t34*(27648+zb*(-213504+zb*(655104+zb*(-1024896+zb*(845312+zb*(-306272+zb*(25488+zb*(-153376+zb*(310612+zb*(-224730+58614*zb)))))))))+t34*(-26624+zb*(84992+zb*(438784+zb*(-2997760+zb*(7410176+zb*(-10221920+zb*(8914208+zb*(-5195440+zb*(1977720+zb*(-403864+19728*zb)))))))))+t34*(47104+zb*(-323072+zb*(659200+zb*(455040+zb*(-4376448+zb*(8818656+zb*(-9709600+zb*(6772752+zb*(-3099360+(864190-108462*zb)*zb))))))))+t34*(-18432+zb*(186368+zb*(-818176+zb*(1978112+zb*(-2776448+zb*(2147360+zb*(-596000+zb*(-436256+zb*(537864+zb*(-255904+51512*zb)))))))))+t34*(zb*(-4608+zb*(97024+zb*(-613504+zb*(1864320+zb*(-3216480+zb*(3408480+zb*(-2286592+zb*(941168+zb*(-201546+11738*zb)))))))))+t34*((-1024+zb*(53760+zb*(-331520+zb*(888032+zb*(-1302656+zb*(1124784+zb*(-560600+(139960-10736*zb)*zb)))))))*pow<2>(zb)+t34*((128+zb*(10816+zb*(-66784+zb*(162560+zb*(-199384+zb*(122972+zb*(-30908+600*zb)))))))*pow<3>(zb)+t34*((1216+zb*(-3584+zb*(-1360+zb*(13656+zb*(-14208+zb*(3872+408*zb))))))*pow<4>(zb)+t34*(160+zb*(-48+zb*(-992+zb*(1404+(-500-24*zb)*zb))))*pow<5>(zb))))))))))+t12*(-3072+zb*(33792+zb*(-281856+zb*(1313024+zb*(-3405440+zb*(5228192+zb*(-4912688+zb*(2832360+zb*(-979676+(194768-19404*zb)*zb))))))))+t34*(-24576+zb*(322560+zb*(-1578496+zb*(4002816+zb*(-5936640+zb*(5452448+zb*(-3248864+zb*(1355736+zb*(-421336+(80264-3912*zb)*zb))))))))+t34*(3072+zb*(-169984+zb*(1305856+zb*(-4655872+zb*(9670912+zb*(-12677664+zb*(10657680+zb*(-5547872+zb*(1604796+zb*(-191296+372*zb)))))))))+t34*(24576+zb*(-185344+zb*(696832+zb*(-1692160+zb*(2720384+zb*(-2909472+zb*(2347744+zb*(-1823472+zb*(1260384+zb*(-523728+84256*zb)))))))))+t34*(zb*(-35840+zb*(115456+zb*(314112+zb*(-2197376+zb*(4771296+zb*(-5531744+zb*(3877408+zb*(-1726232+(476600-63680*zb)*zb))))))))+t34*(zb*(34816+zb*(-283648+zb*(929792+zb*(-1544448+zb*(1327456+zb*(-528160+zb*(116448+zb*(-123488+(85344-14112*zb)*zb))))))))+t34*((25856+zb*(-227584+zb*(813440+zb*(-1546720+zb*(1742912+zb*(-1239536+zb*(571496+zb*(-156200+16336*zb))))))))*pow<2>(zb)+t34*((15872+zb*(-125824+zb*(386464+zb*(-604512+zb*(517072+zb*(-228992+zb*(36976+2944*zb)))))))*pow<3>(zb)+t34*((4992+zb*(-32896+zb*(83408+zb*(-99752+zb*(51668+(-4168-3252*zb)*zb)))))*pow<4>(zb)+t34*((896+zb*(-6016+zb*(12392+zb*(-9384+zb*(1656+456*zb)))))*pow<5>(zb)+t34*(240+zb*(-784+zb*(764+(-216-4*zb)*zb)))*pow<6>(zb))))))))))+t12*(-11264+zb*(104448+zb*(-466432+zb*(1367680+zb*(-2772480+zb*(3723488+zb*(-3095408+zb*(1430712+zb*(-282204+(1504-44*zb)*zb))))))))+t34*(2048+zb*(37888+zb*(-421376+zb*(1569536+zb*(-3009792+zb*(3471136+zb*(-2759152+zb*(1724728+zb*(-801360+(191696-5352*zb)*zb))))))))+t34*(-1024+zb*(-26624+zb*(415232+zb*(-2070656+zb*(5303680+zb*(-7999712+zb*(7394720+zb*(-4062880+zb*(1124820+(-42288-35268*zb)*zb))))))))+t34*(10240+zb*(-132096+zb*(641536+zb*(-1619456+zb*(2269056+zb*(-1603552+zb*(301472+zb*(114976+zb*(178280+zb*(-211928+51472*zb)))))))))+t34*(zb*(16384+zb*(-159744+zb*(746368+zb*(-1968000+zb*(3008096+zb*(-2620704+zb*(1193408+zb*(-197136+zb*(-30088+11416*zb)))))))))+t34*((-9216+zb*(17152+zb*(134400+zb*(-571424+zb*(873344+zb*(-569136+zb*(67576+(81640-24336*zb)*zb)))))))*pow<2>(zb)+t34*((-10624+zb*(50176+zb*(-54048+zb*(-73056+zb*(191760+zb*(-136800+(36280-3688*zb)*zb))))))*pow<3>(zb)+t34*((-7040+zb*(27872+zb*(-27936+zb*(-15712+zb*(45016+zb*(-29192+6992*zb))))))*pow<4>(zb)+t34*((-1856+zb*(7248+zb*(-9464+zb*(3372+(1800-1100*zb)*zb))))*pow<5>(zb)+t34*((-528+zb*(1592+zb*(-1512+(552-104*zb)*zb)))*pow<6>(zb)+t34*(16+zb*(-52+zb*(24+12*zb)))*pow<7>(zb))))))))))+t12*(-3072+zb*(40960+zb*(-205824+zb*(592384+zb*(-1137152+zb*(1449312+zb*(-1076160+zb*(292904+zb*(127164+zb*(-89976+9460*zb)))))))))+t34*(zb*(-3072+zb*(-25088+zb*(194560+zb*(-396160+zb*(257120+zb*(32864+zb*(20552+zb*(-151384+zb*(67256+3352*zb)))))))))+t34*(3072+zb*(-24576+zb*(129536+zb*(-503808+zb*(1212800+zb*(-1696160+zb*(1321952+zb*(-477856+zb*(-38068+(106304-33196*zb)*zb))))))))+t34*(zb*(-13312+zb*(121344+zb*(-425984+zb*(702720+zb*(-426400+zb*(-273824+zb*(555680+zb*(-280768+zb*(38112+2432*zb)))))))))+t34*((-19968+zb*(153088+zb*(-466560+zb*(666400+zb*(-351872+zb*(-167120+zb*(304440+zb*(-146104+27696*zb))))))))*pow<2>(zb)+t34*((-10240+zb*(90752+zb*(-307040+zb*(522144+zb*(-475984+zb*(217504+(-33712-3424*zb)*zb))))))*pow<3>(zb)+t34*((-6400+zb*(59680+zb*(-194848+zb*(294752+zb*(-218616+(74040-8608*zb)*zb)))))*pow<4>(zb)+t34*((-2912+zb*(19872+zb*(-42976+zb*(38816+zb*(-14688+1888*zb)))))*pow<5>(zb)+t34*((-128+zb*(-24+zb*(1196+zb*(-1584+540*zb))))*pow<6>(zb)+t34*((72+zb*(-296+(376-152*zb)*zb))*pow<7>(zb)+t34*(12+zb*(-24+12*zb))*pow<8>(zb))))))))))+t12*(zb*(3584+zb*(-26624+zb*(78592+zb*(-139584+zb*(172288+zb*(-104320+zb*(-54792+zb*(124940+zb*(-61348+7264*zb)))))))))+t34*((-2560+zb*(22784+zb*(5184+zb*(-276768+zb*(645136+zb*(-609384+zb*(252024+zb*(-43528+7112*zb))))))))*pow<2>(zb)+t34*(zb*(-3584+zb*(23552+zb*(-76288+zb*(117184+zb*(24928+zb*(-360240+zb*(511664+zb*(-321052+(98964-15128*zb)*zb))))))))+t34*((5632+zb*(-27904+zb*(5120+zb*(218752+zb*(-565744+zb*(640120+zb*(-366916+(102820-11880*zb)*zb)))))))*pow<2>(zb)+t34*((2816+zb*(7552+zb*(-115360+zb*(331936+zb*(-418560+zb*(257316+zb*(-76358+10658*zb)))))))*pow<3>(zb)+t34*((4544+zb*(-30560+zb*(90320+zb*(-144632+zb*(123748+zb*(-48756+5336*zb))))))*pow<4>(zb)+t34*((6720+zb*(-38896+zb*(81168+zb*(-75268+(29362-3086*zb)*zb))))*pow<5>(zb)+t34*((1808+zb*(-5400+zb*(4484+(-340-552*zb)*zb)))*pow<6>(zb)+t34*((-184+zb*(752+zb*(-862+294*zb)))*pow<7>(zb)+t34*((-28+(44-16*zb)*zb)*pow<8>(zb)+t34*(2-2*zb)*pow<9>(zb))))))))))+t12*((-512+zb*(-3584+zb*(25728+zb*(-49952+zb*(45904+zb*(-36104+zb*(33740+zb*(-18064+2844*zb))))))))*pow<2>(zb)+t34*((4096+zb*(-2048+zb*(-86880+zb*(273632+zb*(-336184+zb*(188360+zb*(-45256+4280*zb)))))))*pow<3>(zb)+t34*((512+zb*(-512+zb*(-12352+zb*(74304+zb*(-183136+zb*(220280+zb*(-129864+(34236-3468*zb)*zb)))))))*pow<2>(zb)+t34*((-18432+zb*(116608+zb*(-286464+zb*(347016+zb*(-215552+(63448-6624*zb)*zb)))))*pow<4>(zb)+t34*((7104+zb*(-54048+zb*(146704+zb*(-179576+zb*(99688+zb*(-20548+676*zb))))))*pow<4>(zb)+t34*((-32+zb*(5664+zb*(-24136+zb*(34632+zb*(-18776+2648*zb)))))*pow<5>(zb)+t34*((-2304+zb*(8712+zb*(-10760+(4436-84*zb)*zb)))*pow<6>(zb)+t34*((-8+zb*(-272+(584-304*zb)*zb))*pow<7>(zb)+t34*(28+zb*(-60+32*zb))*pow<8>(zb))))))))+t12*((-768+zb*(6336+zb*(-13760+zb*(8080+zb*(4024+zb*(-3660+zb*(-880+628*zb)))))))*pow<3>(zb)+t12*((320+zb*(-768+zb*(-1584+zb*(5472+zb*(-4656+zb*(1148+68*zb))))))*pow<4>(zb)+t34*((-1216+zb*(7616+zb*(-14880+zb*(11976+zb*(-3680+184*zb)))))*pow<5>(zb)+t34*((-320+zb*(768+zb*(672+zb*(-3328+zb*(3224+zb*(-1092+76*zb))))))*pow<4>(zb)+t34*((1216+zb*(-7616+zb*(15104+zb*(-12400+(3872-176*zb)*zb))))*pow<5>(zb)+t34*((912+zb*(-2144+zb*(1408+(-12-164*zb)*zb)))*pow<6>(zb)+t34*((-224+zb*(424+(-192-8*zb)*zb))*pow<7>(zb)+t34*(24+zb*(-44+20*zb))*pow<8>(zb))))))+t12*((-32+zb*(-256+zb*(960+zb*(-1024+zb*(350+2*zb)))))*pow<5>(zb)+t12*((32+zb*(-64+32*zb))*pow<7>(zb)+t34*((-32+(64-32*zb)*zb)*pow<7>(zb)+t34*((-32+(64-32*zb)*zb)*pow<7>(zb)+t34*(32+zb*(-64+32*zb))*pow<7>(zb))))+t34*((416+zb*(-1280+zb*(1328+zb*(-472+8*zb))))*pow<6>(zb)+t34*((32+zb*(256+zb*(-960+zb*(1024+zb*(-362+10*zb)))))*pow<5>(zb)+t34*((-416+zb*(1280+zb*(-1312+448*zb)))*pow<6>(zb)+t34*((10-10*zb)*pow<9>(zb)+t34*((-16+(24-8*zb)*zb)*pow<8>(zb)+t34*(2-2*zb)*pow<9>(zb))))))))+t34*((-128+zb*(-13632+zb*(59040+zb*(-91520+zb*(62236+zb*(-17276+1280*zb))))))*pow<4>(zb)+t34*((768+zb*(-4800+zb*(13632+zb*(-24160+zb*(25976+zb*(-14612+(3380-184*zb)*zb))))))*pow<3>(zb)+t34*((-1408+zb*(18240+zb*(-64064+zb*(96320+zb*(-68076+(20660-1672*zb)*zb)))))*pow<4>(zb)+t34*((-4480+zb*(20688+zb*(-31832+zb*(18796+(-2588-584*zb)*zb))))*pow<5>(zb)+t34*((416+zb*(-3264+zb*(5860+zb*(-3444+432*zb))))*pow<6>(zb)+t34*((296+zb*(-524+zb*(92+136*zb)))*pow<7>(zb)+t34*((-20+(60-40*zb)*zb)*pow<8>(zb)+t34*(-4+4*zb)*pow<9>(zb)))))))))))))))))))+t12*(zb*(-6144+zb*(72704+zb*(-369920+zb*(1067264+zb*(-1929728+zb*(2271296+zb*(-1741328+zb*(838320+zb*(-229680+27216*zb)))))))))+t34*(8192+zb*(-128000+zb*(646144+zb*(-1475584+zb*(1326976+zb*(886208+zb*(-3518144+zb*(3902080+zb*(-2229464+(662376-80784*zb)*zb))))))))+t34*(-9216+zb*(268544+zb*(-1843200+zb*(6095040+zb*(-11721696+zb*(14057904+zb*(-10663536+zb*(4945492+zb*(-1247666+zb*(109260+9072*zb)))))))))+t34*(16384+zb*(-150272+zb*(692224+zb*(-2309952+zb*(5908992+zb*(-10805168+zb*(13260032+zb*(-10424940+zb*(4929904+zb*(-1232706+115500*zb)))))))))+t34*(-94720+zb*(653824+zb*(-1657728+zb*(1482304+zb*(870912+zb*(-2193552+zb*(-329592+zb*(3228632+zb*(-2824258+(971594-107381*zb)*zb))))))))+t34*(81920+zb*(-729088+zb*(2709504+zb*(-5262272+zb*(5317888+zb*(-1954640+zb*(-922752+zb*(767928+zb*(213564+zb*(-271468+49346*zb)))))))))+t34*(-12800+zb*(221952+zb*(-1358592+zb*(4215616+zb*(-7462720+zb*(7756464+zb*(-4583936+zb*(1420388+zb*(-279578+(117712-34471*zb)*zb))))))))+t34*(-2048+zb*(-9984+zb*(227328+zb*(-1169600+zb*(2974272+zb*(-4288720+zb*(3607696+zb*(-1738140+zb*(484856+zb*(-109354+23736*zb)))))))))+t34*(zb*(-1024+zb*(-6016+zb*(101056+zb*(-424672+zb*(865232+zb*(-963640+zb*(594272+zb*(-198414+(40426-7283*zb)*zb))))))))+t34*((512+zb*(-5952+zb*(32448+zb*(-88272+zb*(125584+zb*(-93680+zb*(34164+zb*(-5360+586*zb))))))))*pow<2>(zb)+t34*(256+zb*(-1920+zb*(6016+zb*(-9536+zb*(7776+zb*(-2900+zb*(256+47*zb)))))))*pow<3>(zb))))))))))+t12*(4096+zb*(-75264+zb*(493568+zb*(-1756672+zb*(3959744+zb*(-6046944+zb*(6388160+zb*(-4603168+zb*(2149252+zb*(-580812+68040*zb)))))))))+t34*(3072+zb*(8448+zb*(-243200+zb*(1559488+zb*(-5464352+zb*(11708848+zb*(-15830064+zb*(13479060+zb*(-6962150+(1971648-230796*zb)*zb))))))))+t34*(13312+zb*(141568+zb*(-1914368+zb*(7985984+zb*(-16898944+zb*(19886096+zb*(-12132816+zb*(1847540+zb*(2080024+zb*(-1202240+193824*zb)))))))))+t34*(-123904+zb*(793344+zb*(-1478912+zb*(-1678464+zb*(12318880+zb*(-24495344+zb*(26051968+zb*(-15839160+zb*(5045710+(-539032-55044*zb)*zb))))))))+t34*(60928+zb*(-498176+zb*(1868160+zb*(-3939264+zb*(4232096+zb*(-142752+zb*(-5541072+zb*(6735984+zb*(-3406656+zb*(594536+36228*zb)))))))))+t34*(31744+zb*(-222464+zb*(472832+zb*(363904+zb*(-3465248+zb*(6671248+zb*(-6038152+zb*(2461068+zb*(-188734+(-62768-23552*zb)*zb))))))))+t34*(-23040+zb*(190208+zb*(-697088+zb*(1350720+zb*(-1119776+zb*(-607984+zb*(2226192+zb*(-1933612+zb*(668568+(-42424-11656*zb)*zb))))))))+t34*(5120+zb*(-52992+zb*(239872+zb*(-626816+zb*(978208+zb*(-763312+zb*(-34640+zb*(550160+zb*(-365862+zb*(61448+8844*zb)))))))))+t34*(zb*(6144+zb*(-44672+zb*(139840+zb*(-238976+zb*(201216+zb*(23280+zb*(-217976+zb*(182412+zb*(-52908+1548*zb)))))))))+t34*((2304+zb*(-13120+zb*(27520+zb*(-18912+zb*(-23608+zb*(58632+zb*(-47108+(15552-1212*zb)*zb)))))))*pow<2>(zb)+t34*(256+zb*(-576+zb*(-992+zb*(4896+zb*(-6944+zb*(4608+zb*(-1336+80*zb)))))))*pow<3>(zb))))))))))+t12*(5120+zb*(-89344+zb*(615424+zb*(-2268352+zb*(5096672+zb*(-7451632+zb*(7287408+zb*(-4743252+zb*(1960930+zb*(-455668+42696*zb)))))))))+t34*(4096+zb*(141056+zb*(-1435648+zb*(5904448+zb*(-14010496+zb*(21542512+zb*(-22258880+zb*(15210684+zb*(-6404120+(1405722-99372*zb)*zb))))))))+t34*(-59392+zb*(341248+zb*(-885504+zb*(1664384+zb*(-2502176+zb*(1842224+zb*(1305072+zb*(-3773752+zb*(2832618+zb*(-806076+41298*zb)))))))))+t34*(-36864+zb*(466176+zb*(-1915904+zb*(3165568+zb*(-423424+zb*(-5956208+zb*(8614848+zb*(-4731384+zb*(464024+(469154-115832*zb)*zb))))))))+t34*(111104+zb*(-982272+zb*(3840384+zb*(-8632704+zb*(11963840+zb*(-10096592+zb*(4918072+zb*(-1694916+zb*(1195974+zb*(-844156+221126*zb)))))))))+t34*(-67584+zb*(613632+zb*(-2624512+zb*(6995072+zb*(-12614848+zb*(15431472+zb*(-12378352+zb*(6267868+zb*(-2081076+(587942-129656*zb)*zb))))))))+t34*(14848+zb*(-197376+zb*(1102848+zb*(-3506816+zb*(7096064+zb*(-9443728+zb*(8106032+zb*(-4191920+zb*(1141034+zb*(-136028+15210*zb)))))))))+t34*(zb*(22272+zb*(-215040+zb*(908160+zb*(-2171776+zb*(3193328+zb*(-2918000+zb*(1569904+zb*(-415776+zb*(20758+6056*zb)))))))))+t34*((6528+zb*(-61376+zb*(230016+zb*(-448032+zb*(488376+zb*(-289344+zb*(76104+(80-2326*zb)*zb)))))))*pow<2>(zb)+t34*((-192+zb*(-3264+zb*(15648+zb*(-23392+zb*(10352+zb*(5332+zb*(-5688+1204*zb)))))))*pow<3>(zb)+t34*(128+zb*(-640+zb*(704+zb*(704+zb*(-1796+(1144-244*zb)*zb)))))*pow<4>(zb))))))))))+t12*(512+zb*(5632+zb*(25856+zb*(-423104+zb*(1547616+zb*(-2718592+zb*(2570744+zb*(-1216008+zb*(129766+(111795-34206*zb)*zb))))))))+t34*(-8192+zb*(44288+zb*(-338560+zb*(1837376+zb*(-5234944+zb*(8274912+zb*(-7242992+zb*(2810776+zb*(471726+zb*(-838338+223920*zb)))))))))+t34*(-53760+zb*(449024+zb*(-1352320+zb*(1642624+zb*(200416+zb*(-3048144+zb*(3843752+zb*(-1841712+zb*(-441444+(887673-286134*zb)*zb))))))))+t34*(120832+zb*(-915968+zb*(2858880+zb*(-4725312+zb*(4258656+zb*(-1232304+zb*(-2231320+zb*(3655208+zb*(-2399296+zb*(609044+1752*zb)))))))))+t34*(-65536+zb*(550400+zb*(-2175744+zb*(5241984+zb*(-8287072+zb*(8562480+zb*(-5173984+zb*(880136+zb*(1035070+zb*(-690153+122158*zb)))))))))+t34*(6144+zb*(-85760+zb*(588160+zb*(-2236480+zb*(5098528+zb*(-7425200+zb*(7182848+zb*(-4590272+zb*(1766298+(-297822-6284*zb)*zb))))))))+t34*(zb*(-4608+zb*(47232+zb*(-112000+zb*(-117664+zb*(1043632+zb*(-2239936+zb*(2487360+zb*(-1495312+(423005-31646*zb)*zb))))))))+t34*((-26240+zb*(189248+zb*(-534880+zb*(695344+zb*(-272064+zb*(-298552+zb*(355500+zb*(-109000+424*zb))))))))*pow<2>(zb)+t34*((-12480+zb*(75904+zb*(-170848+zb*(163056+zb*(-40304+zb*(-24224+zb*(3582+5512*zb)))))))*pow<3>(zb)+t34*((-2912+zb*(14064+zb*(-23448+zb*(16248+zb*(-5556+(3060-1540*zb)*zb)))))*pow<4>(zb)+t34*(-400+zb*(1456+zb*(-2144+zb*(1776+zb*(-894+220*zb)))))*pow<5>(zb))))))))))+t12*(-1536+zb*(27904+zb*(-199040+zb*(688704+zb*(-1405152+zb*(2022048+zb*(-2343928+zb*(2133040+zb*(-1304840+(442554-59733*zb)*zb))))))))+t34*(-12288+zb*(18176+zb*(414720+zb*(-2318784+zb*(6032384+zb*(-9975616+zb*(11743680+zb*(-9975968+zb*(5718996+zb*(-1927822+282438*zb)))))))))+t34*(58880+zb*(-404480+zb*(1243136+zb*(-2404672+zb*(3301248+zb*(-2936976+zb*(875360+zb*(1362392+zb*(-1893240+(1013916-215431*zb)*zb))))))))+t34*(-2048+zb*(-105984+zb*(608256+zb*(-984384+zb*(-706880+zb*(4872240+zb*(-8071888+zb*(7125464+zb*(-3492832+(775968-17996*zb)*zb))))))))+t34*(-14336+zb*(228096+zb*(-1380736+zb*(4456384+zb*(-8507072+zb*(9773936+zb*(-6343728+zb*(1727744+zb*(229820+(-152754-17445*zb)*zb))))))))+t34*(zb*(-36096+zb*(489472+zb*(-2591808+zb*(7235328+zb*(-11889232+zb*(12019120+zb*(-7540672+zb*(2898688+zb*(-666502+81982*zb)))))))))+t34*((-43264+zb*(434496+zb*(-1787456+zb*(3932432+zb*(-5060688+zb*(3875960+zb*(-1684972+(351824-18563*zb)*zb)))))))*pow<2>(zb)+t34*((-10944+zb*(110912+zb*(-422512+zb*(801840+zb*(-803496+zb*(388496+(-49844-14480*zb)*zb))))))*pow<3>(zb)+t34*((-672+zb*(15696+zb*(-65736+zb*(106336+zb*(-70772+zb*(10972+4330*zb))))))*pow<4>(zb)+t34*((-80+zb*(3280+zb*(-9744+zb*(10172+zb*(-3832+120*zb)))))*pow<5>(zb)+t34*(-128+zb*(480+zb*(-668+(400-70*zb)*zb)))*pow<6>(zb))))))))))+t12*(512+zb*(-5888+zb*(-31104+zb*(373824+zb*(-1325216+zb*(2545904+zb*(-3091744+zb*(2474040+zb*(-1241212+(329088-28196*zb)*zb))))))))+t34*(14336+zb*(-121856+zb*(701440+zb*(-2858112+zb*(7511904+zb*(-12747376+zb*(14257144+zb*(-10453984+zb*(4728080+zb*(-1108880+77244*zb)))))))))+t34*(26112+zb*(-348160+zb*(1734912+zb*(-4625216+zb*(7427232+zb*(-7295408+zb*(4023568+zb*(-889160+zb*(-12176+zb*(-119320+77824*zb)))))))))+t34*(-12288+zb*(156672+zb*(-940032+zb*(3490048+zb*(-8421472+zb*(13222160+zb*(-13390384+zb*(8583584+zb*(-3358692+(748992-78952*zb)*zb))))))))+t34*(zb*(3840+zb*(3200+zb*(-203584+zb*(1092448+zb*(-2850032+zb*(4170256+zb*(-3403776+zb*(1346120+(-101160-57072*zb)*zb))))))))+t34*((36864+zb*(-359040+zb*(1318432+zb*(-2339728+zb*(2061872+zb*(-760616+zb*(36492+zb*(-32576+38472*zb))))))))*pow<2>(zb)+t34*((46144+zb*(-320864+zb*(828016+zb*(-956880+zb*(392568+zb*(128688+zb*(-145464+27392*zb)))))))*pow<3>(zb)+t34*((14560+zb*(-72848+zb*(123088+zb*(-60784+zb*(-45964+(59072-16872*zb)*zb)))))*pow<4>(zb)+t34*((2496+zb*(-12464+zb*(22136+zb*(-16748+zb*(4104+420*zb)))))*pow<5>(zb)+t34*((696+zb*(-2600+zb*(3796+zb*(-2352+460*zb))))*pow<6>(zb)+t34*(32+zb*(-128+(112-16*zb)*zb))*pow<7>(zb))))))))))+t12*(2560+zb*(-17920+zb*(37248+zb*(85888+zb*(-596992+zb*(1324848+zb*(-1595608+zb*(1157032+zb*(-502756+(111080-5408*zb)*zb))))))))+t34*(10240+zb*(-123904+zb*(656384+zb*(-2178176+zb*(4906688+zb*(-7400720+zb*(7203760+zb*(-4233448+zb*(1229876+(-2244-68372*zb)*zb))))))))+t34*(-512+zb*(-41472+zb*(413440+zb*(-1615616+zb*(3384256+zb*(-4201552+zb*(3248688+zb*(-1786784+zb*(1007808+zb*(-567328+159072*zb)))))))))+t34*(zb*(23552+zb*(-277504+zb*(1440512+zb*(-4102784+zb*(6824176+zb*(-6622160+zb*(3513792+zb*(-851280+(69908-18480*zb)*zb))))))))+t34*((34688+zb*(-345728+zb*(1391808+zb*(-2963248+zb*(3546192+zb*(-2239256+zb*(497084+(146696-67884*zb)*zb)))))))*pow<2>(zb)+t34*((896+zb*(-35648+zb*(274000+zb*(-811024+zb*(1125672+zb*(-737600+zb*(180060+3560*zb)))))))*pow<3>(zb)+t34*((-6016+zb*(-20528+zb*(215952+zb*(-495216+zb*(480204+zb*(-199736+25156*zb))))))*pow<4>(zb)+t34*((2384+zb*(-25168+zb*(68464+zb*(-75168+(33724-4016*zb)*zb))))*pow<5>(zb)+t34*((72+zb*(640+zb*(-3160+(3824-1508*zb)*zb)))*pow<6>(zb)+t34*((-112+zb*(508+zb*(-488+140*zb)))*pow<7>(zb)+t34*(-12+zb*(-8+12*zb))*pow<8>(zb))))))))))+t12*(1024+zb*(-9472+zb*(29824+zb*(-13056+zb*(-149536+zb*(395856+zb*(-428432+zb*(211816+zb*(-42328+(8542-4280*zb)*zb))))))))+t34*(1024+zb*(-31232+zb*(225408+zb*(-810624+zb*(1754176+zb*(-2303776+zb*(1573928+zb*(-116136+zb*(-590632+(370060-72028*zb)*zb))))))))+t34*(zb*(2816+zb*(7040+zb*(-131072+zb*(439456+zb*(-705248+zb*(705840+zb*(-605328+zb*(508492+zb*(-299122+76900*zb)))))))))+t34*((-12416+zb*(134016+zb*(-516480+zb*(862048+zb*(-432368+zb*(-507988+zb*(763862+zb*(-324496+33876*zb))))))))*pow<2>(zb)+t34*((-28672+zb*(182464+zb*(-416288+zb*(341960+zb*(133812+zb*(-415762+(249411-46750*zb)*zb))))))*pow<3>(zb)+t34*((-8064+zb*(63840+zb*(-206576+zb*(324240+zb*(-247760+(78262-4180*zb)*zb)))))*pow<4>(zb)+t34*((-9136+zb*(65208+zb*(-156072+zb*(161736+zb*(-69903+8314*zb)))))*pow<5>(zb)+t34*((-3112+zb*(10412+zb*(-9866+zb*(644+1908*zb))))*pow<6>(zb)+t34*((348+zb*(-1578+(2055-874*zb)*zb))*pow<7>(zb)+t34*((44+zb*(-22+8*zb))*pow<8>(zb)+t34*(-7+2*zb)*pow<9>(zb))))))))))+t12*(zb*(-1536+zb*(8704+zb*(-14848+zb*(-10784+zb*(59680+zb*(-38584+zb*(-47232+zb*(61384+(-12436-4362*zb)*zb))))))))+t34*(zb*(-1536+zb*(24064+zb*(-108288+zb*(224704+zb*(-174080+zb*(-220864+zb*(659904+zb*(-624460+(254532-33904*zb)*zb))))))))+t34*((-1024+zb*(-4352+zb*(24928+zb*(-4192+zb*(-80160+zb*(84468+zb*(11130+zb*(-45324+14366*zb))))))))*pow<2>(zb)+t34*((1024+zb*(4032+zb*(-110912+zb*(441616+zb*(-742988+zb*(602672+zb*(-219378+24116*zb)))))))*pow<3>(zb)+t34*((1088+zb*(29216+zb*(-167064+zb*(335368+zb*(-308954+(127270-16975*zb)*zb)))))*pow<4>(zb)+t34*((32+zb*(-6192+zb*(26088+zb*(-38492+(23016-4602*zb)*zb))))*pow<5>(zb)+t34*((4240+zb*(-17852+zb*(25286+zb*(-13344+1883*zb))))*pow<6>(zb)+t34*((100+zb*(568+zb*(-1762+984*zb)))*pow<7>(zb)+t34*((-110+(250-127*zb)*zb)*pow<8>(zb)+t34*(-(t34*pow<10>(zb))+(8-2*zb)*pow<9>(zb))))))))))+t12*((512+zb*(-1408+zb*(-1184+zb*(4976+zb*(12576+zb*(-46392+zb*(44312+(-11724-1652*zb)*zb)))))))*pow<2>(zb)+t34*((512+zb*(-768+zb*(-19392+zb*(97952+zb*(-232136+zb*(322764+zb*(-253370+(94720-10336*zb)*zb)))))))*pow<2>(zb)+t34*((-1408+zb*(7328+zb*(7680+zb*(-77984+zb*(128884+zb*(-84104+(21008-1376*zb)*zb))))))*pow<3>(zb)+t34*((1472+zb*(-27104+zb*(118576+zb*(-214152+zb*(180086+zb*(-64968+6188*zb))))))*pow<4>(zb)+t34*((5968+zb*(-38208+zb*(84400+zb*(-81992+(33392-3700*zb)*zb))))*pow<5>(zb)+t34*((216+zb*(2108+zb*(-7310+(7344-2344*zb)*zb)))*pow<6>(zb)+t34*((-876+zb*(2360+zb*(-1792+392*zb)))*pow<7>(zb)+t34*((82+zb*(-296+156*zb))*pow<8>(zb)+12*t34*pow<9>(zb))))))))+t12*((256+zb*(-1504+zb*(2528+zb*(3112+zb*(-15284+zb*(17042+(-5972-160*zb)*zb))))))*pow<3>(zb)+t12*((-192+zb*(688+zb*(-296+zb*(-2112+zb*(3410+zb*(-1529+38*zb))))))*pow<4>(zb)+t34*((-192+zb*(1888+zb*(-5624+zb*(8832+zb*(-7750+(3030-212*zb)*zb)))))*pow<4>(zb)+t34*((176+zb*(-2680+zb*(7328+zb*(-7564+(3117-342*zb)*zb))))*pow<5>(zb)+t34*((600+zb*(-2240+zb*(2676+(-924-112*zb)*zb)))*pow<6>(zb)+t34*((352+zb*(-966+(689-110*zb)*zb))*pow<7>(zb)+t34*((-46+(166-92*zb)*zb)*pow<8>(zb)+t34*(-5-2*zb)*pow<9>(zb))))))+t12*((32+zb*(-80+zb*(-64+zb*(256+zb*(-150+7*zb)))))*pow<5>(zb)+t34*((32+zb*(-160+zb*(384+zb*(-448+(194-6*zb)*zb))))*pow<5>(zb)+t34*((-80+zb*(384+zb*(-544+(276-31*zb)*zb)))*pow<6>(zb)+t34*((-64+zb*(128+(-44-20*zb)*zb))*pow<7>(zb)+t34*((-32+(34-7*zb)*zb)*pow<8>(zb)+t34*(-(t34*pow<10>(zb))+(10-6*zb)*pow<9>(zb))))))))+t34*((256+zb*(-5504+zb*(25600+zb*(-56976+zb*(75532+zb*(-58892+(21962-2048*zb)*zb))))))*pow<3>(zb)+t34*((608+zb*(2144+zb*(-22176+zb*(46136+zb*(-38618+(13420-1434*zb)*zb)))))*pow<4>(zb)+t34*((-1472+zb*(12496+zb*(-29864+zb*(28792+zb*(-10358+424*zb)))))*pow<5>(zb)+t34*((-2376+zb*(8956+zb*(-11778+(5780-682*zb)*zb)))*pow<6>(zb)+t34*((284+zb*(-1132+(1534-624*zb)*zb))*pow<7>(zb)+t34*((74+zb*(-108+34*zb))*pow<8>(zb)+t34*(2*t34*pow<10>(zb)+(-18+8*zb)*pow<9>(zb))))))))))))))))))))*pow<-1>(2+t12-t34)*pow<-1>(1+t34)*pow<-1>(2+(-1+t12)*zb)*pow<-2>(-1+t12)*pow<-2>(t12+t34)*pow<-2>(zb)*pow<-2>(-2+(3+t12)*zb)*pow<-2>(-2+(2+t12-t34)*zb)*pow<-3>(1+t12)*pow<-3>(-1+t34)*pow<-3>(-1+zb))/16;
    return todouble(foo);
}

// Series in epsilon of c13*bubble(13)+c25*bubble(25) 
template<>
EpsExp qq2yyg1<TT>::SC::bub::c1325(const TT& zb, const TT& t12, const TT& t34)
{
    return times(
                 CounterForge::cGamma(),
                 EpsExp(-1,{
                    qq2yyg1<TT>::SC::bub::c1325<-1>(zb,t12,t34),
                    qq2yyg1<TT>::SC::bub::c1325<0>(zb,t12,t34)
                 }),
                 2
                 );
}

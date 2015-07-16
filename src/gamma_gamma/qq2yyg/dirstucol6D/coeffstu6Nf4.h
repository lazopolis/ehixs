/*This file was produced by Simone Lionetti using a Mathematica script*/

#include "coeffstu6Nf.h"

// Master n. 4: bubble(s34)

// Coefficient order epsilon^0 of master 4
template<>
double qq2yygstu6Nf<4,0>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (-131072+pow<2>(u)*(163840+pow<2>(u)*(-57344+6144*pow<2>(u)))+pow<2>(t34)*(131072+pow<2>(u)*(229376+pow<2>(u)*(-163840+24576*pow<2>(u)))+pow<2>(t34)*(131072+pow<2>(t34)*(-131072+32768*pow<2>(u))+pow<2>(u)*(-425984+pow<2>(u)*(90112+2048*pow<2>(u)))))+t12*(t34*(u*(1474560+pow<2>(u)*(-843776+118784*pow<2>(u)))+pow<2>(t34)*(u*(-819200+pow<2>(u)*(-352256+139264*pow<2>(u)))+pow<2>(t34)*(u*pow<2>(t34)*(32768-8192*pow<2>(u))+u*(-688128+pow<2>(u)*(155648+4096*pow<2>(u))))))+t12*(786432+pow<2>(u)*(-507904+77824*pow<2>(u))+pow<2>(t34)*(655360+pow<2>(u)*(-2408448+561152*pow<2>(u))+pow<2>(t34)*(-1572864+pow<2>(u)*(-212992+151552*pow<2>(u))+pow<2>(t34)*(131072+(-16384-4096*pow<2>(u))*pow<2>(u))))+t12*(t12*(-655360+163840*pow<2>(u)+pow<2>(t34)*(-1572864+pow<2>(t34)*(131072-32768*pow<2>(u))+393216*pow<2>(u)))+t34*(u*(-2228224+557056*pow<2>(u))+pow<2>(t34)*(u*pow<2>(t34)*(131072-32768*pow<2>(u))+u*(-2097152+524288*pow<2>(u)))))))+zb*(229376+pow<2>(u)*(-237568+45056*pow<2>(u))+pow<2>(t34)*(-196608+pow<2>(u)*(-278528+102400*pow<2>(u))+pow<2>(t34)*(-262144+(753664-143360*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(196608+(-245760-4096*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(32768+8192*pow<2>(u)))))+t12*(t12*(-2064384+t12*(t34*(pow<2>(t34)*(-524288*u*pow<2>(t34)+u*(1048576-1703936*pow<2>(u)))+u*(5767168-1441792*pow<2>(u)))+t12*(1835008+t12*t34*(-917504*u-131072*u*pow<2>(t34))+pow<2>(t34)*(2883584+pow<2>(t34)*(-524288-98304*pow<2>(u))-2490368*pow<2>(u))-557056*pow<2>(u)))+(1122304-126976*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-425984+(4104192-909312*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(2850816+pow<2>(t34)*(-360448-8192*pow<2>(u))+(-2072576-274432*pow<2>(u))*pow<2>(u))))+t34*(u*(-3080192+(1236992-94208*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(u*(3014656+(-172032-98304*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(u*(65536+(-548864-4096*pow<2>(u))*pow<2>(u))+8192*pow<2>(t34)*pow<3>(u)))))+zb*(-49152+69632*pow<2>(u)+pow<2>(t34)*(-98304+90112*pow<2>(u)+pow<2>(t34)*(327680-393216*pow<2>(u)+pow<2>(t34)*(-163840+pow<2>(t34)*(-16384-4096*pow<2>(u))+237568*pow<2>(u))))+t12*(t34*(u*(2170880-491520*pow<2>(u))+pow<2>(t34)*(u*(-3244032+196608*pow<2>(u))+pow<2>(t34)*(pow<2>(t34)*(32768*u+8192*u*pow<2>(t34))+u*(1032192+294912*pow<2>(u)))))+t12*(1998848+pow<2>(u)*(-798720+36864*pow<2>(u))+pow<2>(t34)*(-884736+pow<2>(u)*(-2703360+317440*pow<2>(u))+pow<2>(t34)*(-1605632+pow<2>(u)*(2596864+135168*pow<2>(u))+pow<2>(t34)*(491520+4096*pow<2>(t34)*pow<2>(u)+pow<2>(u)*(114688+2048*pow<2>(u)))))+t12*(t12*(-2113536+t12*(t12*(163840+(393216-32768*pow<2>(t34))*pow<2>(t34))+t34*(2260992*u+pow<2>(t34)*(917504*u-32768*u*pow<2>(t34))))+581632*pow<2>(u)+pow<2>(t34)*(-1785856+3547136*pow<2>(u)+pow<2>(t34)*(737280+pow<2>(t34)*(16384-8192*pow<2>(u))+598016*pow<2>(u))))+t34*(u*(-5775360+991232*pow<2>(u))+pow<2>(t34)*(u*(1794048+1523712*pow<2>(u))+pow<2>(t34)*(8192*u*pow<2>(t34)+u*(827392+106496*pow<2>(u))))))))+zb*(-106496+16384*pow<2>(u)+pow<2>(t34)*(352256-49152*pow<2>(u)+pow<2>(t34)*(-409600+pow<2>(t34)*(180224+(-8192-8192*pow<2>(t34))*pow<2>(t34)-16384*pow<2>(u))+49152*pow<2>(u)))+t12*(t34*(-516096*u+pow<2>(t34)*(966656*u+pow<2>(t34)*(-393216*u+pow<2>(t34)*(-49152*u-8192*u*pow<2>(t34)))))+t12*(-835584+t12*(t34*(pow<2>(t34)*(pow<2>(t34)*(8192*u*pow<2>(t34)+u*(-1073152-73728*pow<2>(u)))+u*(-958464-425984*pow<2>(u)))+u*(2547712-155648*pow<2>(u)))+t12*(1236992+t12*(t34*(-1703936*u-1441792*u*pow<2>(t34))+t12*(-294912+pow<2>(t34)*(-786432+32768*pow<2>(t34))))+pow<2>(t34)*(811008+pow<2>(t34)*(-942080-57344*pow<2>(t34)-745472*pow<2>(u))-1687552*pow<2>(u))-188416*pow<2>(u)))+172032*pow<2>(u)+pow<2>(t34)*(704512+pow<2>(t34)*(606208+pow<2>(t34)*(-507904+32768*pow<2>(t34)-122880*pow<2>(u))-761856*pow<2>(u))+712704*pow<2>(u))))+zb*(69632-6144*pow<2>(u)+pow<2>(t34)*(-241664+20480*pow<2>(u)+pow<2>(t34)*(303104-24576*pow<2>(u)+pow<2>(t34)*(-155648+pow<2>(t34)*(20480+4096*pow<2>(t34)-2048*pow<2>(u))+12288*pow<2>(u))))+t12*(t34*(-36864*u+pow<2>(t34)*(114688*u+pow<2>(t34)*(-122880*u+pow<2>(t34)*(49152*u-4096*u*pow<2>(t34)))))+t12*(102400+pow<2>(t34)*(8192+pow<2>(t34)*(-344064+(253952-20480*pow<2>(t34))*pow<2>(t34)))+t12*(t34*(-360448*u+pow<2>(t34)*(-65536*u+425984*u*pow<2>(t34)))+t12*(-315392+t12*(t34*(397312*u+pow<2>(t34)*(827392*u+86016*u*pow<2>(t34)))+t12*(143360+pow<2>(t34)*(561152+(86016-4096*pow<2>(t34))*pow<2>(t34))))+6144*pow<2>(u)+pow<2>(t34)*(-507904+225280*pow<2>(u)+pow<2>(t34)*(638976+247808*pow<2>(u)+pow<2>(t34)*(49152+4096*pow<2>(t34)+12288*pow<2>(u))))))))+zb*(-12288+pow<2>(t34)*(53248+pow<2>(t34)*(-90112+pow<2>(t34)*(73728+pow<2>(t34)*(-28672+4096*pow<2>(t34)))))+zb*pow<2>(t12)*(pow<2>(t34)*(-6144+pow<2>(t34)*(20480+pow<2>(t34)*(-24576+(12288-2048*pow<2>(t34))*pow<2>(t34))))+pow<2>(t34)*(6144+pow<2>(t34)*(24576+2048*pow<2>(t34)))*pow<4>(t12))+t12*(t34*(12288*u+pow<2>(t34)*(-40960*u+pow<2>(t34)*(49152*u+pow<2>(t34)*(-24576*u+4096*u*pow<2>(t34)))))+t12*(12288+pow<2>(t12)*(12288+pow<2>(t34)*(135168+(-110592-36864*pow<2>(t34))*pow<2>(t34))+t12*(t12*(-12288+(-155648-94208*pow<2>(t34))*pow<2>(t34))+t34*(-12288*u+pow<2>(t34)*(-131072*u-53248*u*pow<2>(t34)))))+pow<2>(t34)*(-32768+pow<2>(t34)*(24576-4096*pow<4>(t34)))))))))))*pow<-1>(zb)*pow<-1>(16+(-8+pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-48+(24-3*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(48+pow<2>(u)*(-24+3*pow<2>(u))+pow<2>(t34)*(-16+(8-pow<2>(u))*pow<2>(u))))+pow<2>(t12)*(-16+(8-pow<2>(u))*pow<2>(u)+pow<2>(t34)*(48+pow<2>(u)*(-24+3*pow<2>(u))+pow<2>(t34)*(-48+(24-3*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(16+(-8+pow<2>(u))*pow<2>(u)))))+zb*(-32+8*pow<2>(u)+pow<2>(t34)*(96-24*pow<2>(u)+pow<2>(t34)*(-96+pow<2>(t34)*(32-8*pow<2>(u))+24*pow<2>(u)))+pow<2>(t12)*(32-8*pow<2>(u)+pow<2>(t34)*(-96+24*pow<2>(u)+pow<2>(t34)*(96-24*pow<2>(u)+pow<2>(t34)*(-32+8*pow<2>(u)))))+zb*(24+zb*(-8+pow<2>(t34)*(32+pow<2>(t34)*(-48+(32-8*pow<2>(t34))*pow<2>(t34)))+zb*(1+pow<2>(t34)*(-5+pow<2>(t34)*(10+pow<2>(t34)*(-10+(5-pow<2>(t34))*pow<2>(t34))))+pow<2>(t12)*(-3+pow<2>(t12)*(3+pow<2>(t34)*(-7+pow<2>(t34)*(3+(3-2*pow<2>(t34))*pow<2>(t34)))+pow<2>(t12)*(-1+pow<2>(t34)*(3+(-3+pow<2>(t34))*pow<2>(t34))))+pow<2>(t34)*(9+pow<2>(t34)*(-10+pow<2>(t34)*(6+(-3+pow<2>(t34))*pow<2>(t34))))))+t12*(t34*(8*u+pow<2>(t34)*(-24*u+pow<2>(t34)*(24*u-8*u*pow<2>(t34))))+t12*(16+pow<2>(t34)*(-56+pow<2>(t34)*(72+pow<2>(t34)*(-40+8*pow<2>(t34))))+t12*(t12*(-8+pow<2>(t34)*(24+pow<2>(t34)*(-24+8*pow<2>(t34))))+t34*(-8*u+pow<2>(t34)*(24*u+pow<2>(t34)*(-24*u+8*u*pow<2>(t34))))))))-2*pow<2>(u)+pow<2>(t34)*(-80+4*pow<2>(u)+pow<2>(t34)*(96+pow<2>(t34)*(-48-4*pow<2>(u)+pow<2>(t34)*(8+2*pow<2>(u)))))+t12*(t34*(-16*u+pow<2>(t34)*(48*u+pow<2>(t34)*(-48*u+16*u*pow<2>(t34))))+t12*(-32+pow<2>(t34)*(104+2*pow<2>(u)+pow<2>(t34)*(-120-6*pow<2>(u)+pow<2>(t34)*(56+pow<2>(t34)*(-8-2*pow<2>(u))+6*pow<2>(u))))+t12*(t34*(16*u+pow<2>(t34)*(-48*u+pow<2>(t34)*(48*u-16*u*pow<2>(t34))))+t12*(8+2*pow<2>(u)+pow<2>(t34)*(-24-6*pow<2>(u)+pow<2>(t34)*(24+pow<2>(t34)*(-8-2*pow<2>(u))+6*pow<2>(u))))))))));
    return todouble<my_float>(foo);
}

// Coefficient order epsilon^1 of master 4
template<>
double qq2yygstu6Nf<4,1>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    const my_float foo = (917504+pow<2>(u)*(-884736+(270336-26624*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(-1441792+pow<2>(u)*(-163840+(376832-61440*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(131072+pow<2>(t34)*(393216-98304*pow<2>(u))+pow<2>(u)*(1146880+pow<2>(u)*(-319488+6144*pow<2>(u)))))+t12*(t34*(u*(-5570560+(2883584-372736*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(u*(3801088+(229376-294912*pow<2>(u))*pow<2>(u))+u*pow<2>(t34)*(1769472+pow<2>(u)*(-491520+12288*pow<2>(u)))))+t12*(-3407872+(1802240-237568*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-917504+(6258688-1507328*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(4718592+(-294912-221184*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-393216+98304*pow<2>(u))))+t12*(t12*(2490368-622592*pow<2>(u)+pow<2>(t34)*(3145728-786432*pow<2>(u)+pow<2>(t34)*(-393216+98304*pow<2>(u))))+t34*(u*(6553600-1638400*pow<2>(u))+pow<2>(t34)*(u*(4325376-1081344*pow<2>(u))+u*pow<2>(t34)*(-393216+98304*pow<2>(u)))))))+zb*(-2949120+pow<2>(u)*(2244608+pow<2>(u)*(-483328+26624*pow<2>(u)))+pow<2>(t34)*(4915200+pow<2>(u)*(-180224+pow<2>(u)*(-532480+61440*pow<2>(u)))+pow<2>(t34)*(-983040+pow<2>(t34)*(-983040+(638976-24576*pow<2>(u))*pow<2>(u))+pow<2>(u)*(-2703360+(712704-6144*pow<2>(u))*pow<2>(u))))+t12*(t34*(u*(16449536+pow<2>(u)*(-6832128+655360*pow<2>(u)))+pow<2>(t34)*(u*(-14352384+pow<2>(u)*(1130496+516096*pow<2>(u)))+pow<2>(t34)*(u*pow<2>(t34)*(196608-49152*pow<2>(u))+u*(-2293760+(1818624-24576*pow<2>(u))*pow<2>(u)))))+t12*(11206656+pow<2>(u)*(-5193728+573440*pow<2>(u))+pow<2>(t34)*(1572864+pow<2>(u)*(-16859136+3895296*pow<2>(u))+pow<2>(t34)*(-14745600+pow<2>(u)*(6471680+786432*pow<2>(u))+pow<2>(t34)*(1966080+(-147456-12288*pow<2>(u))*pow<2>(u))))+t12*(t12*(-8257536+t12*t34*(2228224*u+393216*u*pow<2>(t34))+2162688*pow<2>(u)+pow<2>(t34)*(-9043968+6717440*pow<2>(u)+pow<2>(t34)*(1572864+294912*pow<2>(u))))+t34*(u*(-21626880+5308416*pow<2>(u))+pow<2>(t34)*(u*pow<2>(t34)*(1966080-98304*pow<2>(u))+u*(-6553600+5275648*pow<2>(u)))))))+zb*(3866624+pow<2>(u)*(-2097152+245760*pow<2>(u))+pow<2>(t34)*(-7766016+pow<2>(u)*(1425408+135168*pow<2>(u))+pow<2>(t34)*(4030464+(1654784-417792*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-229376+pow<2>(u)*(-1032192+36864*pow<2>(u))+pow<2>(t34)*(98304+49152*pow<2>(u)))))+t12*(t34*(u*(-18776064+(5799936-282624*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(u*(22577152+(-2498560-221184*pow<2>(u))*pow<2>(u))+pow<2>(t34)*(u*pow<2>(t34)*(-294912+122880*pow<2>(u))+u*(-3506176+pow<2>(u)*(-2113536+12288*pow<2>(u))))))+t12*(-14647296+(5750784-417792*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(1703936+(18464768-3233792*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(16777216+(-14237696-872448*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(-3932160+98304*pow<2>(t34)+pow<2>(u)*(-147456+18432*pow<2>(u)))))+t12*(t34*(pow<2>(t34)*(pow<2>(t34)*(98304*u*pow<2>(t34)+u*(-4521984-245760*pow<2>(u)))+u*(-2031616-8110080*pow<2>(u)))+u*(30048256-6062080*pow<2>(u)))+t12*(11403264+t12*(t12*(-622592+pow<2>(t34)*(-786432+98304*pow<2>(t34)))+t34*(-8126464*u+pow<2>(t34)*(-2457600*u+98304*u*pow<2>(t34))))-2965504*pow<2>(u)+pow<2>(t34)*(9895936-14729216*pow<2>(u)+pow<2>(t34)*(-2949120-2015232*pow<2>(u)+49152*pow<2>(t34)*pow<2>(u)))))))+zb*(-2998272+(909312-12288*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(8126464+pow<2>(u)*(-1572864+12288*pow<2>(u))+pow<2>(t34)*(-7700480+pow<2>(u)*(344064+12288*pow<2>(u))+pow<2>(t34)*(3014656+pow<2>(t34)*(-442368-73728*pow<2>(u))+(393216-12288*pow<2>(u))*pow<2>(u))))+t12*(t34*(u*(10502144-1818624*pow<2>(u))+pow<2>(t34)*(u*(-17727488+1179648*pow<2>(u))+pow<2>(t34)*(pow<2>(t34)*(49152*u*pow<2>(t34)+u*(-491520-98304*pow<2>(u)))+u*(7667712+737280*pow<2>(u)))))+t12*(9994240+pow<2>(u)*(-2801664+61440*pow<2>(u))+pow<2>(t34)*(-5554176+pow<2>(u)*(-10248192+854016*pow<2>(u))+pow<2>(t34)*(-8110080+pow<2>(u)*(10813440+319488*pow<2>(u))+pow<2>(t34)*(3915776-245760*pow<2>(t34)+(270336-6144*pow<2>(u))*pow<2>(u))))+t12*(t12*(-8437760+t12*(t12*(1441792+(2588672-98304*pow<2>(t34))*pow<2>(t34))+t34*(9633792*u+pow<2>(t34)*(5996544*u+98304*u*pow<2>(t34))))+1695744*pow<2>(u)+pow<2>(t34)*(-6733824+12460032*pow<2>(u)+pow<2>(t34)*(4538368+pow<2>(t34)*(147456-24576*pow<2>(u))+4218880*pow<2>(u))))+t34*(u*(-21512192+2506752*pow<2>(u))+pow<2>(t34)*(u*(5849088+5193728*pow<2>(u))+pow<2>(t34)*(-245760*u*pow<2>(t34)+u*(6733824+491520*pow<2>(u))))))))+zb*(1679360-202752*pow<2>(u)+pow<2>(t34)*(-5750784+602112*pow<2>(u)+pow<2>(t34)*(7225344-589824*pow<2>(u)+pow<2>(t34)*(-3964928+184320*pow<2>(u)+pow<2>(t34)*(860160-49152*pow<2>(t34)+6144*pow<2>(u)))))+t12*(t12*(-4005888+t12*(t34*(pow<2>(t34)*(pow<2>(t34)*(196608*u*pow<2>(t34)+u*(-5365760-221184*pow<2>(u)))+u*(-1064960-1196032*pow<2>(u)))+u*(7544832-221184*pow<2>(u)))+t12*(3366912+t12*(t12*(-1040384+(-3276800-270336*pow<2>(t34))*pow<2>(t34))+t34*(-4313088*u+pow<2>(t34)*(-6275072*u-552960*u*pow<2>(t34))))+pow<2>(t34)*(4702208+pow<2>(t34)*(-4726784+pow<2>(t34)*(-393216-73728*pow<2>(u))-3291136*pow<2>(u))-4141056*pow<2>(u))-276480*pow<2>(u)))+479232*pow<2>(u)+pow<2>(t34)*(4620288+2605056*pow<2>(u)+pow<2>(t34)*(1163264-3096576*pow<2>(u)+pow<2>(t34)*(-1998848+pow<2>(t34)*(221184+12288*pow<2>(u))))))+t34*(u*(-3035136+73728*pow<2>(u))+pow<2>(t34)*(u*(7077888-122880*pow<2>(u))+pow<2>(t34)*(u*(-5185536+24576*pow<2>(u))+pow<2>(t34)*(-135168*u*pow<2>(t34)+u*(1277952+24576*pow<2>(u)))))))+zb*(-614400+43008*pow<2>(u)+pow<2>(t34)*(2359296-147456*pow<2>(u)+pow<2>(t34)*(-3489792+184320*pow<2>(u)+pow<2>(t34)*(2457600-98304*pow<2>(u)+pow<2>(t34)*(-811008+98304*pow<2>(t34)+18432*pow<2>(u)))))+t12*(t34*(540672*u+pow<2>(t34)*(-1744896*u+pow<2>(t34)*(1990656*u+pow<2>(t34)*(-909312*u+122880*u*pow<2>(t34)))))+t12*(1056768-36864*pow<2>(u)+pow<2>(t34)*(-2002944-24576*pow<2>(u)+pow<2>(t34)*(737280+pow<2>(t34)*(319488+pow<2>(t34)*(-122880+12288*pow<2>(t34)-12288*pow<2>(u))-73728*pow<2>(u))+147456*pow<2>(u)))+t12*(t34*(-1081344*u+pow<2>(t34)*(-245760*u+pow<2>(t34)*(1376256*u-49152*u*pow<2>(t34))))+t12*(-663552+t12*(t34*(540672*u+pow<2>(t34)*(2482176*u+745472*u*pow<2>(t34)))+t12*(221184+pow<2>(t34)*(1732608+pow<2>(t34)*(630784+36864*pow<2>(t34)))))-6144*pow<2>(u)+pow<2>(t34)*(-2088960+319488*pow<2>(u)+pow<2>(t34)*(1916928+854016*pow<2>(u)+pow<2>(t34)*(532480-24576*pow<2>(t34)+61440*pow<2>(u))))))))+zb*(98304+pow<2>(t34)*(-442368+pow<2>(t34)*(786432+pow<2>(t34)*(-688128+(294912-49152*pow<2>(t34))*pow<2>(t34))))+zb*pow<2>(t12)*(pow<2>(t12)*(pow<2>(t12)*pow<2>(t34)*(-6144+pow<2>(t34)*(61440+26624*pow<2>(t34)))+pow<2>(t34)*(-36864+pow<2>(t34)*(86016+pow<2>(t34)*(-61440+12288*pow<2>(t34)))))+pow<2>(t34)*(43008+pow<2>(t34)*(-147456+pow<2>(t34)*(184320+pow<2>(t34)*(-98304+18432*pow<2>(t34))))))+t12*(t34*(-86016*u+pow<2>(t34)*(294912*u+pow<2>(t34)*(-368640*u+pow<2>(t34)*(196608*u-36864*u*pow<2>(t34)))))+t12*(-196608+pow<2>(t34)*(546816+pow<2>(t34)*(-430080+pow<2>(t34)*(-24576+(135168-30720*pow<2>(t34))*pow<2>(t34))))+t12*(t34*(73728*u+pow<2>(t34)*(-122880*u+pow<2>(t34)*(24576*u+24576*u*pow<2>(t34))))+t12*(98304+t12*(t12*pow<2>(t34)*(-264192+(-430080-43008*pow<2>(t34))*pow<2>(t34))+t34*(12288*u+pow<2>(t34)*(-221184*u-282624*u*pow<2>(t34))))+pow<2>(t34)*(159744+(-282624+24576*pow<2>(t34))*pow<4>(t34)))))))))))))*pow<-1>(zb)*pow<-1>(-48+(24-3*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(144+pow<2>(u)*(-72+9*pow<2>(u))+pow<2>(t34)*(-144+(72-9*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(48+pow<2>(u)*(-24+3*pow<2>(u)))))+pow<2>(t12)*(48+pow<2>(u)*(-24+3*pow<2>(u))+pow<2>(t34)*(-144+(72-9*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(144+pow<2>(u)*(-72+9*pow<2>(u))+pow<2>(t34)*(-48+(24-3*pow<2>(u))*pow<2>(u)))))+zb*(144+pow<2>(u)*(-48+3*pow<2>(u))+pow<2>(t34)*(-432+(144-9*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(432+pow<2>(u)*(-144+9*pow<2>(u))+pow<2>(t34)*(-144+(48-3*pow<2>(u))*pow<2>(u))))+pow<2>(t12)*(-144+(48-3*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(432+pow<2>(u)*(-144+9*pow<2>(u))+pow<2>(t34)*(-432+(144-9*pow<2>(u))*pow<2>(u)+pow<2>(t34)*(144+pow<2>(u)*(-48+3*pow<2>(u))))))+zb*(-168+30*pow<2>(u)+pow<2>(t34)*(528-84*pow<2>(u)+pow<2>(t34)*(-576+pow<2>(t34)*(240+pow<2>(t34)*(-24-6*pow<2>(u))-12*pow<2>(u))+72*pow<2>(u)))+t12*(t34*(48*u+pow<2>(t34)*(-144*u+pow<2>(t34)*(144*u-48*u*pow<2>(t34))))+t12*(192-24*pow<2>(u)+pow<2>(t34)*(-600+66*pow<2>(u)+pow<2>(t34)*(648-54*pow<2>(u)+pow<2>(t34)*(-264+6*pow<2>(u)+pow<2>(t34)*(24+6*pow<2>(u)))))+t12*(t34*(-48*u+pow<2>(t34)*(144*u+pow<2>(t34)*(-144*u+48*u*pow<2>(t34))))+t12*(-24-6*pow<2>(u)+pow<2>(t34)*(72+18*pow<2>(u)+pow<2>(t34)*(-72-18*pow<2>(u)+pow<2>(t34)*(24+6*pow<2>(u))))))))+zb*(96+zb*(-27+pow<2>(t34)*(111+pow<2>(t34)*(-174+pow<2>(t34)*(126+pow<2>(t34)*(-39+3*pow<2>(t34)))))+zb*(3+pow<2>(t34)*(-15+pow<2>(t34)*(30+pow<2>(t34)*(-30+(15-3*pow<2>(t34))*pow<2>(t34))))+pow<2>(t12)*(-9+pow<2>(t12)*(9+pow<2>(t34)*(-21+pow<2>(t34)*(9+(9-6*pow<2>(t34))*pow<2>(t34)))+pow<2>(t12)*(-3+pow<2>(t34)*(9+pow<2>(t34)*(-9+3*pow<2>(t34)))))+pow<2>(t34)*(27+pow<2>(t34)*(-30+pow<2>(t34)*(18+pow<2>(t34)*(-9+3*pow<2>(t34)))))))+t12*(t34*(24*u+pow<2>(t34)*(-72*u+pow<2>(t34)*(72*u-24*u*pow<2>(t34))))+t12*(57+pow<2>(t34)*(-195+pow<2>(t34)*(246+pow<2>(t34)*(-138+(33-3*pow<2>(t34))*pow<2>(t34))))+t12*(t34*(-24*u+pow<2>(t34)*(72*u+pow<2>(t34)*(-72*u+24*u*pow<2>(t34))))+t12*(-33+pow<2>(t12)*(3+pow<2>(t34)*(-9+(9-3*pow<2>(t34))*pow<2>(t34)))+pow<2>(t34)*(93+pow<2>(t34)*(-81+pow<2>(t34)*(15+6*pow<2>(t34)))))))))-6*pow<2>(u)+pow<2>(t34)*(-336+12*pow<2>(u)+pow<2>(t34)*(432+pow<2>(t34)*(-240-12*pow<2>(u)+pow<2>(t34)*(48+6*pow<2>(u)))))+t12*(t34*(-72*u+pow<2>(t34)*(216*u+pow<2>(t34)*(-216*u+72*u*pow<2>(t34))))+t12*(-144+pow<2>(t34)*(480+6*pow<2>(u)+pow<2>(t34)*(-576-18*pow<2>(u)+pow<2>(t34)*(288+pow<2>(t34)*(-48-6*pow<2>(u))+18*pow<2>(u))))+t12*(t34*(72*u+pow<2>(t34)*(-216*u+pow<2>(t34)*(216*u-72*u*pow<2>(t34))))+t12*(48+6*pow<2>(u)+pow<2>(t34)*(-144-18*pow<2>(u)+pow<2>(t34)*(144+pow<2>(t34)*(-48-6*pow<2>(u))+18*pow<2>(u)))))))))));
    return todouble<my_float>(foo);
}

// Coefficient of master 4 as a series in epsilon
template<>
Expansion<Parameter::epsilon, double> qq2yygstu6Nf<4>(const my_float& zb, const my_float& t12, const my_float& t34, const my_float& u)
{
    return Expansion<Parameter::epsilon, double>(0,{
        qq2yygstu6Nf<4,0>(zb,t12,t34,u),
        qq2yygstu6Nf<4,1>(zb,t12,t34,u)
    });
}


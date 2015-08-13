/**
 *
 * \file    qq2yyg.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    May 2015
 *
 */

#include "constants.h" // QCD::CF, etc...
#include "boxmaster.h" // box, bubble

#include "qq2yyg.h"

/**
 * \par   patchDelta
 * \brief Relative distance of two Mandelstams for the Taylor-expansion patch to kick in
 * \todo  Find a way to move this to a header file
 */

const double patchDelta = 0.001;

template<typename T>
EpsExp qq2yyg1<T>::LC::bub::eval(const PSpoint& p)
{
    EpsExp fooLCbub(-2,{0.,0.,0.});
    bool on[13] = {true,true,true,true,true,true,true,false,false,false,false,false,false};
    size_t nPatch = 0;
    if (todouble(fabs(p.s13-p.s25)/(fabs(p.s13)+fabs(p.s25)))<patchDelta) {
        on[1-1]=false;
        on[6-1]=false;
        on[8-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s14-p.s25)/(fabs(p.s14)+fabs(p.s25)))<patchDelta) {
        on[2-1]=false;
        on[6-1]=false;
        on[9-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s23-p.s15)/(fabs(p.s23)+fabs(p.s15)))<patchDelta) {
        on[3-1]=false;
        on[5-1]=false;
        on[10-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s24-p.s15)/(fabs(p.s24)+fabs(p.s15)))<patchDelta) {
        on[4-1]=false;
        on[5-1]=false;
        on[11-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s13-p.s24)/(fabs(p.s13)+fabs(p.s24)))<patchDelta*todouble(p.zb)) {
        on[1-1]=false;
        on[4-1]=false;
        on[12-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s14-p.s23)/(fabs(p.s14)+fabs(p.s23)))<patchDelta*todouble(p.zb)) {
        on[2-1]=false;
        on[3-1]=false;
        on[13-1]=true;
        ++nPatch;
    }
    if (nPatch>1) {
        std::cerr << "Error in qq2yyg: too many large cancellations!" << std::endl;
        return EpsExp();
    }
    if (on[1-1])  fooLCbub += times(c<1>(p.zb,p.t12,p.t34,p.u),  bubble(p.s13,2),2);
    if (on[2-1])  fooLCbub += times(c<1>(p.zb,p.t12,-p.t34,-p.u),bubble(p.s14,2),2);
    if (on[3-1])  fooLCbub += times(c<1>(p.zb,-p.t12,p.t34,-p.u),bubble(p.s23,2),2);
    if (on[4-1])  fooLCbub += times(c<1>(p.zb,-p.t12,-p.t34,p.u),bubble(p.s24,2),2);
    if (on[5-1])  fooLCbub += times(c<2>(p.zb,p.t12,p.t34,p.u),  bubble(p.s15,3),3);
    if (on[6-1])  fooLCbub += times(c<2>(p.zb,-p.t12,p.t34,-p.u),bubble(p.s25,3),3);
    if (on[7-1])  fooLCbub += times(c<3>(p.zb,p.t12,p.t34,p.u),  bubble(p.s34,2),2);
    if (on[8-1])  fooLCbub += c1325(p.zb,p.t12,p.t34);
    if (on[9-1])  fooLCbub += c1325(p.zb,p.t12,-p.t34);
    if (on[10-1]) fooLCbub += c1325(p.zb,-p.t12,p.t34);
    if (on[11-1]) fooLCbub += c1325(p.zb,-p.t12,-p.t34);
    if (on[12-1]) fooLCbub += c1324(p.zb,p.t12,p.u);
    if (on[13-1]) fooLCbub += c1324(p.zb,-p.t12,-p.u);
    return QCD::CA*fooLCbub;
}

template<typename T>
EpsExp qq2yyg1<T>::LC::box::eval(const PSpoint& p)
{
    EpsExp fooLCbox(-2,{0.,0.,0.});
    fooLCbox += times(c<1>(p.zb,p.t12,p.t34,p.u),  box6(p.s13,p.s15,p.s24,3),3);
    fooLCbox += times(c<1>(p.zb,p.t12,-p.t34,-p.u),box6(p.s14,p.s15,p.s23,3),3);
    fooLCbox += times(c<1>(p.zb,-p.t12,p.t34,-p.u),box6(p.s23,p.s25,p.s14,3),3);
    fooLCbox += times(c<1>(p.zb,-p.t12,-p.t34,p.u),box6(p.s24,p.s25,p.s13,3),3);
    fooLCbox += times(c<2>(p.zb,p.t12,p.t34,p.u),  box6(p.s13,p.s34,p.s25,3),3);
    fooLCbox += times(c<2>(p.zb,p.t12,-p.t34,-p.u),box6(p.s14,p.s34,p.s25,3),3);
    fooLCbox += times(c<2>(p.zb,-p.t12,p.t34,-p.u),box6(p.s23,p.s34,p.s15,3),3);
    fooLCbox += times(c<2>(p.zb,-p.t12,-p.t34,p.u),box6(p.s24,p.s34,p.s15,3),3);
    fooLCbox += times(c<3>(p.zb,p.t12,p.t34,p.u),  box6(p.s15,p.s25,p.s34,3),3);
    return QCD::CA*fooLCbox;
}

template<typename T>
double qq2yyg1<T>::LC::bub::eval(const PSpoint& p, const int i)
{
    return eval(p).getCoefficient(i);
}

template<typename T>
double qq2yyg1<T>::LC::box::eval(const PSpoint& p, const int i)
{
    return eval(p).getCoefficient(i);
}

template<typename T>
EpsExp qq2yyg1<T>::LC::eval(const PSpoint& p)
{
    return bub::eval(p)+box::eval(p);
}

template<typename T>
double qq2yyg1<T>::LC::eval(const PSpoint& p, const int i)
{
    return bub::eval(p,i)+box::eval(p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::SC::bub::eval(const PSpoint& p)
{
    EpsExp fooSCbub(-2,{0.,0.,0.});
    bool on[16] = {
        true,true,true,true,true,true,true,true,true,true,
        false,false,false,false,false,false
    };
    size_t nPatch = 0;
    if (todouble(fabs(p.s13-p.s25)/(fabs(p.s13)+fabs(p.s25)))<patchDelta) {
        on[2-1]=false;
        on[7-1]=false;
        on[11-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s14-p.s25)/(fabs(p.s14)+fabs(p.s25)))<patchDelta) {
        on[3-1]=false;
        on[7-1]=false;
        on[12-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s23-p.s15)/(fabs(p.s23)+fabs(p.s15)))<patchDelta) {
        on[4-1]=false;
        on[6-1]=false;
        on[13-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s24-p.s15)/(fabs(p.s24)+fabs(p.s15)))<patchDelta) {
        on[5-1]=false;
        on[6-1]=false;
        on[14-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s13-p.s24)/(fabs(p.s13)+fabs(p.s24)))<patchDelta*todouble(p.zb)) {
        on[2-1]=false;
        on[5-1]=false;
        on[15-1]=true;
        ++nPatch;
    }
    if (todouble(fabs(p.s14-p.s23)/(fabs(p.s14)+fabs(p.s23)))<patchDelta*todouble(p.zb)) {
        on[3-1]=false;
        on[4-1]=false;
        on[16-1]=true;
        ++nPatch;
    }
    if (nPatch>1) {
        std::cerr << "Error in qq2yyg: too many large cancellations!" << std::endl;
        return EpsExp();
    }
    if (on[1-1])  fooSCbub += times(c<1>(p.zb,p.t12,p.t34,p.u),  bubble(p.s12,3),3);
    if (on[2-1])  fooSCbub += times(c<2>(p.zb,p.t12,p.t34,p.u),  bubble(p.s13,3),3);
    if (on[3-1])  fooSCbub += times(c<2>(p.zb,p.t12,-p.t34,-p.u),bubble(p.s14,3),3);
    if (on[4-1])  fooSCbub += times(c<2>(p.zb,-p.t12,p.t34,-p.u),bubble(p.s23,3),3);
    if (on[5-1])  fooSCbub += times(c<2>(p.zb,-p.t12,-p.t34,p.u),bubble(p.s24,3),3);
    if (on[6-1])  fooSCbub += times(c<3>(p.zb,p.t12,p.t34,p.u),  bubble(p.s15,3),3);
    if (on[7-1])  fooSCbub += times(c<3>(p.zb,-p.t12,p.t34,-p.u),bubble(p.s25,3),3);
    if (on[8-1])  fooSCbub += times(c<4>(p.zb,p.t12,p.t34,p.u),  bubble(p.s34,3),3);
    if (on[9-1])  fooSCbub += times(c<5>(p.zb,p.t12,p.t34,p.u),  bubble(p.s35,3),3);
    if (on[10-1]) fooSCbub += times(c<5>(p.zb,p.t12,-p.t34,-p.u),bubble(p.s45,3),3);
    if (on[11-1]) fooSCbub += c1325(p.zb,p.t12,p.t34);
    if (on[12-1]) fooSCbub += c1325(p.zb,p.t12,-p.t34);
    if (on[13-1]) fooSCbub += c1325(p.zb,-p.t12,p.t34);
    if (on[14-1]) fooSCbub += c1325(p.zb,-p.t12,-p.t34);
    if (on[15-1]) fooSCbub += c1324(p.zb,p.t12,p.u);
    if (on[16-1]) fooSCbub += c1324(p.zb,-p.t12,-p.u);
    return static_cast<double>(QCD::CA-2.*QCD::CF)*fooSCbub;
}

template<typename T>
EpsExp qq2yyg1<T>::SC::box::eval(const PSpoint& p)
{
    EpsExp fooSCbox(-2,{0.,0.,0.});
    fooSCbox += times(c<1>(p.zb,p.t12,p.t34,p.u),  box6(p.s12,p.s13,p.s45,3),3);
    fooSCbox += times(c<1>(p.zb,p.t12,-p.t34,-p.u),box6(p.s12,p.s14,p.s35,3),3);
    fooSCbox += times(c<1>(p.zb,-p.t12,p.t34,-p.u),box6(p.s12,p.s23,p.s45,3),3);
    fooSCbox += times(c<1>(p.zb,-p.t12,-p.t34,p.u),box6(p.s12,p.s24,p.s35,3),3);
    fooSCbox += times(c<2>(p.zb,p.t12,p.t34,p.u),  box6(p.s12,p.s15,p.s34,3),3);
    fooSCbox += times(c<2>(p.zb,-p.t12,p.t34,-p.u),box6(p.s12,p.s25,p.s34,3),3);
    fooSCbox += times(c<3>(p.zb,p.t12,p.t34,p.u),  box6(p.s13,p.s34,p.s25,3),3);
    fooSCbox += times(c<3>(p.zb,p.t12,-p.t34,-p.u),box6(p.s14,p.s34,p.s25,3),3);
    fooSCbox += times(c<3>(p.zb,-p.t12,p.t34,-p.u),box6(p.s23,p.s34,p.s15,3),3);
    fooSCbox += times(c<3>(p.zb,-p.t12,-p.t34,p.u),box6(p.s24,p.s34,p.s15,3),3);
    fooSCbox += times(c<4>(p.zb,p.t12,p.t34,p.u),  box6(p.s13,p.s35,p.s24,3),3);
    fooSCbox += times(c<4>(p.zb,p.t12,-p.t34,-p.u),box6(p.s14,p.s45,p.s23,3),3);
    fooSCbox += times(c<4>(p.zb,-p.t12,p.t34,-p.u),box6(p.s23,p.s35,p.s14,3),3);
    fooSCbox += times(c<4>(p.zb,-p.t12,-p.t34,p.u),box6(p.s24,p.s45,p.s13,3),3);
    fooSCbox += times(c<5>(p.zb,p.t12,p.t34,p.u),  box6(p.s15,p.s35,p.s24,3),3);
    fooSCbox += times(c<5>(p.zb,p.t12,-p.t34,-p.u),box6(p.s15,p.s45,p.s23,3),3);
    fooSCbox += times(c<5>(p.zb,-p.t12,p.t34,-p.u),box6(p.s25,p.s35,p.s14,3),3);
    fooSCbox += times(c<5>(p.zb,-p.t12,-p.t34,p.u),box6(p.s25,p.s45,p.s13,3),3);
    fooSCbox += times(c<6>(p.zb,p.t12,p.t34,p.u),  box6(p.s34,p.s35,p.s12,3),3);
    fooSCbox += times(c<6>(p.zb,p.t12,-p.t34,-p.u),box6(p.s34,p.s45,p.s12,3),3);
    fooSCbox += times(c<6>(p.zb,p.t12,p.t34,p.u),  box6(p.s35,p.s45,p.s12,3),3);
    return static_cast<double>(QCD::CA-2.*QCD::CF)*fooSCbox;
}

template<typename T>
double qq2yyg1<T>::SC::bub::eval(const PSpoint& p, const int i)
{
    return eval(p).getCoefficient(i);
}

template<typename T>
double qq2yyg1<T>::SC::box::eval(const PSpoint& p, const int i)
{
    return eval(p).getCoefficient(i);
}

template<typename T>
EpsExp qq2yyg1<T>::SC::eval(const PSpoint& p)
{
    return bub::eval(p)+box::eval(p);
}

template<typename T>
double qq2yyg1<T>::SC::eval(const PSpoint& p, const int i)
{
    return bub::eval(p,i)+box::eval(p,i);
}

template<typename T>
EpsExp qq2yyg1<T>::Nf::bub::eval(const PSpoint& p)
{
    EpsExp fooNfbub(-2,{0.,0.,0.});
    fooNfbub += times(c<1>(p.zb,p.t12,p.t34,p.u),  bubble(p.s12,3),2);
    fooNfbub += times(c<2>(p.zb,p.t12,p.t34,p.u),  bubble(p.s34,3),2);
    fooNfbub += times(c<3>(p.zb,p.t12,p.t34,p.u),  bubble(p.s35,3),2);
    fooNfbub += times(c<3>(p.zb,p.t12,-p.t34,-p.u),bubble(p.s45,3),2);
    // CHECK THIS FACTOR
    return -QCD::sumQ2/512.*fooNfbub;
}

template<typename T>
EpsExp qq2yyg1<T>::Nf::box::eval(const PSpoint& p)
{
    EpsExp fooNfbox(-2,{0.,0.,0.});
    fooNfbox += times(c<1>(p.zb,p.t12,p.t34,p.u),  box6(p.s34,p.s35,p.s12,3),1);
    fooNfbox += times(c<1>(p.zb,p.t12,-p.t34,-p.u),box6(p.s34,p.s45,p.s12,3),1);
    fooNfbox += times(c<2>(p.zb,p.t12,p.t34,p.u),  box6(p.s35,p.s45,p.s12,3),1);
    // CHECK THIS FACTOR
    return -QCD::sumQ2/512.*fooNfbox;
}

template<typename T>
EpsExp qq2yyg1<T>::Nf::eval(const PSpoint& p)
{
    return bub::eval(p)+box::eval(p);
}

template<typename T>
double qq2yyg1<T>::Nf::eval(const PSpoint& p, const int i)
{
    return eval(p).getCoefficient(i);
}

template<typename T>
EpsExp qq2yyg1<T>::eval(const PSpoint& p)
{
    return LC::eval(p)+SC::eval(p)+Nf::eval(p);
}

template<typename T>
double qq2yyg1<T>::eval(const PSpoint& p, const int i)
{
    return eval(p).getCoefficient(i);
}

EpsExp foodbl = qq2yyg1<dbl>::eval(qq2yyg1<dbl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp fooqpl = qq2yyg1<qpl>::eval(qq2yyg1<qpl>::PSpoint(0.1,0.2,0.3,0.4));
EpsExp foortn = qq2yyg1<rtn>::eval(qq2yyg1<rtn>::PSpoint(0.1,0.2,0.3,0.4));
//

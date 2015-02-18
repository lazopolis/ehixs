// This file was generated by allcoeffs.sh

#include "qq2yyg/allcoeffs.h"
#include "coeffCA1.h"
#include "coeffCA10.h"
#include "coeffCA11.h"
#include "coeffCA12.h"
#include "coeffCA13.h"
#include "coeffCA14.h"
#include "coeffCA15.h"
#include "coeffCA16.h"
#include "coeffCA2.h"
#include "coeffCA3.h"
#include "coeffCA4.h"
#include "coeffCA5.h"
#include "coeffCA6.h"
#include "coeffCA7.h"
#include "coeffCA8.h"
#include "coeffCA9.h"
#include "coeffCAm2CF1.h"
#include "coeffCAm2CF10.h"
#include "coeffCAm2CF11.h"
#include "coeffCAm2CF12.h"
#include "coeffCAm2CF13.h"
#include "coeffCAm2CF14.h"
#include "coeffCAm2CF15.h"
#include "coeffCAm2CF16.h"
#include "coeffCAm2CF17.h"
#include "coeffCAm2CF18.h"
#include "coeffCAm2CF19.h"
#include "coeffCAm2CF2.h"
#include "coeffCAm2CF20.h"
#include "coeffCAm2CF21.h"
#include "coeffCAm2CF22.h"
#include "coeffCAm2CF23.h"
#include "coeffCAm2CF24.h"
#include "coeffCAm2CF25.h"
#include "coeffCAm2CF26.h"
#include "coeffCAm2CF27.h"
#include "coeffCAm2CF28.h"
#include "coeffCAm2CF29.h"
#include "coeffCAm2CF3.h"
#include "coeffCAm2CF30.h"
#include "coeffCAm2CF31.h"
#include "coeffCAm2CF4.h"
#include "coeffCAm2CF5.h"
#include "coeffCAm2CF6.h"
#include "coeffCAm2CF7.h"
#include "coeffCAm2CF8.h"
#include "coeffCAm2CF9.h"
#include "coeffCF1.h"
#include "coeffCF10.h"
#include "coeffCF11.h"
#include "coeffCF2.h"
#include "coeffCF3.h"
#include "coeffCF4.h"
#include "coeffCF5.h"
#include "coeffCF6.h"
#include "coeffCF7.h"
#include "coeffCF8.h"
#include "coeffCF9.h"

double qq2yyg(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double CAm2CF = QCD::CA-2.*QCD::CF;
    double foo = 0.;
    foo += QCD::CA*productCoeff(qq2yygCA<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<8>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14,s24,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<9>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<10>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14,s23,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<11>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<12>(s12,s13,s14,s23,s24),box(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<13>(s12,s13,s14,s23,s24),box(s23,-s12-s23-s24,s14,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<14>(s12,s13,s14,s23,s24),box(s24,-s12-s23-s24,s13,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<15>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    foo += QCD::CA*productCoeff(qq2yygCA<16>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<8>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<9>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<10>(s12,s13,s14,s23,s24),box(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    foo += QCD::CF*productCoeff(qq2yygCF<11>(s12,s13,s14,s23,s24),box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<1>(s12,s13,s14,s23,s24),bubble(s12,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<2>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<3>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<5>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<6>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<11>(s12,s13,s14,s23,s24),box(s12,s13,s12+s13+s23,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<12>(s12,s13,s14,s23,s24),box(s12,s14,s12+s14+s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<13>(s12,s13,s14,s23,s24),box(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<14>(s12,s13,s14,s23,s24),box(s12,s23,s12+s13+s23,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<15>(s12,s13,s14,s23,s24),box(s12,s24,s12+s14+s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<16>(s12,s13,s14,s23,s24),box(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<17>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<18>(s12,s13,s14,s23,s24),box(s13,s12+s14+s24,s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<19>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<20>(s12,s13,s14,s23,s24),box(s14,s12+s13+s23,s23,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<21>(s12,s13,s14,s23,s24),box(-s12-s13-s14,s12+s14+s24,s24,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<22>(s12,s13,s14,s23,s24),box(-s12-s13-s14,s12+s13+s23,s23,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<23>(s12,s13,s14,s23,s24),box(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<24>(s12,s13,s14,s23,s24),box(s23,s12+s14+s24,s14,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<25>(s12,s13,s14,s23,s24),box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<26>(s12,s13,s14,s23,s24),box(s24,s12+s13+s23,s13,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<27>(s12,s13,s14,s23,s24),box(-s12-s23-s24,s12+s14+s24,s14,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<28>(s12,s13,s14,s23,s24),box(-s12-s23-s24,s12+s13+s23,s13,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<29>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<30>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
    foo += CAm2CF*productCoeff(qq2yygCAm2CF<31>(s12,s13,s14,s23,s24),box(s12+s14+s24,s12+s13+s23,s12,3),0);
    return foo;
}

double qq2yygCAbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCAbub(0.);
    fooCAbub += QCD::CA*productCoeff(qq2yygCA<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yygCA<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yygCA<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yygCA<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yygCA<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yygCA<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooCAbub += QCD::CA*productCoeff(qq2yygCA<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return fooCAbub;
}

double qq2yygCAbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCAbox(0.);
    fooCAbox += QCD::CA*productCoeff(qq2yygCA<8>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14,s24,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yygCA<9>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yygCA<10>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14,s23,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yygCA<11>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yygCA<12>(s12,s13,s14,s23,s24),box(-s12-s13-s14,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yygCA<13>(s12,s13,s14,s23,s24),box(s23,-s12-s23-s24,s14,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yygCA<14>(s12,s13,s14,s23,s24),box(s24,-s12-s23-s24,s13,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yygCA<15>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s23,-s12-s13-s14,3),0);
    fooCAbox += QCD::CA*productCoeff(qq2yygCA<16>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s24,-s12-s13-s14,3),0);
    return fooCAbox;
}

double qq2yygCFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCFbub(0.);
    fooCFbub += QCD::CF*productCoeff(qq2yygCF<1>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yygCF<2>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yygCF<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yygCF<4>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yygCF<5>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yygCF<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooCFbub += QCD::CF*productCoeff(qq2yygCF<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    return fooCFbub;
}

double qq2yygCFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    double fooCFbox(0.);
    fooCFbox += QCD::CF*productCoeff(qq2yygCF<8>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCFbox += QCD::CF*productCoeff(qq2yygCF<9>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooCFbox += QCD::CF*productCoeff(qq2yygCF<10>(s12,s13,s14,s23,s24),box(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooCFbox += QCD::CF*productCoeff(qq2yygCF<11>(s12,s13,s14,s23,s24),box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    return fooCFbox;
}

double qq2yygAFbub(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double CAm2CF = QCD::CA-2.*QCD::CF;
    double fooAFbub(0.);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<1>(s12,s13,s14,s23,s24),bubble(s12,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<2>(s12,s13,s14,s23,s24),bubble(s13,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<3>(s12,s13,s14,s23,s24),bubble(s14,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<5>(s12,s13,s14,s23,s24),bubble(s23,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<6>(s12,s13,s14,s23,s24),bubble(s24,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0);
    fooAFbub += CAm2CF*productCoeff(qq2yygCAm2CF<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0);
    return fooAFbub;
}

double qq2yygAFbox(const double& s12, const double& s13, const double& s14, const double& s23, const double& s24)
{
    const double CAm2CF = QCD::CA-2.*QCD::CF;
    double fooAFbox(0.);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<11>(s12,s13,s14,s23,s24),box(s12,s13,s12+s13+s23,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<12>(s12,s13,s14,s23,s24),box(s12,s14,s12+s14+s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<13>(s12,s13,s14,s23,s24),box(s12,-s12-s13-s14,-s12-s13-s14-s23-s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<14>(s12,s13,s14,s23,s24),box(s12,s23,s12+s13+s23,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<15>(s12,s13,s14,s23,s24),box(s12,s24,s12+s14+s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<16>(s12,s13,s14,s23,s24),box(s12,-s12-s23-s24,-s12-s13-s14-s23-s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<17>(s12,s13,s14,s23,s24),box(s13,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<18>(s12,s13,s14,s23,s24),box(s13,s12+s14+s24,s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<19>(s12,s13,s14,s23,s24),box(s14,-s12-s13-s14-s23-s24,-s12-s23-s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<20>(s12,s13,s14,s23,s24),box(s14,s12+s13+s23,s23,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<21>(s12,s13,s14,s23,s24),box(-s12-s13-s14,s12+s14+s24,s24,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<22>(s12,s13,s14,s23,s24),box(-s12-s13-s14,s12+s13+s23,s23,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<23>(s12,s13,s14,s23,s24),box(s23,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<24>(s12,s13,s14,s23,s24),box(s23,s12+s14+s24,s14,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<25>(s12,s13,s14,s23,s24),box(s24,-s12-s13-s14-s23-s24,-s12-s13-s14,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<26>(s12,s13,s14,s23,s24),box(s24,s12+s13+s23,s13,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<27>(s12,s13,s14,s23,s24),box(-s12-s23-s24,s12+s14+s24,s14,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<28>(s12,s13,s14,s23,s24),box(-s12-s23-s24,s12+s13+s23,s13,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<29>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s12+s14+s24,s12,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<30>(s12,s13,s14,s23,s24),box(-s12-s13-s14-s23-s24,s12+s13+s23,s12,3),0);
    fooAFbox += CAm2CF*productCoeff(qq2yygCAm2CF<31>(s12,s13,s14,s23,s24),box(s12+s14+s24,s12+s13+s23,s12,3),0);
    return fooAFbox;
}
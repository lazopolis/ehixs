/**
 * \file   qq2yy_test.cpp
 * \author Simone Lionetti
 * \date   July 2015
 * \brief  This file contains the tests for the qq2yy matrix elements
 */

//#include "gtest/gtest.h"
#include "xcodetest.h"
#include "fourvector.h"
#include "counterforge.h"
#include "qq2gammagammaX.h"
#include "qq2yyg/qq2yygz1double.h"
#include "qq2yyg/qq2yygz1quad.h"
#include "qq2yyg/qq2yygz0double.h"
#include "qq2yyg/qq2yygz0quad.h"
#include <fstream>

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

void readPSpoint(Momenta& k, fstream& file)
{
    if (file.is_open())
        for (Momenta::iterator it = k.begin(); it != k.end(); ++it)
            for (size_t i=0; i!=4;++i)
                file >> (*it)[i];
    else cerr << "Bad file!" << endl;
    return;
}

Expansion<Parameter::epsilon,double> qq2yyg_collexp(const double lambda, const double z, const double ratio)
{
    return (
            times(
                  Expansion<Parameter::epsilon,double>::exp(-log(lambda*(1.-z)),3),
                  CounterForge::Pqq<1>(z,true,true,3)*qq2yy<0>(ratio),
                  3
                  )+
            times(
                  Expansion<Parameter::epsilon,double>::exp(-log(z),3),
                  CounterForge::Pqq<0>(z,true,true,3)*qq2yy<1>(ratio),
                  3
                  )
            )/(lambda*z);
}

int main(int argc, char**argv)
{

    const double Q = 1./3.;
    const double Nc = 3.;
    const double factor = pow(Q,4)*(16.*consts::pi_square)/Nc;
    std::cout.precision(15);

//    std::vector<Momenta> points;
    Momenta p;
    p.resize(2);
    p[1] = FourVector(0.5,0.,0.,+0.5);
    p[2] = FourVector(0.5,0.,0.,-0.5);

    // Computing qq2yy matrix element for a fixed phase-space point
//    p.resize(4);
//    p[3] = FourVector(0.5,0.304281607670107,0.164409347738594,0.361084850983623);
//    p[4] = FourVector(0.5,-0.304281607670107,-0.164409347738594,-0.361084850983623);
//    points.push_back(p);
//    p[3] = FourVector(0.5,-0.249236336136128,0.250552003163326,0.353701770507703);
//    p[4] = FourVector(0.5,0.249236336136128,-0.250552003163326,-0.353701770507703);
//    points.push_back(p);
//
//    std::cout << " --- q q -> y y --- " << std::endl;
//    for (std::vector<Momenta>::const_iterator it = points.begin(); it != points.end(); ++it)
//    {
//        const double s12 = square((*it)[1]+(*it)[2]);
//        const double s13 = square((*it)[1]-(*it)[3]);
//        const double s14 = square((*it)[1]-(*it)[4]);
//        std::cout << s13 << " " << s14 << " " << s12 << std::endl;
//        std::cout << qq2yy<0,0>(s13/s14) << std::endl;
//        std::cout << factor*qq2yy<0,0>(s13/s14) << std::endl;
//        std::cout << factor*(
//                             qq2yy<1>(s13/s14)
//                             +static_cast<double>(QCD::CF)/consts::Pi*
//                             times(
//                                   Expansion<Parameter::epsilon, double>(-2,{1.,1.5},true),
//                                   qq2yy<0>(s13/s14).setCoefficient(0,0.),
//                                   2
//                                   )
//                             ) << endl;
//        std::cout << 2.*consts::Pi*times(
//                                         qq2yy<1>(s13/s14),
//                                         inverse(qq2yy<0>(s13/s14)/qq2yy<0,0>(s13/s14),3),
//                                         3
//                                         )/qq2yy<0,0>(s13/s14) << endl;
//    }
//    points.clear();

    // Computing qq2yyg matrix element for a fixed phase-space point
    fstream myfile;
    myfile.open("/Users/lionetti/ETH/External/MG5_aMC_v2_3_0/ddx2aa/PROC_loop_sm_0/SubProcesses/P0_ddx_aag/PS.input");

    //std::cout << " --- q q -> y y g --- " << std::endl;
    p.resize(5);
    //while (!myfile.eof())
    //{
        readPSpoint(p,myfile);
        double s12 = square(p[1]+p[2]);
        const double s13 = square(p[1]-p[3])/s12;
        const double s14 = square(p[1]-p[4])/s12;
        const double s23 = square(p[2]-p[3])/s12;
        const double s24 = square(p[2]-p[4])/s12;
        s12 = 1.;
        const double s15 = -1-s13-s14;
        const double s25 = -1-s23-s24;
        const double s35 = 1+s14+s24;
        const double s45 = 1+s13+s23;
        const double zb = -s15-s25;
        const double t12 = (s15-s25)/zb;
        const double t34 = (s35-s45)/zb;
        const double u = s13-s14-s23+s24;
        const __float128 s13q = s13;
        const __float128 s14q = s14;
        const __float128 s23q = s23;
        const __float128 s24q = s24;
        const double beta0 = 25./6.;
        const double lambda = -s15/zb;
        const double lambar = -s25/zb;
        const double z = 1.-zb;
        //std::cout << zb << "\t" << t12 << "\t" << t34 << "\t" << u << endl;
        //std::cout << log(s15*s25) << endl;
        //std::cout << factor*qq2yyg<0>(zb,t12,t34,u) << endl;
    std::cout << zb << "\t" << -s15/zb << "\t" << s13 << "\t" << s14 << "\t" << s23 << "\t" << s24 << "\t";
    std::cout << 2.*consts::Pi*productCoeff(
                                            16./3.*qq2yygz1colexp(s13q,s14q,s23q,s24q),
                                            inverse(qq2yyg<0>(zb,t12,t34,u),3),
                                            0
                                            )/zb << "\t";
    std::cout << 2.*consts::Pi*productCoeff(
                                            16./3.*qq2yygz0colexp(s13q,s14q,s23q,s24q),
                                            inverse(qq2yyg<0>(zb,t12,t34,u),3),
                                            0
                                            )/zb << "\t";
    std::cout << 2*consts::Pi*productCoeff(
                                           qq2yyg_collexp(lambda,z,s13/s14)+qq2yyg_collexp(lambar,z,s13/s14),
                                           inverse(qq2yyg<0>(zb,t12,t34,u),3),
                                           0
                                           )/zb << "\t";
    //}
return 0;

}

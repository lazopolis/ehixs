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
#include "qq2yyg/qq2yyg.h"
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

//Expansion<Parameter::epsilon,double> qq2yyg_collexp(const double lambda, const double z, const double ratio)
//{
//    return (
//            times(
//                  Expansion<Parameter::epsilon,double>::exp(-log(lambda*(1.-z)),3),
//                  CounterForge::Pqq<1>(z,true,true,3)*qq2yy<0>(ratio),
//                  3
//                  )+
//            times(
//                  Expansion<Parameter::epsilon,double>::exp(-log(z),3),
//                  CounterForge::Pqq<0>(z,true,true,3)*qq2yy<1>(ratio),
//                  3
//                  )
//            )/(lambda*z);
//}

int main(int argc, char**argv)
{

    string nPS = argv[1];

    std::cout.precision(15);

    Momenta p;
    p.resize(5);
    p[1] = FourVector(0.5,0.,0.,+0.5);
    p[2] = FourVector(0.5,0.,0.,-0.5);

    // Computing qq2yyg matrix element for a fixed phase-space point
    fstream myfile;
    myfile.open("/Users/lionetti/ETH/External/MG5_aMC_v2_3_0/ddx2aa/PSpoints/point"+nPS+".input");
    readPSpoint(p,myfile);
    myfile.close();

    const qq2yyg1<dbl>::PSpoint pdbl(p);
    const qq2yyg1<qpl>::PSpoint pqpl(p);
    const qq2yyg1<rtn>::PSpoint prtn(p);

    const double beta0 = 25./6.;
    const double lambda = -pdbl.s15/pdbl.zb;
    const double lambar = -pdbl.s25/pdbl.zb;
    const double z = 1.-pdbl.zb;

    std::cout << pdbl.zb << "\t" << lambda << "\t" << pdbl.t12 << "\t" << pdbl.t34 << "\t" << pdbl.u << "\t";

    std::cout << qq2yyg1<dbl>::LC::bub::c<1>(pdbl.zb,pdbl.t12,pdbl.t34,pdbl.u) << "\t";
    std::cout << qq2yyg1<qpl>::LC::bub::c<1>(pqpl.zb,pqpl.t12,pqpl.t34,pqpl.u) << "\t";
    std::cout << qq2yyg1<rtn>::LC::bub::c<1>(prtn.zb,prtn.t12,prtn.t34,prtn.u) << "\t";

//    std::cout << qq2yyg1<dbl>::LC::bub::eval(pdbl) << "\t";
//    std::cout << qq2yyg1<qpl>::LC::bub::eval(pqpl) << "\t";
//    std::cout << qq2yyg1<rtn>::LC::bub::eval(prtn) << "\t";

    std::cout << 2.*consts::Pi*productCoeff(
                                            16./3.*qq2yyg1<rtn>::eval(prtn),
                                            inverse(qq2yyg<0>(pdbl.zb,pdbl.t12,pdbl.t34,pdbl.u),3),
                                            0
                                            )/pdbl.zb << "\t";

//    myfile.open("/Users/lionetti/ETH/External/MG5_aMC_v2_3_0/ddx2aa/PSpoints/point"+nPS+".input.c1");
//    readPSpoint(p,myfile);
//    myfile.close();
//    const double ratio1 = square(p[1]-p[5]-p[3])/square(p[1]-p[5]-p[4]);

//    myfile.open("/Users/lionetti/ETH/External/MG5_aMC_v2_3_0/ddx2aa/PSpoints/point"+nPS+".input.c2");
//    readPSpoint(p,myfile);
//    myfile.close();
//    const double ratio2 = square(p[1]-p[5]-p[3])/square(p[1]-p[5]-p[4]);

//    std::cout << 2*consts::Pi*productCoeff(
//                                           qq2yyg_collexp(lambda,z,ratio1),
//                                           inverse(qq2yyg<0>(zb,t12,t34,u),3),
//                                           0
//                                           )/zb << "\t";

//    std::cout << 2*consts::Pi*productCoeff(
//                                           qq2yyg_collexp(lambar,z,ratio2),
//                                           inverse(qq2yyg<0>(zb,t12,t34,u),3),
//                                           0
//                                           )/zb << "\t";
    return 0;

}
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
#include "isolationcone.h"
//#include "qq2gammagammaX.h"
#include "qq2yyg/qq2yyg.h"
#include "parametrizations.h"
#include <fstream>

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

//Expansion<Parameter::epsilon,double> coll1(const double lambda, const double z, const double ratio)
//{
//    return times(
//                 Expansion<Parameter::epsilon,double>::exp(-log(lambda*(1.-z)),3),
//                 CounterForge::Pqq<1>(z,true,true,3)*qq2yy<0>(ratio),
//                 3
//                 )/(lambda*z);
//}
//
//Expansion<Parameter::epsilon,double> coll2(const double lambda, const double z, const double ratio)
//{
//    return times(
//                 Expansion<Parameter::epsilon,double>::exp(-log(z),3),
//                 CounterForge::Pqq<0>(z,true,true,3)*qq2yy<1>(ratio),
//                 3
//                 )/(lambda*z);
//}

int main(int argc, char**argv)
{

    std::cout.precision(15);

    Bjorken xs(1.,1.);
    Momenta p;
    p.resize(5);
    p[1] = {0.5,0.,0.,+0.5};
    p[2] = {0.5,0.,0.,-0.5};
    Zlambda3PG pGen(p,xs);
    vector<double> ms = {0., 0.};
    pGen.setParameters(1.,ms);
    pGen.computeConstants();

    // Computing qq2yyg matrix element for a fixed phase-space point
    vector<double> randoms = {0.6330989210547898,0.2484647110590450,0.3092652883252196,0.,0.};
//    vector<double> randoms = {0.3630989210547898,0.8484647110590450,0.3092652883252196,0.,0.};
    double& z = randoms[3];
    double& lambda = randoms[4];

    SmoothPhotonIsolation _cone(0.4,1.,0.5);

    //Right now do a single PS scan
//    for (double zeta = 1.e-5; zeta <= 5; zeta=min(zeta+0.5,zeta*2))
    for (double zeta = -5; zeta <= -4.9; zeta+=0.2)
    {
//        z = 1.-exp(-2.*zeta);
        const double ez = exp(zeta);
        z = ez/(ez+1./ez);
        const double zb = 1.-z;
//        cout << endl;
        for (double y = -10; y <= 10; y+=0.2)
        {

            // FULL kinematics
            const double ey = exp(y);
            lambda = ey/(ey+1./ey);
            const double l = lambda;
            pGen.generateFSMomenta(randoms);
            cout << z << "\t" << l << "\t";

            double s12 = square(p[1]+p[2]);
            const double s13 = square(p[1]-p[3])/s12;
            const double s14 = square(p[1]-p[4])/s12;
            const double s23 = square(p[2]-p[3])/s12;
            const double s24 = square(p[2]-p[4])/s12;
            const double s34 = square(p[3]+p[4])/s12;
            s12 = 1.;
            const double s15 = -1-s13-s14;
            const double s25 = -1-s23-s24;
            const double s35 = 1+s14+s24;
            const double s45 = 1+s13+s23;

            const double t12 = (s15-s25)/zb;
            const double t34 = (s35-s45)/zb;
            const double u = s13-s14-s23+s24;

            const __float128 zbq = zb;
            const __float128 t12q = t12;
            const __float128 t34q = t34;
            const __float128 uq = u;
            const __float128 zq = z;

            const __float128 s13q = s13;
            const __float128 s14q = s14;
            const __float128 s23q = s23;
            const __float128 s24q = s24;

//            const cl_RA zbr = rational(zb);
//            const cl_RA t12r = rational(t12);
//            const cl_RA t34r = rational(t34);
//            const cl_RA ur = rational(u);

            // Printing out cut information
//            if (_cone.inside(p[3],p[5])) cout << true << "\t";
//            else cout << false << "\t";
//            if (_cone.inside(p[4],p[5])) cout << true << "\t";
//            else cout << false << "\t";
//            if (_cone.inside(p[3],p[4])) cout << true << "\t";
//            else cout << false << "\t";
//            if (z < pow(20./10000.,2)) cout << true << "\t";
//            else cout << false << "\t";
//            if (p[3].T() < 20./10000.) cout << true << "\t";
//            else cout << false << "\t";
//            if (p[4].T() < 20./10000.) cout << true << "\t";
//            else cout << false << "\t";
//            if (abs(p[3].Y()) > 2.5) cout << true << "\t";
//            else cout << false << "\t";
//            if (abs(p[4].Y()) > 2.5) cout << true << "\t";
//            else cout << false << "\t";

            cout << qq2yygz1LCbub(s13q,s14q,s23q,s24q) << "\t";
            cout << qq2yygz1LCbox(s13q,s14q,s23q,s24q) << "\t";
            cout << qq2yygz1SCbub(s13q,s14q,s23q,s24q) << "\t";
            cout << qq2yygz1SCbox(s13q,s14q,s23q,s24q) << "\t";

//            cout << productCoeff(qq2yygz1LC<1>(zb,t12,t34,u),  bubble(s13,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<1>(zb,t12,-t34,-u),bubble(s14,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<1>(zb,-t12,t34,-u),bubble(s23,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<1>(zb,-t12,-t34,u),bubble(s24,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<2>(zb,t12,t34,u),  bubble(s15,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<2>(zb,-t12,t34,-u),bubble(s25,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<3>(zb,t12,t34,u),  bubble(s34,3),0) << "\t";

//            cout << productCoeff(qq2yygz1LC<4>(zbq,t12q,t34q,uq),  box6(s13,s15,s24,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<4>(zbq,t12q,-t34q,-uq),box6(s14,s15,s23,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<4>(zbq,-t12q,t34q,-uq),box6(s23,s25,s14,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<4>(zbq,-t12q,-t34q,uq),box6(s24,s25,s13,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<5>(zbq,t12q,t34q,uq),  box6(s13,s34,s25,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<5>(zbq,t12q,-t34q,-uq),box6(s14,s34,s25,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<5>(zbq,-t12q,t34q,-uq),box6(s23,s34,s15,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<5>(zbq,-t12q,-t34q,uq),box6(s24,s34,s15,3),0) << "\t";
//            cout << productCoeff(qq2yygz1LC<6>(zbq,t12q,t34q,uq),  box6(s15,s25,s34,3),0) << "\t";

//            cout << box6(s13,s15,s24,1).getCoefficient(1) << "\t";
//            cout << box6(s14,s15,s23,1).getCoefficient(1) << "\t";
//            cout << box6(s23,s25,s14,1).getCoefficient(1) << "\t";
//            cout << box6(s24,s25,s13,1).getCoefficient(1) << "\t";
//            cout << box6(s13,s34,s25,1).getCoefficient(1) << "\t";
//            cout << box6(s14,s34,s25,1).getCoefficient(1) << "\t";
//            cout << box6(s23,s34,s15,1).getCoefficient(1) << "\t";
//            cout << box6(s24,s34,s15,1).getCoefficient(1) << "\t";
//            cout << box6(s15,s25,s34,1).getCoefficient(1) << "\t";

//            cout << qq2yygz1LC<4>(zbr,t12r,t34r,ur).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<4>(zbr,t12r,-t34r,-ur).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<4>(zbr,-t12r,t34r,-ur).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<4>(zbr,-t12r,-t34r,ur).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<5>(zbr,t12r,t34r,ur).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<5>(zbr,t12r,-t34r,-ur).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<5>(zbr,-t12r,t34r,-ur).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<5>(zbr,-t12r,-t34r,ur).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<6>(zbr,t12r,t34r,ur).getCoefficient(-1) << "\t";

//            cout << qq2yygz1LC<4>(zbq,t12q,t34q,uq).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<4>(zbq,t12q,-t34q,-uq).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<4>(zbq,-t12q,t34q,-uq).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<4>(zbq,-t12q,-t34q,uq).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<5>(zbq,t12q,t34q,uq).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<5>(zbq,t12q,-t34q,-uq).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<5>(zbq,-t12q,t34q,-uq).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<5>(zbq,-t12q,-t34q,uq).getCoefficient(-1) << "\t";
//            cout << qq2yygz1LC<6>(zbq,t12q,t34q,uq).getCoefficient(-1) << "\t";

            // 1-COLLINEAR kinematics
            lambda = 0.;
            pGen.generateFSMomenta(randoms);

//            const double ratio1 = square(p[1]-p[5]-p[3])/square(p[1]-p[5]-p[4]);
//            cout << square(p[1]-p[5]-p[3]) << "\t" << square(p[2]-p[4]) << "\t";
//            cout << square(p[1]-p[5]-p[4]) << "\t" << square(p[2]-p[3]) << "\t";

//            cout << ratio1 << "\t";
//            cout << t12 << "\t" << t34 << "\t" << u << "\t";
//            cout << "\t" << times(coll1(l,z,ratio1),inverse(qq2yyg<0>(zb,t12,t34,u),3),3) << "\t\t";
//            cout << times(coll2(l,z,ratio1),inverse(qq2yyg<0>(zb,t12,t34,u),3),3) << "\t\t";
//            cout << qq2yyg<0>(zb,t12,t34,u).getCoefficient(0)/((CounterForge::Pqq<0>(z)).getCoefficient(0)/z*qq2yy<0,0>(ratio1)/l/zb) << "\t";
//            cout << inverse(qq2yyg<0>(zb,t12,t34,u),3) << "\t";
//            std::cout << 2*consts::Pi*productCoeff(
//                                                   coll1(l,z,ratio1)+coll2(l,z,ratio1),
//                                                   inverse(qq2yyg<0>(zb,t12,t34,u),3),
//                                                   0
//                                                   )/zb << "\t";
            cout << endl;

        }
    }

    return 0;

}

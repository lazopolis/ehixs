/**
 * \file   counterforge_test.cpp
 * \author Simone Lionetti
 * \date   November 2014
 * \brief  This file contains the tests for the CounterForge namespace
 */

//#include "gtest/gtest.h"
#include "xcodetest.h"
#include "counterforge.h"
using namespace std;

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

/// \note This test is expected to break down when cGamma is activated (cGamma ≠ 1)

int main(int argc, char**argv)
{

    size_t& acc = Expansion<Parameter::epsilon, double>::accuracy;

    // Testing f1_1overz to order 5
    acc = 5;
    Expansion<Parameter::epsilon, double> myf1_1overz = CounterForge::f1_1overz(0.123456);
    ASSERT_TRUE(
                (myf1_1overz.getCoefficient(-2) == 0.) &&
                (fabs(myf1_1overz.getCoefficient(-1) - 4.18374) <= 0.00001) &&
                (fabs(myf1_1overz.getCoefficient( 0) - 2.48360) <= 0.00001) &&
                (fabs(myf1_1overz.getCoefficient( 1) - 2.03223) <= 0.00001) &&
                (fabs(myf1_1overz.getCoefficient( 2) - 1.87346) <= 0.00001)
                );

    // Testing f1_1overz to order 5
    acc = 5;
    Expansion<Parameter::epsilon, double> myf1_1minus1overz = CounterForge::f1_1minus1overz(0.123456);
    ASSERT_TRUE(
                (myf1_1minus1overz.getCoefficient(-2) == -2.) &&
                (fabs(myf1_1minus1overz.getCoefficient(-1) - 3.92020) <= 0.00001) &&
                (fabs(myf1_1minus1overz.getCoefficient( 0) - 4.07877) <= 0.00001) &&
                (fabs(myf1_1minus1overz.getCoefficient( 1) - 2.89846) <= 0.00001) &&
                (fabs(myf1_1minus1overz.getCoefficient( 2) - 2.51292) <= 0.00001)
                );

    // Testing r4 to order 10
    acc = 10;
    Expansion<Parameter::epsilon, double> myr4 = CounterForge::r4();
    bool r4passed = true;
    for (int i = 0; i<myr4.maxTerm(); ++i)
        r4passed = r4passed && (-2.*myr4.getCoefficient(i)/static_cast<double>(QCD::Nc+1./QCD::Nc)==pow(2.,i));
    ASSERT_TRUE(r4passed);

    // Testing r3 to order 3
    acc = 3;
    cout.precision(10);
    Expansion<Parameter::epsilon, double> myr3 = CounterForge::r3(0.123456,CounterForge::Scheme::CDR,acc);
    ASSERT_TRUE(
                (myr3.getCoefficient(-2) == -3.) &&
                (fabs(myr3.getCoefficient(-1) - 5.18302) <= 0.00001) &&
                (fabs(myr3.getCoefficient( 0) - 7.37089) <= 0.00001)
                );
    myr3 = CounterForge::r3(0.987654,CounterForge::Scheme::CDR,acc);
    ASSERT_TRUE(
                (myr3.getCoefficient(-2) == -3.) &&
                (fabs(myr3.getCoefficient(-1) + 13.1501) <= 0.0001) &&
                (fabs(myr3.getCoefficient( 0) + 17.4714) <= 0.0001)
                );
    CounterForge myforge(CounterForge::Scheme::CDR);
    ASSERT_TRUE(
                myr3==myforge.fastr3(0.987654)
                );
    ASSERT_EQUAL(myforge.fastPqq<1>(0.123456),myforge.Pqq<1>(0.123456));

//    ::testing::InitGoogleTest(&argc, argv);
//    return RUN_ALL_TESTS();

    return 0;

}

/**
 * \file   expansion_test.cpp
 * \author Simone Lionetti
 * \date   November 2014
 * \brief  This file contains the tests for the Expansion class
 */

#include "gtest/gtest.h"
//#include "xcodetest.h"
#include "expansion.h"
using namespace std;

TEST(ExpansionBasicTest,expansion_test)
{

    Expansion<Parameter::epsilon, double>::accuracy = 0;
    Expansion<Parameter::epsilon, double> pippo(-2, {1,3.14,17});
    Expansion<Parameter::epsilon, double> pluto(0, {-7,1.13});
    Expansion<Parameter::epsilon, double> cani(-2, {1.,3.14,10,1.13});
    Expansion<Parameter::epsilon, double> epsilon(1,1.);
    vector<double> roba = {-1.,-17.14,-14.74};
    Expansion<Parameter::epsilon, double> robaexp(-1,roba);
    ASSERT_TRUE(pippo+pluto==cani);
    ASSERT_TRUE((epsilon*pippo-2.*pluto+robaexp).isZero());
    pippo.setCoefficient(1,0.);
    pippo.setCoefficient(2,0.);
    pippo.setCoefficient(3,0.);
    cout << pippo << endl;
    cout << inverse(pippo,5,false) << endl;
    cout << times(pippo,inverse(pippo,8,false),8) << endl;

    Expansion<Parameter::alphas, double>::accuracy = 2;
    Expansion<Parameter::alphas, double> orazio (0, {1,1});
    Expansion<Parameter::alphas, double> gaspare(0, {1,-1});
    Expansion<Parameter::alphas, double> one(0,1.,true);
    ASSERT_TRUE(gaspare*orazio==one);

}

int main(int argc, char**argv)
{

    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();

    return 0;

}

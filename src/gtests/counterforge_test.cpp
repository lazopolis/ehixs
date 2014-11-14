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

int main(int argc, char**argv)
{

    Expansion<Parameter::epsilon, double>::accuracy = 3;
    const size_t& acc = Expansion<Parameter::epsilon, double>::accuracy;
    cout << CounterForge::geometric<Parameter::epsilon, double>(1.,acc) << endl
    << CounterForge::geometric<Parameter::epsilon, double>(2.,acc) << endl
        << Expansion<Parameter::epsilon, double>(2,0.5,true) << endl;

    cout << CounterForge::f2() << endl;
    cout << "r3: " << CounterForge::r3(0.2345) << "\n"
        << "r4: " << CounterForge::r4() << "\n";

    Expansion<Parameter::alphas, double>::accuracy = 2;
    Expansion<Parameter::alphas, double> orazio (0, {1,1});
    Expansion<Parameter::alphas, double> gaspare(0, {1,-1});
    Expansion<Parameter::alphas, double> one(0,1.,true);
    ASSERT_TRUE(gaspare*orazio==one);
    
//    ::testing::InitGoogleTest(&argc, argv);
//    return RUN_ALL_TESTS();

    return 0;

}

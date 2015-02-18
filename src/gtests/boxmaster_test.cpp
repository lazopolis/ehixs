/**
 * \file   boxmaster_test.cpp
 * \author Simone Lionetti
 * \date   February 2015
 * \brief  This file contains the tests for the CounterForge namespace
 */

//#include "gtest/gtest.h"
#include "xcodetest.h"
#include "boxmaster.h"
#include <chrono>
#include <random>
#include <cfloat>
using namespace std;

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

/// \note This test is expected to break down when cGamma is activated (cGamma ≠ 1)

int main(int argc, char**argv)
{

    Expansion<Parameter::epsilon, double>::accuracy = 0;

    size_t seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::default_random_engine generator(seed);
    std::uniform_real_distribution<double> distribution(-10.,10.);

    for (size_t i = 0; i<10; ++i){
        const double s = distribution(generator);
        const double t = distribution(generator);
        const double M2 = distribution(generator);
        Expansion<Parameter::epsilon, double> diff;
        diff = times(
                     bubble(s,3)+bubble(t,3)-bubble(M2,3)+
                     times(
                           Expansion<Parameter::epsilon, double>(1,M2-s-t,true),
                           box6(s,t,M2,2),
                           2
                           ),
                     2.*Expansion<Parameter::epsilon, double>(-1,{1.,-2.},true),
                     3
                     ) - s*t*box(s,t,M2,3);
        bool res = true;
        for (int i = diff.minTerm(); i<diff.maxTerm(); ++i)
            if (diff.getCoefficient(i)>10.*DBL_EPSILON) res = false;
        cout << true << "=" << res << endl;
    }


//    ::testing::InitGoogleTest(&argc, argv);
//    return RUN_ALL_TESTS();

    return 0;

}

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

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

/// \note This test is expected to break down when cGamma is activated (cGamma ≠ 1)

int main(int argc, char**argv)
{

    Expansion<Parameter::epsilon, double>::accuracy = 0;

    std::size_t seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::default_random_engine generator(static_cast<unsigned int>(seed));
    std::uniform_real_distribution<double> distribution(-10.,10.);

    for (std::size_t i = 0; i<100; ++i){
        const double s = distribution(generator);
        const double t = distribution(generator);
        const double M2 = distribution(generator);
        //cout << "s = " << s << "\tt = " << t << "\tM2 = " << M2 << "\t";
        Expansion<Parameter::epsilon, double> diff;
        diff = times(
                     bubble(s,3)+bubble(t,3)-bubble(M2,3)+box6(s,t,M2,2),
                     2.*Expansion<Parameter::epsilon, double>(-1,{1.,-2.},true),
                     3
                     ) - s*t*box(s,t,M2,3);
        bool res = true;
        //cout << "diff = " << diff << "\t";
        for (int i = diff.minTerm(); i<diff.maxTerm(); ++i)
            if (diff.getCoefficient(i)>1000.*DBL_EPSILON) res = false;
        std::cout << true << " = " << res << std::endl;
    }

    for (std::size_t i = 0; i<100; ++i){
        const double s = distribution(generator);
        const double t = distribution(generator);
        //cout << "s = " << s << "\tt = " << t << "\tM2 = " << M2 << "\t";
        Expansion<Parameter::epsilon, double> diff;
        diff = times(
                     bubble(s,4)-bubble(t,4),
                     Expansion<Parameter::epsilon, double>(-1,{1.,-2.},true),
                     3
                     )/(s-t) - tri2(s,t,3);
        bool res = true;
        //cout << "diff = " << diff << "\t";
        for (int i = diff.minTerm(); i<diff.maxTerm(); ++i)
            if (diff.getCoefficient(i)>1000.*DBL_EPSILON) res = false;
        std::cout << true << " = " << res << std::endl;
    }

    // Testing triangle series expansion
//    for (double sr = 1.; sr>1.e-16; sr/=2){
//        const double s = 1.+sr;
//        const Expansion<Parameter::epsilon, double> full = times(
//                                                                 bubble(s,4)-bubble(1.,4),
//                                                                 Expansion<Parameter::epsilon, double>(-1,{1.,-2.},true),
//                                                                 3
//                                                                 )/(s-1.);
//        const Expansion<Parameter::epsilon, double> b1 = times(
//                                                                 bubble(s,4),
//                                                                 Expansion<Parameter::epsilon, double>(-1,{1.,-2.},true),
//                                                                 4
//                                                                 )/(s-1.);
//        const Expansion<Parameter::epsilon, double> b2 = -times(
//                                                               bubble(1.,4),
//                                                               Expansion<Parameter::epsilon, double>(-1,{1.,-2.},true),
//                                                               4
//                                                               )/(s-1.);
//        const Expansion<Parameter::epsilon, double> taylor = tri2(s,1.,3);
//        std::cout << sr << "\t\t" << full  << "\t\t" << b2 << "\t\t" << taylor << std::endl;
//    }

//    ::testing::InitGoogleTest(&argc, argv);
//    return RUN_ALL_TESTS();

    return 0;

}

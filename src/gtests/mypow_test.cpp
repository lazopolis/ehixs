/**
 * \file   mypow_test.cpp
 * \author Simone Lionetti
 * \date   April 2015
 * \brief  Tests for my pow function
 */

//#include "gtest/gtest.h"
#include "xcodetest.h"
#include "cln/rational.h"
#include "cln/rational_io.h"
#include "cln/float.h"
#define BaseT cln::cl_RA
#include "mypow.h"
#undef BaseT

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

int main(int argc, char**argv)
{
    cln::cl_RA pippo = 3;
    std::cout << pow<3>(pippo) << std::endl;
    std::cout << pow<1>(pippo) << std::endl;
    std::cout << pow<-1>(2) << std::endl;
    std::cout << pow<10>(2) << std::endl;
    std::cout << pow<-6>(2) << std::endl;
    std::cout << pow<7>(-2) << std::endl;

    std::cout << 1.032e60+3-1.032e60 << std::endl;
    std::cout << cln::rational(1.032e60)+cln::rational(3.)-cln::rational(1.e60)-cln::rational(3.2e58) << std::endl;
//    std::cout << cln::rational(1.032e60)+cln::rational(3.)-cln::rational(1.e60)-cln::rational(3.2e58) << std::endl;
//    std::cout << cln::rational(1.e60)+cln::rational(3.2e58) << std::endl;

//    ::testing::InitGoogleTest(&argc, argv);
//    return RUN_ALL_TESTS();

    return 0;

}

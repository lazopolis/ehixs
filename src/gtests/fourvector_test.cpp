/**
 * \file   fourvector_test.cpp
 * \author Simone Lionetti
 * \date   September 2014
 * \brief  This file contains the tests for the FourVector class
 */

#include "gtest/gtest.h"
#include "fourvector.h"
#include <cfloat>
using namespace std;

TEST(FourVectorTest,fourvectortest)
{
    
    // Testing Axis
    ASSERT_TRUE( Axis::x+Axis::x == Axis::y );

    // Testing constructors (effectively later)
    FourVector v;
    FourVector n = FourVector(1.,0.,0.,1.);
    const double nbarfoo[4] = {1.,0.,0.,-1.};
    FourVector nbar = FourVector(nbarfoo);
    FourVector ncpy = n;

    // Testing operator-, operator+ and operator==
    ASSERT_TRUE(n-nbar==n+(-nbar)) << "FourVector: basic sum and difference not working correctly.";

    // Testing operators +=, -=, *=, /=
    nbar*=2.;
    nbar+=FourVector(3., 0., 1., 2.);
    nbar-=FourVector(2., 0.,-2., 0.);
    nbar/=3.;
    ASSERT_TRUE( nbar == FourVector(1.,0.,1.,0.) ) << "FourVector: error in (at least) one of the operators +=, -=, *= or /=.";

    // Testing boosts, Y, eta, operator* and operator/ (with doubles)
    const double beta = 1./consts::Pi;
    const double gamma = 1./sqrt(1.-beta*beta);
    const double Y = log(gamma*(1.+beta));
    v = gamma*n*(1+beta);
    n.boost(beta);
    ASSERT_TRUE( n == v ) << "FourVector: boost in one direction by velocity beta is faulty.";
    ASSERT_TRUE( fabs(v.rapBoost(-Y)[3] - (n/(gamma*(1.+beta)))[3]) < DBL_EPSILON ) << "FourVector: error in rapidity boost.";
    ASSERT_TRUE( (v.boost(0.1, 0.1, 0.1).boost(-0.1, -0.1, -0.1)-ncpy).abs() < sqrt(2.)*DBL_EPSILON ) << "FourVector: generic boost is inconsistent.";
    ASSERT_TRUE( FourVector(0., 0., 1., 1.).abs() == sqrt(2.) ) << "FourVector: three-vector modulus does not work.";
    ASSERT_TRUE( FourVector(0., 0., 0.3, sqrt(3.)).T(Axis::z) == 0.3 ) << "FourVector: problem when computing transverse component.";
    ASSERT_TRUE( fabs(FourVector(4.4,1.2,2.4,0.).rapBoost(0.12345).Y() - 0.12345) < DBL_EPSILON ) << "FourVector: rapidity check failed.";
    ASSERT_TRUE( fabs(FourVector(0.5*sqrt(2),0.,0.5,0.5).Y() - FourVector(0.5*sqrt(2),0.,0.5,0.5).eta()) < DBL_EPSILON ) << "FourVector: rapidity and pseudo-rapidity do not correspond.";
    
    // Testing rotate and phi
    ASSERT_TRUE( fabs(FourVector(1,1,0,0).rotate(consts::Pi/6.).phi() - consts::Pi/6.) < DBL_EPSILON ) << "FourVector: error in azimuthal rotation and/or angle.";

    // Testing square and scalar product
    ASSERT_TRUE( FourVector(1.,0.,0.,0.5)*FourVector(1.,0.,0.2,0.5) == 0.75 ) << "FourVector: scalar product between FourVectors not working correctly.";
    ASSERT_TRUE( square(FourVector(1.5432,0.,0.,1.5432)) == 0. ) << "FourVector: square Lorentz norm does not pass basic test.";

    return;

}

int main(int argc, char**argv)
{

    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();

}

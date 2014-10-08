/** testing UserInterface.*:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>



using namespace std;

#include "gtest/gtest.h"
#include "higgs_eft.h"
#include "math.h"
class HiggsEFTMe: public ::testing::Test {
protected:
    virtual void SetUp()
        {
            err=1e-4;
            z=0.72021484375;
            lz = log(1.-z);
        }
    double err;
    double z;
    double lz;
};


// testing the log(muf/mh) *INDEPENDENT* terms of delta and plus
// partonic cross sections within the Higgs eft, aka Egg_LO, NLO, NNLO and N3LO
// against ihixs3
//
// the delta pieces are pure numbers
// the plus pieces are pure numbers times log(1-z)^a
// we have tested here with z=0.72021484375

TEST_F(HiggsEFTMe,LO_delta)
{
    const double res = HEFT::n_LO_delta();
    const double exp = 1.0;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,NLO_delta)
{
    const double res = HEFT::n_NLO_delta();
    const double exp = 9.8696;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,NNLO_delta)
{
    const double res = HEFT::n_NNLO_delta();
    const double exp = 13.61056;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,N3LO_delta)
{
    const double res = HEFT::n_N3LO_delta();
    const double exp = 1124.308;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,NLO_plus)
{
    const double res = HEFT::n_NLO_D1()*lz;
    const double exp = -15.2848;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,NNLO_plus)
{
    const double res =   HEFT::n_NNLO_D0()
                        +HEFT::n_NNLO_D1()*lz
                        +HEFT::n_NNLO_D2()*lz*lz
                        +HEFT::n_NNLO_D3()*lz*lz*lz;
    const double exp = 161.259;
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
}

TEST_F(HiggsEFTMe,N3LO_plus)
{
    const double res =   HEFT::n_N3LO_D0()
                        +HEFT::n_N3LO_D1()*lz
                        +HEFT::n_N3LO_D2()*lz*lz
                        +HEFT::n_N3LO_D3()*lz*lz*lz
                        +HEFT::n_N3LO_D4()*pow(lz,4.)
                        +HEFT::n_N3LO_D5()*pow(lz,5.);
    
    const double exp = 23173.424870322367;
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
}


int main(int argc, char**argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return  RUN_ALL_TESTS();
    return 0;
}

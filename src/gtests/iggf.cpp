/** testing ehixs:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>


#include "Production.h"
//#include "GluonFusionInclusive.h"
#include "GluonFusion.h"
#include "Process.h"
#include "GGF_Inclusive.h"
using namespace std;

#include "gtest/gtest.h"


/*
TEST(interpolator_test,nlo_reg)
{
     IME_Interpolator nlo_reg(&gg_NLO_reg);
     nlo_reg.initialize();
     double x=0.24;
     double xs=nlo_reg.give_f(x);
     
     double expected_xs = gg_NLO_reg(x);
     double err=1e-7;
     cout<<"\nInterpolated = "<<xs<<" vs "<<expected_xs<<" = expected"<<endl;
     EXPECT_LT(fabs(xs-expected_xs)/expected_xs,err);
}
*/
/*
TEST(interpolator_test,nnlo_reg)
{
     IME_Interpolator nnlo_reg(&gg_NNLO_reg2);
     nnlo_reg.initialize();
     double x=1.0-0.0002;
     double xs=nnlo_reg.give_f(x);
     
     double expected_xs = gg_NNLO_reg(x);
     double err=1e-7;
     cout<<"\nInterpolated = "<<xs<<" vs "<<expected_xs<<" = expected"<<endl;
     EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}
*/
int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}

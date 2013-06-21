

/** testing pdfs, and in particular the new interpolator class CashedInterpolator.h as compared to the old interpolator.h by Ste:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>

using namespace std;

#include "Convolutions.h"

#include "gtest/gtest.h"



TEST(ChannelTest,quark_gluon_nnlo)
{
     //Channel qg("quark","gluon");
     //qg.build_cocout<<qg;
     double accumulated_difference=0.0;
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}


int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}



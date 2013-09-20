/** testing ehixs:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>


#include "ggf_headers.h"

using namespace std;

#include "gtest/gtest.h"

/*
TEST(nlo_gluon_quark_test,one_over_e)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-1;
     chs.push_back(channel_name("gluon","quark","NLO",pole));
     double mur=1.0;
     double muf=1.0;
     
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs = 0.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
}


TEST(nlo_quark_gluon_test,one_over_e)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-1;
     chs.push_back(channel_name("quark","gluon","NLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs = 0.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/
TEST(nlo_gluon_gluon_test,one_over_e)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-1;
     chs.push_back(channel_name("gluon","gluon","NLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs = 0.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
}

int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}
































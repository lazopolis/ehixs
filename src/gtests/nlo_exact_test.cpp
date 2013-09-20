/** testing ehixs:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>

#include "ggf_headers.h"
using namespace std;

#include "gtest/gtest.h"


TEST(lo_exact,finite)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("gluon","gluon","LO",pole,"exact"));
     double mur=1.0;
     double muf=1.0;
     int pertord=0;
     check_sectors(chs,mur,muf,pole,pertord,res,"exact");
     double xs=res[0];
     double err=res[1];
     
     double expected_xs = 7.614;// +- 0.004
     EXPECT_LT(fabs(xs-expected_xs),err);
}

/*
TEST(nlo_soft_exact,finite)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("gluon","gluon","NLO",pole,"exact"));
     double mur=1.0;
     double muf=1.0;
     int pertord=1;
     vector<int> one_sector;one_sector.push_back(3);
     check_sectors(chs,mur,muf,pole,pertord,res,"exact",one_sector);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs = 3.3309;// +- 0.0016
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/
/*
TEST(nlo_hard_exact,finite)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("gluon","gluon","NLO",pole,"exact"));
     double mur=1.0;
     double muf=1.0;
     int pertord=1;
     vector<int> one_sector;one_sector.push_back(3);
     check_sectors(chs,mur,muf,pole,pertord,res,"exact",one_sector);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs = 3.3309;// +- 0.0016
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/
/*
TEST(nlo_exact_real_gg,finite)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("gluon","gluon","NLO",pole,"exact"));
     double mur=1.0;
     double muf=1.0;
     int pertord=1;
     vector<int> one_sector;one_sector.push_back(3);
     check_sectors(chs,mur,muf,pole,pertord,res,"exact",one_sector);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs = 3.3309;// +- 0.0016
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/



int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}
































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

TEST(nnlo_gluon_quark_test,one_over_e_cubed)
{
     int pole=-3;
     vector<channel_name> chs;
     double res[2];
     chs.push_back(channel_name("gluon","quark","NNLO",pole));
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

/*
TEST(nnlo_gluon_quark_test,one_over_e_square)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-2;
     chs.push_back(channel_name("gluon","quark","NNLO",pole));
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

/*
TEST(nnlo_gluon_quark_test,one_over_e)
{
     vector<channel_name> chs;
     double res[2];
     int pole=-1;
     chs.push_back(channel_name("gluon","quark","NNLO",pole));
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

TEST(nnlo_gluon_quark_test,finite)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("gluon","quark","NNLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
//     vector<int> spec_sectors;spec_sectors.push_back(27);spec_sectors.push_back(28);spec_sectors.push_back(29);
//     check_sectors(chs,mur,muf,pole,pertord,res,spec_sectors);
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs = -0.461/2.0;
     EXPECT_LT(fabs(xs-expected_xs),err);
}



int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}
































/** testing ehixs:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>
using namespace std;


#include "ggf_headers.h"



#include "gtest/gtest.h"


TEST(LO_test,xsection_muf_muf_mh)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("gluon","gluon","LO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=2;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs =7.608;
     EXPECT_LT(fabs(xs-expected_xs),err);
}
 

/*
TEST(NLO_test,gg_xsection_muf_muf_mh)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("gluon","gluon","NLO",pole));
     double mur=1.0;
     double muf=1.0;
     int pertord=1;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs =7.923;// NLO piece alone. The total NLO (including the LO contribution with NLO pdfs) is 13.845;
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/
/*
TEST(NLO_test,gg_xsection_mu_wild)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("gluon","gluon","NLO",pole));
     double mur=0.125;
     double muf=0.56789;
     int pertord=1;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs =24.2895-12.1688;// NLO piece alone. The total NLO (including the LO contribution with NLO pdfs) is 24.2895;
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/
/*
TEST(NLO_test,qg_xsection_mu_is_mh)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("quark","gluon","NLO",pole));
     chs.push_back(channel_name("gluon","quark","NLO",pole));

     double mur=1.0;
     double muf=1.0;
     int pertord=1;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs =-0.1966;
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/
/*
TEST(NLO_test,qg_xsection_mu_wild)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("quark","gluon","NLO",pole));
     chs.push_back(channel_name("gluon","quark","NLO",pole));
     
     double mur=0.4356;
     double muf=0.56789;
     int pertord=1;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs =0.42925;
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/
/*
TEST(NLO_test,qqbar_xsection_mu_wild)
{
     vector<channel_name> chs;
     double res[2];
     int pole=0;
     chs.push_back(channel_name("quark","antiquark","NLO",pole));
     
     double mur=0.125;
     double muf=0.56789;
     int pertord=1;
     check_sectors(chs,mur,muf,pole,pertord,res);
     double xs=res[0];
     double err=res[1];
     
     double expected_xs =0.0248037;
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
































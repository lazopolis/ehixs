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
#include "OneDInterpolator.h"
using namespace std;

#include "gtest/gtest.h"


/*
TEST(interpolator_test,x_squared)
{
     DummyClass my_object;
     Interpolator interpol(&my_object);
     interpol.initialize();
     double x=1.0-1e-16;
     double xs=interpol.give_f(x);
     
     double expected_xs = my_object.ff(x);
     cout<<setprecision(16)<<"\nres="<<xs<<" vs expected="<<expected_xs<<endl;
     double err=1e-8;
     EXPECT_LT(fabs(xs-expected_xs),err);
}
*/


double ff( double  x){return pow(x*(1.0-x),3.24);}

TEST(Interpolator_peaking_at_zero_and_one,outside_low)
{
     Interpolator_peaking_at_zero_and_one interpol(&ff);
     interpol.initialize();
     double x=0.003;
     double xs=interpol.give_f(x);
     
     double expected_xs = ff(x);
     cout<<setprecision(16)<<"\nres="<<xs<<" vs expected="<<expected_xs<<endl;
     double err=1e-8;
     EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}

TEST(Interpolator_peaking_at_zero_and_one,outside_high)
{
     Interpolator_peaking_at_zero_and_one interpol(&ff);
     interpol.initialize();
     double x=0.997;
     double xs=interpol.give_f(x);
     
     double expected_xs = ff(x);
     cout<<setprecision(16)<<"\nres="<<xs<<" vs expected="<<expected_xs<<endl;
     double err=1e-8;
     EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}

TEST(Interpolator_peaking_at_zero_and_one,inside_low)
{
     Interpolator_peaking_at_zero_and_one interpol(&ff);
     interpol.initialize();
     double x=1.0-0.98999;
     double xs=interpol.give_f(x);
     
     double expected_xs = ff(x);
     cout<<setprecision(16)<<"\nres="<<xs<<" vs expected="<<expected_xs<<endl;
     double err=1e-8;
     EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}

TEST(Interpolator_peaking_at_zero_and_one,inside_high)
{
     Interpolator_peaking_at_zero_and_one interpol(&ff);
     interpol.initialize();
     double x=0.98999;
     double xs=interpol.give_f(x);
     
     double expected_xs = ff(x);
     cout<<setprecision(16)<<"\nres="<<xs<<" vs expected="<<expected_xs<<endl;
     double err=1e-8;
     EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}
/*
TEST(interpolator_test,luminosity)
{
     double tau=1.23e-8;
     InterpolatedLuminosity* lumi=new InterpolatedLuminosity(5,125.0,125.0,2,"MSTW",0,tau);
     lumi->add_pair(
                      Luminosity::pdf_desc(0,0,0,0),
                      Luminosity::pdf_desc(0,0,0,0)
                      );
     
     
     
     LuminosityInterpolator LI(lumi);
     LI.initialize();
     
     double x1=0.4567;
     double z=.0723;
     
     double xs=LI.give_f(z);
     
     double expected_xs = 0.34;
     double err=1e-8;
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
































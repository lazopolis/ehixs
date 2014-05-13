//    testing gamma* gamma*
//    Achilleas Lazopoulos, lazopoli@phys.ethz.ch


#include <iostream>
#include <cmath>
#include <string>
using namespace std;

#include "gtest/gtest.h"

#include "event.h"
#include "gamma_star_gamma_star_me.h"

#include "virtual_amplitude.h"

class Masters :  public ::testing::Test {
protected:
    virtual void SetUp() {
        kbr.SetMaxMomentumID(6);
        kbr.Set(1,2, 955.1019552011871);
        kbr.Set(1,3, -731.5762320546179);
        kbr.Set(1,4, -218.5257231465693);
        kbr.Set(2,3, -218.5257231465692);
        kbr.Set(2,4,-731.576232054618);
        kbr.Set(3,4, 955.1019552011871);
        kbr.Set(3, 1.0 );
        kbr.Set(4, 4.0 );
        kbr.compute_dimensionless_invariants();
        kk.push_back(kbr.q(3));
        kk.push_back(kbr.q(4));
        kk.push_back(kbr.q(1,4));
        kk.push_back(kbr.q(1,3));
    }
    KinematicInvariants kbr;
    vector<double> kk;
    GstarVirtual V;
};

TEST_F(Masters,I1em1)
{
    const double test_result = V.Master(1,kbr,-1);
    const double expected = 1.0;
    cout <<"\t\t"<<V.Master(1,-1);
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected
    <<endl;
}

TEST_F(Masters,I1)
{
    const double test_result = V.Master(1,kbr,0);
    const double expected = 8.861818094;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I1e1)
{
    const double test_result =  V.Master(1,kbr,1);
    const double expected = 37.97604184;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}



TEST_F(Masters,I2)
{
    const double test_result =  V.Master(2,kbr,0);
    const double expected = 7.475523733;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I3)
{
    const double test_result = V.Master(3,kbr,0);
    const double expected = 3.474914361;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I4)
{
    const double test_result = V.Master(4,kbr,0);
    const double expected = 2.266616666;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I5)
{
    const double test_result = V.Master(5,kbr,0);
    const double expected = 7.475523733;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I6)
{
    const double test_result = V.Master(6,kbr,0);
    const double expected = 8.861818094;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I7)
{
    const double test_result = V.Master(7,kbr,0);
    const double expected = 2.0;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I8)
{
    const double test_result = V.Master(8,kbr,0);
    const double expected = 2.0;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I9)
{
    const double test_result = V.Master(9,kbr,0);
    const double expected = 41.00601265;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I10)
{
    const double test_result = V.Master(10,kbr,0);
    const double expected = 41.00601265;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I11)
{
    const double test_result = V.Master(11,kbr,0);
    const double expected = -37.88834699;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(Masters,I12)
{
    const double test_result = V.Master(12,kbr,0);
    const double expected = -11.51515106;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

class NewVirtualNLO :  public ::testing::Test {
protected:
    NewVirtualNLO(){};
    virtual void SetUp() {
        kbr.SetMaxMomentumID(6);
        kbr.Set(1,2, 955.1019552011871);
        kbr.Set(1,3, -731.5762320546179);
        kbr.Set(1,4, -218.5257231465693);
        kbr.Set(2,3, -218.5257231465692);
        kbr.Set(2,4,-731.576232054618);
        kbr.Set(3,4, 955.1019552011871);
        kbr.Set(3, 1.0 );
        kbr.Set(4, 4.0 );
        kbr.compute_dimensionless_invariants();
        
        
        }
    GstarVirtual V;
    KinematicInvariants kbr;
};


TEST_F(NewVirtualNLO,implementation)
{
    double me_sq = V.Evaluate(kbr);
    
    const double test_result = me_sq;
    const double expected =  453.13838490;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected
    <<"\n--> fails because of pi^2/4 term now added"
    <<endl;
}


class NewVirtualNLOSoft :  public ::testing::Test {
protected:
    
    virtual void SetUp() {
        kbr.SetMaxMomentumID(6);
        kbr.Set(1,2, 955.1019552011871);
        kbr.Set(1,3, -731.5762320546179);
        kbr.Set(1,4, -218.5257231465693);
        kbr.Set(2,3, -218.5257231465692);
        kbr.Set(2,4,-731.576232054618);
        kbr.Set(3,4, 955.1019552011871);
        kbr.Set(3, 1.0 );
        kbr.Set(4, 4.0 );
        kbr.compute_dimensionless_invariants();
    }
    GstarGstarMeNLOSoft V;
    KinematicInvariants kbr;
};




TEST_F(NewVirtualNLOSoft,CataniE2)
{
    const double test_result = V.Catani(kbr,-2);
    const double expected = 0.0;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(NewVirtualNLOSoft,CataniE1)
{
    const double test_result = V.Catani(kbr,-1);
    const double expected = 0.0;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(NewVirtualNLOSoft,BornEpsilon)
{
    const double test_result = V.born_.e(kbr);
    const double expected = -224.4595905;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(NewVirtualNLOSoft,BornEpsilonSquare)
{
    const double test_result = V.born_.e2(kbr);
    const double expected = 135.512277;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(NewVirtualNLOSoft,TotalVirtualForSinglePoint)
{
    const double test_result = V.eval_me(kbr);
    const double expected = 184.9022410;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}


class Masters2Loop :  public ::testing::Test{
protected:
    virtual void SetUp() {
        kbr.SetMaxMomentumID(6);
        kbr.Set(1,2, 955.1019552011871);
        kbr.Set(1,3, -731.5762320546179);
        kbr.Set(1,4, -218.5257231465693);
        kbr.Set(2,3, -218.5257231465692);
        kbr.Set(2,4,-731.576232054618);
        kbr.Set(3,4, 955.1019552011871);
        kbr.Set(3, 1.0 );
        kbr.Set(4, 4.0 );
        kbr.compute_dimensionless_invariants();
        kk.push_back(kbr.q(3));
        kk.push_back(kbr.q(4));
        kk.push_back(kbr.q(1,4));
        kk.push_back(kbr.q(1,3));
        
       
        
    }
    
    double check_master(int whichmaster, int eps){
        const double test_result = V.Master(whichmaster,kbr,eps);
        const double expected = MasterResult[whichmaster-1][eps+3];
        cout<<"res="<<test_result<<" | "<<expected<<" =expected "<<endl;
        return fabs(test_result-expected);
    }
    
    double check_coeff(int whichmaster, int eps){
        const double test_result = V.Coefficient(whichmaster,kbr,eps);
        const double expected = CoeffsResult[whichmaster-1][eps+2];
        cout<<"res="<<test_result<<" | "<<expected<<" =expected "<<endl;
        const double diff = fabs(test_result - expected); 
        if (expected==0.0) return diff;
        else return diff/fabs(expected);

    }
    
    
    KinematicInvariants kbr;
    vector<double> kk;
    GstarVirtualVirtual V;
    static const double MasterResult[12][6];
    static const double CoeffsResult[12][6];
};

const double Masters2Loop::MasterResult[12][6]={
    {0., 0., 0.5719958008e-1, .5405262348, 3.178266042, 14.26367336},
    {0., 0., .1914916591, 1.346805519, 6.826304944, 26.87219583}, 
    {0., .5000000000, 9.361818094, 71.42030413, 361.7616368,0.0},
    {0., .5000000000, 7.975523733, 48.32395712, 204.2884877,0.0}, 
    {0., .5000000000, 7.975523733, 53.83095062, 245.9287926,0.0}, 
    {0., .5000000000, 9.361818094, 64.20011954, 299.6327806,0.0},        
    {0., .5000000000, 2.500000000, 2.920263730, -2.802795146,0.0}, 
    {0., .5000000000, 2.500000000, 2.920263730, -2.802795146,0.0},
    {0., 0., 41.00601265, 440.8356645,0.0,0.0}, 
    {0., 0., 41.00601265, 392.2178212,0.0,0.0}, 
    {1.092665364, -20.51482085, 72.96732268, 1252.272692,0.0,0.0}, 
    {.3263849730, -7.705367266, 30.30134759, 362.3990301,0.0,0.0}
};

const double Masters2Loop::CoeffsResult[12][6]={
{-561.1637013, 4423.346068, -11860.56501, 12277.89914}, 
{-253.4420258, 1227.115135, -2691.911104, 3463.114373}, 
{0., 32.09832635, -140.6829308, 221.1959962, -147.0774663}, 
{0., 48.53203397, -146.4182526, 226.0629810, -237.6975017}, 
{0., 32.09832688, -139.1171047, 218.3292973, -146.9253238}, 
{0., 48.53203403, -146.6208761, 226.3130635, -237.7635502}, 
{0., 3.929072387, -83.13600247, 166.3839605, -53.04567931}, 
{0., -45.02729376, 96.83304693, -112.9961048, 178.9677928}, 
{0., 0., -63.86234094, 89.35950400}, 
{0., 0., -96.55519966, 2.172647778}, 
{0., 0., 14.68808630, 8.824236192, 57.65628407, 168.0077028}, 
{0., 0., 74.34783776, 147.0230518, 635.8526240, 1932.851438}
};

double QQec(double z,double zp,double w);

TEST(Qec,specificValue)
{
const double res = QQec(.9958075574, 0.10514166e-2, .7659666365);
const double expected = 99.50830201;
EXPECT_LT(fabs(res-expected),1e-6);
}

double CCL3(const double z, const double zp);

TEST(CCL3,specificValue)
{
    const double res = CCL3( .9989485834, 0.41924426e-2);
    const double expected = 22.84071063;
    EXPECT_LT(fabs(res-expected),1e-6);
}

TEST_F(Masters2Loop, I1_m3) {EXPECT_LT(check_master(1,-3),1e-6);}
TEST_F(Masters2Loop, I1_m2) {EXPECT_LT(check_master(1,-2),1e-6);}
TEST_F(Masters2Loop, I1_m1) {EXPECT_LT(check_master(1,-1),1e-6);}
TEST_F(Masters2Loop, I1_0 ) {EXPECT_LT(check_master(1, 0),1e-6);}
TEST_F(Masters2Loop, I1_1 ) {EXPECT_LT(check_master(1, 1),1e-6);}
TEST_F(Masters2Loop, I1_2 ) {EXPECT_LT(check_master(1, 2),1e-6);}

TEST_F(Masters2Loop, I2_m3) {EXPECT_LT(check_master(2,-3),1e-6);}
TEST_F(Masters2Loop, I2_m2) {EXPECT_LT(check_master(2,-2),1e-6);}
TEST_F(Masters2Loop, I2_m1) {EXPECT_LT(check_master(2,-1),1e-6);}
TEST_F(Masters2Loop, I2_0 ) {EXPECT_LT(check_master(2, 0),1e-6);}
TEST_F(Masters2Loop, I2_1 ) {EXPECT_LT(check_master(2, 1),1e-6);}
TEST_F(Masters2Loop, I2_2 ) {EXPECT_LT(check_master(2, 2),1e-6);}

TEST_F(Masters2Loop, I3_m3) {EXPECT_LT(check_master(3,-3),1e-6);}
TEST_F(Masters2Loop, I3_m2) {EXPECT_LT(check_master(3,-2),1e-6);}
TEST_F(Masters2Loop, I3_m1) {EXPECT_LT(check_master(3,-1),1e-6);}
TEST_F(Masters2Loop, I3_0 ) {EXPECT_LT(check_master(3, 0),1e-6);}
TEST_F(Masters2Loop, I3_1 ) {EXPECT_LT(check_master(3, 1),1e-6);}
TEST_F(Masters2Loop, I3_2 ) {EXPECT_LT(check_master(3, 2),1e-6);}

TEST_F(Masters2Loop, I4_m3) {EXPECT_LT(check_master(4,-3),1e-6);}
TEST_F(Masters2Loop, I4_m2) {EXPECT_LT(check_master(4,-2),1e-6);}
TEST_F(Masters2Loop, I4_m1) {EXPECT_LT(check_master(4,-1),1e-6);}
TEST_F(Masters2Loop, I4_0 ) {EXPECT_LT(check_master(4, 0),1e-6);}
TEST_F(Masters2Loop, I4_1 ) {EXPECT_LT(check_master(4, 1),1e-6);}
TEST_F(Masters2Loop, I4_2 ) {EXPECT_LT(check_master(4, 2),1e-6);}

TEST_F(Masters2Loop, I5_m3) {EXPECT_LT(check_master(5,-3),1e-6);}
TEST_F(Masters2Loop, I5_m2) {EXPECT_LT(check_master(5,-2),1e-6);}
TEST_F(Masters2Loop, I5_m1) {EXPECT_LT(check_master(5,-1),1e-6);}
TEST_F(Masters2Loop, I5_0 ) {EXPECT_LT(check_master(5, 0),1e-6);}
TEST_F(Masters2Loop, I5_1 ) {EXPECT_LT(check_master(5, 1),1e-6);}
TEST_F(Masters2Loop, I5_2 ) {EXPECT_LT(check_master(5, 2),1e-6);}

TEST_F(Masters2Loop, I6_m3) {EXPECT_LT(check_master(6,-3),1e-6);}
TEST_F(Masters2Loop, I6_m2) {EXPECT_LT(check_master(6,-2),1e-6);}
TEST_F(Masters2Loop, I6_m1) {EXPECT_LT(check_master(6,-1),1e-6);}
TEST_F(Masters2Loop, I6_0 ) {EXPECT_LT(check_master(6, 0),1e-6);}
TEST_F(Masters2Loop, I6_1 ) {EXPECT_LT(check_master(6, 1),1e-6);}
TEST_F(Masters2Loop, I6_2 ) {EXPECT_LT(check_master(6, 2),1e-6);}

TEST_F(Masters2Loop, I7_m3) {EXPECT_LT(check_master(7,-3),1e-6);}
TEST_F(Masters2Loop, I7_m2) {EXPECT_LT(check_master(7,-2),1e-6);}
TEST_F(Masters2Loop, I7_m1) {EXPECT_LT(check_master(7,-1),1e-6);}
TEST_F(Masters2Loop, I7_0 ) {EXPECT_LT(check_master(7, 0),1e-6);}
TEST_F(Masters2Loop, I7_1 ) {EXPECT_LT(check_master(7, 1),1e-6);}
TEST_F(Masters2Loop, I7_2 ) {EXPECT_LT(check_master(7, 2),1e-6);}

TEST_F(Masters2Loop, I8_m3) {EXPECT_LT(check_master(8,-3),1e-6);}
TEST_F(Masters2Loop, I8_m2) {EXPECT_LT(check_master(8,-2),1e-6);}
TEST_F(Masters2Loop, I8_m1) {EXPECT_LT(check_master(8,-1),1e-6);}
TEST_F(Masters2Loop, I8_0 ) {EXPECT_LT(check_master(8, 0),1e-6);}
TEST_F(Masters2Loop, I8_1 ) {EXPECT_LT(check_master(8, 1),1e-6);}
TEST_F(Masters2Loop, I8_2 ) {EXPECT_LT(check_master(8, 2),1e-6);}

TEST_F(Masters2Loop, I9_m3) {EXPECT_LT(check_master(9,-3),1e-6);}
TEST_F(Masters2Loop, I9_m2) {EXPECT_LT(check_master(9,-2),1e-6);}
TEST_F(Masters2Loop, I9_m1) {EXPECT_LT(check_master(9,-1),1e-6);}
TEST_F(Masters2Loop, I9_0 ) {EXPECT_LT(check_master(9, 0),1e-6);}
TEST_F(Masters2Loop, I9_1 ) {EXPECT_LT(check_master(9, 1),1e-6);}
TEST_F(Masters2Loop, I9_2 ) {EXPECT_LT(check_master(9, 2),1e-6);}

TEST_F(Masters2Loop, I10_m3) {EXPECT_LT(check_master(10,-3),1e-6);}
TEST_F(Masters2Loop, I10_m2) {EXPECT_LT(check_master(10,-2),1e-6);}
TEST_F(Masters2Loop, I10_m1) {EXPECT_LT(check_master(10,-1),1e-6);}
TEST_F(Masters2Loop, I10_0 ) {EXPECT_LT(check_master(10, 0),1e-4);}
TEST_F(Masters2Loop, I10_1 ) {EXPECT_LT(check_master(10, 1),1e-6);}
TEST_F(Masters2Loop, I10_2 ) {EXPECT_LT(check_master(10, 2),1e-6);}

TEST_F(Masters2Loop, I11_m3) {EXPECT_LT(check_master(11,-3),1e-6);}
TEST_F(Masters2Loop, I11_m2) {EXPECT_LT(check_master(11,-2),1e-6);}
TEST_F(Masters2Loop, I11_m1) {EXPECT_LT(check_master(11,-1),1e-6);}
TEST_F(Masters2Loop, I11_0 ) {EXPECT_LT(check_master(11, 0),1e-4);}
TEST_F(Masters2Loop, I11_1 ) {EXPECT_LT(check_master(11, 1),1e-6);}
TEST_F(Masters2Loop, I11_2 ) {EXPECT_LT(check_master(11, 2),1e-6);}

TEST_F(Masters2Loop, I12_m3) {EXPECT_LT(check_master(12,-3),1e-6);}
TEST_F(Masters2Loop, I12_m2) {EXPECT_LT(check_master(12,-2),1e-6);}
TEST_F(Masters2Loop, I12_m1) {EXPECT_LT(check_master(12,-1),1e-6);}
TEST_F(Masters2Loop, I12_0 ) {EXPECT_LT(check_master(12, 0),1e-6);}
TEST_F(Masters2Loop, I12_1 ) {EXPECT_LT(check_master(12, 1),1e-6);}
TEST_F(Masters2Loop, I12_2 ) {EXPECT_LT(check_master(12, 2),1e-6);}


TEST_F(Masters2Loop, c1_m2) {EXPECT_LT(check_coeff(1,-2),1e-4);}
TEST_F(Masters2Loop, c1_m1) {EXPECT_LT(check_coeff(1,-1),1e-4);}
TEST_F(Masters2Loop, c1_0 ) {EXPECT_LT(check_coeff(1,0),1e-4);}
TEST_F(Masters2Loop, c1_1 ) {EXPECT_LT(check_coeff(1,1),1e-4);}
TEST_F(Masters2Loop, c1_2 ) {EXPECT_LT(check_coeff(1,2),1e-4);}

TEST_F(Masters2Loop, c2_m2) {EXPECT_LT(check_coeff(2,-2),1e-4);}
TEST_F(Masters2Loop, c2_m1) {EXPECT_LT(check_coeff(2,-1),1e-4);}
TEST_F(Masters2Loop, c2_0 ) {EXPECT_LT(check_coeff(2,0),1e-4);}
TEST_F(Masters2Loop, c2_1 ) {EXPECT_LT(check_coeff(2,1),1e-4);}
TEST_F(Masters2Loop, c2_2 ) {EXPECT_LT(check_coeff(2,2),1e-4);}

TEST_F(Masters2Loop, c3_m2) {EXPECT_LT(check_coeff(3,-2),1e-4);}
TEST_F(Masters2Loop, c3_m1) {EXPECT_LT(check_coeff(3,-1),1e-4);}
TEST_F(Masters2Loop, c3_0 ) {EXPECT_LT(check_coeff(3,0),1e-4);}
TEST_F(Masters2Loop, c3_1 ) {EXPECT_LT(check_coeff(3,1),1e-4);}
TEST_F(Masters2Loop, c3_2 ) {EXPECT_LT(check_coeff(3,2),1e-4);}

TEST_F(Masters2Loop, c4_m2) {EXPECT_LT(check_coeff(4,-2),1e-4);}
TEST_F(Masters2Loop, c4_m1) {EXPECT_LT(check_coeff(4,-1),1e-4);}
TEST_F(Masters2Loop, c4_0 ) {EXPECT_LT(check_coeff(4,0),1e-4);}
TEST_F(Masters2Loop, c4_1 ) {EXPECT_LT(check_coeff(4,1),1e-4);}
TEST_F(Masters2Loop, c4_2 ) {EXPECT_LT(check_coeff(4,2),1e-4);}

TEST_F(Masters2Loop, c5_m2) {EXPECT_LT(check_coeff(5,-2),1e-4);}
TEST_F(Masters2Loop, c5_m1) {EXPECT_LT(check_coeff(5,-1),1e-4);}
TEST_F(Masters2Loop, c5_0 ) {EXPECT_LT(check_coeff(5,0),1e-4);}
TEST_F(Masters2Loop, c5_1 ) {EXPECT_LT(check_coeff(5,1),1e-4);}
TEST_F(Masters2Loop, c5_2 ) {EXPECT_LT(check_coeff(5,2),1e-4);}

TEST_F(Masters2Loop, c6_m2) {EXPECT_LT(check_coeff(6,-2),1e-4);}
TEST_F(Masters2Loop, c6_m1) {EXPECT_LT(check_coeff(6,-1),1e-4);}
TEST_F(Masters2Loop, c6_0 ) {EXPECT_LT(check_coeff(6,0),1e-4);}
TEST_F(Masters2Loop, c6_1 ) {EXPECT_LT(check_coeff(6,1),1e-4);}
TEST_F(Masters2Loop, c6_2 ) {EXPECT_LT(check_coeff(6,2),1e-4);}

TEST_F(Masters2Loop, c7_m2) {EXPECT_LT(check_coeff(7,-2),1e-4);}
TEST_F(Masters2Loop, c7_m1) {EXPECT_LT(check_coeff(7,-1),1e-4);}
TEST_F(Masters2Loop, c7_0 ) {EXPECT_LT(check_coeff(7,0),1e-4);}
TEST_F(Masters2Loop, c7_1 ) {EXPECT_LT(check_coeff(7,1),1e-4);}
TEST_F(Masters2Loop, c7_2 ) {EXPECT_LT(check_coeff(7,2),1e-4);}

TEST_F(Masters2Loop, c8_m2) {EXPECT_LT(check_coeff(8,-2),1e-4);}
TEST_F(Masters2Loop, c8_m1) {EXPECT_LT(check_coeff(8,-1),1e-4);}
TEST_F(Masters2Loop, c8_0 ) {EXPECT_LT(check_coeff(8,0),1e-4);}
TEST_F(Masters2Loop, c8_1 ) {EXPECT_LT(check_coeff(8,1),1e-4);}
TEST_F(Masters2Loop, c8_2 ) {EXPECT_LT(check_coeff(8,2),1e-4);}

TEST_F(Masters2Loop, c9_m2) {EXPECT_LT(check_coeff(9,-2),1e-4);}
TEST_F(Masters2Loop, c9_m1) {EXPECT_LT(check_coeff(9,-1),1e-4);}
TEST_F(Masters2Loop, c9_0 ) {EXPECT_LT(check_coeff(9,0),1e-4);}
TEST_F(Masters2Loop, c9_1 ) {EXPECT_LT(check_coeff(9,1),1e-4);}
TEST_F(Masters2Loop, c9_2 ) {EXPECT_LT(check_coeff(9,2),1e-4);}

TEST_F(Masters2Loop, c10_m2) {EXPECT_LT(check_coeff(10,-2),1e-4);}
TEST_F(Masters2Loop, c10_m1) {EXPECT_LT(check_coeff(10,-1),1e-4);}
TEST_F(Masters2Loop, c10_0 ) {EXPECT_LT(check_coeff(10,0),1e-4);}
TEST_F(Masters2Loop, c10_1 ) {EXPECT_LT(check_coeff(10,1),1e-4);}
TEST_F(Masters2Loop, c10_2 ) {EXPECT_LT(check_coeff(10,2),1e-4);}

TEST_F(Masters2Loop, c11_m2) {EXPECT_LT(check_coeff(11,-2),1e-4);}
TEST_F(Masters2Loop, c11_m1) {EXPECT_LT(check_coeff(11,-1),1e-4);}
TEST_F(Masters2Loop, c11_0 ) {EXPECT_LT(check_coeff(11,0),1e-4);}
TEST_F(Masters2Loop, c11_1 ) {EXPECT_LT(check_coeff(11,1),1e-4);}
TEST_F(Masters2Loop, c11_2 ) {EXPECT_LT(check_coeff(11,2),1e-4);}

TEST_F(Masters2Loop, c12_m2) {EXPECT_LT(check_coeff(12,-2),1e-4);}
TEST_F(Masters2Loop, c12_m1) {EXPECT_LT(check_coeff(12,-1),1e-4);}
TEST_F(Masters2Loop, c12_0 ) {EXPECT_LT(check_coeff(12,0),1e-4);}
TEST_F(Masters2Loop, c12_1 ) {EXPECT_LT(check_coeff(12,1),1e-4);}
TEST_F(Masters2Loop, c12_2 ) {EXPECT_LT(check_coeff(12,2),1e-4);}



class NewVirtualNNLOSoft :  public ::testing::Test {
protected:
    virtual void SetUp() {
        kbr.SetMaxMomentumID(6);
        kbr.Set(1,2, 955.1019552011871);
        kbr.Set(1,3, -731.5762320546179);
        kbr.Set(1,4, -218.5257231465693);
        kbr.Set(2,3, -218.5257231465692);
        kbr.Set(2,4,-731.576232054618);
        kbr.Set(3,4, 955.1019552011871);
        kbr.Set(3, 1.0 );
        kbr.Set(4, 4.0 );
        kbr.compute_dimensionless_invariants();
        
        VV.PassScales(12.5,12.5);
    }
    GstarGstarMeNNLOSoft VV;
    KinematicInvariants kbr;
};

TEST_F(NewVirtualNNLOSoft,CataniE3)
{
    const double test_result = VV.Catani(kbr,-3);
    const double expected = 0.0;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}


TEST_F(NewVirtualNNLOSoft,CataniE2)
{
    const double test_result = VV.Catani(kbr,-2);
    const double expected = 0.0;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(NewVirtualNNLOSoft,CataniE1)
{
    const double test_result = VV.Catani(kbr,-1);
    const double expected = 0.0;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}


TEST_F(NewVirtualNNLOSoft,VVrenormalized)
{
    const double test_result = VV.VVRenormalized(kbr);
    const double expected = -2137.440383;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected)/fabs(expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(NewVirtualNNLOSoft,VVimplementation)
{
    const double test_result = VV.eval_me(kbr);
    const double expected = -1253.230110/2.;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected)/fabs(expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}



class NewVirtualNNLOSoftLimitU :  public ::testing::Test {
protected:
    virtual void SetUp() {
        kbr.SetMaxMomentumID(6);
        kbr.Set(1,2, 62676759.07146515);
        kbr.Set(1,3, -7650.534886159003);
        kbr.Set(1,4, -62665008.536578983);
        kbr.Set(2,3, -62665008.53657898);
        kbr.Set(2,4, -7650.534886159003);
        kbr.Set(3,4, 62676759.07146515);
        kbr.Set(3, 1600 );
        kbr.Set(4, 2500 );
        kbr.compute_dimensionless_invariants();
        
        VV.PassScales(1.0,1.0);
    }
    GstarGstarMeNNLOSoft VV;
    KinematicInvariants kbr;
};

TEST_F(NewVirtualNNLOSoftLimitU,VVimplementation)
{
    const double test_result = VV.eval_me(kbr);
    const double expected = -3.67549108836769228e08/2.;
    
    const double err = 1e-6;
    EXPECT_LT(fabs(test_result-expected)/fabs(expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

class VirtualNNLO :  public ::testing::Test {
protected:
    VirtualNNLO(){};
    virtual void SetUp() {
        kbr.SetMaxMomentumID(6);
        kbr.Set(1,2, 955.1019552011871);
        kbr.Set(1,3, -731.5762320546179);
        kbr.Set(1,4, -218.5257231465693);
        kbr.Set(2,3, -218.5257231465692);
        kbr.Set(2,4,-731.576232054618);
        kbr.Set(3,4, 955.1019552011871);
        kbr.Set(3, 1.0 );
        kbr.Set(4, 4.0 );
        kbr.compute_dimensionless_invariants();
    }
    GstarVirtual V;
    GstarVirtualVirtual VV;
    KinematicInvariants kbr;
};


TEST_F(VirtualNNLO,CoefficientXMaster)
{
    const double test_result = VV.Epsilon0(kbr) * consts::nf /2.0;
    const double expected = -1286.859798;
    
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected)/fabs(expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(VirtualNNLO,Ce2XCoefficientXMaster)
{
    const double test_result = (VV.Epsilon0(kbr)
                                - 1./2.*consts::pi_square*VV.EpsilonM2(kbr)
                                -14./3.*consts::z3*VV.EpsilonM3(kbr))* consts::nf /2.0;
    const double expected = -2086.67547;
    
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected)/fabs(expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(VirtualNNLO,VVrenormalizedAt1)
{
    const double Ce2X_ci_mi = consts::nf/2.*(VV.Epsilon0(kbr)
    - 1./2.*consts::pi_square*VV.EpsilonM2(kbr)
    -14./3.*consts::z3*VV.EpsilonM3(kbr));
    
    const double CeX_ci_mi_V = V.EpsilonP1(kbr)
                    -(1./4.)*consts::pi_square*V.EpsilonM1(kbr)
                    -(7./3.)*V.EpsilonM2(kbr)*consts::z3;
    const double test_result = Ce2X_ci_mi + consts::nf/3.*CeX_ci_mi_V;
    const double expected = -2137.4403837;
    
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected)/fabs(expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(VirtualNNLO,V1CeXCoefficientXMaster)
{
    const double test_result = V.Epsilon0(kbr)-consts::pi_square/4.*V.EpsilonM2(kbr); 
    
    const double expected = 453.138383;
    
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected)/fabs(expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}



TEST_F(VirtualNNLO,Vepsilon1)
{
    const double test_result = V.EpsilonP1(kbr);
    
    const double expected = -63.5932497;
    
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected)/fabs(expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}






class RealNLO: public ::testing::Test {
protected:
    RealNLO():shifted_left(4),shifted_right(4),kk(5){}
    virtual void SetUp() {
        
        kk.SetBoundaries(9.0,1000.0);
        kk.SetMassesSquared(1.0,4.0);
        shifted_left.SetMassesSquared(1.0,4.0);
        shifted_left.SetBoundaries(9.0,1000.0);
        shifted_right.SetMassesSquared(1.0,4.0);
        shifted_right.SetBoundaries(9.0,1000.0);
        
        xx_vegas[0]=0.564;//~x1*x2
        xx_vegas[1]=0.123123;//~x1/x2
        xx_vegas[2]=0.23;// angle between photons
        xx_vegas[3]=0.56;// angle between photons
        xx_vegas[4]=0.6;//~z
        xx_vegas[5]=0.1;//lambda
        xx_vegas[6]=0.8642;//phi / 2 Pi
        
        kk.generate_kinematics(xx_vegas);
        shifted_left.SetZ(kk.z);
        shifted_right.SetZ(kk.z);
        shifted_left.generate_kinematics(xx_vegas);
        shifted_right.generate_kinematics(xx_vegas);
    }
    double xx_vegas[7];
    GstarGstarMeNLOHard R;
    NLOKinematics kk;
    LOKinematicsShiftedLeft shifted_left;
    LOKinematicsShiftedRight shifted_right;
};



TEST_F(RealNLO,implementation)
{

    double me_sq = R.R(kk.invariants());
    cout<<kk;
    const double test_result = me_sq;
    const double expected = 49.52101928*2.0;
//    const double expected = 501.0297676/78.95683*3.0/2.0;
    
    const double err = 0.01;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}



TEST_F(RealNLO,momentum_conservation)
{

    double res=0.0;
    for(int i=0;i<4;i++)
        {
        double loc = kk.p1[i]+kk.p2[i]-kk.p3[i]-kk.p4[i]-kk.p5[i];
        //cout<<"\n mu = "<<i<<" : "<<loc;
        res=res+loc;
        }
    cout<<endl;
    double me_sq = R.eval_me(kk.invariants());
    
    const double test_result = res;
    const double expected = 0.0;
    
    const double err = 1e-10;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}

TEST_F(RealNLO,sij_identity)
{
    const double test_result = kk.s(1,2)+kk.s(1,3)+kk.s(1,4)+kk.s(1,5)+kk.s(2,3)+kk.s(2,4)+kk.s(2,5)+kk.s(3,4)+kk.s(3,5)+kk.s(4,5);
    const double expected = 3.0*(kk.s(4)+kk.s(3));
    
    const double err = 1e-10;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}



TEST_F(RealNLO,collinear_limit_1)
{
        
    xx_vegas[0]=0.4564;//~x1*x2
    xx_vegas[1]=0.123123;//~x1/x2
    xx_vegas[2]=0.423;// angle between photons
    xx_vegas[3]=0.556;// angle between photons
    xx_vegas[4]=0.324;//~z
    xx_vegas[5]=0.31;//lambda
    xx_vegas[6]=0.8642;//phi / 2 Pi
    kk.generate_kinematics(xx_vegas);
    shifted_left.SetZ(kk.z);
    shifted_right.SetZ(kk.z);
    shifted_left.generate_kinematics(xx_vegas);
    shifted_right.generate_kinematics(xx_vegas);
    double me_sq;
    double limit;
    for (int i=0;i<20;i++)
        {
        xx_vegas[5]=xx_vegas[5]/4.0;
        
        kk.generate_kinematics(xx_vegas);
        shifted_left.SetZ(kk.z);
        shifted_right.SetZ(kk.z);
        shifted_left.generate_kinematics(xx_vegas);
        shifted_right.generate_kinematics(xx_vegas);
        
        me_sq = R.eval_me(kk.invariants());
        limit = 1.0/2.0*R.PP(1.0/kk.z) * R.born_(shifted_left.invariants())/kk.s(1,5);
        cout<<setprecision(16)<<"\nlambda = "<<setw(20)<<kk.lambda
            <<" M^2 "<<setw(20)<<me_sq
            <<" vs limit "<<setw(20)<<limit<<"\t diff "<<setw(20)<<fabs(me_sq-limit)/me_sq
        <<"\t\t"<<R.collinear_limit(2);

        }
    cout<<endl;
        
    const double test_result = me_sq;
    const double expected = limit;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected)/fabs(test_result),err)
        <<"[   INFO   ] test_result = "<<test_result
        <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}

TEST_F(RealNLO,collinear_limit_2)
{
    
    xx_vegas[0]=0.4564;//~x1*x2
    xx_vegas[1]=0.123123;//~x1/x2
    xx_vegas[2]=0.423;// angle between photons
    xx_vegas[3]=0.556;// angle between photons
    xx_vegas[4]=0.324;//~z
    xx_vegas[5]=0.31;//lambda
    xx_vegas[6]=0.8642;//phi / 2 Pi
    kk.generate_kinematics(xx_vegas);
    shifted_left.SetZ(kk.z);
    shifted_right.SetZ(kk.z);
    shifted_left.generate_kinematics(xx_vegas);
    shifted_right.generate_kinematics(xx_vegas);    
    double me_sq;
    double limit;
    for (int i=0;i<20;i++)
        {
        xx_vegas[5]=1.0-(1.0-xx_vegas[5])/4.0;
        
        kk.generate_kinematics(xx_vegas);
        shifted_left.SetZ(kk.z);
        shifted_right.SetZ(kk.z);
        shifted_left.generate_kinematics(xx_vegas);
        shifted_right.generate_kinematics(xx_vegas);
        me_sq = R.eval_me(kk.invariants());
        limit = 1.0/2.0*R.PP(1.0/kk.z) * R.born_(shifted_right.invariants())/kk.s(2,5);
        cout<<setprecision(16)<<"\nlambda = "<<setw(20)<<kk.lambda
        <<" M^2 "<<setw(20)<<me_sq
        <<" vs limit "<<setw(20)<<limit<<"\t diff "<<setw(20)<<fabs(me_sq-limit)/me_sq<<"\t\t"<<R.collinear_limit(1);
        
        }
    cout<<endl;
    
    const double test_result = me_sq;
    const double expected = limit;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected)/fabs(test_result),err)
    <<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}


//---------------------------------------------------------------

class RealNLOQuarkGluon: public ::testing::Test {
protected:
    RealNLOQuarkGluon():shifted_right(4),kk(5){}
    virtual void SetUp() {
        
        kk.SetBoundaries(9.0,1000.0);
        kk.SetMassesSquared(1.0,4.0);
        shifted_right.SetMassesSquared(1.0,4.0);
        shifted_right.SetBoundaries(9.0,1000.0);
        
        xx_vegas[0]=0.564;//~x1*x2
        xx_vegas[1]=0.123123;//~x1/x2
        xx_vegas[2]=0.23;// angle between photons
        xx_vegas[3]=0.56;// angle between photons
        xx_vegas[4]=0.6;//~z
        xx_vegas[5]=0.1;//lambda
        xx_vegas[6]=0.8642;//phi / 2 Pi
        
        kk.generate_kinematics(xx_vegas);
        shifted_right.SetZ(kk.z);
        shifted_right.generate_kinematics(xx_vegas);
    }
    double xx_vegas[7];
    GstarGstarMENLOHardQuarkGluon R;
    NLOKinematics kk;
    LOKinematicsShiftedRight shifted_right;
};




TEST_F(RealNLOQuarkGluon,collinear_limit_2)
{
    
    xx_vegas[0]=0.4564;//~x1*x2
    xx_vegas[1]=0.123123;//~x1/x2
    xx_vegas[2]=0.423;// angle between photons
    xx_vegas[3]=0.556;// angle between photons
    xx_vegas[4]=0.324;//~z
    xx_vegas[5]=0.31;//lambda
    xx_vegas[6]=0.8642;//phi / 2 Pi
    kk.generate_kinematics(xx_vegas);
    shifted_right.SetZ(kk.z);
    shifted_right.generate_kinematics(xx_vegas);    
    double me_sq;
    double limit;
    const double correction_for_averaging = 3./8.;
    for (int i=0;i<20;i++)
    {
        xx_vegas[5]=1.0-(1.0-xx_vegas[5])/4.0;
        
        kk.generate_kinematics(xx_vegas);
        shifted_right.SetZ(kk.z);
        shifted_right.generate_kinematics(xx_vegas);
        
        me_sq = correction_for_averaging *R.Rcrossed(kk.invariants());
        const double collinear2 = 
        limit = - 1./4.
                *(1.-2.*kk.z*(1.-kk.z))/kk.s(2,5)
                *R.born_(shifted_right.invariants())/kk.z;
        cout<<setprecision(16)<<"\nlambda = "<<setw(20)<<kk.lambda
        <<" M^2 "<<setw(20)<<me_sq
        <<" vs limit "<<setw(20)<<limit<<"\t diff "<<setw(20)<<fabs(me_sq-limit)/me_sq;
        
    }
    cout<<endl;
    
    const double test_result = me_sq;
    const double expected = limit;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected)/fabs(test_result),err)
    <<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}



//---------------------------------------------------------------





class RealNNLO: public ::testing::Test {
protected:
    RealNNLO():hard(),kk(5),shifted_left(4),shifted_right(4),kk_nlo(5){}
    virtual void SetUp() {

        kk.SetMassesSquared(1.0,4.0);
        kk.SetBoundaries(9.0,1000.0);
        shifted_left.SetMassesSquared(1.0,4.0);
        shifted_left.SetBoundaries(9.0,1000.0);
        shifted_right.SetMassesSquared(1.0,4.0);
        shifted_right.SetBoundaries(9.0,1000.0);
        kk_nlo.SetMassesSquared(1.0,4.0);
        kk_nlo.SetBoundaries(9.0,1000.0);
        xx_vegas[0]=0.564;//~x1*x2
        xx_vegas[1]=0.123123;//~x1/x2
        xx_vegas[2]=0.23;// angle between photons
        xx_vegas[3]=0.56;// angle between photons
        xx_vegas[4]=0.6;//~z
        xx_vegas[5]=0.1;//lambda
        xx_vegas[6]=0.8642;//phi / 2 Pi
        xx_vegas[7]=0.524642;//rho

        kk.generate_kinematics(xx_vegas);
        shifted_left.SetZ(kk.z);
        shifted_right.SetZ(kk.z);
        shifted_left.generate_kinematics(xx_vegas);
        shifted_right.generate_kinematics(xx_vegas);
        kk_nlo.generate_kinematics(xx_vegas);
    }
    double xx_vegas[8];
    GstarGstarMeNNLOHard hard;
    NNLOKinematics kk;
    LOKinematicsShiftedLeft shifted_left;
    LOKinematicsShiftedRight shifted_right;
    NLOKinematics kk_nlo;
};


TEST_F(RealNNLO,implementation)
{
        cout<<setprecision(16);
        cout<<kk;
    double me_sq = hard.RR(kk.invariants());
    
    const double test_result = me_sq;
    const double expected = 51.91077814*4.0;
    
    const double err = 0.01;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected
    <<"\t ratio res/expected = "<<test_result/expected<<endl;
    
}




TEST_F(RealNNLO,momentum_conservation)
{
//
//    cout<<"\np1 = "<<kk.p1<<"\t p1^2= "<<kk.p1*kk.p1;
//    cout<<"\np2 = "<<kk.p2<<"\t p2^2= "<<kk.p2*kk.p2;
//    cout<<"\np3 = "<<kk.p3<<"\t p3^2= "<<kk.p3*kk.p3;
//    cout<<"\np4 = "<<kk.p4<<"\t p4^2= "<<kk.p4*kk.p4;
//    cout<<"\np5 = "<<kk.p5<<"\t p5^2= "<<kk.p5*kk.p5;
    //    cout<<endl;
    //    cout<<"\np3com = "<<kk.p3com;
    //    cout<<"\np4com = "<<kk.p4com;
    //    cout<<"\n p34^2 = "<<pow(kk.p3com[0]+kk.p4com[0],2.0)<<" q^2= "<<kk.z*kk.s12;
    //    cout<<" vs, at lab, "<<2.0*(kk.p3*kk.p4)+kk.s3+kk.s4;
    double res=0.0;
    for(int i=0;i<4;i++)
    {
        double loc = kk.p1[i]+kk.p2[i]-kk.p3[i]-kk.p4[i]-kk.p5[i];
        cout<<"\n mu = "<<i<<" : "<<loc;
        res=res+loc;
    }
    cout<<endl;
    
    const double test_result = res;
    const double expected = 0.0;
    
    const double err = 1e-10;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}


TEST_F(RealNNLO,sij_identity)
{
    const double test_result = kk.s(1,2)+kk.s(1,3)+kk.s(1,4)+kk.s(1,5)+kk.s(2,3)+kk.s(2,4)+kk.s(2,5)+kk.s(3,4)+kk.s(3,5)+kk.s(4,5);
    const double expected = 3.0*(kk.s(4)+kk.s(3)+kk.s(5));
    
    const double err = 1e-10;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}


TEST_F(RealNNLO,collinear_limit_2)
{
    
    xx_vegas[0]=0.4564;//~x1*x2
    xx_vegas[1]=0.123123;//~x1/x2
    xx_vegas[2]=0.423;// angle between photons
    xx_vegas[3]=0.556;// angle between photons
    xx_vegas[4]=0.29;//~z
    xx_vegas[5]=0.31;//lambda
    xx_vegas[6]=0.8642;//phi / 2 Pi
    xx_vegas[7]=0.123;//rho / 2 Pi
    kk.generate_kinematics(xx_vegas);
    shifted_left.SetZ(kk.z);
    shifted_right.SetZ(kk.z);
    shifted_left.generate_kinematics(xx_vegas);
    shifted_right.generate_kinematics(xx_vegas);
    
    double me_sq;
    double limit;
    for (int i=0;i<20;i++)
    {
        xx_vegas[5]=1.0-(1.0-xx_vegas[5])/4.0;
        
        kk.generate_kinematics(xx_vegas);
        shifted_left.SetZ(kk.z);
        shifted_right.SetZ(kk.z);
        shifted_left.generate_kinematics(xx_vegas);
        shifted_right.generate_kinematics(xx_vegas);
        me_sq = hard.RR(kk.invariants());
        limit = hard.PPt2(kk.z,kk.rho)*hard.born_(shifted_right.invariants())/kk.z/(-kk.s(2,5));
        cout<<setprecision(16)<<"\nlambda = "<<setw(20)<<kk.lambda
        <<" M^2 "<<setw(20)<<me_sq
        <<" vs limit "<<setw(20)<<limit
        <<"\t diff "<<setw(20)<<fabs(me_sq-limit)/me_sq
        <<"\t ratio = "<<setw(20)<<me_sq/limit;
        
    }
    cout<<endl;
    
    const double test_result = me_sq;
    const double expected = limit;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected)/fabs(test_result),err)
    <<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}

TEST_F(RealNNLO,collinear_limit_1)
{
    
//    xx_vegas[0]=0.4564;//~x1*x2
//    xx_vegas[1]=0.123123;//~x1/x2
//    xx_vegas[2]=0.423;// angle between photons
//    xx_vegas[3]=0.556;// angle between photons
//    xx_vegas[4]=0.9;//~z
//    xx_vegas[5]=0.31;//lambda
//    xx_vegas[6]=0.8642;//phi / 2 Pi
//    xx_vegas[7]=0.123;//rho / 2 Pi
//    kk.generate_bjorken_xs(xx_vegas);
//    kk.SetNNLOKinematics(xx_vegas);
    
    double me_sq;
    double limit;
    for (int i=0;i<20;i++)
    {
        xx_vegas[5]=xx_vegas[5]/4.0;
        
        kk.generate_kinematics(xx_vegas);
        shifted_left.generate_kinematics(xx_vegas);
        me_sq = hard.RR(kk.invariants());
        limit = hard.PPt1(kk.z,kk.rho)*hard.born_(shifted_left.invariants())/kk.z/(-kk.s(1,5));
        cout<<setprecision(16)<<"\nlambda = "<<setw(20)<<kk.lambda
        <<" M^2 "<<setw(20)<<me_sq
        <<" vs limit "<<setw(20)<<limit
        <<"\t diff "<<setw(20)<<fabs(me_sq-limit)/me_sq
        <<"\t ratio = "<<setw(20)<<me_sq/limit;
        
    }
    cout<<endl;
    
    const double test_result = me_sq;
    const double expected = limit;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected)/fabs(test_result),err)
    <<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}

TEST_F(RealNNLO,sij_identity_for_single_collinear_projection)
{
    const double test_result = kk_nlo.s(1,2)+kk_nlo.s(1,3)
                                +kk_nlo.s(1,4)+kk_nlo.s(1,5)
                                +kk_nlo.s(2,3)+kk_nlo.s(2,4)
                                +kk_nlo.s(2,5)
                                +kk_nlo.s(3,4)+kk_nlo.s(3,5)
                                +kk_nlo.s(4,5);
    const double expected = 3.0*(kk_nlo.s(4)+kk_nlo.s(3));
    
    const double err = 1e-10;
    EXPECT_LT(fabs(test_result-expected),err)
    <<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}

TEST_F(RealNNLO,single_collinear)
{
    
    xx_vegas[0]=0.4564;//~x1*x2
    xx_vegas[1]=0.123123;//~x1/x2
    xx_vegas[2]=0.423;// angle between photons
    xx_vegas[3]=0.556;// angle between photons
    xx_vegas[4]=0.9;//~z
    xx_vegas[5]=0.31;//lambda
    xx_vegas[6]=0.8642;//phi / 2 Pi
    xx_vegas[7]=0.123;//rho / 2 Pi
    kk.generate_kinematics(xx_vegas);
    kk_nlo.generate_kinematics(xx_vegas);
    double me_sq;
    double limit;
    for (int i=0;i<20;i++)
    {
        xx_vegas[7]=1.0-(1.0-xx_vegas[7])/4.0;
        
        kk.generate_kinematics(xx_vegas);
        kk_nlo.generate_kinematics(xx_vegas);
        me_sq = hard.RR(kk.invariants());
        limit = hard.R(kk_nlo.invariants())*2.0;
        cout<<setprecision(16)<<"\nrho = "<<setw(20)<<kk.rho
        <<" M^2 "<<setw(20)<<me_sq
        <<" vs limit "<<setw(20)<<limit
        <<"\t diff "<<setw(20)<<fabs(me_sq-limit)/me_sq
        <<"\t ratio = "<<setw(20)<<me_sq/limit;
        
    }
    cout<<endl;
    
    const double test_result = me_sq;
    const double expected = limit;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected)/fabs(test_result),err)
    <<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}

TEST_F(RealNNLO,double_soft)
{
    
    xx_vegas[0]=0.4564;//~x1*x2
    xx_vegas[1]=0.123123;//~x1/x2
    xx_vegas[2]=0.423;// angle between photons
    xx_vegas[3]=0.556;// angle between photons
    xx_vegas[4]=0.1;//~z
    xx_vegas[5]=0.31;//lambda
    xx_vegas[6]=0.8642;//phi / 2 Pi
    xx_vegas[7]=0.123;//rho / 2 Pi
    kk.generate_kinematics(xx_vegas);
    kk_nlo.generate_kinematics(xx_vegas);
    shifted_left.generate_kinematics(xx_vegas);
    shifted_right.generate_kinematics(xx_vegas);
    double me_sq;
    double limit;
    for (int i=0;i<20;i++)
    {
        xx_vegas[4]=1.0-(1.0-xx_vegas[4])/4.0;
        
        kk.generate_kinematics(xx_vegas);
        kk_nlo.generate_kinematics(xx_vegas);
        shifted_left.generate_kinematics(xx_vegas);
        shifted_right.generate_kinematics(xx_vegas);
        
        me_sq = hard.RR(kk.invariants());
        double coll1 = hard.PPt1(kk.z,kk.rho)
                    *hard.born_(shifted_left.invariants())/kk.z/(-kk.s(1,5));
        double coll2 = hard.PPt2(kk.z,kk.rho)
                    *hard.born_(shifted_right.invariants())/kk.z/(-kk.s(2,5));
        double real = hard.R(kk_nlo.invariants());
        
        double counter1 = hard.PP(kk_nlo.z) 
                        * hard.born_(shifted_left.invariants())
                        /kk.z/(-kk_nlo.s(1,5))
                      ;
        double counter2 = hard.PP(kk_nlo.z) 
                        * hard.born_(shifted_right.invariants())
                        /kk.z/(-kk_nlo.s(2,5));
        
        const double zbar = (1.0-kk.z);
        limit = (1.0-kk.z)*( coll1 + coll2 + real - counter1 - counter2);
        cout<<setprecision(8)<<"\nz = "<<setw(10)<<kk.z
        <<" M^2 "<<setw(10)<<me_sq*zbar/2.
        <<" vs limit "<<setw(10)<<limit
       // <<"\t diff "<<setw(10)<<fabs(me_sq-limit)/me_sq
       // <<"\t ratio = "<<setw(10)<<me_sq/limit
       // <<"\t col1 = "<<coll1*zbar
       // <<"\t col2 = "<<coll2*zbar
       // <<"\t real = "<<real*zbar
        <<"\t counter1 = "<<counter1*zbar
        <<"\t counter2 = "<<counter2*zbar
        ;
        
    }
    cout<<endl;
    
    const double test_result = me_sq*(1.0-kk.z)/2.0;
    const double expected = limit;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected)/fabs(test_result),err)
    <<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}


TEST_F(RealNNLO,double_soft_version2)
{
    
    xx_vegas[0]=0.4564;//~x1*x2
    xx_vegas[1]=0.123123;//~x1/x2
    xx_vegas[2]=0.423;// angle between photons
    xx_vegas[3]=0.556;// angle between photons
    xx_vegas[4]=0.1;//~z
    xx_vegas[5]=0.31;//lambda
    xx_vegas[6]=0.8642;//phi / 2 Pi
    xx_vegas[7]=0.123;//rho / 2 Pi
    kk.generate_kinematics(xx_vegas);
    kk_nlo.generate_kinematics(xx_vegas);
    shifted_left.generate_kinematics(xx_vegas);
    shifted_right.generate_kinematics(xx_vegas);
    
    double me_sq;
    double limit;
    for (int i=0;i<20;i++)
    {
        xx_vegas[4]=1.0-(1.0-xx_vegas[4])/4.0;
        
        kk.generate_kinematics(xx_vegas);
        kk_nlo.generate_kinematics(xx_vegas);
        shifted_left.generate_kinematics(xx_vegas);
        shifted_right.generate_kinematics(xx_vegas);
        
        me_sq = hard.RR(kk.invariants());
        double coll1 = hard.PPt1(kk.z,kk.rho)
        *hard.born_(shifted_left.invariants())/kk.z/(-kk.s(1,5));
        double coll2 = hard.PPt2(kk.z,kk.rho)
        *hard.born_(shifted_right.invariants())/kk.z/(-kk.s(2,5));
        double real = hard.R(kk_nlo.invariants());
        
        
        
        double counter1 = hard.PP(kk_nlo.z) 
        * hard.born_(shifted_left.invariants())
        /kk.z/(-kk_nlo.s(1,5))
        ;
        double counter2 = hard.PP(kk_nlo.z) 
        * hard.born_(shifted_right.invariants())
        /kk.z/(-kk_nlo.s(2,5));
        
        const double zbar = (1.0-kk.z);
        limit = (1.0-kk.z)*( coll1 + coll2 + 2.*real - counter1 - counter2);
        cout<<setprecision(8)<<"\nz = "<<setw(10)<<kk.z
        <<" M^2 "<<setw(10)<<me_sq*zbar
        <<" vs limit "<<setw(10)<<limit
        // <<"\t diff "<<setw(10)<<fabs(me_sq-limit)/me_sq
        // <<"\t ratio = "<<setw(10)<<me_sq/limit
        // <<"\t col1 = "<<coll1*zbar
        // <<"\t col2 = "<<coll2*zbar
        // <<"\t real = "<<real*zbar
        <<"\t counter1 = "<<counter1*zbar
        <<"\t counter2 = "<<counter2*zbar
        ;
        
    }
    cout<<endl;
    
    const double test_result = me_sq*(1.0-kk.z);
    const double expected = limit;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected)/fabs(test_result),err)
    <<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}


TEST_F(RealNNLO,double_soft_direct)
{
    
    xx_vegas[0]=0.4564;//~x1*x2
    xx_vegas[1]=0.123123;//~x1/x2
    xx_vegas[2]=0.423;// angle between photons
    xx_vegas[3]=0.556;// angle between photons
    xx_vegas[4]=0.1;//~z
    xx_vegas[5]=0.31;//lambda
    xx_vegas[6]=0.8642;//phi / 2 Pi
    xx_vegas[7]=0.123;//rho / 2 Pi
    

    kk.SetMassesSquared(1.0,4.0);
    kk.SetBoundaries(2500.0,169000000.0);
    shifted_left.SetMassesSquared(1.0,4.0);
    shifted_left.SetBoundaries(2500.0,169000000.0);
    shifted_right.SetMassesSquared(1.0,4.0);
    shifted_right.SetBoundaries(2500.0,169000000.0);
    kk_nlo.SetMassesSquared(1.0,4.0);
    kk_nlo.SetBoundaries(2500.0,169000000.0);

    kk.generate_kinematics(xx_vegas);
    kk_nlo.generate_kinematics(xx_vegas);
    shifted_left.generate_kinematics(xx_vegas);
    shifted_right.generate_kinematics(xx_vegas);
    double me_sq;
    double limit;
    for (int i=0;i<26;i++)
    {
        xx_vegas[4]=1.0-(1.0-xx_vegas[4])/4.0;
        kk.generate_kinematics(xx_vegas);
        kk_nlo.generate_kinematics(xx_vegas);
        shifted_left.generate_kinematics(xx_vegas);
        shifted_right.generate_kinematics(xx_vegas);
        
        const double zbar = (1.0-kk.z);

        me_sq = hard.RR(kk.invariants())*zbar;
        
        double soft = 2.0 * 4.0/3.0 * kk.s(1,2)/kk.s(1,5)/kk.s(2,5) * hard.born_(shifted_left.invariants());
        
        
        limit = soft*zbar;
//        cout<<setprecision(16)<<kk_shifted
//        <<setw(16)<<" RR="<<hard.RR(kk)
//        <<setw(16)<<" SS="<<soft
//        ;
        
        cout<<setprecision(16)<<"\nz = "<<setw(10)<<kk.z
        <<" M^2 "<<setw(10)<<me_sq
        <<" vs limit "<<setw(10)<<limit
         <<"\t rel diff "<<setw(10)<<fabs(me_sq-limit)/me_sq
         <<"\t abs diff "<<setw(10)<<fabs(me_sq-limit)
        ;
        
    }
    cout<<endl;
    
    const double test_result = me_sq;
    const double expected = limit;
    
    const double err = 1e-4;
    EXPECT_LT(fabs(test_result-expected)/fabs(test_result),err)
    <<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}


int main(int argc, char**argv)
{
    cout << "\ntesting g* g* \n" << endl;
    
    ::testing::InitGoogleTest(&argc, argv);
    return  RUN_ALL_TESTS();
    
    return 0;
}

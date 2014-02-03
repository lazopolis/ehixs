//    testing gamma* gamma*
//    Achilleas Lazopoulos, lazopoli@phys.ethz.ch


#include <iostream>
#include <cmath>
#include <string>
using namespace std;

#include "gtest/gtest.h"

#include "event.h"
#include "gamma_star_gamma_star_me.h"



class VirtualNLO: public ::testing::Test {
protected:
    VirtualNLO():V(GstarGstarMeNLOSoft(my_box)){}
    virtual void SetUp() {
        
        kk.tau = 0.9; // smax is a protected member of the base class
        kk.smax = 1000.0;
        kk.smin = 9.0;
        kk.s3=1.0;
        kk.s4=4.0;
        
        xx_vegas[0]=0.564;//~x1*x2
        xx_vegas[1]=0.123123;//~x1/x2
        xx_vegas[2]=0.23;// angle between photons
        xx_vegas[3]=0.56;// angle between photons
        xx_vegas[4]=0.6;//~z
        xx_vegas[5]=0.1;//lambda
        xx_vegas[6]=0.8642;//phi / 2 Pi
        
        kk.z=1.0;
        kk.generate_bjorken_xs(xx_vegas);
        kk.SetLOKinematics(xx_vegas);
    }
    double xx_vegas[7];
    EventBox my_box;
    GstarGstarMeNLOSoft V;
    KinematicVariables kk;
};

TEST_F(VirtualNLO,implementation)
{
    cout<<setprecision(16);
    cout<<"\n s12 = "<<kk.s12;
    cout<<"\n s13 = "<<kk.s13;
    cout<<"\n s14 = "<<kk.s14;
    cout<<"\n s23 = "<<kk.s23;
    cout<<"\n s24 = "<<kk.s24;
    cout<<"\n s34 = "<<kk.s34;
    cout<<"\n s3 = "<<kk.s3;
    cout<<"\ns4= "<<kk.s4;
//    cout<<"\np1    "<<kk.p1;
//    cout<<"\np2    "<<kk.p2;
//    cout<<"\np3    "<<kk.p3;
//    cout<<"\np3com "<<kk.p3com;
//    cout<<"\np4    "<<kk.p4;
//    cout<<"\np4com "<<kk.p4com;
//    cout<<"\np5    "<<kk.p5;
    double me_sq = V.eval_me(kk);
    
    const double test_result = me_sq;
    const double expected = 508.3981879*2.0;
    
    const double err = 0.01;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}


class RealNLO: public ::testing::Test {
protected:
    RealNLO():R(GstarGstarMeNLOHard(my_box)){}
    virtual void SetUp() {
        
        kk.tau = 0.9; // smax is a protected member of the base class
        kk.smax = 1000.0;
        kk.smin = 9.0;
        kk.s3=1.0;
        kk.s4=4.0;
        
        xx_vegas[0]=0.564;//~x1*x2
        xx_vegas[1]=0.123123;//~x1/x2
        xx_vegas[2]=0.23;// angle between photons
        xx_vegas[3]=0.56;// angle between photons
        xx_vegas[4]=0.6;//~z
        xx_vegas[5]=0.1;//lambda
        xx_vegas[6]=0.8642;//phi / 2 Pi
        
        kk.generate_bjorken_xs(xx_vegas);
        kk.SetNLOKinematics(xx_vegas);
    }
    double xx_vegas[7];
    EventBox my_box;
    GstarGstarMeNLOHard R;
    KinematicVariables kk;
};



TEST_F(RealNLO,implementation)
{
//    cout<<setprecision(16);
//        cout<<"\n s12 = "<<kk.s12;
//        cout<<"\n s13 = "<<kk.s13;
//        cout<<"\n s14 = "<<kk.s14;
//        cout<<"\n s15 = "<<kk.s15;
//        cout<<"\n s23 = "<<kk.s23;
//        cout<<"\n s24 = "<<kk.s24;
//        cout<<"\n s25 = "<<kk.s25;
//        cout<<"\n s34 = "<<kk.s34;
//        cout<<"\n s35 = "<<kk.s35;
//        cout<<"\n s45 = "<<kk.s45;
//        cout<<"\n s3 = "<<kk.s3;
//        cout<<"\ns4= "<<kk.s4;
//        cout<<"\np1    "<<kk.p1;
//        cout<<"\np2    "<<kk.p2;
//        cout<<"\np3    "<<kk.p3;
//        cout<<"\np3com "<<kk.p3com;
//        cout<<"\np4    "<<kk.p4;
//        cout<<"\np4com "<<kk.p4com;
//        cout<<"\np5    "<<kk.p5;
    double me_sq = R.eval_me(kk);
    
    const double test_result = me_sq;
    const double expected = 501.0297676;
    
    const double err = 0.01;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}



TEST_F(RealNLO,momentum_conservation)
{
        
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
        //cout<<"\n mu = "<<i<<" : "<<loc;
        res=res+loc;
        }
    cout<<endl;
    double me_sq = R.eval_me(kk);
    
    const double test_result = res;
    const double expected = 0.0;
    
    const double err = 1e-10;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
    
}

TEST_F(RealNLO,sij_identity)
{
    const double test_result = kk.s12+kk.s13+kk.s14+kk.s15+kk.s23+kk.s24+kk.s25+kk.s34+kk.s35+kk.s45;
    const double expected = 3.0*(kk.s4+kk.s3);
    
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
    kk.generate_bjorken_xs(xx_vegas);
    kk.SetNLOKinematics(xx_vegas);
    
    double me_sq;
    double limit;
    for (int i=0;i<20;i++)
        {
        xx_vegas[5]=xx_vegas[5]/4.0;
        
        kk.generate_bjorken_xs(xx_vegas);
        kk.SetNLOKinematics(xx_vegas);
        me_sq = R.eval_me(kk);
        limit = R.collinear_limit(kk,1);
        cout<<setprecision(16)<<"\nlambda = "<<setw(20)<<kk.lambda
            <<" M^2 "<<setw(20)<<me_sq
            <<" vs limit "<<setw(20)<<limit<<"\t diff "<<setw(20)<<fabs(me_sq-limit)/me_sq
        <<"\t\t"<<R.collinear_limit(kk,2);

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
    kk.generate_bjorken_xs(xx_vegas);
    kk.SetNLOKinematics(xx_vegas);
    
    double me_sq;
    double limit;
    for (int i=0;i<20;i++)
        {
        xx_vegas[5]=1.0-(1.0-xx_vegas[5])/4.0;
        
        kk.generate_bjorken_xs(xx_vegas);
        kk.SetNLOKinematics(xx_vegas);
        me_sq = R.eval_me(kk);
        limit = R.collinear_limit(kk,2);
        cout<<setprecision(16)<<"\nlambda = "<<setw(20)<<kk.lambda
        <<" M^2 "<<setw(20)<<me_sq
        <<" vs limit "<<setw(20)<<limit<<"\t diff "<<setw(20)<<fabs(me_sq-limit)/me_sq<<"\t\t"<<R.collinear_limit(kk,1);
        
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

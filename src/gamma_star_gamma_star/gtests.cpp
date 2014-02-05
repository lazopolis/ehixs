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
    kk.s12 = 955.1019552011871;
    kk.s13 = -731.5762320546179;
    kk.s14 = -218.5257231465693;
    kk.s23 = -218.5257231465692;
    kk.s24 = -731.576232054618;
    kk.s34 = 955.1019552011871;
    kk.s3 = 1.0 ;
    kk.s4 = 4.0 ;
    
    double me_sq = V.eval_me(kk);
    
    const double test_result = me_sq;
    const double expected = -6343.623082;
    
    const double err = 0.01;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}


class VirtualNNLO: public ::testing::Test {
protected:
    VirtualNNLO():VV(GstarGstarMeNNLOSoft(my_box)){}
    virtual void SetUp() {
        
        kk.s12 = 955.1019552011871;
        kk.s13 = -731.5762320546179;
        kk.s14 = -218.5257231465693;
        kk.s23 = -218.5257231465692;
        kk.s24 = -731.576232054618;
        kk.s34 = 955.1019552011871;
        kk.s3 = 1.0 ;
        kk.s4 = 4.0 ;
         Nc=3.0;
         CF=4.0/3.0;
         Nf=5.0;
        
         u = kk.s23/kk.s12;
         t = kk.s13/kk.s12;
         q3 = kk.s3/kk.s12;
         q4 = kk.s4/kk.s12;
         uu = kk.s3/kk.s12;
         vv = kk.s4/kk.s12;
         z =  0.5*uu-0.5*vv+0.5
        +0.5*sqrt(uu*uu-2.0*uu*vv+vv*vv-2.0*uu-2.0*vv+1.0);
         zp = 0.5*uu-0.5*vv+0.5
        -0.5*sqrt(uu*uu-2.0*uu*vv+vv*vv-2.0*uu-2.0*vv+1.0);
    }
    EventBox my_box;
    GstarGstarMeNNLOSoft VV;
    KinematicVariables kk;
    double u,t,q3,q4,uu,vv,z,zp,Nc,CF,Nf;
};


TEST_F(VirtualNNLO,implementation_TT37)
{
    cout<<setprecision(16);
    const double test_result = VV.TT37(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -9689.40858104894373797517414120;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(VirtualNNLO,implementation_TT36)
{
    cout<<setprecision(16);
    const double test_result = VV.TT36(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -2913.91477889412441292350101870;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}
TEST_F(VirtualNNLO,implementation_TT35)
{
    const double test_result = VV.TT35(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -24146.7013315511342778093888422;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(VirtualNNLO,implementation_TT34)
{
    const double test_result = VV.TT34(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -21609.4558847343723416062706732;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err)<<"[   INFO   ] test_result = "<<test_result
    <<" +- "<<err<<" | expected = "<<expected<<endl;
}

TEST_F(VirtualNNLO,implementation_TT33)
{
    const double test_result = VV.TT33(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = 967.831296321577430099096296403;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT32)
{
    const double test_result = VV.TT32(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = 3.38215526011150905993084244400;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT31)
{
    const double test_result = VV.TT31(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = 583.379864097706618141218191411;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT30)
{
    const double test_result = VV.TT30(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = 106716.502403794687343734613015;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT29)
{
    const double test_result = VV.TT29(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -1160.11167697307097793329515454;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT28)
{
    const double test_result = VV.TT28(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -5.28185379363880741316787378677;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT27)
{
    const double test_result = VV.TT27(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -11.5657050994512919919994595325;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT26)
{
    const double test_result = VV.TT26(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -582.295842995884578811847476484;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT25)
{
    const double test_result = VV.TT25(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -2.92600135216594815568149877453;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT24)
{
    const double test_result = VV.TT24(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = 161347.564729601884305252315502;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT23)
{
    const double test_result = VV.TT23(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = 385.851864412344514840664682027;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT22)
{
    const double test_result = VV.TT22(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -1935.68044135218675070562882026;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT21)
{
    const double test_result = VV.TT21(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -2.03789163155393019221124836309;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT20)
{
    const double test_result = VV.TT20(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -767.306657350827012218104009301;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT19)
{
    const double test_result = VV.TT19(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -1.34787697330208523715823109677;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT18)
{
    const double test_result = VV.TT18(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -.851455571825556348110817022163;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT17)
{
    const double test_result = VV.TT17(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -579.078053736226206028007886928;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT16)
{
    const double test_result = VV.TT16(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -1.32519501120760920615664875542;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT15)
{
    const double test_result = VV.TT15(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -376.553938497653696872012239553;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT14)
{
    const double test_result = VV.TT14(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = -383.452534895281095038310878619;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT13)
{
    const double test_result = VV.TT13(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = .847872482726991024895305788899;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
}

TEST_F(VirtualNNLO,implementation_TT12)
{
    const double test_result = VV.TT12(u,t,q3,q4,z,zp,Nc,Nf,CF);
    const double expected = 963.700238231755431426885893699;
    const double err = 1e-5;
    EXPECT_LT(fabs(test_result-expected),err);
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

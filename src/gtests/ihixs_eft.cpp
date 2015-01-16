/** testing UserInterface.*:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>



using namespace std;

#include "gtest/gtest.h"
#include "higgs_eft.h"
//#include "inclusive_process.h"
#include "math.h"
class HiggsEFTMe: public ::testing::Test {
protected:
    virtual void SetUp()
        {
            err=1e-4;
            z=0.72021484375;
            lz = log(1.-z);
        }
    double err;
    double z;
    double lz;
};


// testing the log(muf/mh) *INDEPENDENT* terms of delta and plus
// partonic cross sections within the Higgs eft, aka Egg_LO, NLO, NNLO and N3LO
// against ihixs3
//
// the delta pieces are pure numbers
// the plus pieces are pure numbers times log(1-z)^a
// we have tested here with z=0.72021484375

TEST_F(HiggsEFTMe,LO_delta)
{
    const double res = HEFT::n_LO_delta();
    const double exp = 1.0;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,NLO_delta)
{
    const double res = HEFT::n_NLO_delta();
    const double exp = 9.8696;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,NNLO_delta)
{
    const double res = HEFT::n_NNLO_delta();
    const double exp = 13.61056;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,N3LO_delta)
{
    const double res = HEFT::n_N3LO_delta();
    const double exp = 1124.308;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,N3LO_delta_Logs_muf)
{
    const double L=log(0.34)*2.;
    const double res = HEFT::n_N3LO_delta_L()*L
                    +HEFT::n_N3LO_delta_L2()*L*L
                    +HEFT::n_N3LO_delta_L3()*L*L*L;
    const double exp = -1379.1391893802765 ;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,NLO_plus)
{
    const double res = HEFT::n_NLO_D1()*lz;
    const double exp = -15.2848;
    EXPECT_LT(abs((res-exp)/exp),err);
}

TEST_F(HiggsEFTMe,NNLO_plus)
{
    const double res =   HEFT::n_NNLO_D0()
                        +HEFT::n_NNLO_D1()*lz
                        +HEFT::n_NNLO_D2()*lz*lz
                        +HEFT::n_NNLO_D3()*lz*lz*lz;
    const double exp = 161.259;
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
}

TEST_F(HiggsEFTMe,N3LO_plus)
{
    const double res =   HEFT::n_N3LO_D0()
                        +HEFT::n_N3LO_D1()*lz
                        +HEFT::n_N3LO_D2()*lz*lz
                        +HEFT::n_N3LO_D3()*lz*lz*lz
                        +HEFT::n_N3LO_D4()*pow(lz,4.)
                        +HEFT::n_N3LO_D5()*pow(lz,5.);
    
    const double exp = 23173.424870322367;
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
}

TEST(QuarkGluonMe,NLO_real)
{
    const double err=1e-4;
    const double z=  0.85171737512117740      ;
    const double L=  -2.4079456086518722      ;
    const double exp = -0.45699212489258401      ;
    const double res = HEFT::qg_nlo_r_lz0(z, L)
                    +HEFT::qg_nlo_r_lz1(z, L);
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;

}

TEST(QuarkGluonMe,NNLO_real)
{
    const double err=1e-4;
    const double z=   4.0067637905890435E-002 ;
    const double L=  -2.4079456086518722      ;
    const double exp =   1956.8745797159809      ;
    const double res = HEFT::qg_nnlo_r_lz0_const(z, L)
                        +HEFT::qg_nnlo_r_lz0_logz(z, L)
                        +HEFT::qg_nnlo_r_lz0_logz_sq(z, L)
                        +HEFT::qg_nnlo_r_lz0_logz_cube(z, L)
                        +HEFT::qg_nnlo_r_lz1(z, L)
                        +HEFT::qg_nnlo_r_lz2(z, L)
                        +HEFT::qg_nnlo_r_lz3(z, L)
                        ;
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
    
}

TEST(QuarkGluonMe,N3LO_real_pure_L)
{
    const double err=1e-4;
    const double z=  0.47778320312500000      ;
    const double L=  -2.4079456086518722      ;
    const double exp =  -110.31958404031639      ;
    const double res =
     HEFT::qg_n3lo_r_lz0(z, L)-HEFT::qg_n3lo_r_lz0(z, 0.0)
    +HEFT::qg_n3lo_r_lz1(z, L)-HEFT::qg_n3lo_r_lz1(z, 0.0)
    +HEFT::qg_n3lo_r_lz2(z, L)-HEFT::qg_n3lo_r_lz2(z, 0.0)
    +HEFT::qg_n3lo_r_lz3(z, L)-HEFT::qg_n3lo_r_lz3(z, 0.0)
    +HEFT::qg_n3lo_r_lz4(z, L)-HEFT::qg_n3lo_r_lz4(z, 0.0)
    +HEFT::qg_n3lo_r_lz5(z, L)-HEFT::qg_n3lo_r_lz5(z, 0.0)
    ;
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
    
}



TEST(QuarkAntiQuarkMe,NLO_real)
{
    const double err=1e-14;
    const double z=  0.26902993725957480      ;
    const double L=  -2.4079456086518722      ;
    const double exp =   1.7206176577339973      ;
    const double res =HEFT::qqb_nlo_r_lz0(z, L);

    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
    
}

TEST(QuarkAntiQuarkMe,NNLO_real)
{
    const double err=1e-11;
    const double z=  0.38549804687500000      ;
    const double L=  -2.4079456086518722      ;
    const double exp = -0.63860952025809681      ;
    const double res =HEFT::qqb_nnlo_r_lz0_const(z, L)
    +HEFT::qqb_nnlo_r_lz0_logz(z, L)
    +HEFT::qqb_nnlo_r_lz0_logz_sq(z, L)
    +HEFT::qqb_nnlo_r_lz0_logz_cube(z, L)
    +HEFT::qqb_nnlo_r_lz1(z, L)
    +HEFT::qqb_nnlo_r_lz2(z, L)
    ;
    
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
    
}

TEST(QuarkQuarkMe,NNLO_real)
{
    const double err=1e-10;
    const double z=  0.99284246852927549      ;
    const double L=  -2.4079456086518722      ;
    const double exp =  0.22253443807349785      ;
    const double res =HEFT::qq_nnlo_r_lz0_const(z, L)
    +HEFT::qq_nnlo_r_lz0_logz(z, L)
    +HEFT::qq_nnlo_r_lz0_logz_sq(z, L)
    +HEFT::qq_nnlo_r_lz0_logz_cube(z, L)
    +HEFT::qq_nnlo_r_lz1(z, L)
    +HEFT::qq_nnlo_r_lz2(z, L)
    ;
    
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
    
}

TEST(QuarkQuarkPrimeMe,NNLO_real)
{
    const double err=1e-10;
    const double z=   2.1448782891849743E-002 ;
    const double L=  -2.4079456086518722      ;
    const double exp =   2367.8814723140094      ;
    const double res =HEFT::q1q2_nnlo_r_lz0_const(z, L)
    +HEFT::q1q2_nnlo_r_lz0_logz(z, L)
    +HEFT::q1q2_nnlo_r_lz0_logz_sq(z, L)
    +HEFT::q1q2_nnlo_r_lz0_logz_cube(z, L)
    +HEFT::q1q2_nnlo_r_lz1(z, L)
    +HEFT::q1q2_nnlo_r_lz2(z, L)
    ;
    
    EXPECT_LT(abs((res-exp)/exp),err)<<" res="<<res<<" expected = "<<exp<<endl;
    
}

TEST(LogMufOverMh,N3LO_gg)
{
    const double z=   2.1448782891849743E-002 ;
    const double L=  -2.4079456086518722      ;
    const double falko = HEFT::LEggN3LOregFalko(z, L);
    const double ste = HEFT::LEggNNNLOregSte(z, L);
    cout<<"\nfalko="<<falko;
    cout<<"\nste  ="<<ste<<endl;
    const double err=1e-5;
    EXPECT_LT(abs(falko-ste)/abs(ste),err)
        <<" falko="<<falko
        <<" ste="<<ste
        <<"\t % diff = "<<abs(falko-ste)/abs(ste)
        <<endl;
    
}

TEST(LogMufOverMh,N3LO_gg2)
{
    const double z=   .591448782891849743 ;
    const double L=  2.9324079456086518722      ;
    for (int i=1;i<20;i++)
    {
        double zz=(i)/20.0;
        const double falko = HEFT::LEggN3LOregFalko(zz, L);
        const double ste = HEFT::LEggNNNLOregSte(zz, L);
        cout<<"\nz="<<setw(6)<<zz<<" L="<<setw(8)<<L
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<abs(falko-ste)/abs(ste);
        
    }
    
    for (int i=1;i<20;i++)
    {
        double LL=-10.2 + 10.0 * (i)/20.0;
        const double falko = HEFT::LEggN3LOregFalko(z, LL);
        const double ste = HEFT::LEggNNNLOregSte(z, LL);
        cout<<"\nz="<<setw(6)<<fixed<<setprecision(3)<<z<<" L="<<setw(8)<<LL
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<scientific<<abs(falko-ste)/abs(ste);
        
    }
    
    const double falko = HEFT::LEggN3LOregFalko(z, L);
    const double ste = HEFT::LEggNNNLOregSte(z, L);
    cout<<"\nfalko="<<falko;
    cout<<"\nste  ="<<ste<<endl;
    const double err=1e-5;
    EXPECT_LT(abs(falko-ste)/abs(ste),err)
    <<" falko="<<falko
    <<" ste="<<ste
    <<"\t % diff = "<<abs(falko-ste)/abs(ste)
    <<endl;
    
}

TEST(LogMufOverMh,N3LO_qg)
{
    const double z=   .591448782891849743 ;
    const double L=  -1.3456      ;
    for (int i=1;i<20;i++)
    {
        double zz=(i)/20.0;
        const double falko = HEFT::LEqgN3LOregFalko(zz, L);
        const double ste = HEFT::LEqgNNNLOregSte(zz, L);
        cout<<"\nz="<<setw(6)<<zz<<" L="<<setw(8)<<L
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<abs(falko-ste)/abs(ste);
        
    }
    
    for (int i=1;i<20;i++)
    {
        double LL=-10.+1e-3 + 20.0 * (i)/20.0;
        const double falko = HEFT::LEqgN3LOregFalko(z, LL);
        const double ste = HEFT::LEqgNNNLOregSte(z, LL);
        cout<<"\nz="<<setw(6)<<fixed<<setprecision(3)<<z<<" L="<<setw(8)<<LL
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<scientific<<abs(falko-ste)/abs(ste);
        
    }
    
    const double falko = HEFT::LEqgN3LOregFalko(z, L);
    const double ste = HEFT::LEqgNNNLOregSte(z, L);
    cout<<"\nfalko="<<falko;
    cout<<"\nste  ="<<ste<<endl;
    const double err=1e-5;
    EXPECT_LT(abs(falko-ste)/abs(ste),err)
    <<" falko="<<falko
    <<" ste="<<ste
    <<"\t % diff = "<<abs(falko-ste)/abs(ste)
    <<endl;
    
}


TEST(LogMufOverMh,N3LO_qqbar)
{
    const double z=   .591448782891849743 ;
    const double L=  -1.3456      ;
    for (int i=1;i<20;i++)
    {
        double zz=(i)/20.0;
        const double falko = HEFT::LEqqbN3LOregFalko(zz, L);
        const double ste = HEFT::LEqqbNNNLOregSte(zz, L);
        cout<<"\nz="<<setw(6)<<zz<<" L="<<setw(8)<<L
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<abs(falko-ste)/abs(ste);
        
    }
    
    for (int i=1;i<20;i++)
    {
        double LL=-10.+1e-3 + 20.0 * (i)/20.0;
        const double falko = HEFT::LEqqbN3LOregFalko(z, LL);
        const double ste = HEFT::LEqqbNNNLOregSte(z, LL);
        cout<<"\nz="<<setw(6)<<fixed<<setprecision(3)<<z<<" L="<<setw(8)<<LL
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<scientific<<abs(falko-ste)/abs(ste);
        
    }
    
    const double falko = HEFT::LEqqbN3LOregFalko(z, L);
    const double ste = HEFT::LEqqbNNNLOregSte(z, L);
    cout<<"\nfalko="<<falko;
    cout<<"\nste  ="<<ste<<endl;
    const double err=1e-5;
    EXPECT_LT(abs(falko-ste)/abs(ste),err)
    <<" falko="<<falko
    <<" ste="<<ste
    <<"\t % diff = "<<abs(falko-ste)/abs(ste)
    <<endl;
    
}


TEST(LogMufOverMh,N3LO_qq)
{
    const double z=   .591448782891849743 ;
    const double L=  -1.3456      ;
    for (int i=1;i<20;i++)
    {
        double zz=(i)/20.0;
        const double falko = HEFT::LEqqN3LOregFalko(zz, L);
        const double ste = HEFT::LEqqNNNLOregSte(zz, L);
        cout<<"\nz="<<setw(6)<<zz<<" L="<<setw(8)<<L
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<abs(falko-ste)/abs(ste);
        
    }
    
    for (int i=1;i<20;i++)
    {
        double LL=-10.+1e-3 + 20.0 * (i)/20.0;
        const double falko = HEFT::LEqqN3LOregFalko(z, LL);
        const double ste = HEFT::LEqqNNNLOregSte(z, LL);
        cout<<"\nz="<<setw(6)<<fixed<<setprecision(3)<<z<<" L="<<setw(8)<<LL
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<scientific<<abs(falko-ste)/abs(ste);
        
    }
    
    const double falko = HEFT::LEqqN3LOregFalko(z, L);
    const double ste = HEFT::LEqqNNNLOregSte(z, L);
    cout<<"\nfalko="<<falko;
    cout<<"\nste  ="<<ste<<endl;
    const double err=1e-5;
    EXPECT_LT(abs(falko-ste)/abs(ste),err)
    <<" falko="<<falko
    <<" ste="<<ste
    <<"\t % diff = "<<abs(falko-ste)/abs(ste)
    <<endl;
    
}


TEST(LogMufOverMh,N3LO_q1q2)
{
    const double z=   .591448782891849743 ;
    const double L=  -1.3456      ;
    for (int i=1;i<20;i++)
    {
        double zz=(i)/20.0;
        const double falko = HEFT::LEq1q2N3LOregFalko(zz, L);
        const double ste = HEFT::LEq1q2NNNLOregSte(zz, L);
        cout<<"\nz="<<setw(6)<<zz<<" L="<<setw(8)<<L
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<abs(falko-ste)/abs(ste);
        
    }
    
    for (int i=1;i<20;i++)
    {
        double LL=-10.+1e-3+sqrt(2.)/2. + 20.0 * (i)/20.0;
        const double falko = HEFT::LEq1q2N3LOregFalko(z, LL);
        const double ste = HEFT::LEq1q2NNNLOregSte(z, LL);
        cout<<"\nz="<<setw(6)<<fixed<<setprecision(3)<<z<<" L="<<setw(8)<<LL
        <<"\tfalko="<<setw(9)<<falko
        <<"\tste  ="<<setw(9)<<ste
        <<"\t % diff = "<<setw(12)<<scientific<<abs(falko-ste)/abs(ste);
        
    }
    
    const double falko = HEFT::LEq1q2N3LOregFalko(z, L);
    const double ste = HEFT::LEq1q2NNNLOregSte(z, L);
    cout<<"\nfalko="<<falko;
    cout<<"\nste  ="<<ste<<endl;
    const double err=1e-5;
    EXPECT_LT(abs(falko-ste)/abs(ste),err)
    <<" falko="<<falko
    <<" ste="<<ste
    <<"\t % diff = "<<abs(falko-ste)/abs(ste)
    <<endl;
    
}




int main(int argc, char**argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return  RUN_ALL_TESTS();
    return 0;
}





















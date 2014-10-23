/** testing UserInterface.*:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>



using namespace std;

#include "gtest/gtest.h"
#include "higgs_eft.h"
#include "inclusive_process.h"
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


class GG_EFT_mur_eq_muf_eq_mh: public ::testing::Test {
protected:
    virtual void SetUp()
    {
        UI.m_higgs = 125.;
        UI.mur=125.;
        UI.muf=125.;
        UI.Etot = 13000.;
        UI.perturbative_order = 2;
        UI.epsrel = 0.5e-3;
        UI.matrix_element_approximation = "pure_eft";
        IP = new InclusiveProcess(UI);
        Kfactor = 1.0621829361965891;
        // ihixs 3.0 returns the xs * rescaling factor
        // so we need to divide by this Kfactor the ihixs results to get pure eft
    }
    
    UserInterface UI;
    InclusiveProcess* IP;
    double Kfactor;
};

TEST_F(GG_EFT_mur_eq_muf_eq_mh,LO_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(2);
    const double mc_error = IP->CoefficientAlphaSError(2);
    //ihixs value 13.74 +- 0.05
    const double exp = 13.736/Kfactor;
    const double exp_err = 0.05/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh,NLO_delta_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(3);
    const double mc_error = IP->CoefficientAlphaSError(3);
    //ihixs value 7.51245 +- 0.040977
    const double exp = 7.51245/Kfactor;
    const double exp_err = 0.040977/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh,NLO_plus_contribution)
{
    IP->Evaluate("D0");
    IP->Evaluate("D1");
    const double res =   IP->CoefficientAlphaS(3);
    const double mc_error = IP->CoefficientAlphaSError(3);
    //ihixs value 4.97792 +- 0.0175971
    const double exp = 4.97792/Kfactor;
    const double exp_err = 0.0175971/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh,NLO_real_contribution)
{
    IP->Evaluate("NLO Real const");
    IP->Evaluate("NLO Real log(1-z)");
    const double res =   IP->CoefficientAlphaS(3);
    const double mc_error = IP->CoefficientAlphaSError(3);
    //ihixs value 5.34196 +- 0.0230309
    const double exp = 5.34196/Kfactor;
    const double exp_err = 0.0230309/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh,NNLO_delta_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(4);
    const double mc_error = IP->CoefficientAlphaSError(4);
    //ihixs value 1.46225 +- 0.00797594  	chisq 0.03542 (10 df)
    const double exp = 1.46225/Kfactor;
    const double exp_err = 0.00797594/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh,NNLO_plus_contribution)
{
    IP->Evaluate("D0");
    IP->Evaluate("D1");
    IP->Evaluate("D2");
    IP->Evaluate("D3");
    const double res =   IP->CoefficientAlphaS(4);
    const double mc_error = IP->CoefficientAlphaSError(4);
    //ihixs value 2.89595 +- 0.0388459
    const double exp = 2.89595/Kfactor;
    const double exp_err = 0.0388459/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh,NNLO_real_contribution)
{
    IP->Evaluate("NLO Real const");
    IP->Evaluate("NLO Real log(1-z)");
    IP->Evaluate("NNLO Real const");
    IP->Evaluate("NNLO Real log(z)");
    IP->Evaluate("NNLO Real log(z)^2");
    IP->Evaluate("NNLO Real log(z)^3");
    IP->Evaluate("NNLO Real log(1-z)");
    IP->Evaluate("NNLO Real log(1-z)^2");
    IP->Evaluate("NNLO Real log(1-z)^3");

    const double res =   IP->CoefficientAlphaS(4);
    const double mc_error = IP->CoefficientAlphaSError(4);
    //ihixs value 7.10374 +- 0.0520862
    const double exp = 7.10374/Kfactor;
    const double exp_err = 0.0520862/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}



TEST_F(GG_EFT_mur_eq_muf_eq_mh,N3LO_delta_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(5);
    const double mc_error = IP->CoefficientAlphaSError(5);
    //ihixs value 0.852627 +- 0.0046507  	chisq 0.03542 (10 df)
    const double exp = 0.852627/Kfactor;
    const double exp_err = 0.0046507/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh,N3LO_plus_contribution)
{
    IP->Evaluate("D0");
    IP->Evaluate("D1");
    IP->Evaluate("D2");
    IP->Evaluate("D3");
    IP->Evaluate("D4");
    IP->Evaluate("D5");
    const double res =   IP->CoefficientAlphaS(5);
    const double mc_error = IP->CoefficientAlphaSError(5);
    //ihixs value -0.516721 +- 0.0600
    const double exp = -0.516721/Kfactor;
    const double exp_err = 0.0600/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

class GG_EFT_mur_eq_muf_eq_mh_over_2: public ::testing::Test {
protected:
    virtual void SetUp()
    {
        UI.m_higgs = 125.;
        UI.mur=125./2.;
        UI.muf=125./2.;
        UI.Etot = 13000.;
        UI.perturbative_order = 2;
        UI.epsrel = 0.5e-4;
        UI.matrix_element_approximation = "pure_eft";
        IP = new InclusiveProcess(UI);
        Kfactor = 1.0547838527754434;
        // ihixs 3.0 returns the xs * rescaling factor
        // so we need to divide by this Kfactor the ihixs results to get pure eft
        // note that this factor depends on mur (through the mur dependence of quark masses that enter the LO_exact/LO_eff ratio)
    }
    
    UserInterface UI;
    InclusiveProcess* IP;
    double Kfactor;
};




TEST_F(GG_EFT_mur_eq_muf_eq_mh_over_2,LO_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(2);
    const double mc_error = IP->CoefficientAlphaSError(2);
    //ihixs value 15.8981 +- 0.0882885
    const double exp = 15.8981/Kfactor;
    const double exp_err = 0.0882885/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh_over_2,NLO_delta_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(3);
    const double mc_error = IP->CoefficientAlphaSError(3);
    //ihixs value 9.65311 +- 0.0536076
    const double exp = 9.65311/Kfactor;
    const double exp_err = 0.0536076/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh_over_2,NLO_plus_contribution)
{
    IP->Evaluate("D0");
    IP->Evaluate("D1");
    const double res =   IP->CoefficientAlphaS(3);
    const double mc_error = IP->CoefficientAlphaSError(3);
    //ihixs value 1.61312 +- 0.0230065
    const double exp = 1.61312/Kfactor;
    const double exp_err = 0.0230065/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh_over_2,NLO_real_contribution)
{
    IP->Evaluate("NLO Real const");
    IP->Evaluate("NLO Real log(1-z)");
    const double res =   IP->CoefficientAlphaS(3);
    const double mc_error = IP->CoefficientAlphaSError(3);
    //ihixs value 7.7515 +- 0.0571221
    const double exp = 7.7515/Kfactor;
    const double exp_err = 0.0571221/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}




TEST_F(GG_EFT_mur_eq_muf_eq_mh_over_2,NNLO_delta_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(4);
    const double mc_error = IP->CoefficientAlphaSError(4);
    //ihixs value 3.2132 +- 0.0178442  	chisq 0.03542 (10 df)
    const double exp = 3.2132/Kfactor;
    const double exp_err = 0.0178442/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh_over_2,NNLO_plus_contribution)
{
    IP->Evaluate("D0");
    IP->Evaluate("D1");
    IP->Evaluate("D2");
    IP->Evaluate("D3");
    const double res =   IP->CoefficientAlphaS(4);
    const double mc_error = IP->CoefficientAlphaSError(4);
    //ihixs value -0.557854 +- 0.0367134
    const double exp =  -0.557854/Kfactor;
    const double exp_err = 0.0367134/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh_over_2,NNLO_real_contribution)
{
    IP->Evaluate("NLO Real const");
    IP->Evaluate("NLO Real log(1-z)");
    IP->Evaluate("NNLO Real const");
    IP->Evaluate("NNLO Real log(z)");
    IP->Evaluate("NNLO Real log(z)^2");
    IP->Evaluate("NNLO Real log(z)^3");
    IP->Evaluate("NNLO Real log(1-z)");
    IP->Evaluate("NNLO Real log(1-z)^2");
    IP->Evaluate("NNLO Real log(1-z)^3");
    
    const double res =   IP->CoefficientAlphaS(4);
    const double mc_error = IP->CoefficientAlphaSError(4);
    //ihixs value 5.76391 +- 0.0459448
    const double exp = 5.76391/Kfactor;
    const double exp_err = 0.0459448/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh_over_2,N3LO_delta_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(5);
    const double mc_error = IP->CoefficientAlphaSError(5);
    //ihixs value 0.932932 +- 0.00225753  	chisq 0.03542 (10 df)
    const double exp = 0.932626/Kfactor;
    const double exp_err = 0.00225753/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_eq_muf_eq_mh_over_2,N3LO_plus_contribution)
{
    IP->Evaluate("D0");
    IP->Evaluate("D1");
    IP->Evaluate("D2");
    IP->Evaluate("D3");
    IP->Evaluate("D4");
    IP->Evaluate("D5");
    const double res =   IP->CoefficientAlphaS(5);
    const double mc_error = IP->CoefficientAlphaSError(5);
    //ihixs value 0.665845 +- 0.0511546
    //ihixs value -1.60858 +- 0.0909292
    const double exp = (0.665845-1.60858)/Kfactor;
    const double exp_err = 0.103/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

class GG_EFT_mur_dif_muf_dif_mh: public ::testing::Test {
protected:
    virtual void SetUp()
    {
        UI.m_higgs = 125.;
        UI.mur=125./2.;
        UI.muf=125.*2.;
        UI.Etot = 13000.;
        UI.perturbative_order = 2;
        UI.epsrel = 0.5e-4;
        UI.matrix_element_approximation = "pure_eft";
        IP = new InclusiveProcess(UI);
        Kfactor = 1.0547838527754434;
        // ihixs 3.0 returns the xs * rescaling factor
        // so we need to divide by this Kfactor the ihixs results to get pure eft
        // note that this factor depends on mur (through the mur dependence of quark masses that enter the LO_exact/LO_eff ratio)
        
    }
    
    UserInterface UI;
    InclusiveProcess* IP;
    double Kfactor;
};



TEST_F(GG_EFT_mur_dif_muf_dif_mh,LO_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(2);
    const double mc_error = IP->CoefficientAlphaSError(2);
    //ihixs value 17.421 +- 0.0919379
    const double exp = 17.421/Kfactor;
    const double exp_err = 0.0919379/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_dif_muf_dif_mh,NLO_delta_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(3);
    const double mc_error = IP->CoefficientAlphaSError(3);
    //ihixs value 3.26313 +- 0.0172209
    const double exp = 3.26313/Kfactor;
    const double exp_err = 0.0172209/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_dif_muf_dif_mh,NLO_plus_contribution)
{
    IP->Evaluate("D0");
    IP->Evaluate("D1");
    const double res =   IP->CoefficientAlphaS(3);
    const double mc_error = IP->CoefficientAlphaSError(3);
    //ihixs value 12.5133 +- 0.0466142
    const double exp = 12.5133/Kfactor;
    const double exp_err = 0.0466142/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_dif_muf_dif_mh,NLO_real_contribution)
{
    IP->Evaluate("NLO Real const");
    IP->Evaluate("NLO Real log(1-z)");
    const double res =   IP->CoefficientAlphaS(3);
    const double mc_error = IP->CoefficientAlphaSError(3);
    //ihixs value 6.86655 +- 0.0272477
    const double exp = 6.86655/Kfactor;
    const double exp_err = 0.0272477/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}




TEST_F(GG_EFT_mur_dif_muf_dif_mh,NNLO_delta_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(4);
    const double mc_error = IP->CoefficientAlphaSError(4);
    //ihixs value -7.19691 +- 0.0379811   	chisq 0.03542 (10 df)
    const double exp = -7.19691/Kfactor;
    const double exp_err = 0.0379811/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_dif_muf_dif_mh,NNLO_plus_contribution)
{
    IP->Evaluate("D0");
    IP->Evaluate("D1");
    IP->Evaluate("D2");
    IP->Evaluate("D3");
    const double res =   IP->CoefficientAlphaS(4);
    const double mc_error = IP->CoefficientAlphaSError(4);
    //ihixs value 6.08688 +- 0.0920419
    const double exp =  6.08688/Kfactor;
    const double exp_err = 0.0920419/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_dif_muf_dif_mh,NNLO_real_contribution)
{
    IP->Evaluate("NLO Real const");
    IP->Evaluate("NLO Real log(1-z)");
    IP->Evaluate("NNLO Real const");
    IP->Evaluate("NNLO Real log(z)");
    IP->Evaluate("NNLO Real log(z)^2");
    IP->Evaluate("NNLO Real log(z)^3");
    IP->Evaluate("NNLO Real log(1-z)");
    IP->Evaluate("NNLO Real log(1-z)^2");
    IP->Evaluate("NNLO Real log(1-z)^3");
    
    const double res =   IP->CoefficientAlphaS(4);
    const double mc_error = IP->CoefficientAlphaSError(4);
    //ihixs value 12.6044 +- 0.118082
    const double exp = 12.6044/Kfactor;
    const double exp_err = 0.118082/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}


TEST_F(GG_EFT_mur_dif_muf_dif_mh,N3LO_delta_contribution)
{
    IP->Evaluate("delta");
    const double res =   IP->CoefficientAlphaS(5);
    const double mc_error = IP->CoefficientAlphaSError(5);
    //ihixs value 2.1337 +- 0.0112604  	chisq 0.03542 (10 df)
    const double exp = 2.1337/Kfactor;
    const double exp_err = 0.0112604/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}

TEST_F(GG_EFT_mur_dif_muf_dif_mh,N3LO_plus_contribution)
{
    IP->Evaluate("D0");
    IP->Evaluate("D1");
    IP->Evaluate("D2");
    IP->Evaluate("D3");
    IP->Evaluate("D4");
    IP->Evaluate("D5");
    const double res =   IP->CoefficientAlphaS(5);
    const double mc_error = IP->CoefficientAlphaSError(5);
    //ihixs value -4.95466 +- 0.0797427
    //ihixs value -1.70862 +- 0.103628
    const double exp = (-4.95466-1.70862)/Kfactor;
    const double exp_err = 0.13/Kfactor;
    double err = max(mc_error,exp_err);
    EXPECT_LT(abs(res-exp),err)<<" res="<<res<<" +- "<<err
    <<" expected = "<<exp<<endl;
}


int main(int argc, char**argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return  RUN_ALL_TESTS();
    return 0;
}





















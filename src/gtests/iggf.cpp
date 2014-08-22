/** testing ehixs:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>


using namespace std;

#include "gtest/gtest.h"
//#include "exact_lo_inclusive.h"
#include "inclusive_process.h"


class IntegratedLuminosity: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        
        lumi = new Luminosity(UI);
        lumi_stack = new LuminosityStack;
        lumi_stack->set(lumi,"gg");
    }
    
    double expectation(const double& expected){
        myintegral->Configure(lumi_stack,pow(125.0,2.0)/pow(8000.0,2.0));
        myintegral->call_vegas();
        return(fabs(myintegral->result()-expected)<myintegral->error());
    }
    
    void set_IL(LuminosityIntegral* myint){myintegral=myint;}
    
    LuminosityIntegral* myintegral;
    UserInterface UI;
    Luminosity* lumi;
    LuminosityStack* lumi_stack;
};



TEST_F(IntegratedLuminosity,Delta)
{
    set_IL(new LuminosityIntegralDelta);
    EXPECT_TRUE(expectation(5.20185e+05));
}

TEST_F(IntegratedLuminosity,DD0)
{
    set_IL(new LuminosityIntegralDD0);
    EXPECT_TRUE(expectation(-7.64862e+05));
}

TEST_F(IntegratedLuminosity,DD1)
{
    set_IL(new LuminosityIntegralDD1);
    EXPECT_TRUE(expectation(8.80170e+05));
}

TEST_F(IntegratedLuminosity,DD2)
{
    set_IL(new LuminosityIntegralDD2);
    EXPECT_TRUE(expectation(-1.87170e+06));
}

TEST_F(IntegratedLuminosity,DD3)
{
    set_IL(new LuminosityIntegralDD3);
    EXPECT_TRUE(expectation(5.78286e+06));
}

TEST_F(IntegratedLuminosity,DD4)
{
    set_IL(new LuminosityIntegralDD4);
    EXPECT_TRUE(expectation(-2.34549e+07));
}

TEST_F(IntegratedLuminosity,DD5)
{
    set_IL(new LuminosityIntegralDD5);
    EXPECT_TRUE(expectation(1.18069e+08));
}


class IntegratedLuminosityQG: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        
        lumi = new Luminosity(UI);
        lumi_stack = new LuminosityStack;
        lumi_stack->set(lumi,"qg");
    }
    
    double expectation(const double& expected){
        myintegral->Configure(lumi_stack,pow(125.0,2.0)/pow(8000.0,2.0));
        myintegral->call_vegas();
        return(fabs(myintegral->result()-expected)<myintegral->error());
    }
    
    void set_IL(LuminosityIntegral* myint){myintegral=myint;}
    
    LuminosityIntegral* myintegral;
    UserInterface UI;
    Luminosity* lumi;
    LuminosityStack* lumi_stack;
};


TEST_F(IntegratedLuminosityQG,DD0)
{
    set_IL(new LuminosityIntegralDD0);
    EXPECT_TRUE(expectation(-1.31482e+06));
}

TEST_F(IntegratedLuminosityQG,DD1)
{
    set_IL(new LuminosityIntegralDD1);
    EXPECT_TRUE(expectation(1.48384e+06));
}

TEST_F(IntegratedLuminosityQG,DD2)
{
    set_IL(new LuminosityIntegralDD2);
    EXPECT_TRUE(expectation(-3.12945e+06));
}

TEST_F(IntegratedLuminosityQG,DD3)
{
    set_IL(new LuminosityIntegralDD3);
    EXPECT_TRUE(expectation(9.61899e+06));
}

TEST_F(IntegratedLuminosityQG,DD4)
{
    set_IL(new LuminosityIntegralDD4);
    EXPECT_TRUE(expectation(-3.89307e+07));
}

TEST_F(IntegratedLuminosityQG,DD5)
{
    set_IL(new LuminosityIntegralDD5);
    EXPECT_TRUE(expectation(1.95766e+08));
}

class IntegratedLuminosityQQB: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        
        lumi = new Luminosity(UI);
        lumi_stack = new LuminosityStack;
        lumi_stack->set(lumi,"qqb");
    }
    
    double expectation(const double& expected){
        myintegral->Configure(lumi_stack,pow(125.0,2.0)/pow(8000.0,2.0));
        myintegral->call_vegas();
        return(fabs(myintegral->result()-expected)<myintegral->error());
    }
    
    void set_IL(LuminosityIntegral* myint){myintegral=myint;}
    
    LuminosityIntegral* myintegral;
    UserInterface UI;
    Luminosity* lumi;
    LuminosityStack* lumi_stack;
};


TEST_F(IntegratedLuminosityQQB,DD0)
{
    set_IL(new LuminosityIntegralDD0);
    EXPECT_TRUE(expectation(-5.11636e+04));
}

TEST_F(IntegratedLuminosityQQB,DD1)
{
    set_IL(new LuminosityIntegralDD1);
    EXPECT_TRUE(expectation(5.67120e+04));
}

TEST_F(IntegratedLuminosityQQB,DD2)
{
    set_IL(new LuminosityIntegralDD2);
    EXPECT_TRUE(expectation(-1.18621e+05));
}

TEST_F(IntegratedLuminosityQQB,DD3)
{
    set_IL(new LuminosityIntegralDD3);
    EXPECT_TRUE(expectation(3.63193e+05));
}

TEST_F(IntegratedLuminosityQQB,DD4)
{
    set_IL(new LuminosityIntegralDD4);
    EXPECT_TRUE(expectation(-1.46729e+06));
}

TEST_F(IntegratedLuminosityQQB,DD5)
{
    set_IL(new LuminosityIntegralDD5);
    EXPECT_TRUE(expectation(7.37160e+06));
}



/*
TEST(interpolator_test,nlo_reg)
{
     IME_Interpolator nlo_reg(&gg_NLO_reg);
     nlo_reg.initialize();
     double x=0.24;
     double xs=nlo_reg.give_f(x);
     
     double expected_xs = gg_NLO_reg(x);
     double err=1e-7;
     cout<<"\nInterpolated = "<<xs<<" vs "<<expected_xs<<" = expected"<<endl;
     EXPECT_LT(fabs(xs-expected_xs)/expected_xs,err);
}
*/
/*
TEST(interpolator_test,nnlo_reg)
{
     IME_Interpolator nnlo_reg(&gg_NNLO_reg2);
     nnlo_reg.initialize();
     double x=1.0-0.0002;
     double xs=nnlo_reg.give_f(x);
     
     double expected_xs = gg_NNLO_reg(x);
     double err=1e-7;
     cout<<"\nInterpolated = "<<xs<<" vs "<<expected_xs<<" = expected"<<endl;
     EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}
*/
/*
TEST(ISP,type_0_x1LO)
{

    IS_Parametrization_LO * ISP = new IS_Parametrization_LO_type_0;
    double tau = pow(125.0,2.0)/pow(8000.0,2.0);
    ISP -> set_tau(tau);
	double xx_vegas=0.270599;
    ISP -> set_up_initial_state(xx_vegas);
    double res = ISP->x1LO();
    double expected = 0.00231815;
    double err = 1e-4;
    cout<<"\n res = "<<res<<" \t expected = "<<expected<<endl;
    EXPECT_LT(fabs(res-expected)/fabs(expected),err);
}

TEST(ISP,type_0_meas)
{
    
    IS_Parametrization_LO * ISP = new IS_Parametrization_LO_type_0;
    double tau = pow(125.0,2.0)/pow(8000.0,2.0);
    ISP -> set_tau(tau);
	double xx_vegas=0.316199;
    //xvegas=0.316199	x1LO=0.00338738	jac=0.0281754	measLO=8.31777
   // &&&&&----- xvegas=0.316199	jac=0.0281755	xLO=0.00338739


    ISP -> set_up_initial_state(xx_vegas);
    double res = ISP->measLO();
    double expected = 8.31777;
    double err = 1e-4;
    cout<<"\n res = "<<res<<" \t expected = "<<expected<<endl;
    EXPECT_LT(fabs(res-expected)/fabs(expected),err);
}



TEST(generalSetUp,LO_exact_coefficient)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    //Model->quarks[1]->Y() = 0.0;
    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "MSTW";
    UI->pdf_error = false;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 8000.0;
    
    Exact_LO_Inclusive default_LO(Model, UI);
    double res = default_LO.ME_sq();
    double expected =  0.996072;
    double err = 1e-4;
    cout<<"\n res = "<<res<<" \t expected = "<<expected<<endl;
    EXPECT_LT(fabs(res-expected)/fabs(expected),err);
}

TEST(generalSetUp,alpha_s)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    //Model->quarks[1]->Y() = 0.0;
    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "MSTW";
    UI->pdf_error = false;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 8000.0;
    
    Exact_LO_Inclusive default_LO(Model, UI);
    double res = default_LO.alpha_s_used();
    double expected =  0.148957;
    double err = 1e-4;
    cout<<"\n res = "<<res<<" \t expected = "<<expected<<endl;
    EXPECT_LT(fabs(res-expected)/fabs(expected),err);
}



TEST(generalSetUp,cur_lumiLO)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    //Model->quarks[1]->Y() = 0.0;
    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "MSTW";
    UI->pdf_error = false;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 8000.0;
    
    
    Luminosity* lumi = new Luminosity(*UI);
    lumi->add_pair(pdf_desc(0,0,0,0),pdf_desc(0,0,0,0));
    
    InitialState my_in_state(UI,lumi);
    my_in_state.new_state(0.580635);
    double res = my_in_state.luminosity();
    
    double expected =  29.1684;
    double err = 1e-4;
    cout<<"\n res = "<<res<<" \t expected = "<<expected<<endl;
    EXPECT_LT(fabs(res-expected)/fabs(expected),err);
}

TEST(generalSetUp,integrand)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    //Model->quarks[1]->Y() = 0.0;
    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "MSTW";
    UI->pdf_error = false;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 8000.0;
    
    
    Luminosity * lumi=new Luminosity(*UI);
    lumi->add_pair(pdf_desc(0,0,0,0),pdf_desc(0,0,0,0));
    
    IntegratedContribution integral(UI,lumi);
    

    double xx[1];xx[0]=0.279098;
    double res = integral.evaluateIntegral(xx);
    double expected =  147.254;
    double err = 1e-4;
    cout<<"\n res = "<<res<<" \t expected = "<<expected<<endl;
    EXPECT_LT(fabs(res-expected)/fabs(expected),err);
}

TEST(generalSetUp,init)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    //Model->quarks[1]->Y() = 0.0;
    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "MSTW";
    UI->pdf_error = false;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 8000.0;
    
    Exact_LO_Inclusive default_LO(Model, UI);
    default_LO.perform();
    double xs = default_LO.cross_section();
    double err = default_LO.mc_error();
    double expected_xs =  9.64074;//0.00918932
    cout<<"\n xs = "<<xs<<" \t expected = "<<expected_xs<<endl;
    EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}

TEST(generalSetUp,harlander_top_only_8TeV)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    Model->quarks[1]->set_Y(0.0);
    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "MSTW";
    UI->pdf_error = false;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 8000.0;
    
    Exact_LO_Inclusive default_LO(Model, UI);
    default_LO.perform();
    double xs = default_LO.cross_section();
    double err = default_LO.mc_error();
    double expected_xs =  0.103143147E+02;
    cout<<"\n xs = "<<xs<<" \t expected = "<<expected_xs<<endl;
    EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}

TEST(generalSetUp,harlander_top_only_13TeV)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    Model->quarks[1]->set_Y(0.0);
    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "MSTW";
    UI->pdf_error = false;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 13000.0;
    
    Exact_LO_Inclusive default_LO(Model, UI);
    default_LO.perform();
    double xs = default_LO.cross_section();
    double err = default_LO.mc_error();
    double expected_xs =  0.246005765E+02;
    cout<<"\n xs = "<<xs<<" \t expected = "<<expected_xs<<endl;
    EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}

TEST(generalSetUp,harlander_top_infty_13TeV)
{
    CModel *Model = new CModel;
    Model->quarks[0]->set_pole_mass(5000.0);
    Model->quarks[1]->set_Y(0.0);
    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "MSTW";
    UI->pdf_error = false;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 13000.0;
    
    Exact_LO_Inclusive default_LO(Model, UI);
    default_LO.perform();
    double xs = default_LO.cross_section();
    double err = default_LO.mc_error();
    double expected_xs =  0.230862304E+02;
    cout<<"\n xs = "<<xs<<" \t expected = "<<expected_xs<<endl;
    EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}

TEST(generalSetUp,harlander_top_bottom_13TeV)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    //Model->quarks[1]->Y() = 0.0;
    Model->quarks[1]->set_msbar_mass(4.3,4.3);

    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "MSTW";
    UI->pdf_error = false;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 13000.0;
    
    Exact_LO_Inclusive default_LO(Model, UI);
    default_LO.perform();
    double xs = default_LO.cross_section();
    double err = default_LO.mc_error();
    double expected_xs =  0.221489564E+02;
    cout<<"\n xs = "<<xs<<" \t expected = "<<expected_xs<<endl;
    EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}

TEST(CoolInt,selfKeepingCentralXS)
{
    AnotherInt my_int;
    my_int.call_vegas();
    double actual = my_int.central_value();
    double expected = my_int.result();
    double err = 1e-4;
    cout<<"\n actual = "<<actual<<" \t expected = "<<expected<<endl;
    EXPECT_LT(fabs(actual-expected)/fabs(expected),err);
}

TEST(PDFerror,harlander_top_only_8TeV)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    Model->quarks[1]->set_Y(0.0);
    
    UserInterface* UI = new UserInterface;
    UI->number_of_flavours = 5.0;
    UI->muf_over_mhiggs = 0.5;
    UI->m_higgs = Model->higgs.m();
    UI->mur_over_mhiggs = 0.5;
    UI->perturbative_order = 0;
    UI->pdf_provider = "NNPDF";
    UI->pdf_error = true;
    UI->epsrel=1e-3;
    UI->epsabs = 1e-10;
    UI->Etot = 8000.0;
    
    Exact_LO_Inclusive default_LO(Model, UI);
    default_LO.perform();
    default_LO.show_pdf_results();
    vector<double> pdf_error = default_LO.pdf_error();
    double actual = pdf_error[0];
    double err = 1e-5;
    double expected =  0.1;
    cout<<"\n actual = "<<actual<<" \t expected = "<<expected<<endl;
    EXPECT_LT(fabs(actual-expected)/fabs(expected),err);
}
*/

int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}

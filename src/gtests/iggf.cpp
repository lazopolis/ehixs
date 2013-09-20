/** testing ehixs:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>


using namespace std;

#include "gtest/gtest.h"
#include "exact_lo_inclusive.h"

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
    //Model->quarks[1]->Y = 0.0;
    
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
    //Model->quarks[1]->Y = 0.0;
    
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
    //Model->quarks[1]->Y = 0.0;
    
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
    
    
    Luminosity* lumi = new Luminosity(UI->number_of_flavours,
                                      UI->muf_over_mhiggs * UI->m_higgs,
                                      UI->mur_over_mhiggs * UI->m_higgs,
                                      UI->perturbative_order,
                                      UI->pdf_provider,
                                      UI->pdf_error);
    lumi->add_pair(Luminosity::F_g_00,Luminosity::F_g_00);
    
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
    //Model->quarks[1]->Y = 0.0;
    
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
    
    
    Luminosity * lumi=new Luminosity(UI->number_of_flavours,
                        UI->muf_over_mhiggs * UI->m_higgs,
                        UI->mur_over_mhiggs * UI->m_higgs,
                        UI->perturbative_order,
                        UI->pdf_provider,
                        UI->pdf_error);
    lumi->add_pair(Luminosity::F_g_00,Luminosity::F_g_00);
    
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
    //Model->quarks[1]->Y = 0.0;
    
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
    Model->quarks[1]->Y = 0.0;
    
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
    Model->quarks[1]->Y = 0.0;
    
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
    Model->quarks[0]->set_m_at_ref_scale(5000.0);
    Model->quarks[1]->Y = 0.0;
    
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
    //Model->quarks[1]->Y = 0.0;
    Model->quarks[1]->set_reference_scale(4.3);
    Model->quarks[1]->set_m_at_ref_scale(4.3);
    
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
    Model->quarks[1]->Y = 0.0;
    
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


int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}

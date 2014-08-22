
//    testing convolutions on the fly vs standard interpolated ones
//    Achilleas Lazopoulos, lazopoli@phys.ethz.ch
//    

#include <iostream>
#include <cmath>
#include <string>
using namespace std;

#include "gtest/gtest.h"
#include "pdf_hub.h"
#include "vegas_adaptor.h"




class ConvolutionIntegral: public CoolInt
{
public:
    ConvolutionIntegral():CoolInt(){};
    double evaluateIntegral(const double xx[]);
    void setPdf(CPDF* ze_pdf){the_pdf = ze_pdf;}
    void set_x(const double& xx){the_x=xx;}
private:
    CPDF* the_pdf;
    double the_x;
};

double ConvolutionIntegral::evaluateIntegral(const double xx[])
{
    vector<double> xv;
    xv.push_back(the_x);
    xv.push_back(xx[0]);
    return the_pdf->give_f(xv,0);
    
}




class TestingLiveColvolutions: public ::testing::TestWithParam<int > {
protected:
    virtual void SetUp() {
        CI.setParams(1,0.01,1e-10,
                     1000,100000000,1000,100);
        UI_interpolator.m_higgs = 125.0;
        UI_interpolator.perturbative_order = 2;
        UI_interpolator.pdf_provider = "MSTW";
        UI_interpolator.pdf_error = false;
        UI_interpolator.Etot = 8000.0;
        UI_interpolator.muf_over_mhiggs=1.0;
        UI_interpolator.mur_over_mhiggs=1.0;
        UI_interpolator.convolutions_by_interpolation = true;
        interpolated = new PDFHub(UI_interpolator);
        
        UI_live.m_higgs = 125.0;
        UI_live.perturbative_order = 2;
        UI_live.pdf_provider = "MSTW";
        UI_live.pdf_error = false;
        UI_live.Etot = 8000.0;
        UI_live.muf_over_mhiggs=1.0;
        UI_live.mur_over_mhiggs=1.0;
        UI_live.convolutions_by_interpolation = false;
        live = new PDFHub(UI_live);
        
        err = 1e-3;
    }
    
    void set_pdfs(const pdf_desc& the_desc)
        {
            f_interpolated  = interpolated->construct_or_locate_pdf(the_desc);
            f_live = live->construct_or_locate_pdf(the_desc);
            CI.setPdf(f_live);
        }
    
    double diff(const double& x,int i, int j)
        {
            set_pdfs(pdf_desc(i,j,1,1));
            CI.set_x(x);
            CI.call_vegas();
            err = CI.error();
            cout<<"\n interpolated ("<<x<<")="<<f_interpolated->give_f(x,0)
            <<"\t integrated_live ("<<x<<")="<<CI.result()<<endl;
            return ( fabs(f_interpolated->give_f(x,0)-CI.result()) < err ) ;
        
        }

    UserInterface UI_interpolator;
    UserInterface UI_live;
    PDFHub* interpolated;
    PDFHub* live;
    CPDF* f_interpolated;
    CPDF* f_live;
    ConvolutionIntegral CI;
    double err;
};



//TEST_F(TestingLiveColvolutions,F_0_0_0_0)
//{
//    set_pdfs(pdf_desc(0,0,0,0));
//    EXPECT_LT(fabs(diff(0.234)),err);
//}

TEST_P(TestingLiveColvolutions,F_0_x_1_1)
{
    EXPECT_TRUE(diff(0.234,0,GetParam()));
}

INSTANTIATE_TEST_CASE_P(F_0_x_1_1,
                        TestingLiveColvolutions,
                        ::testing::Range(-5,6));


TEST_P(TestingLiveColvolutions,F_x_x_1_1)
{
    EXPECT_TRUE(diff(0.234,GetParam(),GetParam()));
}
INSTANTIATE_TEST_CASE_P(F_x_x_1_1,
                        TestingLiveColvolutions,
                        ::testing::Range(-5,6));

TEST_P(TestingLiveColvolutions,F_x_0_1_1)
{
    EXPECT_TRUE(diff(0.234,GetParam(),0));
}
INSTANTIATE_TEST_CASE_P(F_x_0_1_1,
                        TestingLiveColvolutions,
                        ::testing::Range(-5,6));

int main(int argc, char**argv)
{
    cout << "\ntesting ehixs\n" << endl;
    
    ::testing::InitGoogleTest(&argc, argv);
    return  RUN_ALL_TESTS();
    
    return 0;
}



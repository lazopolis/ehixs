//    testing ehixs
//    Achilleas Lazopoulos, lazopoli@phys.ethz.ch
//    gluon fusion / NLO / effective field theory / all initial state channels

#include <iostream>
#include <cmath>
#include <string>
using namespace std;


#include "ggf_headers.h"



#include "gtest/gtest.h"

class SectorPerformer{
public:
    SectorPerformer(){};
    void run_sector_expecting(const double& expected_xs, const UserInterface& UI)
    {
        Process* cur_process = new Process(UI);
        cur_process->perform();
        double xs=cur_process->total_xs();
        double err=cur_process->total_err();
        EXPECT_LT(fabs(xs-expected_xs),err)<<"[   INFO   ] xs = "<<xs
        <<" +- "<<err<<" | expected = "<<expected_xs;
    }
    void run_necessary_sectors_expecting(const double& expected_xs, UserInterface& UI){
        int number_of_sectors = 1;
        double xs = 0.0;
        double err = 0.0;
        for (int i=0;i<number_of_sectors;i++)
            {
            stringstream s;s<<i;
            UI.sector_for_production = s.str();
            Process* cur_process = new Process(UI);
            number_of_sectors = cur_process->number_of_necessary_sectors();
            cur_process->perform();
            
            xs += cur_process->total_xs();
            err += pow(cur_process->total_err(),2.0);
            }
        err = sqrt(err);
        EXPECT_LT(fabs(xs-expected_xs),err)<<"[   INFO   ] xs = "<<xs
        <<" +- "<<err<<" | expected = "<<expected_xs;
    }
    string sector_name(const UserInterface& UI){
        Process* cur_process = new Process(UI);
        return cur_process->sector_name();
    }
};

class GluonGluon: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        UI.matrix_element_approximation = "effective";
        UI.pole=0;
        UI.Fleft = "gluon";
        UI.Fright = "gluon";
    }
    
    void run_sector_expecting(const double& expected_xs)
                    {SP.run_sector_expecting(expected_xs,UI);}
    
    void run_necessary_sectors_expecting(const double& expected_xs)
                    {SP.run_necessary_sectors_expecting(expected_xs,UI);}
    string sector_name(){return SP.sector_name(UI);}

    UserInterface UI;
    SectorPerformer SP;
};

TEST_F(GluonGluon,LO_xsection_muf_muf_mh)
{
    UI.alpha_s_power = 2;// LO sector
    UI.perturbative_order = 0; // LO pdfs and a_s
    UI.sector_for_production = "0"; // the first (and only) sector
    run_sector_expecting(7.608);
}

TEST_F(GluonGluon,NLO_log_sector_name)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "0"; // the first  sector
    ASSERT_EQ(sector_name(),"F_gluon_from_gluon_00(*)F_gluon_from_gluon_00(*)(c0^2 a^2)(*)(a)*(2*b0*L)(*)(1)(*)S(gluon,gluon,B,a^0,e^0 ,dim=2) : a^3,e^0");
}


TEST_F(GluonGluon,NLO_log_sector_value)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "0"; // the first  sector
    run_sector_expecting(0.0);// because mur=muf=mh
}

TEST_F(GluonGluon,NLO_2xC0xC1_sector_name)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "1"; // the first  sector
    ASSERT_EQ(sector_name(),"F_gluon_from_gluon_00(*)F_gluon_from_gluon_00(*)(2*c0*c1* a^3)(*)(1)(*)(1)(*)S(gluon,gluon,B,a^0,e^0 ,dim=2) : a^3,e^0");
}


TEST_F(GluonGluon,NLO_2xC0xC1_sector_value)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "1"; // the first  sector
    run_sector_expecting(1.18836);
}

TEST_F(GluonGluon,NLO_b0_sector_name)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "2"; // the first  sector
    ASSERT_EQ(sector_name(),"F_gluon_from_gluon_00(*)F_gluon_from_gluon_00(*)(c0^2 a^2)(*)(a)*(-2*b0/e)(*)(1)(*)S(gluon,gluon,B,a^0,e^1 ,dim=2) : a^3,e^0");
}


TEST_F(GluonGluon,NLO_b0_sector_value)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "2"; // the first  sector
    run_sector_expecting(-0.828253);
}

TEST_F(GluonGluon,NLO_soft_sector_name)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "3"; // the first  sector
    ASSERT_EQ(sector_name(),"F_gluon_from_gluon_00(*)F_gluon_from_gluon_00(*)(c0^2 a^2)(*)(1)(*)(1)(*)S(gluon,gluon,S,a^1,e^0 ,dim=2) : a^3,e^0");
}


TEST_F(GluonGluon,NLO_soft_sector_value)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "3"; // the first  sector
    run_sector_expecting(2.13249);
}

TEST_F(GluonGluon,NLO_hard_sector_name)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "4"; // the first  sector
    ASSERT_EQ(sector_name(),"F_gluon_from_gluon_00(*)F_gluon_from_gluon_00(*)(c0^2 a^2)(*)(1)(*)(1)(*)S(gluon,gluon,H,a^1,e^0 ,dim=4) : a^3,e^0");
}
// slow test
 TEST_F(GluonGluon,DISABLED_NLO_hard_sector_value)
 {
 UI.alpha_s_power = 3;// NLO sectors
 UI.perturbative_order = 1; // NLO pdfs and a_s
 UI.sector_for_production = "4"; // the first  sector
 run_sector_expecting(5.78733);
 }

 TEST_F(GluonGluon,NLO_conv_right_sector_name)
 {
 UI.alpha_s_power = 3;// NLO sectors
 UI.perturbative_order = 1; // NLO pdfs and a_s
 UI.sector_for_production = "5"; // the first  sector
 ASSERT_EQ(sector_name(),"F_gluon_from_gluon_00(*)F_gluon_from_gluon_11(*)(c0^2 a^2)(*)(1)(*)(1)(*)S(gluon,gluon,B,a^0,e^1 ,dim=2) : a^3,e^0");
 }
 
 TEST_F(GluonGluon,NLO_conv_right_sector_value)
 {
 UI.alpha_s_power = 3;// NLO sectors
 UI.perturbative_order = 1; // NLO pdfs and a_s
 UI.sector_for_production = "5"; // the first  sector
 run_sector_expecting(-0.18081);
 }
 
 TEST_F(GluonGluon,NLO_conv_left_sector_name)
 {
 UI.alpha_s_power = 3;// NLO sectors
 UI.perturbative_order = 1; // NLO pdfs and a_s
 UI.sector_for_production = "6"; // the first  sector
 ASSERT_EQ(sector_name(),"F_gluon_from_gluon_11(*)F_gluon_from_gluon_00(*)(c0^2 a^2)(*)(1)(*)(1)(*)S(gluon,gluon,B,a^0,e^1 ,dim=2) : a^3,e^0");
 }


TEST_F(GluonGluon,NLO_conv_left_sector_value)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "6"; // the first  sector
    run_sector_expecting(-0.180795);
}


// slow test, total NLO gg->hX effective at mur=muf=mh
TEST_F(GluonGluon,DISABLED_NLO_xsection_muf_muf_mh)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    run_necessary_sectors_expecting(7.923);
    // NLO piece alone. The total NLO (including the LO contribution with NLO pdfs) is 13.845;
}



TEST_F(GluonGluon,NLO_log_sector_value_wild_mu)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.mur_over_mhiggs = 0.125;
    UI.muf_over_mhiggs = 0.56789;
    UI.sector_for_production = "0"; // the first  sector
    run_sector_expecting(-7.43281);
}




TEST_F(GluonGluon,NLO_2xC0xC1_sector_value_wild_mu)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.mur_over_mhiggs = 0.125;
    UI.muf_over_mhiggs = 0.56789;
    UI.sector_for_production = "1"; // the first  sector
    run_sector_expecting(3.52285);
}



TEST_F(GluonGluon,NLO_b0_sector_value_wild_mu)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.mur_over_mhiggs = 0.125;
    UI.muf_over_mhiggs = 0.56789;
    UI.sector_for_production = "2"; // the first  sector
    run_sector_expecting(-2.45532);
}



TEST_F(GluonGluon,NLO_soft_sector_value_wild_mu)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.mur_over_mhiggs = 0.125;
    UI.muf_over_mhiggs = 0.56789;
    UI.sector_for_production = "3"; // the first  sector
    run_sector_expecting(6.32166);
}

 //slow test
 TEST_F(GluonGluon,DISABLED_NLO_hard_sector_value_wild_mu)
 {
 UI.alpha_s_power = 3;// NLO sectors
 UI.perturbative_order = 1; // NLO pdfs and a_s
 UI.mur_over_mhiggs = 0.125;
 UI.muf_over_mhiggs = 0.56789;
 UI.sector_for_production = "4"; // the first  sector
 run_sector_expecting(13.0871);
 }


TEST_F(GluonGluon,NLO_conv_right_sector_value_wild_mu)
{
UI.alpha_s_power = 3;// NLO sectors
UI.perturbative_order = 1; // NLO pdfs and a_s
UI.mur_over_mhiggs = 0.125;
UI.muf_over_mhiggs = 0.56789;
UI.sector_for_production = "5"; // the first  sector
run_sector_expecting(-0.462926);
}

TEST_F(GluonGluon,NLO_conv_left_sector_value_wild_mu)
{
UI.alpha_s_power = 3;// NLO sectors
UI.perturbative_order = 1; // NLO pdfs and a_s
UI.mur_over_mhiggs = 0.125;
UI.muf_over_mhiggs = 0.56789;
UI.sector_for_production = "6"; // the first  sector
run_sector_expecting(-0.462891);
}


//slow test total NLO gg->hX effective at wild scales
TEST_F(GluonGluon,DISABLED_NLO_xs_mu_wild)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.mur_over_mhiggs = 0.125;
    UI.muf_over_mhiggs = 0.56789;
    run_necessary_sectors_expecting(24.2895-12.1688);// NLO piece alone. The total NLO (including the LO contribution with NLO pdfs) is 24.2895

}


class QuarkGluon: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        UI.matrix_element_approximation = "effective";
        UI.pole=0;
        UI.Fleft = "quark";
        UI.Fright = "gluon";
    }
    
    void run_sector_expecting(const double& expected_xs)
    {SP.run_sector_expecting(expected_xs,UI);}
    
    void run_necessary_sectors_expecting(const double& expected_xs)
    {SP.run_necessary_sectors_expecting(expected_xs,UI);}
    string sector_name(){return SP.sector_name(UI);}
    
    UserInterface UI;
    SectorPerformer SP;
};

TEST_F(QuarkGluon,NLO_xsection_mu_is_mh)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    run_necessary_sectors_expecting(-0.1966/2.0);
}


TEST_F(QuarkGluon,NLO_xsection_mu_wild)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.muf_over_mhiggs=0.56789;
    UI.mur_over_mhiggs=0.4356;
    run_necessary_sectors_expecting(0.42925/2.0);

}


class QuarkAntiQuark: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        UI.matrix_element_approximation = "effective";
        UI.pole=0;
        UI.Fleft = "quark";
        UI.Fright = "antiquark";
    }
    
    void run_sector_expecting(const double& expected_xs)
    {SP.run_sector_expecting(expected_xs,UI);}
    
    void run_necessary_sectors_expecting(const double& expected_xs)
    {SP.run_necessary_sectors_expecting(expected_xs,UI);}
    string sector_name(){return SP.sector_name(UI);}
    
    UserInterface UI;
    SectorPerformer SP;
};



TEST_F(QuarkAntiQuark,NLO_xsection_mu_wild)
{
    UI.alpha_s_power = 3;// NLO sectors
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.muf_over_mhiggs=0.56789;
    UI.mur_over_mhiggs=0.125;
    run_necessary_sectors_expecting(0.0248037);
}


int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}
































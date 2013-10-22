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
    
    void run_the_following_sectors_expecting(const vector<int> & sectors,
                                             const double& expected_xs,
                                              UserInterface& UI)
        {
        int number_of_sectors = sectors.size();
        double xs = 0.0;
        double err = 0.0;
        for (int i=0;i<number_of_sectors;i++)
            {
            stringstream s;s<<sectors[i];
            UI.sector_for_production = s.str();
            Process* cur_process = new Process(UI);
            cur_process->perform();
        
            xs += cur_process->total_xs();
            err += pow(cur_process->total_err(),2.0);
            }
        err = sqrt(err);
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

class GluonGluonExact: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        UI.matrix_element_approximation = "exact";
        UI.pole=0;
        UI.Fleft = "gluon";
        UI.Fright = "gluon";
    }
    
    void run_sector_expecting(const double& expected_xs)
    {SP.run_sector_expecting(expected_xs,UI);}
    
    void run_necessary_sectors_expecting(const double& expected_xs)
    {SP.run_necessary_sectors_expecting(expected_xs,UI);}
    string sector_name(){return SP.sector_name(UI);}
    
    void run_the_following_sectors_expecting(const vector<int> & sectors,
                                             const double& expected_xs)
    {SP.run_the_following_sectors_expecting(sectors,expected_xs,UI);}
    
    UserInterface UI;
    SectorPerformer SP;
};

TEST_F(GluonGluonExact,LO_xsection_muf_muf_mh)
{
    UI.alpha_s_power = 2;// LO sector
    UI.perturbative_order = 0; // LO pdfs and a_s
    UI.sector_for_production = "0"; // the first (and only) sector
    // The value below is for the SM
    // with m_top = 172.7 and mb(10GeV) = 3.63
    //run_sector_expecting(7.614);
    // with m_top = 163.7 at 163.7 in the  MSbar scheme
    //7.6551	+/- 0.0507
    run_sector_expecting(7.6551);
}



TEST_F(GluonGluonExact,Soft)
{
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "2"; // Soft
    // The value below is for the SM
    // with m_top = 172.7 and mb(10GeV) = 3.63
    //run_sector_expecting(3.3309);// +- 0.0016
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // 3.2353	+/- 0.0211
    run_sector_expecting(3.235);// +- 0.0211
}

TEST_F(GluonGluonExact,SoftWildMu)
{
    // the dependence on mu's here is implicit only (the log piece
    // is a different sector), i.e. through a_s and through the virtual part
    // if the scheme is MS_bar
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "2"; // Soft
    UI.muf_over_mhiggs=0.74536;
    UI.mur_over_mhiggs=0.4567;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // 4.67297 +- 0.0304455
    // note: to obtain this from ihixs you need to set, in the nlo_exact
    // delta piece,  the log piece to zero
    //  (in ehixs is a different sector)
    run_sector_expecting(4.67297);// +- 0.0304455
}

TEST_F(GluonGluonExact,RenormLogPiece)
{
    // the log(mur/muf) piece proportional to b0
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.sector_for_production = "0"; // Soft
    UI.muf_over_mhiggs=0.74536;
    UI.mur_over_mhiggs=0.4567;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // -1.1538 +- 0.007 
    // note: to obtain this from ihixs you need to set to zero the log piece
    //       which in ehixs is a different sector
    run_sector_expecting(-1.1538);// +- 0.007
}

TEST_F(GluonGluonExact,Poles_soft_and_collinear)
{
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.pole = -1; // Hard contribution
    UI.muf_over_mhiggs=0.74536;
    UI.mur_over_mhiggs=0.4567;
    
    run_necessary_sectors_expecting(0.0);
}

TEST_F(GluonGluonExact,Hard_mu_eq_mh)
{
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.pole = 0;
        // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // 5.83504 +- 0.0512352
    vector<int> sectors_to_run;
    sectors_to_run.push_back(0);// -b0/e * B * L
    sectors_to_run.push_back(1);// -b0/e * B_e
    sectors_to_run.push_back(3);// hard
    sectors_to_run.push_back(4);// conv 1
    sectors_to_run.push_back(5);// conv 2
    run_the_following_sectors_expecting(sectors_to_run,4.8098);// +- 0.0512352
}


TEST_F(GluonGluonExact,wild_mur)
{
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.pole = 0;
    UI.muf_over_mhiggs=1.0;
    UI.mur_over_mhiggs=0.4567;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // 6.8616 +- 0.00601156
    vector<int> sectors_to_run;
    sectors_to_run.push_back(1);// -b0/e * B_e
    sectors_to_run.push_back(3);// hard
    sectors_to_run.push_back(4);// conv 1
    sectors_to_run.push_back(5);// conv 2
    run_the_following_sectors_expecting(sectors_to_run,6.8616);// +- 0.00601156
}

TEST_F(GluonGluonExact,Hard_wild_muf_but_equal_to_mur)
{
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.pole = 0;
    UI.muf_over_mhiggs=0.74536;
    UI.mur_over_mhiggs=0.74536;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // 4.64599 +- 0.0288452 
    vector<int> sectors_to_run;
    sectors_to_run.push_back(1);// -b0/e * B_e
    sectors_to_run.push_back(3);// hard
    sectors_to_run.push_back(4);// conv 1
    sectors_to_run.push_back(5);// conv 2
    run_the_following_sectors_expecting(sectors_to_run,4.64599);// +- 0.0288452
}


TEST_F(GluonGluonExact,Total_wild_mu)
{
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.pole = 0;
    UI.muf_over_mhiggs=0.74536;
    UI.mur_over_mhiggs=0.4567;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    //9.3534	+/- 0.00886
    run_necessary_sectors_expecting(9.3534);// +- 0.009

}


class QuarkGluonExact: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        UI.matrix_element_approximation = "exact";
        UI.pole=0;
        UI.Fleft = "quark";
        UI.Fright = "gluon";
    }
    
    void run_sector_expecting(const double& expected_xs)
    {SP.run_sector_expecting(expected_xs,UI);}
    
    void run_necessary_sectors_expecting(const double& expected_xs)
    {SP.run_necessary_sectors_expecting(expected_xs,UI);}
    string sector_name(){return SP.sector_name(UI);}
    
    void run_the_following_sectors_expecting(const vector<int> & sectors,
                                             const double& expected_xs)
    {SP.run_the_following_sectors_expecting(sectors,expected_xs,UI);}
    
    UserInterface UI;
    SectorPerformer SP;
};

TEST_F(QuarkGluonExact,Pole)
{
    // the log(mur/muf) piece proportional to b0
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.pole = -1;
    vector<int> sectors_to_run;
    sectors_to_run.push_back(0);//  convolution 1/e
    sectors_to_run.push_back(1);//  qg hard 1/e

    UI.muf_over_mhiggs=0.74536;
    UI.mur_over_mhiggs=0.4567;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // -1.1538 +- 0.007
    // note: to obtain this from ihixs you need to set to zero the log piece
    //       which in ehixs is a different sector
    run_the_following_sectors_expecting(sectors_to_run,0.0);
}



TEST_F(QuarkGluonExact,finite)
{
    // the log(mur/muf) piece proportional to b0
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    vector<int> sectors_to_run;

    sectors_to_run.push_back(0);//  qg hard 1/e
    sectors_to_run.push_back(1);//  convolution 1/e
    
    UI.muf_over_mhiggs=0.74536;
    UI.mur_over_mhiggs=0.4567;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // for both qg and gq
    //0.197872 +- 0.000913985
    run_the_following_sectors_expecting(sectors_to_run,0.197872/2.0);
}


class GluonQuarkExact: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        UI.matrix_element_approximation = "exact";
        UI.pole=0;
        UI.Fleft = "gluon";
        UI.Fright = "quark";
    }
    
    void run_sector_expecting(const double& expected_xs)
    {SP.run_sector_expecting(expected_xs,UI);}
    
    void run_necessary_sectors_expecting(const double& expected_xs)
    {SP.run_necessary_sectors_expecting(expected_xs,UI);}
    string sector_name(){return SP.sector_name(UI);}
    
    void run_the_following_sectors_expecting(const vector<int> & sectors,
                                             const double& expected_xs)
    {SP.run_the_following_sectors_expecting(sectors,expected_xs,UI);}
    
    UserInterface UI;
    SectorPerformer SP;
};

TEST_F(GluonQuarkExact,Pole)
{
    // the log(mur/muf) piece proportional to b0
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    UI.pole = -1;
    vector<int> sectors_to_run;
    sectors_to_run.push_back(0);//  convolution 1/e
    sectors_to_run.push_back(1);//  qg hard 1/e
    
    UI.muf_over_mhiggs=0.74536;
    UI.mur_over_mhiggs=0.4567;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // -1.1538 +- 0.007
    // note: to obtain this from ihixs you need to set to zero the log piece
    //       which in ehixs is a different sector
    run_the_following_sectors_expecting(sectors_to_run,0.0);
}



TEST_F(GluonQuarkExact,finite)
{
    // the log(mur/muf) piece proportional to b0
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    vector<int> sectors_to_run;
    
    sectors_to_run.push_back(0);//  qg hard 1/e
    sectors_to_run.push_back(1);//  convolution 1/e
    
    UI.muf_over_mhiggs=0.74536;
    UI.mur_over_mhiggs=0.4567;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    // for both qg and gq
    //0.197872 +- 0.000913985
    run_the_following_sectors_expecting(sectors_to_run,0.197872/2.0);
}




class QuarkAntiquarkExact: public ::testing::Test {
protected:
    virtual void SetUp() {
        UI.m_higgs = 125.0;
        UI.perturbative_order = 2;
        UI.pdf_provider = "MSTW";
        UI.pdf_error = false;
        UI.Etot = 8000.0;
        UI.muf_over_mhiggs=1.0;
        UI.mur_over_mhiggs=1.0;
        UI.matrix_element_approximation = "exact";
        UI.pole=0;
        UI.Fleft = "quark";
        UI.Fright = "antiquark";
    }
    
    void run_sector_expecting(const double& expected_xs)
    {SP.run_sector_expecting(expected_xs,UI);}
    
    void run_necessary_sectors_expecting(const double& expected_xs)
    {SP.run_necessary_sectors_expecting(expected_xs,UI);}
    string sector_name(){return SP.sector_name(UI);}
    
    void run_the_following_sectors_expecting(const vector<int> & sectors,
                                             const double& expected_xs)
    {SP.run_the_following_sectors_expecting(sectors,expected_xs,UI);}
    
    UserInterface UI;
    SectorPerformer SP;
};

TEST_F(QuarkAntiquarkExact,finite_wild_mu)
{
    // the log(mur/muf) piece proportional to b0
    UI.alpha_s_power = 3;// NLO sector
    UI.perturbative_order = 1; // NLO pdfs and a_s
    vector<int> sectors_to_run;
    
    sectors_to_run.push_back(0);//  qg hard 1/e
    sectors_to_run.push_back(1);//  convolution 1/e
    
    UI.muf_over_mhiggs=0.83457;
    UI.mur_over_mhiggs=0.74536;
    // The value below is for the SM
    // with m_top = 163.7 @ 163.7 in the MSbar scheme
    //0.024617	+/- 2.23e-05
    run_necessary_sectors_expecting(0.02456);//  +- 0.00002
}


int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}
































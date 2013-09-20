/** testing ehixs:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>


using namespace std;

#include "gtest/gtest.h"
#include "model.h"

TEST(generalSetUp,harlander_top_bottom_13TeV)
{
    CModel *Model = new CModel;
    //Model->quarks[0]->set_m_at_ref_scale(20.0);
    //Model->quarks[1]->Y = 0.0;
    Model->quarks[1]->set_reference_scale(4.3);
    Model->quarks[1]->set_m_at_ref_scale(4.3);
        
    
    double xs = default_LO.cross_section();
    double err = default_LO.mc_error();
    double expected_xs =  0.221489564E+02;
    cout<<"\n xs = "<<xs<<" \t expected = "<<expected_xs<<endl;
    EXPECT_LT(fabs(xs-expected_xs)/fabs(expected_xs),err);
}



int main(int argc, char**argv)
{
    cout << "\ntesting ehixs\n" << endl;
    
    ::testing::InitGoogleTest(&argc, argv);
    return  RUN_ALL_TESTS();
    
    return 0;
}




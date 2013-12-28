

/** testing pdfs, and in particular the new interpolator class CashedInterpolator.h as compared to the old interpolator.h by Ste:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>

using namespace std;

#include "pdf.h"

#include "gtest/gtest.h"







class TestingNloReal: public CoolInt
{
public:
    TestingNloReal():CoolInt(){};
    double evaluateIntegral(const double xx[]);
private:
};

double TestingNloReal::evaluateIntegral(const double xx[])
{
    double z=0.732;
    double lambda = xx[0];
    complex<double> born = born_exact_summed_over_quarks(_model);
    
    return (
            pow(z,4.0)/2.0*sum_of_abs_sq_of_Aqi(z,lambda,_model)
            - pow(1.0-z+z*z,2.0) * pow(abs(born),2.0)
            )
    /lambda/(1.0-lambda)  + 11.0/6.0 * pow(1.0-z,4.0);
}


TEST(ggf_nlo_exact_real, DISABLED_large_mt_limit)
{
    CModel* Model = new CModel();
    Model->quarks[0]->set_pole_mass(2000.0);
    Model->quarks[1]->set_Y(0.0);
    
    Model->consolidate(0.117, 1.0, 1,125.0);
    
    
    TestingNloReal my_dude;
    my_dude.setModel(Model);
    my_dude.call_vegas();
    cout<<"\nres="<<my_dude.result()<<endl;
    EXPECT_LT(abs(my_dude.result()),1e-5);
}







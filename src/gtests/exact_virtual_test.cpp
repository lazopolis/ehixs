/** testing UserInterface.*:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>


#include "gluon_fusion.h"

using namespace std;

#include "gtest/gtest.h"
#include "nlo_exact_matrix_elements.h"
#include "vegas_adaptor.h"

TEST(ggf_nlo_exact_virtual,mt_infinity)
{
    complex<double> res,expected,x,y,mq;
    //: if you don't want to see the limiting behavior
    //: comment out from here
    cout<<"\n======================================================"
    <<"\n\t The limit mt->infinity should be 11/2"
    <<"\n\t note that 11/2 is approached well until ~3TeV."
    <<" and then starts deviating, due to cancelations between HPLs"
    <<endl;
    double mh=125.0;
    double mqs[20]={100.0,170.0,300.0,500.0,1000.0,
                    2000.0,2500.0,2800.0,3000.0,3100.0,
                    3200.0,3300.0,3500.0,4000.0,5000.0,
                    10000.0,20000.0,50000.0,75000.0,100000.0};
    for (int i=0;i<20;i++)
        {
        mq= complex<double>(mqs[i],1e-15);

        y =  pow(mq,2.0) / pow(mh,2.0);
        x= ( sqrt(1.0-4.0*y) - 1.0 ) / ( sqrt(1.0-4.0*y) + 1.0 );
        res= ggf_exact_virtual_ep0(x);
        expected=complex<double>(11.0/2.0,0.0);
    
        cout<<setprecision(16)<<endl
            <<"\tx="<<x<<"\t|x|="<<abs(x)
            <<setprecision(6)<<"\tres="<<real(res)<<"\t|RE(res-expected)|="
            <<setprecision(1)<<scientific<<abs(real(res-expected))
            <<fixed<<"\tmq="<<mq;
        
         }
    cout<<"\n======================================================";
    cout<<endl;
    //: until here
    mq=complex<double>(3200.0,0.0);
    y =  pow(mq,2.0) / pow(mh,2.0);
    x= ( sqrt(1.0-4.0*y) - 1.0 ) / ( sqrt(1.0-4.0*y) + 1.0 );
    res= ggf_exact_virtual_ep0(x);
    expected=complex<double>(11.0/2.0,0.0);
    EXPECT_LT(abs(real(res-expected)),1e-3);
}

TEST(ggf_nlo_exact_virtual,mt_zero)
{
    complex<double> res,expected,x,y,mq;
    //: if you don't want to see the limiting behavior
    //: comment out from here
    cout<<"\n======================================================"
    <<"\n\t The limit mt->0 should be zero"
    <<endl;
    double mh=125.0;
    double mqs[10]={100.0,10.0,5.0,1.0,1e-1,1e-2,1e-3,1e-4,1e-5,1e-6};
    for (int i=0;i<10;i++)
        {
        mq= complex<double>(mqs[i],1e-15);
        
        y =  pow(mq,2.0) / pow(mh,2.0);
        x= ( sqrt(1.0-4.0*y) - 1.0 ) / ( sqrt(1.0-4.0*y) + 1.0 );
        res= ggf_exact_virtual_ep0(x);
        expected=complex<double>(0.0,0.0);
        
        cout<<setprecision(16)<<endl
        <<"\tx="<<x<<"\t|x|="<<abs(x)
        <<setprecision(6)<<scientific<<"\tres="<<real(res)<<"\t|RE(res-expected)|="
        <<setprecision(1)<<scientific<<abs(real(res-expected))
        <<"\tmq="<<mq;
        
        }
    cout<<"\n======================================================";
    cout<<endl;
    //: until here
    mq=complex<double>(1e-4,0.0);
    y =  pow(mq,2.0) / pow(mh,2.0);
    x= ( sqrt(1.0-4.0*y) - 1.0 ) / ( sqrt(1.0-4.0*y) + 1.0 );
    res= ggf_exact_virtual_ep0(x);
    expected=complex<double>(0.0,0.0);
    EXPECT_LT(abs(real(res-expected)),1e-6);
}

TEST(ggf_nlo_exact_virtual,threshold)
{
    complex<double> res,expected,x,y,mq;
    // threshold limits
    double pisq=consts::pi_square;
    double z3=consts::z3;
    
    double limF2la = -9.0 + 7.0/4.0*pisq - 2.0*pisq*log(2.0) + 7.0*z3;
    double limF2lb = 3.0 - 3.0/4.0*pisq;
    double limG2la = -3.0 + 1.0/2.0*pisq*log(2) - 7.0/4.0*z3;
    expected = complex<double>(-2.0*limF2la -2.0*limF2lb*4.0/3.0
                                - 9.0/2.0 * limG2la,0.0);
    
    
    //: if you don't want to see the limiting behavior
    //: comment out from here
    cout<<"\n======================================================"
    <<"\n\t The limit mt->mh/2 should be "
    <<endl;
    double mh=125.0;
    double mqs[20]={-30.0,-15.0,-1.0,-0.1,-0.01,-0.001,-1e-4,-1e-5,-1e-6,-1e-15,
                    1e-15,1e-6,1e-5,1e-4,1e-3,1e-2,0.1,1.0,15.0,30.0};
    for (int i=0;i<20;i++)
        {
        mq= complex<double>(mh/2.0+mqs[i],0.0 );
        
        y =  pow(mq,2.0) / pow(mh,2.0);
        x= ( sqrt(1.0-4.0*y) - 1.0 ) / ( sqrt(1.0-4.0*y) + 1.0 );
        res= ggf_exact_virtual_ep0(x);
        
        cout<<setprecision(16)<<endl
        <<"\tx="<<x<<"\t|x|="<<abs(x)
        <<setprecision(6)<<scientific<<"\tres="
        <<real(res)<<"\t|RE(res-expected)|="
        <<setprecision(1)<<scientific<<abs(real(res-expected))
        <<fixed<<setprecision(16)<<"\tmq="<<mq;
        
        }
    cout<<"\n======================================================";
    cout<<"\n---- data for Harlander's plot, fig. 5a in hep-ph/0509189"
    <<"\n\t tau=mh^2/(4*mt^2)"
    <<endl;

    for (int i=0;i<100;i++)
        {

        complex<double> tau =complex<double>( 2.0*double(i)/double(100)+1e-10,0.0);
        y = 1.0/4.0/tau;
        x= ( sqrt(1.0-4.0*y) - 1.0 ) / ( sqrt(1.0-4.0*y) + 1.0 );
        res= ggf_exact_virtual_ep0(x);
        
        cout<<setprecision(16)<<endl
        <<real(1.0/y/4.0)<<" "<<real(res);
        
        }
    cout<<"\n======================================================";
    cout<<endl;
    //: until here
    mq=complex<double>(mh/2.0,0.0);
    y =  pow(mq,2.0) / pow(mh,2.0);
    x= ( sqrt(1.0-4.0*y) - 1.0 ) / ( sqrt(1.0-4.0*y) + 1.0 );
    res= ggf_exact_virtual_ep0(x);
    cout<<"\n|RE(res-expected)|="<<abs(real(res-expected))<<endl;
    EXPECT_LT(abs(real(res-expected)),1e-6);
}


TEST(ggf_nlo_exact_virtual,threshold_F2lb)
{
    complex<double> res,expected,x,y,mq;
    // threshold limits
    double pisq=consts::pi_square;
    double z3=consts::z3;
    
    double limF2la = -9.0 + 7.0/4.0*pisq - 2.0*pisq*log(2.0) + 7.0*z3;
    double limF2lb = 3.0 - 3.0/4.0*pisq;
    double limG2la = -3.0 + 1.0/2.0*pisq*log(2) - 7.0/4.0*z3;

    double mh=125.0;

    mq=complex<double>(mh/2.0,0.0);
    y =  pow(mq,2.0) / pow(mh,2.0);
    x= ( sqrt(1.0-4.0*y) - 1.0 ) / ( sqrt(1.0-4.0*y) + 1.0 );
    res= F2lb(x);
    EXPECT_LT(abs(real(res-limF2lb)),1e-5);
}

TEST(ggf_nlo_exact_real, limit_z_to_1)
{
    double lambda = 0.35;
    CModel* Model = new CModel();
    Model->quarks[1]->Y = 0.0;
    vector<double> as_at_mz;as_at_mz.push_back(0.117);
    Model->evolve(as_at_mz, 62.5, 1);
    Model->set_Xq_for_quarks();
    
    cout<<"\n---------\n"<<"mh = "<<Model->higgs.m()<<endl;
    cout<<"Contributing quarks: "<<endl;
    for (int i=0;i<Model->quarks.size();i++)
        {
        if (Model->quarks[i]->Y != 0.0)
            {
            ParticleObject* quark = Model->quarks[i];
            cout<<setprecision(8)
                <<quark->name<<" "
                <<quark->Y<<" "
                <<sqrt(quark->complex_mass_squared_at_mur())
                <<endl;
            }
        }
    cout<<"-----";
    double res,expected_limit;
    
    complex<double> born = born_exact_summed_over_quarks(Model);
    cout<<"\nBorn = "<<born<<endl;
    for (int i=1;i<10;i++)
        {
        double z = 1.0 - 0.1*pow(0.1,double(i));
        
        res = sum_of_abs_sq_of_Aqi(z,lambda,Model);
        expected_limit = 2.0/pow(z,4.0)*
                                pow(1.0-z+z*z,2.0)*pow(abs(born),2.0);
        cout<<"\n evaluated = "<< res << " expected = "<<expected_limit<<endl;
        }
    EXPECT_LT(abs(res-expected_limit),1e-5);
}



TEST(ggf_nlo_exact_real, limit_lambda_to_1)
{
    double z = 0.75;
    CModel* Model = new CModel();
    Model->quarks[1]->Y = 0.0;
    vector<double> as_at_mz;as_at_mz.push_back(0.117);
    Model->evolve(as_at_mz, 62.5, 1);
    Model->set_Xq_for_quarks();
    
    cout<<"\n---------\n"<<"mh = "<<Model->higgs.m()<<endl;
    cout<<"Contributing quarks: "<<endl;
    for (int i=0;i<Model->quarks.size();i++)
        {
        if (Model->quarks[i]->Y != 0.0)
            {
            ParticleObject* quark = Model->quarks[i];
            cout<<setprecision(8)
            <<quark->name<<" "
            <<quark->Y<<" "
            <<sqrt(quark->complex_mass_squared_at_mur())
            <<endl;
            }
        }
    cout<<"-----";
    double res,expected_limit;
    
    complex<double> born = born_exact_summed_over_quarks(Model);
    cout<<"\nBorn = "<<born<<endl;
    for (int i=1;i<8;i++)
        {
        double lambda = 1.0 - 0.1*pow(0.1,double(i));
        
        res = sum_of_abs_sq_of_Aqi(z,lambda,Model);
        expected_limit = 2.0/pow(z,4.0)*
        pow(1.0-z+z*z,2.0)*pow(abs(born),2.0);
        cout<<"\nlambda="<<lambda
            <<": evaluated = "<< res
            << " expected = "<<expected_limit<<endl;
        }
    EXPECT_LT(abs(res-expected_limit),1e-5);
}

TEST(ggf_nlo_exact_real, limit_lambda_to_0)
{
    double z = 0.655;
    CModel* Model = new CModel();
    Model->quarks[1]->Y = 0.0;
    vector<double> as_at_mz;as_at_mz.push_back(0.117);
    Model->evolve(as_at_mz, 62.5, 1);
    Model->set_Xq_for_quarks();
    
    cout<<"\n---------\n"<<"mh = "<<Model->higgs.m()<<endl;
    cout<<"Contributing quarks: "<<endl;
    for (int i=0;i<Model->quarks.size();i++)
        {
        if (Model->quarks[i]->Y != 0.0)
            {
            ParticleObject* quark = Model->quarks[i];
            cout<<setprecision(8)
            <<quark->name<<" "
            <<quark->Y<<" "
            <<sqrt(quark->complex_mass_squared_at_mur())
            <<endl;
            }
        }
    cout<<"-----";
    double res,expected_limit;
    
    complex<double> born = born_exact_summed_over_quarks(Model);
    cout<<"\nBorn = "<<born<<endl;
    for (int i=1;i<8;i++)
        {
        double lambda =  0.1*pow(0.1,double(i));
        
        res = sum_of_abs_sq_of_Aqi(z,lambda,Model);
        expected_limit = 2.0/pow(z,4.0)*
        pow(1.0-z+z*z,2.0)*pow(abs(born),2.0);
        cout<<"\nlambda="<<lambda
        <<": evaluated = "<< res
        << " expected = "<<expected_limit<<endl;
        }
    EXPECT_LT(abs(res-expected_limit),1e-5);
}


class TestingNloReal: public CoolInt
{
public:
    TestingNloReal():CoolInt(){};
    double evaluateIntegral(const double xx[]);
    void setModel(CModel* Model){_model = Model;}
private:
    CModel* _model;
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


TEST(ggf_nlo_exact_real, large_mt_limit)
{
    CModel* Model = new CModel();
    Model->quarks[0]->set_m_at_ref_scale(20.0);
    Model->quarks[1]->Y = 0.0;
    vector<double> as_at_mz;as_at_mz.push_back(0.117);
    Model->evolve(as_at_mz, 62.5, 1);
    Model->set_Xq_for_quarks();
    
    TestingNloReal my_dude;
    my_dude.setModel(Model);
    my_dude.call_vegas();
    cout<<"\nres="<<my_dude.result()<<endl;
    EXPECT_LT(abs(my_dude.result()),1e-5);
}




int main(int argc, char**argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return  RUN_ALL_TESTS();
    return 0;
}

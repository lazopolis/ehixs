#include<complex>
#include "constants.h"
#include "chaplin.h"
#include "interface_to_amplitudes.h"
#include "model.h"
#include "nlo_exact_matrix_elements.h"
#include <iostream>
using namespace std;




complex<double> F2lb(const complex<double> & x)
{
    return 6.0*x*(pow(x,2.0)+6.0*x+1.0)*HPL2(0,0,x)
    /pow(-1.0+x,4.0)
    -6.0*x*(1.0+x)*log(x)
    /pow(-1.0+x,3.0)
    -12.0*x
    /pow(-1.0+x,2.0);
}


complex<double> ggf_exact_virtual_ep0(const complex<double> & x)
{
    //	Below is an implementation of the two_loop virtual
    //  amplitude in the presence of heavy quarks (the non-pi^2 piece)
    //	The implementation (by Stephan) follows eq.26-30
    //	in http://arxiv.org/abs/hep-ph/0611266
    //  see eq.28 of  http://arxiv.org/abs/hep-ph/0611266
    //
    //  It is also equal to B1*F0 of Harlander & Kant, see
    //      http://arxiv.org/abs/hep-ph/0509189
    //
    //
    //  note:   the function actually returns a real
    //          (the conjugate is added at the end)
    //  Limits:
    //	1)Its limit as m->infty is 11/2*born
    //
    //  this limit is tested by running the exact_virtual_test
    //  note:   the implementation here, from Bonciani et al (0611266)
    //          appears to have numerical instabilities as mt->infinity.
    //          In particular, the function below approaches 11/2 if mt<3TeV
    //          smoothly (it is then equal to 11/2 up to 0.0003)
    //          but the it deviates from 11/2 presumably due to cancelations
    //          between HPLs as x->1 from above on the complex circle
    //
    //  note:   the limit does not depend on the imaginary part of mq
    //          hence 11/2 is NOT a test of the correctness
    //          of the imaginary part!
    //
    //  2)mt->0 has the limit zero
    //
    //  3)mt->mh/2 is the threshold limit
    //      it is equal to 47/2-(3/2)*Pi^2+(7/4)*Pi^2*ln(2)-(49/8)*Zeta(3)
    //                      = 13.30489968
    //  all these limits are implemented as tests at exact_virtual_test.cpp
    
    complex<double> H2 = 4.0/5.0*pow(consts::z2,2.0)
                        + 2.0*consts::z3
                        + 3.0*consts::z3/2.0*log(x)
                        - 3.0*consts::z3*log(1.0-x)
                        + consts::z2*HPL2(1,0,x)
                        + 1.0/4.0*(1.0+2*consts::z2)*HPL2(0,0,x)
                        - 2.0*HPL3(1,0,0,x)
                        + HPL4(0,0,-1,0,x)
                        + 1.0/4.0*HPL4(0,0,0,0,x)
                        + 2.0*HPL4(1,0,-1,0,x)
                        - HPL4(1,0,0,0,x);
    complex<double> G2lCA_onehalf =
                4.0* x/pow(x-1.0,2.0) *
                (
                 3.0
                 + x*(1.0+8.0*x+3.0*x*x) /pow(x-1.0,3.0)*HPL3(0,0,0,x)
                 -2.0*pow(1.0+x,2.0)/pow(1.0-x,2.0)* H2
                 +consts::z3
                 -HPL3(1,0,0,x)
                 )
                ;
    // see eq.11 of http://arxiv.org/abs/hep-ph/0611266
    complex<double> H1 = 9.0*pow(consts::z2,2.0)/10.0
    + 2.0*consts::z3*log(x)
    + consts::z2*HPL2(0,0,x)
    + 1.0/4.0*HPL4(0,0,0,0,x)
    + 7.0/2.0*HPL4(0,1,0,0,x)
    - 2.0*HPL4(0,-1,0,0,x)
    + 4.0*HPL4(0,0,-1,0,x)
    - HPL4(0,0,1,0,x);
    
    complex<double> F2la = 36.0*x/pow(x-1.0,2.0)
    - 4.0*x*(1.0-14.0*x+x*x)/pow(x-1.0,4.0)*consts::z3
    - 4.0*x*(1.0+x)*log(x)/pow(x-1.0,3.0)
    - 8.0*x*(1.0+9.0*x+x*x)/pow(x-1.0,4.0)*HPL2(0,0,x)
    +2.0*x*(3.0+25.0*x-7.0*x*x+3.0*pow(x,3.0))/pow(x-1.0,5.0)*HPL3(0,0,0,x)
    + 4.0*x*(1.0+2.0*x+x*x)/pow(x-1.0,4.0)*
        (consts::z2*log(x) + 4.0*HPL3(0,-1,0,x) - HPL3(0,1,0,x))
    + 4.0*x*(5.0-6.0*x+5.0*pow(x,2.0))/pow(x-1.0,4.0)*HPL3(1,0,0,x)
    - 8.0*x*(1.0+x+pow(x,2.0)+pow(x,3.0))/pow(x-1.0,5.0)*H1;
    
    
   
    
    //: putting the above together: we keep here N arbitrary for readability reasons only
    double N=3.0;
    double Cf=(N*N-1.0)/2.0/N;
    complex<double> res =1.0/2.0
                        * (Cf * F2la  +  N * G2lCA_onehalf
                           + Cf*4.0/3.0 * F2lb(x)
                           )
                        / (-2.0/3.0);
    
    return res+conj(res);
    
}





complex<double> born_exact_summed_over_quarks(CModel* Model)
{
    
    complex<double> ME ;
    
    for (int i=0;i<Model->quarks.size();i++)
        {
        ME = ME + Model->quarks[i]->Y * born(Model->quarks[i]->X);
        }
    return(ME);
}

complex<double> born(complex<double> x)
{
    
    //: the expression below goes to 1 as mq->infty, i.e. as x->1
    complex<double > res=(-3.0)*x/pow(1.0-x,2.0)*
                (2.0-pow(1.0+x,2.0)/pow(1.0-x,2.0)*HPL2(0,0,x));    
    return res;
}




double sum_of_abs_sq_of_Aqi(const double &z,const double & lambda,CModel* Model)
{
    complex<double> ME ;
    double mh = Model->higgs.m();
    complex<double> A1(0.0,0.0);
    complex<double> A2(0.0,0.0);
    complex<double> A3(0.0,0.0);
    complex<double> A4(0.0,0.0);
    for (int i=0;i<Model->quarks.size();i++)
        {
        double Yq = Model->quarks[i]->Y;
        if (Yq!=0.0)
            {
            complex<double> Wq = Model->quarks[i]->Wq;
            complex<double> mq_cplx = sqrt(
                            Model->quarks[i]->complex_mass_squared_at_mur());
            complex<double> factor = Yq * Wq * 3.0/32.0 /pow(mh,2.0);
            A1 += Aq1(z,lambda,mq_cplx,mh) * factor;
            A2 += Aq2a(z,lambda,mq_cplx,mh) * factor;
            A3 += Aq2b(z,lambda,mq_cplx,mh) * factor;
            A4 += Aq2c(z,lambda,mq_cplx,mh) * factor;
            //cout<<"\n%%% "<<Model->quarks[i]->name<<" "<<z<<" "<<lambda;
            }
        }
        return(    pow(abs(A1),2.0)
                + pow(abs(A2),2.0)
                + pow(abs(A3),2.0)
                + pow(abs(A4),2.0));
}

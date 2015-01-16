#include "gluon_fusion_exact_coefficients.h"
#include "chaplin.h"

//: external function definitions
#include "nlo_exact_matrix_elements.h"


GluonFusionExactCoefficients::GluonFusionExactCoefficients
(const CModel & themodel)
{
    Model = themodel;
    LO_exact_coefficient.push_back(LO_exact_e0());
    LO_exact_coefficient.push_back(LO_exact_e1());
    LO_exact_coefficient.push_back(LO_exact_e2());
    
    NLO_soft_exact_coefficient.push_back(NLO_soft_exact_e0());
    
}

double GluonFusionExactCoefficients::LO_exact_e0()
{
    
    complex<double> ME ;
    
    for (int i=0;i<Model.quarks.size();i++)
    {
        
        ME = ME + Model.quarks[i]->Y() * born(Model.quarks[i]->X());
        cout<<"\n"<<Model.quarks[i]->name()
        <<"\t"<<Model.quarks[i]->Y() * born(Model.quarks[i]->X())
        <<"\t"<<Model.quarks[i]->X()<<"\t"<<Model.quarks[i]->Wq()
        <<"\t"<<Model.quarks[i]->m()<<"\t"<<Model.quarks[i]->cm_sq();
    }
    
    cout<<"\n LO exact : "<<pow(abs(ME),2.0);
    cout<<"\n Born exact:"<<
    pow(abs(h_exact::born_exact_summed_over_quarks(&Model)),2.0);
    
    return(pow(abs(ME),2.0));
}

double GluonFusionExactCoefficients::LO_exact_e1()
{
    
    complex<double> ME ;
    
    for (int i=0;i<Model.quarks.size();i++)
    {
        ME = ME + Model.quarks[i]->Y() * born_e(Model.quarks[i]->X());
    }
    return(pow(abs(ME),2.0));
}

double GluonFusionExactCoefficients::LO_exact_e2()
{
    
    complex<double> ME ;
    
    for (int i=0;i<Model.quarks.size();i++)
    {
        ME = ME + Model.quarks[i]->Y() * born_e(Model.quarks[i]->X());
    }
    return(pow(abs(ME),2.0));
}

complex<double> GluonFusionExactCoefficients::born(complex<double> x)
{
    
    //: the expression below goes to 1 as mq->infty, i.e. as x->1
    complex<double > res=(-3.0)*x/pow(1.0-x,2.0)*(2.0-pow(1.0+x,2.0)/pow(1.0-x,2.0)*HPL2(0,0,x));
    //res = -16.0*pow(1.0+x,2.0)/x*HPL2(0,0,x)+32.0*pow(1.0-x,2.0)/x;
    
    return res;
}

complex<double> GluonFusionExactCoefficients::born_e(complex<double> x)
{
    //: copied (and translated) from ihixs, which copied from hggtotal
    complex<double > res=  -3.0*x/pow(1.0-x,4.0) *
    (
     +2.0 * (1.0 - x*x) * HPL1(0,x)
     +2.0 * (1.0 + x*x) * HPL2(0,0,x)
     + pow(1.0+x,2.0) *
     (
      1.0/6.0 * consts::pi_square  *HPL1(0,x)
      + 2.0 * HPL3(0,-1,0,x)
      - HPL3(0,0,0,x)
      + 3.0 * consts::z3
      )
     +2.0 * pow(1.0-x,2.0)
     );
    
    
    
    return res;
}

complex<double> GluonFusionExactCoefficients::born_e2(complex<double> x)
{
    //: copied (and translated) from ihixs, which copied from hggtotal
    
    complex<double > res = -16.0/3.0*pow(1.0+x,2)/x*consts::pi_square *HPL2(0,-1,x)
    +(-16.0/3.0*(1.0+x*x)/x*consts::pi_square+32.0*pow(1.0+x,2)/x*consts::z3-32.0*(1.0+x)*(-1.0+x)/x)*HPL1(0,x)
    +32.0*pow(1.0+x,2)/x*HPL4(0,0,-1,0,x)
    +64.0*(-1.0+x)*(1.0+x)/x*HPL2(-1,0,x)
    +(8.0/3.0*pow(1.0+x,2)/x*consts::pi_square-16.0*(3.0*x+1.0)*(-1.0+x)/x)*HPL2(0,0,x)
    -64.0*pow(1.0+x,2)/x*HPL4(0,-1,-1,0,x)
    +32.0*pow(1.0+x,2)/x*HPL4(0,-1,0,0,x)
    -64.0*(1.0+x*x)/x*HPL3(0,-1,0,x)
    +32.0*(1.0+x*x)/x*HPL3(0,0,0,x)
    -16.0*pow(1.0+x,2)/x*HPL4(0,0,0,0,x)
    +2.0/9.0*pow(1.0+x,2)/x*pow(consts::pi_square,2.0)
    +16.0/3.0*(-1.0+x)*(1.0+x)/x*consts::pi_square
    -96.0*(1.0+x*x)/x*consts::z3
    +64.0*pow(-1.0+x,2.0)/x;
    return res;
}


double GluonFusionExactCoefficients::NLO_soft_exact_e0()
{
    
    complex<double> V(0.0,0.0) ;
    complex<double> Born(0.0,0.0);
    for (int i=0;i<Model.quarks.size();i++)
    {
        // every quark has a scheme: on-shell or msbar
        // the ggf_exact_virtual_ep0 below is equal to
        // A + B * scheme_dependent_coeff
        // where scheme_dependent_coeff = 4/3 for on-shell scheme
        // and   scheme_dependent_coeff = log(mq^2/mur^2) for MS_bar
        double scheme_dependent_coeff;
        if (Model.quarks[i]->scheme()=="on-shell")
            scheme_dependent_coeff = 4.0/3.0;
        else if (Model.quarks[i]->scheme()=="msbar")
        {
            scheme_dependent_coeff = 2.0 * log(Model.quarks[i]->m()
                                               / Model.mu_r());
        }
        V = V + Model.quarks[i]->Y() * h_exact::ggf_exact_virtual_ep0(Model.quarks[i]->X(),scheme_dependent_coeff);
        Born = Born+Model.quarks[i]->Y() * born(Model.quarks[i]->X());
        //cout<<"\t\t ME="<<ME;
        
    }
    //cout<<"\n ME = "<<ME;
    double res = 2.0 * real(Born * conj(V))
    + pow(abs(Born),2.0) * consts::pi_square;
    cout<<"\n NLO soft= "<<res<<"\t"<<conj(V);
    cout<<"\n NLO soft : 2.0*real(B*Vstar) = "<<2.0*real(Born * conj(V))
    <<" |B|^2*pi^2 = "<<pow(abs(Born),2.0) * consts::pi_square;
    return(res);
}




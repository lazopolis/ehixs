#include "luminosity_integrals.h"


double LuminosityIntegralDelta::evaluateIntegral(const double* xx)
{
    const double x1= xx[0];
    const double measure = 1./x1;
    // the check :  x2 = tau/x1 is in [0,1]
    // is done in lumi and 0.0 is returned if it fails
    //cout<<"x1="<<x1<<" x2= "<<tau_/x1<<" tau = "<<tau_<<" L="<<lumi_->give(x1,tau_/x1)<<endl;
    return measure*lumi_->give(x1,tau_/x1,_muF);
}

double LuminosityIntegralPlus::evaluateIntegral(const double* xx)
{
    const double x1= xx[0];
    const double z = xx[1];
    const double measure = 1./x1;
    // the check :  x2 = tau/x1 is in [0,1]
    // is done in lumi and 0.0 is returned if it fails
    const double res=  measure
    *( lumi_->give(x1,tau_/x1/z,_muF) - lumi_->give(x1,tau_/x1,_muF) )
    / (1.-z)
    * pow(log(1.-z), log_power());
    //cout<<"\nres="<<res<<"\t"<<x1<<" "<<z<<endl;
    return res;
}

double LuminosityIntegralReal::evaluateIntegral(const double* xx)
{
    const double x1= xx[0];
    const double z = xx[1];
    const double measure = 1./x1;
    // the check :  x2 = tau/x1 is in [0,1]
    // is done in lumi and 0.0 is returned if it fails
    
    
    const double res=  measure
    *lumi_->give(x1,tau_/x1/z,_muF)
    * (*_f)(z,_L);
    //cout<<"\nres="<<res<<"\t"<<x1<<" "<<z<<endl;
    return res;
}


double LuminosityIntegralRealExact::evaluateIntegral(const double* xx)
{
    const double x1= xx[0];
    const double z = xx[1];
    const double lambda = xx[2];
    const double measure = 1./x1;
    // the check :  x2 = tau/x1 is in [0,1]
    // is done in lumi and 0.0 is returned if it fails
    

    
    const double res=  measure
    *lumi_->give(x1,tau_/x1/z,_muF)
    * (*_f)(z,lambda,_L,_model_ptr);
    //cout<<"\nres="<<res<<"\t"<<x1<<" "<<z<<endl;
    return res;
}



#include "gluon_fusion_parametrization.h"
#include<math.h>
using namespace std;


GluonFusionParametrization::GluonFusionParametrization()
{
    parametrization_switch_=2;
}

void GluonFusionParametrization::parametrization_for_LO_kinematics(double* xx_vegas)
{
    
    // parametrization 3 creates weird instabilities when nlo pdfs
    // are convoluted with a sector that needs z-subtraction
    
    if (parametrization_switch_==0)
    {
        //: parametrization z/x1
        x1LO=xx_vegas[0];
        x2LO=tau_/x1LO;
        measLO = 1.0/x1LO;
        zLO=1.0;
        
    }
    else if (parametrization_switch_==1)
    {
        
        //: old parametrization z/x1~rap
        double jac_from_rap_param ;
        double x= generate_x1(jac_from_rap_param,xx_vegas[0]);
        
        measLO = 1.0/x*jac_from_rap_param;
        
        
        
        
        x1LO=x;
        zLO=1.0;
        x2LO= tau_/x;
    }
    else if (parametrization_switch_==2)
    {
        //: new parametrization u1,u2
        double u2=xx_vegas[0];
        double u1=0.0;//xx_vegas[1];
        
        
        double U = log(tau_/(1.0-u1*(1.0-tau_)));
        
        x1LO = exp((1.0-u2)*U);
        x2LO = exp(u2*U);
        zLO=1.0;
        measLO =  log((1.0-u1*(1.0-tau_))/tau_);
    }
    else if (parametrization_switch_==3)
    {
        double zz=1.0;
        double yy=xx_vegas[0];
        measLO = -log(tau_/zz);
        x1LO = exp(yy*log(tau_));
        x2LO = exp((1.0-yy)*log(tau_));
        zLO = 1.0;
    }
    //cout<<"\n % x1LO="<<x1LO;
    
    cursLO=pow(mh_,2.0)/zLO;
    //     cout<<"\nhello "<<x1<<x2<<"\t"<<xx_vegas[0]<<"\t"<<xx_vegas[1]<<"\t"<<tau_;
}



void GluonFusionParametrization::parametrization_for_NLO_kinematics(double* xx_vegas)
{
    // parametrization 3 creates weird instabilities when nlo pdfs
    // are convoluted with a sector that needs z-subtraction
    
    if (parametrization_switch_==0)
    {
        //: parametrization z/x1
        x1=xx_vegas[0];
        x2=xx_vegas[1];
        meas = 1.0/x1;
        z=tau_/x1/x2;
        
    }
    else if (parametrization_switch_==1)
    {
        
        //: old parametrization z/x1~rap
        double jac_from_rap_param ;
        double x= generate_x1(jac_from_rap_param,xx_vegas[0]);
        
        meas = 1.0/x*jac_from_rap_param;
        
        x1=x;
        //: modification to protect ourselves from z=0
        //z=xx_vegas[1];
        double z_min=1e-14;
        z = z_min + xx_vegas[1] * (1.0-z_min);
        meas = meas * (1.0-z_min);
        x2= tau_/x1/z;
    }
    else if (parametrization_switch_==2)
    {
        //: new parametrization u1,u2
        double u2=xx_vegas[0];
        double u1=xx_vegas[1];
        
        
        double U = log(tau_/(1.0-u1*(1.0-tau_)));
        meas =  log((1.0-u1*(1.0-tau_))/tau_);
        x1 = exp((1.0-u2)*U);
        x2 = exp(u2*U);
        z=1.0-u1*(1.0-tau_);
    }
    else if (parametrization_switch_==3)
    {
        double yy=xx_vegas[0];
        double zz=xx_vegas[1];
        meas = -log(tau_/zz);
        x1 = exp(yy*log(tau_/zz));
        x2 = exp((1.0-yy)*log(tau_/zz));
        z = zz;
        
    }
    const double almost_zero =0.0;// 1e-23;
    if (x1>1.0-almost_zero or x2>1.0-almost_zero or x1<almost_zero or x2<almost_zero)
    {
        meas=0.0;
    }
    // cout<<"\n % x1="<<x1<<"\t"<<xx_vegas[0]<<"\n tau_="<<tau_;
    curs=pow(mh_,2.0)/z;
    
    Log_1mz=log(1.0-z);
    lambda = xx_vegas[2];
    phi=xx_vegas[3];
    
}


double GluonFusionParametrization::generate_x1(double & jac_from_rap_param,
                                               const double&x0)
{
    double xlambda=x0;
    double umax = -0.5*log(tau_);
    double umin = 0.5*log(tau_);
    double u=umin+(umax-umin)*xlambda;
    jac_from_rap_param = sqrt(tau_)*exp(u)*(umax-umin);
    double x= sqrt(tau_)*exp(u);
    return x;
}





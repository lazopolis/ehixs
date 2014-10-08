

#ifndef LUMINOSITY_INTEGRALS_H
#define LUMINOSITY_INTEGRALS_H

#include "vegas_adaptor.h"
#include "luminosity.h"


class LuminosityIntegral: public CoolInt
{
public:
    virtual double evaluateIntegral(const double* xx)=0;
    virtual void set_initial_flavors()=0;
    void Configure(NewLuminosity* lumi, const double& tau)
    {lumi_=lumi; tau_ = tau;set_initial_flavors();
        _epsrel = 1e-2;
        _epsabs = 1e-10;
    }
protected:
    NewLuminosity* lumi_;
    double tau_;
};

class LuminosityIntegralDelta: public LuminosityIntegral
{
public:
    double evaluateIntegral(const double* xx);
};

class LuminosityIntegralPlus: public LuminosityIntegral
{
public:
    LuminosityIntegralPlus(){set_dimensions(2);}
    double evaluateIntegral(const double* xx);
    virtual double log_power()=0;
};

class LuminosityIntegralReal: public LuminosityIntegral
{
public:
    typedef double (*f_ptr)(const double&,const double&) ;
    
    LuminosityIntegralReal(f_ptr f,const double& L):_f(f),_L(L){set_dimensions(2);}
    double evaluateIntegral(const double* xx);
private:
    f_ptr _f;
    double _L;//log(muf^2/mh^2)
};

class gg_real : public LuminosityIntegralReal
{
public:
    gg_real(f_ptr f,const double& L):LuminosityIntegralReal(f,L){};
    
    void set_initial_flavors(){lumi_->add_pair(QCD::g, QCD::g);}
};



class gg_delta : public LuminosityIntegralDelta
{
public:
    void set_initial_flavors(){lumi_->add_pair(QCD::g, QCD::g);}
};

class gg_plus_0 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 0.0;}
};

class gg_plus_1 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 1.0;}
};

class gg_plus_2 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 2.0;}
};

class gg_plus_3 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 3.0;}
};

class gg_plus_4 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 4.0;}
};

class gg_plus_5 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 5.0;}
};


#endif
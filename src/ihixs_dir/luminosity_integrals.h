

#ifndef LUMINOSITY_INTEGRALS_H
#define LUMINOSITY_INTEGRALS_H

#include "vegas_adaptor.h"
#include "luminosity.h"
#include "user_interface.h"

class LuminosityIntegral: public CoolInt
{
public:
    virtual double evaluateIntegral(const double* xx)=0;
    virtual void set_initial_flavors()=0;
    void Configure(NewLuminosity* lumi, const double& tau,const UserInterface UI)
    {lumi_=lumi; tau_ = tau;set_initial_flavors();
        _epsrel = UI.epsrel;
        _epsabs = UI.epsabs;
        _mineval = UI.mineval;
        _maxeval = UI.maxeval;
        _nstart = UI.nstart;
        _nincrease = UI.nincrease;
    }
    double tau(){return tau_;}
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




class qg_real : public LuminosityIntegralReal
{
public:
    qg_real(f_ptr f,const double& L):LuminosityIntegralReal(f,L){};
    
    void set_initial_flavors()
    {
        lumi_->clear_pairs();
        for (int i=-5;i<6;i++)
        {
            if (i!=0)
            {
                //:  q g + qbar g
                lumi_->add_pair(i, QCD::g);
                //:  g q + g qbar
                lumi_->add_pair(QCD::g, i);
            }
        }
        
    }
};

class qqb_real : public LuminosityIntegralReal
{
public:
    qqb_real(f_ptr f,const double& L):LuminosityIntegralReal(f,L){};
    
    void set_initial_flavors()
    {
        lumi_->clear_pairs();
        for (int i=-5;i<6;i++)
        {
            if (i!=0)
            {
                //:  q qbar + qbar q
                lumi_->add_pair(i, -i);
            }
        }
        
    }
};

class qq_real : public LuminosityIntegralReal
{
public:
    qq_real(f_ptr f,const double& L):LuminosityIntegralReal(f,L){};
    
    void set_initial_flavors()
    {
        lumi_->clear_pairs();
        for (int i=-5;i<6;i++)
        {
            if (i!=0)
            {
                //:  q q
                lumi_->add_pair(i, i);
            }
        }
        
    }
};

class q1q2_real : public LuminosityIntegralReal
{
public:
    q1q2_real(f_ptr f,const double& L):LuminosityIntegralReal(f,L){};
    
    void set_initial_flavors()
    {
        lumi_->clear_pairs();
        for (int i=-5;i<6;i++)
        {
            for (int j=-5;j<6;j++)
            {
                if (i!=0 and j!=0 and i!=j and i!=-j)
                {
                //:  q1 q2
                lumi_->add_pair(i, j);
                }
            }
        }
        
    }
};


class gg_real : public LuminosityIntegralReal
{
public:
    gg_real(f_ptr f,const double& L):LuminosityIntegralReal(f,L){};
    
    void set_initial_flavors(){lumi_->clear_pairs();lumi_->add_pair(QCD::g, QCD::g);}
};



class gg_delta : public LuminosityIntegralDelta
{
public:
    void set_initial_flavors(){lumi_->clear_pairs();lumi_->add_pair(QCD::g, QCD::g);}
};

class gg_plus_0 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->clear_pairs();lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 0.0;}
};

class gg_plus_1 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->clear_pairs();lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 1.0;}
};

class gg_plus_2 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->clear_pairs();lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 2.0;}
};

class gg_plus_3 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->clear_pairs();lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 3.0;}
};

class gg_plus_4 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->clear_pairs();lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 4.0;}
};

class gg_plus_5 : public LuminosityIntegralPlus
{
public:
    void set_initial_flavors(){lumi_->clear_pairs();lumi_->add_pair(QCD::g, QCD::g);}
    double log_power(){return 5.0;}
};

// RealExact integrals depend on 3 random numbers (x1,z,lambda)
// they also keep function pointers with different signature
// because we need to pass the pointer of CModel around to get access
// to masses and couplings
class CModel;

class LuminosityIntegralRealExact: public LuminosityIntegral
{
public:
    typedef double (*f_ptr2)(const double& z, const double& lambda,
    const double& L,CModel* model);
    
    LuminosityIntegralRealExact(f_ptr2 f,const double& L,CModel* model_ptr)
    {_f=f;_L=L;set_dimensions(3);_model_ptr = model_ptr;}
    double evaluateIntegral(const double* xx);
private:
    f_ptr2 _f;
    double _L;//log(muf^2/mh^2)
    CModel* _model_ptr;
};

class gg_real_exact : public LuminosityIntegralRealExact
{
public:
    gg_real_exact(f_ptr2 f,const double& L,CModel* model_ptr):LuminosityIntegralRealExact(f,L,model_ptr){};
    
    void set_initial_flavors(){lumi_->clear_pairs();lumi_->add_pair(QCD::g, QCD::g);}
};


class qg_real_exact : public LuminosityIntegralRealExact
{
public:
    qg_real_exact(f_ptr2 f,const double& L,CModel* model_ptr):LuminosityIntegralRealExact(f,L,model_ptr){};
    
    void set_initial_flavors()
    {
        lumi_->clear_pairs();
        for (int i=-5;i<6;i++)
        {
            if (i!=0)
            {
                //:  q g + qbar g
                lumi_->add_pair(i, QCD::g);
                //:  g q + g qbar
                lumi_->add_pair(QCD::g, i);
            }
        }
        
    }
};

class qqb_real_exact : public LuminosityIntegralRealExact
{
public:
    qqb_real_exact(f_ptr2 f,const double& L,CModel* model_ptr):LuminosityIntegralRealExact(f,L,model_ptr){};
    
    void set_initial_flavors()
    {
        lumi_->clear_pairs();
        for (int i=-5;i<6;i++)
        {
            if (i!=0)
            {
                //:  q qbar + qbar q
                lumi_->add_pair(i, -i);
            }
        }
        
    }
};




#endif
#ifndef INCLUSIVE_PROCESS_H
#define INCLUSIVE_PROCESS_H

#include "user_interface.h"
#include "vegas_adaptor.h"
#include "luminosity.h"
#include "constants.h"
#include "model.h"
#include <vector>
using namespace std;

#include "higgs_eft.h"


class LuminosityIntegral: public CoolInt
{
public:
    virtual double evaluateIntegral(const double* xx)=0;
    virtual void set_initial_flavors()=0;
    void Configure(NewLuminosity* lumi, const double& tau)
    {lumi_=lumi; tau_ = tau;set_initial_flavors();
        _epsrel = 1e-3;
        _epsabs = 1e-3;
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

class WilsonCoefficient{
public:
    void Configure(const double& log_muf_over_mt_sq);
    double w(int i) const {return _w[i];}
    
private:
    vector<double> _w;
};

class SigmaTerm{
public:
    SigmaTerm(const string& thetype, const vector<double>& val){_type=thetype;_val = val;}
    double operator[](int i){return _val[i];}
    void multiply(const double& c){for (int i=0;i<_val.size();i++) _val[i]= c* _val[i];}
    void wc_expansion(const WilsonCoefficient& wc);
    
    void multiply_by_as_pi(const double& as_pi){for (int i=0;i<_val.size();i++) _val[i]= pow(as_pi,i+2.)* _val[i];}
    
    friend ostream& operator<<(ostream&, const SigmaTerm&);
    string type(){return _type;}
private:
    string _type;//delta, plus, reg
    vector<double> _val;
};

class InclusiveProcess
{
public:
    InclusiveProcess(const UserInterface& UI);
    void Evaluate();
    
    friend ostream& operator<<(ostream&, const InclusiveProcess&);
//    double gg_delta_LO();
//    double gg_delta_NLO();
//    double gg_delta_NNLO();
//    double gg_delta_N3LO();
    
    double sigma(const string& type_id,int i);
    double sigma_delta(int i);
    double sigma_D0(int i);
    double sigma_D1(int i);
    double sigma_D2(int i);
    double sigma_D3(int i);
    double sigma_D4(int i);
    double sigma_D5(int i);

//    double gg_D0_NLO();
//    double gg_D1_NLO();
//    double gg_plus_NLO(){return gg_D0_NLO()+gg_D1_NLO();}
//    
//    double gg_D0_NNLO();
//    double gg_D1_NNLO();
//    double gg_D2_NNLO();
//    double gg_D3_NNLO();
//    double gg_plus_NNLO(){return gg_D0_NNLO()+gg_D1_NNLO()+gg_D2_NNLO()
//                                +gg_D3_NNLO();}
//
//    double gg_D0_N3LO();
//    double gg_D1_N3LO();
//    double gg_D2_N3LO();
//    double gg_D3_N3LO();
//    double gg_D4_N3LO();
//    double gg_D5_N3LO();
//    double gg_plus_N3LO(){
//                        return gg_D0_N3LO()
//                                +gg_D1_N3LO()
//                                +gg_D2_N3LO()
//                                +gg_D3_N3LO()
//                                +gg_D4_N3LO()
//                                +gg_D5_N3LO();}

private:
    
    gg_delta D_gg;
    gg_plus_0 P0_gg;
    gg_plus_1 P1_gg;
    gg_plus_2 P2_gg;
    gg_plus_3 P3_gg;
    gg_plus_4 P4_gg;
    gg_plus_5 P5_gg;
    vector<LuminosityIntegral*> _lumi_int;
private:
    CModel _model;
    double _as_pi;
    NewLuminosity* _lumi;
    HiggsEFT _eft;
    
    WilsonCoefficient _wc;
    double _prefactor;
    
    double _log_muf_over_mt_sq;
    double _log_muf_mh_sq;
    double _log_mur_over_muf_sq;
    
    vector<SigmaTerm*> _sigma;
    vector<double> _sigma_delta;
    vector<double> _sigma_D0;
    vector<double> _sigma_D1;
    vector<double> _sigma_D2;
    vector<double> _sigma_D3;
    vector<double> _sigma_D4;
    vector<double> _sigma_D5;
    
    vector<double> evolve_xs_to_mur(const vector<double>& x);
    vector<double> construct_n_delta_at_muf();
    vector<double> construct_n_D0_at_muf();
    vector<double> construct_n_D1_at_muf();
    vector<double> construct_n_D2_at_muf();
    vector<double> construct_n_D3_at_muf();
    vector<double> construct_n_D4_at_muf();
    vector<double> construct_n_D5_at_muf();
    
    vector<double> wc_x_n(const WilsonCoefficient& wc, const vector<double> x);

};


/*
class CrossSectionTerm : public CoolInt
{
public:
    CrossSectionTerm(){};
    void perform(){call_vegas();}
    virtual double evaluateIntegral(const double xx[])=0;
    LuminosityStack lumistack;
protected:
     
    double matrix_element;
    double x1;
    double measure;
    double tau;
    double z;
protected:
    virtual void generate_initial_state_variables(const double xx[])=0;
    virtual void set_matrix_element()=0;
    virtual double LL()=0;    
};

class DeltaTerm : public CrossSectionTerm
{
public:
    DeltaTerm(){set_dimensions(1);}
    double evaluateIntegral(const double xx[]);
protected:
    void generate_initial_state_variables(const double xx[]);
    double LL();
};

class PlusTerm : public CrossSectionTerm
{
public:
    PlusTerm(){set_dimensions(2);}
    double evaluateIntegral(const double xx[]);
protected:
    void generate_initial_state_variables(const double xx[]);
    double LL();
};

class RegularTerm : public CrossSectionTerm
{
public:
    RegularTerm(){set_dimensions(2);}
    double evaluateIntegral(const double xx[]);
protected:
    void generate_initial_state_variables(const double xx[]);
    double LL();
};


class LO_delta_gg : public DeltaTerm
{
public:
    void set_matrix_element();
};

class NLO_delta_gg : public DeltaTerm
{
public:
    void set_matrix_element();
};

class NNLO_delta_gg : public DeltaTerm
{
public:
    void set_matrix_element();
};

class NLO_plus_gg : public PlusTerm
{
public:
    void set_matrix_element();
};

class NNLO_plus_gg : public PlusTerm
{
public:
    void set_matrix_element();
};

class NLO_reg_gg : public RegularTerm
{
public:
    void set_matrix_element();
};

class NNLO_reg_gg : public RegularTerm
{
public:
    void set_matrix_element();
};

class InclusiveProcess
{
public: 
    InclusiveProcess(const UserInterface& UI);
    void perform();
private:
    LO_delta_gg EggLOdelta;
    NLO_delta_gg EggNLOdelta;
    NNLO_delta_gg EggN2LOdelta;
    NLO_plus_gg EggNLOplus;
    NNLO_plus_gg EggN2LOplus;
    NLO_reg_gg EggNLOregular;
    NNLO_reg_gg EggN2LOregular;
    Luminosity* lumi;
};
*/
#endif
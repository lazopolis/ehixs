#ifndef EXACT_LO_INCLUSIVE_H
#define EXACT_LO_INCLUSIVE_H

#include "vegas_adaptor.h"
#include "model.h"
#include "luminosity.h"
#include<complex>
using namespace std;



class IS_Parametrization_LO
{
public://methods
    IS_Parametrization_LO(){_measLO=0.0;_x1LO=0.0;_x2LO=0.0;_zLO=0.0;}
    virtual void set_up_initial_state(double x){};
    void set_tau(const double & tau){_tau=tau;}
    double measLO(){return _measLO;}
    double x1LO(){return _x1LO;}
    double x2LO(){return _x2LO;}
private://methods
public://data
protected://data
    double _measLO;
    double _x1LO;
    double _x2LO;
    double _zLO;
    double _tau;

};

class IS_Parametrization_LO_type_0 : public IS_Parametrization_LO
{
public://methods
    IS_Parametrization_LO_type_0():IS_Parametrization_LO(){};
    void set_up_initial_state(double x);
private://methods
};


class InitialState
{
public://data
private://data
    IS_Parametrization_LO * _ISP;
    Luminosity * _lumi;
public://methods
    InitialState(UserInterface* UI,Luminosity* lumi);
    double measure_x_luminosity();
    double measure_x_ith_luminosity(unsigned);

    double luminosity(){return _lumi->LL(0);}
    void new_state(const double &xx);
    int pdf_size(){return _lumi->pdf_size();}
private://methods
};


class IntegratedContribution : public CoolInt
{
public://data
public://methods
    IntegratedContribution(UserInterface* UI,Luminosity* lumi);
    double xs(){return _xs;}
    double mc_error(){return _err;}
    double evaluateIntegral(const double xx[]);
    void perform();
    
private://data
    InitialState * _in_state;
    double _phase_space_independent_prefactor;
    double _xs;
    double _err;
    
private://methods
};


class Coefficient{

public://methods
    Coefficient(unsigned i){_coeff = vector<double>(i,0.0);}
    double component(int i){return _coeff[i];}
    void set_component(unsigned i,const double & xx){_coeff[i] = xx;}
private://data
    vector<double> _coeff;

};


class ResultHolder{
public://methods
    ResultHolder(unsigned long i);
    double cross_section(){return _cross_section[0];}
    double mc_error(){return _mc_error[0];}
    vector<double> pdf_error_vector(){return _pdf_error;}
    double pdf_error_plus(){return _pdf_error[0];}
    double pdf_error_minus(){return _pdf_error[1];}
    void set_xs_component(unsigned  i,const double & xx){_cross_section[i]=xx;}
    void set_err_component(unsigned i,const double & xx){_mc_error[i]=xx;}
    void compute_pdf_error(Luminosity* lumi)
                    {_pdf_error = lumi->calculate_pdf_error(_cross_section);}
    void show_pdf_results();
    bool pdf_error_has_been_computed();
private:
    vector<double> _cross_section;
    vector<double> _mc_error;
    vector<double> _pdf_error;
};

class CoefficientIntegralBinder
{
public://methods
    CoefficientIntegralBinder(Coefficient*,IntegratedContribution*);
    void perform();
    void compute_pdf_error(Luminosity* lumi){_results->compute_pdf_error(lumi);}
    double xs(){return _results->cross_section();}
    double mc_error(){return _results->mc_error();}
    vector<double> pdf_error_vector(){return _results->pdf_error_vector();}
    void show_pdf_results(){_results->show_pdf_results();}
    string pdf_error_in_string_format();
private://data
    IntegratedContribution * _integral;
    Coefficient * _coefficient;
    ResultHolder* _results;
};


class InclusiveTerm
{
public://methods
    double cross_section(){return _binder->xs();}
    double mc_error(){return _binder->mc_error();}
    vector<double> pdf_error(){return _binder->pdf_error_vector();}
    void show_pdf_results(){_binder->show_pdf_results();}
    string pdf_error_in_string_format(){return _binder->pdf_error_in_string_format();}
    void perform();

protected://data
    CoefficientIntegralBinder* _binder;
    IntegratedContribution *_integral;
    Coefficient* _coefficient;
    UserInterface * UI;
    Luminosity *lumi;
    CModel* Model;
protected://methods
    void _set_up_coefficient(const double &c,const double & alpha_power);

};


class Exact_LO_Inclusive : public InclusiveTerm
{
public://data
    

public://methods
    Exact_LO_Inclusive(CModel*,UserInterface*);
    ~Exact_LO_Inclusive(){};
    
    double ME_sq(){return _ME_sq;}
    double alpha_s_used(){return Model->alpha_strong[0];}
    double cur_lumiLO(const double x);
        
private://data
    double _ME_sq;

    
    
    
    
private://methods
    void evaluate_phase_space_independent_exact_LO_ME();
    complex<double> born(complex<double> x);
    void set_up_gg_lumi();
    void _compute_pdf_error();

};

#endif
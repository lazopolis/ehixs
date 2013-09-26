#include "exact_lo_inclusive.h"

#include "chaplin/chaplin.h"


#include<math.h>
using namespace std;


ResultHolder::ResultHolder(unsigned long i)
{
    _cross_section = vector<double>(i,0.0);
    _mc_error = vector<double>(i,0.0);
    _pdf_error = vector<double>(i,0.0);

}

void ResultHolder::show_pdf_results()
{
    cout<<"\n:::::                              ::::::";
    for (int i=0;i<_cross_section.size();i++)
        {
        cout<<"\n"<<i+1<<" : "<<_cross_section[i];
        }
    cout<<"\n:::::                              ::::::";
    
}

bool ResultHolder::pdf_error_has_been_computed()
{
    if (_pdf_error.size()==2)
        return true;
    else return false;
}
//------------------------------------------------------------------------------

CoefficientIntegralBinder::CoefficientIntegralBinder(
                            Coefficient* coeff,
                            IntegratedContribution* integral)
{
    _coefficient = coeff;
    _integral = integral;
    _results = new ResultHolder(_integral->number_of_components());
}

void CoefficientIntegralBinder::perform()
{
    
    for (unsigned i=0;i<_integral->number_of_components();i++)
        {
        _results->set_xs_component(i,
                                _integral->give_res_component(i)
                                   * _coefficient->component(i));
        _results->set_err_component(i,
                                   _integral->give_err_component(i)
                                    * _coefficient->component(i));
        }
}

string CoefficientIntegralBinder::pdf_error_in_string_format()
{
    stringstream s;
    if (_results->pdf_error_has_been_computed())
        {
        s<<" + "<<_results->pdf_error_plus()<<" "<<_results->pdf_error_minus();
        }
    else
        {
        s<<" not computed ";
        }
    return s.str();
}

//------------------------------------------------------------------------------

Exact_LO_Inclusive:: Exact_LO_Inclusive(CModel * imodel, UserInterface* iui)
{
    Model = imodel;
    UI = iui;
    set_up_gg_lumi();
    Model->evolve(lumi->alpha_s_at_mz_vector(),
                  UI->mur_over_mhiggs * UI->m_higgs,
                  UI->perturbative_order);
    Model->set_Xq_for_quarks();
    
    //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
    //: Gf = 1.16637*10^{-5} * 0.389379*10^9
    double  _pref_sgg = 35.0309;
    double  _alpha_power = 2.0;
    
    evaluate_phase_space_independent_exact_LO_ME();
    
    _set_up_coefficient(_ME_sq*_pref_sgg,_alpha_power);
    
        
    _integral = new IntegratedContribution(UI,lumi);

}



void Exact_LO_Inclusive::set_up_gg_lumi()
{
    lumi=new Luminosity(*UI);
    lumi->add_pair(pdf_desc(0,0,0,0),pdf_desc(0,0,0,0));
}

void Exact_LO_Inclusive::evaluate_phase_space_independent_exact_LO_ME()
{
    complex<double> ME;
    
    for (int i=0;i<Model->quarks.size();i++)
        {
        if (Model->quarks[i]->Y != 0.0)
            {
            ME = ME + Model->quarks[i]->Y * born(Model->quarks[i]->X);
            }
        }
    _ME_sq = pow(abs(ME),2.0);
}

complex<double> Exact_LO_Inclusive::born(complex<double> x)
{
    //: the expression below goes to 1 as mq->infty, i.e. as x->1
    return (-3.0)*x/pow(1.0-x,2.0)
                        *(2.0-pow(1.0+x,2.0)/pow(1.0-x,2.0)*HPL2(0,0,x));
}

//-----------------------------------------------------------------------------

void InclusiveTerm::perform()
{
    _integral->perform();
    _binder = new CoefficientIntegralBinder(_coefficient,_integral);
    _binder->perform();
    if (UI->pdf_error)
        {
        _binder->compute_pdf_error(lumi);
        }
}

void InclusiveTerm::_set_up_coefficient(const double &c,
                                        const double & alpha_power)
{
    _coefficient = new Coefficient(lumi->pdf_size());
    
    for (unsigned i=0;i<lumi->pdf_size();i++)
        {
        _coefficient->set_component(i,
                                    c * pow(Model->alpha_strong[i]/consts::Pi,alpha_power)
                                    );
        }
    
}


//-----------------------------------------------------------------------------

IntegratedContribution::IntegratedContribution(UserInterface* UI,
                                               Luminosity* lumi)
{
    set_number_of_xs_values_we_keep(lumi->pdf_size());
    _in_state = new InitialState(UI,lumi);
    setParams(1,UI->epsrel,UI->epsabs);
}

double IntegratedContribution::evaluateIntegral(const double xx[])
{
    _in_state->new_state(xx[0]);
    double res = _in_state->measure_x_luminosity();
    for (int i=0;i<_in_state->pdf_size();i++)
        {
        _running_xs_values[i] = _in_state->measure_x_ith_luminosity(i);
        }
    return res;
}

void IntegratedContribution::perform()
{
    call_vegas();
    _xs = result();
    _err = error();
}

//-----------------------------------------------------------------------------

InitialState::InitialState(UserInterface* UI, Luminosity * lumi)
{
    _lumi = lumi;
    _ISP = new IS_Parametrization_LO_type_0;
    double tau =  pow(UI->m_higgs,2.0)/pow(UI->Etot,2.0);
    _ISP -> set_tau(tau);
}

void InitialState::new_state(const double & xx)
{
    _ISP->set_up_initial_state(xx);
    _lumi->set_cur_lumi(_ISP->x1LO(),_ISP->x2LO());
}

double InitialState::measure_x_luminosity(){return _ISP->measLO()*_lumi->LL(0);}
double InitialState::measure_x_ith_luminosity(unsigned i)
    {
    return _ISP->measLO()*_lumi->LL(i);
    }

//-----------------------------------------------------------------------------


void IS_Parametrization_LO_type_0:: set_up_initial_state(double x)
{
    
    double xlambda=x;
    double umax = -0.5*log(_tau);
    double umin = 0.5*log(_tau);
    double u=umin+(umax-umin)*xlambda;
    //: old parametrization z/x1~rap
    double jac_from_rap_param = sqrt(_tau)*exp(u)*(umax-umin);
    _x1LO= sqrt(_tau)*exp(u);
    _measLO = 1.0/_x1LO*jac_from_rap_param;
    _zLO=1.0;
    _x2LO= _tau/_x1LO;


    
}










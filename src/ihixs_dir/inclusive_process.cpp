#include "inclusive_process.h"




double LuminosityIntegralDelta::evaluateIntegral(const double* xx)
{
    const double x1= xx[0];
    const double measure = 1./x1;
    // the check :  x2 = tau/x1 is in [0,1]
    // is done in lumi and 0.0 is returned if it fails
    //cout<<"x1="<<x1<<" x2= "<<tau_/x1<<" tau = "<<tau_<<" L="<<lumi_->give(x1,tau_/x1)<<endl;
    return measure*lumi_->give(x1,tau_/x1);
}

double LuminosityIntegralPlus::evaluateIntegral(const double* xx)
{
    const double x1= xx[0];
    const double z = xx[1];
    const double measure = 1./x1;
    // the check :  x2 = tau/x1 is in [0,1]
    // is done in lumi and 0.0 is returned if it fails
    const double res=  measure
            *( lumi_->give(x1,tau_/x1/z) - lumi_->give(x1,tau_/x1) )
            / (1.-z)
            * pow(log(1.-z), log_power());
    //cout<<"\nres="<<res<<"\t"<<x1<<" "<<z<<endl;
    return res;
}


ostream& operator<<(ostream& stream, const SigmaTerm& st)
{
    stream<<st._type;
    for (int i=0;i<st._val.size();i++)
    {
        stream<<" "<<i<<" : "<<st._val[i];
    }
    stream<<endl;
    return stream;
}

InclusiveProcess::InclusiveProcess(const UserInterface& UI)
{
    
    _lumi = new NewLuminosity(UI);
    
    const double tau = pow(UI.m_higgs,2.)/pow(UI.Etot,2.);
    D_gg.Configure(new NewLuminosity(UI),tau);
    P0_gg.Configure(new NewLuminosity(UI),tau);
    P1_gg.Configure(new NewLuminosity(UI),tau);
    P2_gg.Configure(new NewLuminosity(UI),tau);
    P3_gg.Configure(new NewLuminosity(UI),tau);
    P4_gg.Configure(new NewLuminosity(UI),tau);
    P5_gg.Configure(new NewLuminosity(UI),tau);
    _model.Configure(
                     _lumi->alpha_s_at_mz(),
                     UI.mur_over_mhiggs,
                     UI.perturbative_order,
                     UI.m_higgs
                     );
    // Setting up alpha_s
    _as_pi = _model.alpha_strong()/consts::Pi;
    cout << "\n[CrossSection]: a_s = " << _as_pi * consts::Pi<<endl;
    _log_muf_over_mt_sq = 2.*log(UI.muf/_model.top.m());
    _log_muf_mh_sq = 2. * log(UI.muf/_model.higgs.m());
    _log_mur_over_muf_sq = 2. *  log(UI.mur/UI.muf);
    // setting up wilson coeffs
    _wc.Configure(_log_muf_over_mt_sq);

    //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
    //: Gf = 1.16637*10^{-5} * 0.389379*10^9
    _prefactor = 35.0309;
    
    //: constructing the delta and plus terms

    _sigma.push_back(
            new SigmaTerm("delta",construct_n_delta_at_muf()));
    _sigma.push_back(
                     new SigmaTerm("D0",construct_n_D0_at_muf()));
    _sigma.push_back(
                     new SigmaTerm("D1",construct_n_D1_at_muf()));
    _sigma.push_back(
                     new SigmaTerm("D2",construct_n_D2_at_muf()));
    _sigma.push_back(
                     new SigmaTerm("D3",construct_n_D3_at_muf()));
    _sigma.push_back(
                     new SigmaTerm("D4",construct_n_D4_at_muf()));
    _sigma.push_back(
                     new SigmaTerm("D5",construct_n_D5_at_muf()));
                     
    _lumi_int.push_back(&D_gg);
    _lumi_int.push_back(&P0_gg);
    _lumi_int.push_back(&P1_gg);
    _lumi_int.push_back(&P2_gg);
    _lumi_int.push_back(&P3_gg);
    _lumi_int.push_back(&P4_gg);
    _lumi_int.push_back(&P5_gg);
   
}

ostream& operator<<(ostream& stream, const InclusiveProcess& ip)
{
    for (int i=0;i<ip._sigma.size();i++)
        {
            stream<< *(ip._sigma[i]);
        }
    return stream;
}


void WilsonCoefficient::Configure(const double& log_muf_over_mt_sq)
{
    double c0 = 1.;
    double c1 = 11./4.;
    double c2 = 2777./288. - consts::nf * 67./96.+ log_muf_over_mt_sq * (19./16.+consts::nf/3.);
    double c3 = -2892659.0/41472.
    +(897943./9216.)*consts::z3
    +(1733./288.)*log_muf_over_mt_sq
    +(209./64.)* pow(log_muf_over_mt_sq,2.)
    +consts::nf*(
                 (55./54.)*log_muf_over_mt_sq
                 +40291./20736.
                 -(110779./13824.)*consts::z3
                 +(23./32.)*pow(log_muf_over_mt_sq,2.)
                 )
    +pow(consts::nf,2.)*(
                         -6865./31104.
                         +(77./1728.)*log_muf_over_mt_sq
                         -(1./18.)* pow(log_muf_over_mt_sq,2)
                         );
    
    _w.push_back(c0*c0);
    _w.push_back(2.*c0*c1);
    _w.push_back(c1*c1+2.*c0*c2);
    _w.push_back(2.*c1*c2+2.*c3*c0);
}

void InclusiveProcess::Evaluate()
{
    D_gg.call_vegas();
    P0_gg.call_vegas();
    P1_gg.call_vegas();
    P2_gg.call_vegas();
    P3_gg.call_vegas();
    P4_gg.call_vegas();
    P5_gg.call_vegas();
    
    _sigma_delta = wc_x_n(_wc,construct_n_delta_at_muf());
    _sigma_D0 = wc_x_n(_wc,construct_n_D0_at_muf());
    _sigma_D1 = wc_x_n(_wc,construct_n_D1_at_muf());
    _sigma_D2 = wc_x_n(_wc,construct_n_D2_at_muf());
    _sigma_D3 = wc_x_n(_wc,construct_n_D3_at_muf());
    _sigma_D4 = wc_x_n(_wc,construct_n_D4_at_muf());
    _sigma_D5 = wc_x_n(_wc,construct_n_D5_at_muf());
    
    for (int i=0;i<_sigma.size();i++)
    {
        _sigma[i]->multiply(_lumi_int[i]->result());
        _sigma[i]->multiply(_prefactor);
        _sigma[i]->wc_expansion(_wc);
        _sigma[i]->multiply_by_as_pi(_as_pi);
        _sigma[i]->multiply(1.065);
    }
}    

double InclusiveProcess::sigma(const string& type_id,int i)
{
    for (int k=0;k<_sigma.size();k++)
    {
        if (type_id==_sigma[k]->type())
        {
            return (*_sigma[k])[i]* _prefactor 
            * pow(_as_pi,double(i+2)) // <- note the i+2: the LO is a^2
            ;
        }
    }
    cout<<"\nThe term "<<type_id<<" you asked for was not found!"<<endl;
    exit(0);
}


double InclusiveProcess::sigma_delta(int i)
{
    return _sigma_delta[i] * _prefactor 
            * pow(_as_pi,double(i+2)) // <- note the i+2: the LO is a^2
            * D_gg.result();
}

double InclusiveProcess::sigma_D0(int i)
{
    return _sigma_D0[i] * _prefactor 
    * pow(_as_pi,double(i+2)) // <- note the i+2: the LO is a^2
    * P0_gg.result();
}

double InclusiveProcess::sigma_D1(int i)
{
    return _sigma_D1[i] * _prefactor 
    * pow(_as_pi,double(i+2)) // <- note the i+2: the LO is a^2
    * P1_gg.result();
}

double InclusiveProcess::sigma_D2(int i)
{
    return _sigma_D2[i] * _prefactor 
    * pow(_as_pi,double(i+2)) // <- note the i+2: the LO is a^2
    * P2_gg.result();
}

double InclusiveProcess::sigma_D3(int i)
{
    return _sigma_D3[i] * _prefactor 
    * pow(_as_pi,double(i+2)) // <- note the i+2: the LO is a^2
    * P3_gg.result();
}

double InclusiveProcess::sigma_D4(int i)
{
    return _sigma_D4[i] * _prefactor 
    * pow(_as_pi,double(i+2)) // <- note the i+2: the LO is a^2
    * P4_gg.result();
}

double InclusiveProcess::sigma_D5(int i)
{
    return _sigma_D5[i] * _prefactor 
    * pow(_as_pi,double(i+2)) // <- note the i+2: the LO is a^2
    * P5_gg.result();
}
// here we assume the vector x contains the coefficients of 
// x0 + a * x1 + a^2 * x2 + a^3 * x3
// we then evolve following a ->a (1+b0*L*a+(b0^2L^2+b1*L)a^2)
vector<double> InclusiveProcess::evolve_xs_to_mur(const vector<double>& x)
{
    const double Lrf = _log_mur_over_muf_sq;
    const double b0 = consts::beta_zero;
    const double b1 = consts::beta_one;
    vector<double> res;
    res.push_back(x[0]);
    res.push_back(x[1]);
    res.push_back(x[2] + b0 * Lrf * x[1]);
    res.push_back(x[3] 
                  + 2.*b0 * Lrf * x[2]
                  + (b0*b0*Lrf*Lrf + b1*Lrf) * x[1]);
    return res;              

}

vector<double> InclusiveProcess::construct_n_delta_at_muf()
{
    vector<double> n_delta_at_muf;
    n_delta_at_muf.push_back(_eft.n_LO_delta());
    n_delta_at_muf.push_back(_eft.n_NLO_delta());
    n_delta_at_muf.push_back(_eft.n_NNLO_delta()
                             + _eft.n_NNLO_delta_L()*_log_muf_mh_sq
                             + _eft.n_NNLO_delta_L2()*pow(_log_muf_mh_sq,2.));
    n_delta_at_muf.push_back(_eft.n_N3LO_delta()
                             + _eft.n_N3LO_delta_L()*_log_muf_mh_sq
                             + _eft.n_N3LO_delta_L2()*pow(_log_muf_mh_sq,2.)
                             + _eft.n_N3LO_delta_L3()*pow(_log_muf_mh_sq,3.));
    return n_delta_at_muf;

}

vector<double> InclusiveProcess::construct_n_D0_at_muf()
{
    vector<double> res;
    res.push_back(0.0);
    res.push_back(_eft.n_NLO_D0_L() * _log_muf_mh_sq);
    res.push_back(_eft.n_NNLO_D0()
                             + _eft.n_NNLO_D0_L()*_log_muf_mh_sq
                             + _eft.n_NNLO_D0_L2()*pow(_log_muf_mh_sq,2.));
    res.push_back(_eft.n_N3LO_D0()
                             + _eft.n_N3LO_D0_L()*_log_muf_mh_sq
                             + _eft.n_N3LO_D0_L2()*pow(_log_muf_mh_sq,2.)
                             + _eft.n_N3LO_D0_L3()*pow(_log_muf_mh_sq,3.));
    return res;
}

vector<double> InclusiveProcess::construct_n_D1_at_muf()
{
    vector<double> res;
    res.push_back(0.0);
    res.push_back(_eft.n_NLO_D1());
    res.push_back(_eft.n_NNLO_D1()
                  + _eft.n_NNLO_D1_L()*_log_muf_mh_sq
                  + _eft.n_NNLO_D1_L2()*pow(_log_muf_mh_sq,2.));
    res.push_back(_eft.n_N3LO_D1()
                  + _eft.n_N3LO_D1_L()*_log_muf_mh_sq
                  + _eft.n_N3LO_D1_L2()*pow(_log_muf_mh_sq,2.)
                  + _eft.n_N3LO_D1_L3()*pow(_log_muf_mh_sq,3.));
    return res;
}

vector<double> InclusiveProcess::construct_n_D2_at_muf()
{
    vector<double> res;
    res.push_back(0.0);
    res.push_back(0.0);
    res.push_back(_eft.n_NNLO_D2()
                  + _eft.n_NNLO_D2_L()*_log_muf_mh_sq);
    res.push_back(_eft.n_N3LO_D2()
                  + _eft.n_N3LO_D2_L()*_log_muf_mh_sq
                  + _eft.n_N3LO_D2_L2()*pow(_log_muf_mh_sq,2.)
                  + _eft.n_N3LO_D2_L3()*pow(_log_muf_mh_sq,3.));
    return res;
}

vector<double> InclusiveProcess::construct_n_D3_at_muf()
{
    vector<double> res;
    res.push_back(0.0);
    res.push_back(0.0);
    res.push_back(_eft.n_NNLO_D3());
    res.push_back(_eft.n_N3LO_D3()
                  + _eft.n_N3LO_D3_L()*_log_muf_mh_sq
                  + _eft.n_N3LO_D3_L2()*pow(_log_muf_mh_sq,2.));
    return res;
}

vector<double> InclusiveProcess::construct_n_D4_at_muf()
{
    vector<double> res;
    res.push_back(0.0);
    res.push_back(0.0);
    res.push_back(0.0);
    res.push_back(_eft.n_N3LO_D4()
                  + _eft.n_N3LO_D4_L()*_log_muf_mh_sq);
    return res;
}

vector<double> InclusiveProcess::construct_n_D5_at_muf()
{
    vector<double> res;
    res.push_back(0.0);
    res.push_back(0.0);
    res.push_back(0.0);
    res.push_back(_eft.n_N3LO_D5());
    return res;
}

// assumption: wc = a^2 * (w0+a * w1 + a^2 * W2 + a^3 * w3) 
// assumption: x = x0 + a * x1 + a^2 * x2 + a^3 * x3 
vector<double> InclusiveProcess::wc_x_n(const WilsonCoefficient& wc, const vector<double> x)
{
    vector<double> res;
    res.push_back(wc.w(0)*x[0]);
    res.push_back(wc.w(1)*x[0] + wc.w(0)*x[1]);
    res.push_back(wc.w(2)*x[0] + wc.w(1)*x[1] + wc.w(0)*x[2]);
    res.push_back(wc.w(3)*x[0] + wc.w(2)*x[1] + wc.w(1)*x[2] + wc.w(0)*x[3]);
    return res;
}

void SigmaTerm::wc_expansion(const WilsonCoefficient& wc)
{
    vector<double> res;
    res.push_back(wc.w(0)*_val[0]);
    res.push_back(wc.w(1)*_val[0] + wc.w(0)*_val[1]);
    res.push_back(wc.w(2)*_val[0] + wc.w(1)*_val[1] + wc.w(0)*_val[2]);
    res.push_back(wc.w(3)*_val[0] + wc.w(2)*_val[1] + wc.w(1)*_val[2] + wc.w(0)*_val[3]);
    
    _val = res;
}


/*
double InclusiveProcess::gg_delta_LO()
{
    return  _prefactor 
            * pow(_as_pi,2.) 
            * D_gg.result()
            * _wc.w(0) 
            * _eft.n_LO_delta()
                            ;
}

double InclusiveProcess::gg_delta_NLO()
{
    return _prefactor * pow(_as_pi,3.) * D_gg.result()
                        * (
                             _wc.w(0) * _eft.n_NLO_delta()
                           + _wc.w(1) * _eft.n_LO_delta()
                           );
}

double InclusiveProcess::gg_delta_NNLO(){
    return _prefactor * pow(_as_pi,4.) * D_gg.result()
                        * (
                             _wc.w(0) * (  
                                     _eft.n_NNLO_delta()
                                   + _eft.n_NNLO_delta_L()*_log_muf_mh_sq
                                   + _eft.n_NNLO_delta_L2()*pow(_log_muf_mh_sq,2.)
                                   )
                           + _wc.w(1) * _eft.n_NLO_delta()
                           + _wc.w(2) * _eft.n_LO_delta()
                           );
                           }

                       
                           
                           
double InclusiveProcess::gg_delta_N3LO(){ return _prefactor * pow(_as_pi,5.) * D_gg.result()
                        *(
                              _wc.w(0) * (
                                    _eft.n_N3LO_delta()
                                    + _eft.n_N3LO_delta_L()*_log_muf_mh_sq
                                    + _eft.n_N3LO_delta_L2()*pow(_log_muf_mh_sq,2.)
                                    + _eft.n_N3LO_delta_L3()*pow(_log_muf_mh_sq,3.)
                                    )
                          + _wc.w(1) * (  _eft.n_NNLO_delta()
                                  + _eft.n_NNLO_delta_L()*_log_muf_mh_sq
                                  + _eft.n_NNLO_delta_L2()*pow(_log_muf_mh_sq,2.)
                                  )
                            + _wc.w(2) * _eft.n_NLO_delta()
                            + _wc.w(3) * _eft.n_LO_delta()
                        );
                        }

double InclusiveProcess::gg_D0_NLO(){ return _prefactor * pow(_as_pi,3.) * P0_gg.result()
                        * _wc.w(0) * _eft.n_NLO_D0_L() * _log_muf_mh_sq ;
                        }
                        
double InclusiveProcess::gg_D1_NLO(){ return _prefactor * pow(_as_pi,3.) * P1_gg.result()
                        * _wc.w(0) * _eft.n_NLO_D1();
    }
    
double InclusiveProcess::gg_D0_NNLO(){ return  _prefactor * pow(_as_pi,4.) * P0_gg.result()
                        * (
                            _wc.w(0) * (  _eft.n_NNLO_D0()
                                  + _eft.n_NNLO_D0_L()*_log_muf_mh_sq
                                  + _eft.n_NNLO_D0_L2()*pow(_log_muf_mh_sq,2.)
                                  )
                           + _wc.w(1) * _eft.n_NLO_D0_L() * _log_muf_mh_sq
                           );
                           }
                           
double InclusiveProcess::gg_D1_NNLO(){ return _prefactor * pow(_as_pi,4.) * P1_gg.result()
                        * (
                           _wc.w(0) * (  _eft.n_NNLO_D1()
                                 + _eft.n_NNLO_D1_L()*_log_muf_mh_sq
                                 + _eft.n_NNLO_D1_L2()*pow(_log_muf_mh_sq,2.)
                                 )
                           + _wc.w(1) * _eft.n_NLO_D1()
                           );
                           }
                           
double InclusiveProcess::gg_D2_NNLO(){ return   _prefactor * pow(_as_pi,4.) * P2_gg.result()
                        *   _wc.w(0) * (  _eft.n_NNLO_D2()
                                 + _eft.n_NNLO_D2_L()*_log_muf_mh_sq
                                 )
                           ;
                           }
                           
double InclusiveProcess::gg_D3_NNLO(){return  _prefactor * pow(_as_pi,4.) * P3_gg.result()
                        *   _wc.w(0) * (  _eft.n_NNLO_D3()
                                  )
                            ;
    }
    
double InclusiveProcess::gg_D0_N3LO(){ return  _prefactor * pow(_as_pi,5.) * P0_gg.result()
                        *(
                        _wc.w(0) * (
                              _eft.n_N3LO_D0()
                              + _eft.n_N3LO_D0_L()*_log_muf_mh_sq
                              + _eft.n_N3LO_D0_L2()*pow(_log_muf_mh_sq,2.)
                              + _eft.n_N3LO_D0_L3()*pow(_log_muf_mh_sq,3.)
                              )
                          + _wc.w(1) * (  _eft.n_NNLO_D0()
                                  + _eft.n_NNLO_D0_L()*_log_muf_mh_sq
                                  + _eft.n_NNLO_D0_L2()*pow(_log_muf_mh_sq,2.)
                                  )
                          + _wc.w(2) * _eft.n_NLO_D0_L() * _log_muf_mh_sq
                        );
                        }
    
double InclusiveProcess::gg_D1_N3LO(){ return  _prefactor * pow(_as_pi,5.) * P1_gg.result()
                        *(
                            _wc.w(0) * (
                                  _eft.n_N3LO_D1()
                                  + _eft.n_N3LO_D1_L()*_log_muf_mh_sq
                                  + _eft.n_N3LO_D1_L2()*pow(_log_muf_mh_sq,2.)
                                  + _eft.n_N3LO_D1_L3()*pow(_log_muf_mh_sq,3.)
                                  )
                          + _wc.w(1) * (  _eft.n_NNLO_D1()
                                  + _eft.n_NNLO_D1_L()*_log_muf_mh_sq
                                  + _eft.n_NNLO_D1_L2()*pow(_log_muf_mh_sq,2.)
                                  )
                          + _wc.w(2) * _eft.n_NLO_D1()
                          );
                        }
                          
double InclusiveProcess::gg_D2_N3LO(){ return _prefactor * pow(_as_pi,5.) * P2_gg.result()
                        *(
                            _wc.w(0) * (
                                  _eft.n_N3LO_D2()
                                  + _eft.n_N3LO_D2_L()*_log_muf_mh_sq
                                  + _eft.n_N3LO_D2_L2()*pow(_log_muf_mh_sq,2.)
                                  + _eft.n_N3LO_D2_L3()*pow(_log_muf_mh_sq,3.)
                                  )
                          + _wc.w(1) * (  _eft.n_NNLO_D2()
                                  + _eft.n_NNLO_D2_L()*_log_muf_mh_sq
                                  )
                          );}
                          
double InclusiveProcess::gg_D3_N3LO(){ return _prefactor * pow(_as_pi,5.) * P3_gg.result()
                        *(
                          _wc.w(0) * (
                                _eft.n_N3LO_D3()
                                + _eft.n_N3LO_D3_L()*_log_muf_mh_sq
                                + _eft.n_N3LO_D3_L2()*pow(_log_muf_mh_sq,2.)
                                )
                          + _wc.w(1) * (  _eft.n_NNLO_D3()
                                  )
                          );}
                          
double InclusiveProcess::gg_D4_N3LO(){return  _prefactor * pow(_as_pi,5.) * P4_gg.result()
    *(
      _wc.w(0) * (
            _eft.n_N3LO_D4()
            + _eft.n_N3LO_D4_L()*_log_muf_mh_sq
            )
      );
    }
    
double InclusiveProcess::gg_D5_N3LO(){ return _prefactor * pow(_as_pi,5.) * P5_gg.result()
    *(
      _wc.w(0) * (
            _eft.n_N3LO_D5()
            )
      );
    }
    
*/


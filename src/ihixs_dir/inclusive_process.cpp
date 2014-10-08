#include "inclusive_process.h"




ostream& operator<<(ostream& stream, const InclusiveProcess& ip)
{
    cout<<left<<setw(22)<<" "<<setw(12)<<"LO"
    <<setw(12)<<"NLO"
    <<setw(12)<<"N2LO"
    <<setw(12)<<"N3LO"<<endl;
    for (int i=0;i<ip._sigma.size();i++)
    {
        stream<< *(ip._sigma[i]);
    }
    return stream;
}


InclusiveProcess::InclusiveProcess(const UserInterface& UI)
{
    
    _lumi = new NewLuminosity(UI);
    
    const double tau = pow(UI.m_higgs,2.)/pow(UI.Etot,2.);
    
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
                     new SigmaTerm("delta",HEFT::n_delta_at_muf(_log_muf_mh_sq),
                     new gg_delta));
    _sigma.push_back(
                     new SigmaTerm("D0",HEFT::n_D0_at_muf(_log_muf_mh_sq),
                     new gg_plus_0));
    _sigma.push_back(
                     new SigmaTerm("D1",HEFT::n_D1_at_muf(_log_muf_mh_sq),
                     new gg_plus_1));
    _sigma.push_back(
                     new SigmaTerm("D2",HEFT::n_D2_at_muf(_log_muf_mh_sq),
                     new gg_plus_2));
    _sigma.push_back(
                     new SigmaTerm("D3",HEFT::n_D3_at_muf(_log_muf_mh_sq),
                     new gg_plus_3));
    _sigma.push_back(
                     new SigmaTerm("D4",HEFT::n_D4_at_muf(_log_muf_mh_sq),
                     new gg_plus_4));
    _sigma.push_back(
                     new SigmaTerm("D5",HEFT::n_D5_at_muf(_log_muf_mh_sq),
                     new gg_plus_5));
    
    
    double nlo_ind[] = {0.0,1.0,0.0,0.0};
    vector<double> nlo_inds(nlo_ind,nlo_ind+sizeof(nlo_ind) / sizeof(double));

    _sigma.push_back(
                     new SigmaTerm("NLO Real const",
                                   nlo_inds,
                                   new gg_real(&HEFT::nlo_r_lz0,_log_muf_mh_sq)));
    _sigma.push_back(
                     new SigmaTerm("NLO Real log(1-z)",
                                   nlo_inds,
                                   new gg_real(&HEFT::nlo_r_lz1,_log_muf_mh_sq)));
    
    double nnlo_ind[] = {0.0,0.0,1.0,0.0};
    vector<double> nnlo_inds(nnlo_ind,nnlo_ind+sizeof(nnlo_ind) / sizeof(double));
    
    _sigma.push_back(
                     new SigmaTerm("NNLO Real const",
                                   nnlo_inds,
                                   new gg_real(&HEFT::nnlo_r_lz0_const,_log_muf_mh_sq)));
    _sigma.push_back(
                     new SigmaTerm("NNLO Real log(z)",
                                   nnlo_inds,
                                   new gg_real(&HEFT::nnlo_r_lz0_logz,_log_muf_mh_sq)));
    _sigma.push_back(
                     new SigmaTerm("NNLO Real log(z)^2",
                                   nnlo_inds,
                                   new gg_real(&HEFT::nnlo_r_lz0_logz_sq,_log_muf_mh_sq)));
    _sigma.push_back(
                     new SigmaTerm("NNLO Real log(z)^3",
                                   nnlo_inds,
                                   new gg_real(&HEFT::nnlo_r_lz0_logz_cube,_log_muf_mh_sq)));
    _sigma.push_back(
                     new SigmaTerm("NNLO Real log(1-z)",
                                   nnlo_inds,
                                   new gg_real(&HEFT::nnlo_r_lz1,_log_muf_mh_sq)));
    _sigma.push_back(
                     new SigmaTerm("NNLO Real log(1-z)^2",
                                   nnlo_inds,
                                   new gg_real(&HEFT::nnlo_r_lz2,_log_muf_mh_sq)));
    _sigma.push_back(
                     new SigmaTerm("NNLO Real log(1-z)^3",
                                   nnlo_inds,
                                   new gg_real(&HEFT::nnlo_r_lz3,_log_muf_mh_sq)));
    
    for (int i=0;i<_sigma.size();i++)
    {
        _sigma[i]->ConfigureLumi(new NewLuminosity(UI),tau);
    }
    

   
}





void InclusiveProcess::Evaluate()
{
    for (int i=0;i<_sigma.size();i++)
    {
        cout<<"--> computing "<< _sigma[i]->type()<<endl;
        _sigma[i]->CallVegas();
        _sigma[i]->multiply(_prefactor);
        _sigma[i]->wc_expansion(_wc);
        _sigma[i]->multiply_by_as_pi(_as_pi);
        _sigma[i]->multiply(1.065);
    }
}    

double InclusiveProcess::CoefficientAlphaS(int as_order)
{
    double res=0.0;
    for (int i=0;i<_sigma.size();i++)
    {
        res += _sigma[i]->operator[](as_order-2);// note that as_order starts at 2
    }
    return res;
}

double InclusiveProcess::sigma(const string& type_id,int i)
{
    for (int k=0;k<_sigma.size();k++)
    {
        if (type_id==_sigma[k]->type())
        {
            return (*_sigma[k])[i];
        }
    }
    cout<<"\nThe term "<<type_id<<" you asked for was not found!"<<endl;
    exit(0);
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






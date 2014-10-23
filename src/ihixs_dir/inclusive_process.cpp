#include "inclusive_process.h"




ostream& operator<<(ostream& stream, const InclusiveProcess& ip)
{
    string pordername[4]={"LO","NLO","NNLO","N3LO"};
    stream<<left<<setw(26)<<" "
    <<setw(24)<<"LO"
    <<setw(24)<<"NLO"
    <<setw(24)<<"N2LO"
    <<setw(24)<<"N3LO"<<endl;
    for (int i=0;i<ip._sigma.size();i++)
    {
        stream<< *(ip._sigma[i]);
    }
    
    if (ip._scale_variation)
    {
        stream<<"\n"<<setw(24)<<"Scale Variation per term";
        for (int i=0;i<ip._mur_vector.size();i++)
        {
            stream<<setw(19)<<setprecision(2)<<right
                <<ip._mur_vector[i]<<setprecision(16)<<left;
        }
        
        stream<<endl;
        for (int i=0;i<ip._sigma.size();i++)
        {
            
            for (int porder=0;porder<4;porder++)
            {
                ostringstream name;
                name<<ip._sigma[i]->type()<<":"<<pordername[porder];
                if (not(ip._sigma[i]->IsZero(porder+2)))
                {
                    stream<<setw(25)<<name.str();
                    for (int imu=0;imu<ip._mur_vector.size();imu++)
                    {
                        
                        stream<< ip._sigma[i]->print_scale_result(ip._mur_vector[imu],porder+2);
                    }
                    stream<<endl;
                }
                
            }
            
        }
        stream<<"total xs per order"<<endl;
        for (int porder=0;porder<4;porder++)
        {
            ostringstream name;
            name<<pordername[porder];
            stream<<setw(25)<<name.str();
            for (int imu=0;imu<ip._mur_vector.size();imu++)
            {
                ResultPair res(0.0,0.0);
                for (int i=0;i<ip._sigma.size();i++)
                {
                    res = res + ip._sigma[i]->give(porder+2,ip._mur_vector[imu]);
                }
                stream<< res;
            }
            stream<<endl;
        }
        
        stream<<"total xs"<<endl;
        ostringstream name;
        name<<"sigma(mur)";
        stream<<setw(25)<<name.str();
        for (int imu=0;imu<ip._mur_vector.size();imu++)
        {
            ResultPair res(0.0,0.0);
            for (int porder=0;porder<4;porder++)
            {
                for (int i=0;i<ip._sigma.size();i++)
                {
                    res = res + ip._sigma[i]->give(porder+2,ip._mur_vector[imu]);
                }
                
            }
            stream<< res;
        }
        stream<<endl;
    }
    
    return stream;
}


InclusiveProcess::InclusiveProcess(const UserInterface& UI)
{
    _UI=UI;
    _scale_variation=true;
    _is_central_scale = true;
    _int_qcd_perturbative_order = 0;
    if (UI.qcd_perturbative_order=="LO") _int_qcd_perturbative_order=2;
    if (UI.qcd_perturbative_order=="NLO") _int_qcd_perturbative_order=3;
    if (UI.qcd_perturbative_order=="NNLO") _int_qcd_perturbative_order=4;
    if (UI.qcd_perturbative_order=="N3LO") _int_qcd_perturbative_order=5;

    if (UI.matrix_element_approximation != "pure_eft")
    {
        cout<<"\n we can't do "<<UI.matrix_element_approximation<<" yet";
        cout<<endl;
        exit(0);
    }
    _lumi = new NewLuminosity(UI);
    
    const double tau = pow(UI.m_higgs,2.)/pow(UI.Etot,2.);
    
    //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
    //: Gf = 1.16637*10^{-5} * 0.389379*10^9
    _prefactor = 35.0309;
    // computing log(muf^2/mh^2)
    _log_muf_mh_sq = 2. * log(UI.muf/_model.higgs.m());
    // setting up a list of all the mur scales we will compute
    int number_of_murs = 5;// including the central scale
    for (int i=0;i<number_of_murs;i++)
    {
        double new_mur = exp(log(UI.mur/2.)+i*2.*log(2.)/(number_of_murs-1));
        _mur_vector.push_back(new_mur);
    }
    // setting up the model and as_pi for the central scale
    _current_mur = _UI.mur;
    SetMurDependentParameters(_current_mur);
    //: constructing the delta and plus terms
    AsSeries Delta = HEFT::n_delta_at_muf(_log_muf_mh_sq);
    AsSeries D0 = HEFT::n_D0_at_muf(_log_muf_mh_sq);
    AsSeries D1 = HEFT::n_D1_at_muf(_log_muf_mh_sq);
    AsSeries D2 = HEFT::n_D2_at_muf(_log_muf_mh_sq);
    AsSeries D3 = HEFT::n_D3_at_muf(_log_muf_mh_sq);
    AsSeries D4 = HEFT::n_D4_at_muf(_log_muf_mh_sq);
    AsSeries D5 = HEFT::n_D5_at_muf(_log_muf_mh_sq);
    
    if (UI.qcd_perturbative_order=="LO")
    {
        Delta.Truncate(0);
        _sigma.push_back(new SigmaTerm("delta",Delta,new gg_delta));
    }
    if (UI.qcd_perturbative_order=="NLO")
    {
        Delta.Truncate(1);
        D0.Truncate(1);
        D1.Truncate(1);
        _sigma.push_back(new SigmaTerm("delta",Delta,new gg_delta));
        _sigma.push_back(new SigmaTerm("D0",D0,new gg_plus_0));
        _sigma.push_back(new SigmaTerm("D1",D1,new gg_plus_1));
        _sigma.push_back(new SigmaTerm("NLO Real const",AsSeries(1,1.0),
                                       new gg_real(&HEFT::nlo_r_lz0,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NLO Real log(1-z)",AsSeries(1,1.0),
                                       new gg_real(&HEFT::nlo_r_lz1,_log_muf_mh_sq)));
    }
    if (UI.qcd_perturbative_order=="NNLO")
    {
        Delta.Truncate(2);
        D0.Truncate(2);
        D1.Truncate(2);
        D2.Truncate(2);
        D3.Truncate(2);
        
        _sigma.push_back(new SigmaTerm("delta",Delta,new gg_delta));
        _sigma.push_back(new SigmaTerm("D0",D0,new gg_plus_0));
        _sigma.push_back(new SigmaTerm("D1",D1,new gg_plus_1));
        _sigma.push_back(new SigmaTerm("D2",D2,new gg_plus_2));
        _sigma.push_back(new SigmaTerm("D3",D3,new gg_plus_3));
        _sigma.push_back(new SigmaTerm("NLO Real const",AsSeries(1,1.0),
                                       new gg_real(&HEFT::nlo_r_lz0,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NLO Real log(1-z)",AsSeries(1,1.0),
                                       new gg_real(&HEFT::nlo_r_lz1,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real const",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz0_const,_log_muf_mh_sq)));
        
        _sigma.push_back(new SigmaTerm("NNLO Real log(z)",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz0_logz,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(z)^2",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz0_logz_sq,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(z)^3",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz0_logz_cube,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(1-z)",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz1,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(1-z)^2",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz2,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(1-z)^3",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz3,_log_muf_mh_sq)));
    }
    if (UI.qcd_perturbative_order=="N3LO")
    {
        Delta.Truncate(3);
        D0.Truncate(3);
        D1.Truncate(3);
        D2.Truncate(3);
        D3.Truncate(3);
        D4.Truncate(3);
        D5.Truncate(3);
        
        _sigma.push_back(new SigmaTerm("delta",Delta,new gg_delta));
        _sigma.push_back(new SigmaTerm("D0",D0,new gg_plus_0));
        _sigma.push_back(new SigmaTerm("D1",D1,new gg_plus_1));
        _sigma.push_back(new SigmaTerm("D2",D2,new gg_plus_2));
        _sigma.push_back(new SigmaTerm("D3",D3,new gg_plus_3));
        _sigma.push_back(new SigmaTerm("D4",D4,new gg_plus_4));
        _sigma.push_back(new SigmaTerm("D5",D5,new gg_plus_5));
        _sigma.push_back(new SigmaTerm("NLO Real const",AsSeries(1,1.0),
                                       new gg_real(&HEFT::nlo_r_lz0,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NLO Real log(1-z)",AsSeries(1,1.0),
                                       new gg_real(&HEFT::nlo_r_lz1,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real const",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz0_const,_log_muf_mh_sq)));
        
        _sigma.push_back(new SigmaTerm("NNLO Real log(z)",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz0_logz,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(z)^2",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz0_logz_sq,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(z)^3",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz0_logz_cube,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(1-z)",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz1,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(1-z)^2",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz2,_log_muf_mh_sq)));
        _sigma.push_back(new SigmaTerm("NNLO Real log(1-z)^3",AsSeries(2,1.0),
                                       new gg_real(&HEFT::nnlo_r_lz3,_log_muf_mh_sq)));
        
    }
    
    for (int i=0;i<_sigma.size();i++)
    {
        _sigma[i]->ConfigureLumi(_lumi,tau,UI);
    }
}



void InclusiveProcess::Evaluate()
{
    for (int i=0;i<_sigma.size();i++)
    {
        Evaluate(_sigma[i]);
    }
    if (_scale_variation)
    {
        for (int i=0;i<_mur_vector.size();i++)
        {
            _current_mur=_mur_vector[i];
            _is_central_scale = false;
            
            SetMurDependentParameters(_current_mur);
            for (int j=0;j<_sigma.size();j++)
            {
                _sigma[j]->RewindToPostVegas();
                Evaluate(_sigma[j]);
            }
        }
    }
    
}

void InclusiveProcess::Evaluate(const string& type)
{
    SigmaTerm* term=find_term(type);
    Evaluate(term);
}

void InclusiveProcess::Evaluate(SigmaTerm* term)
{
    term->CallVegas();
    term->multiply(_prefactor);
    term->wc_expansion(_wc);
    term->evolve_from_muf_to_mur(_log_mur_over_muf_sq);
    term->multiply_by_as_pi(_as_pi);
    if (_is_central_scale) term->SaveCentral(_current_mur);
    else term->Save(_current_mur);
    term->Truncate(_int_qcd_perturbative_order);
}

void InclusiveProcess::SetMurDependentParameters(const double& mur)
{
    _model.Configure(
                     _lumi->alpha_s_at_mz(),
                     mur/_UI.m_higgs,
                     _UI.perturbative_order,
                     _UI.m_higgs
                     );
    // Setting up alpha_s
    _as_pi = _model.alpha_strong()/consts::Pi;
    cout << "\n[CrossSection]: a_s = " << _as_pi * consts::Pi<<endl;
    _log_muf_over_mt_sq = 2.*log(_UI.muf/_model.top.m());
    
    _log_mur_over_muf_sq = 2. *  log(mur/_UI.muf);
    // setting up wilson coeffs
    _wc.Configure(_log_muf_over_mt_sq);
}

double InclusiveProcess::CoefficientAlphaS(int as_order)
{
    ResultPair res(0.0,0.0);
    for (int i=0;i<_sigma.size();i++)
    {
        if (_sigma[i]->Evaluated())
            res = res + _sigma[i]->ResultCentral().term_of_order(as_order);// note that as_order starts at 2
    }
    return res.val();
}

double InclusiveProcess::CoefficientAlphaSError(int as_order)
{
    ResultPair res(0.0,0.0);
    for (int i=0;i<_sigma.size();i++)
    {
        if (_sigma[i]->Evaluated())
            res = res + _sigma[i]->ResultCentral().term_of_order(as_order);// note that as_order starts at 2
    }
    return res.err();
}

ResultPair InclusiveProcess::TotalCentral()
{
    ResultPair res(0.0,0.0);
    for (int i=0;i<_sigma.size();i++)
    {
        if (_sigma[i]->Evaluated())
        {
            for (int as_order=0;as_order<4;as_order++)
                res = res + _sigma[i]->ResultCentral().term_of_order(as_order+2);
        }
    }
    return res;
}



SigmaTerm* InclusiveProcess::find_term(const string& type_id)
{
    for (int k=0;k<_sigma.size();k++)
    {
        if (type_id==_sigma[k]->type())
        {
            return _sigma[k];
        }
    }
    cout<<"\nThe term "<<type_id<<" you asked for was not found!"<<endl;
    exit(0);
}






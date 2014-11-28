#include "inclusive_process.h"
#include "gluon_fusion_exact_coefficients.h"


ostream& operator<<(ostream& stream, const Channel& ch)
{
    for (int i=0;i<ch.size();i++)
        stream<<*(ch.Term(i));
    return stream;
}


ostream& operator<<(ostream& stream, const InclusiveProcess& ip)
{
   string pordername[4]={"LO","NLO","NNLO","N3LO"};
//    stream<<left<<setw(26)<<" "
//    <<setw(24)<<"LO"
//    <<setw(24)<<"NLO"
//    <<setw(24)<<"N2LO"
//    <<setw(24)<<"N3LO"<<endl;
    for (int i=0;i<ip._channels.size();i++)
    {
        stream<< *(ip._channels[i]);
    }
    /*
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
    }*/
    
    return stream;
}

void Channel::Truncate(int a_order)
{
    //cout<<"\nTruncating at a^"<<a_order<<endl;
    vector<SigmaTerm*> new_terms;
    for (int i=0;i<_terms.size();i++)
    {
        SigmaTerm* curterm = _terms[i];
        //cout<<"\nbefore "<<*curterm;
        curterm->Truncate(a_order);
        //cout<<"\nafter "<<*curterm;
        if (not(curterm->IsZero()))
        {
            new_terms.push_back(curterm);
        }
        //else cout<<" <- didn't make it"<<endl;
    }
    _terms = new_terms;
}

AsSeries Channel::Sum()
{
    AsSeries sum(2,0.0);
    for (int i=0;i<_terms.size();i++)
        sum=sum+_terms[i]->ResultCentral();
    return sum;
}

ResultPair Channel::Result()
{
    return Sum().AddUp();
}

HiggsGGFChannelGG::HiggsGGFChannelGG(const double& log_muf_mh_sq)
{
    _name = "gg";
    // g g channel
    AsSeries Delta = HEFT::n_delta_at_muf(log_muf_mh_sq);
    AsSeries D0 = HEFT::n_D0_at_muf(log_muf_mh_sq);
    AsSeries D1 = HEFT::n_D1_at_muf(log_muf_mh_sq);
    AsSeries D2 = HEFT::n_D2_at_muf(log_muf_mh_sq);
    AsSeries D3 = HEFT::n_D3_at_muf(log_muf_mh_sq);
    AsSeries D4 = HEFT::n_D4_at_muf(log_muf_mh_sq);
    AsSeries D5 = HEFT::n_D5_at_muf(log_muf_mh_sq);
    _terms.push_back(new SigmaTerm("delta",Delta,new gg_delta));
    _terms.push_back(new SigmaTerm("D0",D0,new gg_plus_0));
    _terms.push_back(new SigmaTerm("D1",D1,new gg_plus_1));
    _terms.push_back(new SigmaTerm("D2",D2,new gg_plus_2));
    _terms.push_back(new SigmaTerm("D3",D3,new gg_plus_3));
    _terms.push_back(new SigmaTerm("D4",D4,new gg_plus_4));
    _terms.push_back(new SigmaTerm("D5",D5,new gg_plus_5));
    _terms.push_back(new SigmaTerm("NLO Real const",AsSeries(1,1.0),
                                   new gg_real(&HEFT::nlo_r_lz0,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("NLO Real log(1-z)",AsSeries(1,1.0),
                                   new gg_real(&HEFT::nlo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("NNLO Real const",AsSeries(2,1.0),
                                   new gg_real(&HEFT::nnlo_r_lz0_const,log_muf_mh_sq)));
    
    _terms.push_back(new SigmaTerm("NNLO Real log(z)",AsSeries(2,1.0),
                                   new gg_real(&HEFT::nnlo_r_lz0_logz,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("NNLO Real log(z)^2",AsSeries(2,1.0),
                                   new gg_real(&HEFT::nnlo_r_lz0_logz_sq,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("NNLO Real log(z)^3",AsSeries(2,1.0),
                                   new gg_real(&HEFT::nnlo_r_lz0_logz_cube,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("NNLO Real log(1-z)",AsSeries(2,1.0),
                                   new gg_real(&HEFT::nnlo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("NNLO Real log(1-z)^2",AsSeries(2,1.0),
                                   new gg_real(&HEFT::nnlo_r_lz2,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("NNLO Real log(1-z)^3",AsSeries(2,1.0),
                                   new gg_real(&HEFT::nnlo_r_lz3,log_muf_mh_sq)));
    
}

HiggsGGFChannelQG::HiggsGGFChannelQG(const double& log_muf_mh_sq)
{
    _name = "qg";
    // q g channel
    _terms.push_back(new SigmaTerm("qg NLO Real const",AsSeries(1,1.0),
                                   new qg_real(&HEFT::qg_nlo_r_lz0,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg NLO Real log(1-z)",AsSeries(1,1.0),
                                   new qg_real(&HEFT::qg_nlo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg NNLO Real const",AsSeries(2,1.0),
                                   new qg_real(&HEFT::qg_nnlo_r_lz0_const,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg NNLO Real log(z)",AsSeries(2,1.0),
                                   new qg_real(&HEFT::qg_nnlo_r_lz0_logz,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg NNLO Real log(z)^2",AsSeries(2,1.0),
                                   new qg_real(&HEFT::qg_nnlo_r_lz0_logz_sq,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg NNLO Real log(z)^3",AsSeries(2,1.0),
                                   new qg_real(&HEFT::qg_nnlo_r_lz0_logz_cube,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg NNLO Real log(1-z)",AsSeries(2,1.0),
                                   new qg_real(&HEFT::qg_nnlo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg NNLO Real log(1-z)^2",AsSeries(2,1.0),
                                   new qg_real(&HEFT::qg_nnlo_r_lz2,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg NNLO Real log(1-z)^3",AsSeries(2,1.0),
                                   new qg_real(&HEFT::qg_nnlo_r_lz3,log_muf_mh_sq)));
    
    _terms.push_back(new SigmaTerm("qg N3LO Real const",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz0,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^2",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz2,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^3",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz3,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^4",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz4,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^5",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz5,log_muf_mh_sq)));
    
}

HiggsGGFChannelQQBAR::HiggsGGFChannelQQBAR(const double& log_muf_mh_sq)
{
    _name = "q qbar";

    // q qbar channel
    _terms.push_back(new SigmaTerm("qqb NLO Real const",AsSeries(1,1.0),
                                   new qqb_real(&HEFT::qqb_nlo_r_lz0,log_muf_mh_sq)));
    
    _terms.push_back(new SigmaTerm("qqb NNLO Real const",AsSeries(2,1.0),
                                   new qqb_real(&HEFT::qqb_nnlo_r_lz0_const,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qqb NNLO Real log(z)",AsSeries(2,1.0),
                                   new qqb_real(&HEFT::qqb_nnlo_r_lz0_logz,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qqb NNLO Real log(z)^2",AsSeries(2,1.0),
                                   new qqb_real(&HEFT::qqb_nnlo_r_lz0_logz_sq,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qqb NNLO Real log(z)^3",AsSeries(2,1.0),
                                   new qqb_real(&HEFT::qqb_nnlo_r_lz0_logz_cube,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qqb NNLO Real log(1-z)",AsSeries(2,1.0),
                                   new qqb_real(&HEFT::qqb_nnlo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qqb NNLO Real log(1-z)^2",AsSeries(2,1.0),
                                   new qqb_real(&HEFT::qqb_nnlo_r_lz2,log_muf_mh_sq)));
    
}

HiggsGGFChannelQQ::HiggsGGFChannelQQ(const double& log_muf_mh_sq)
{
    _name = "qq";
    // q q channel
    _terms.push_back(new SigmaTerm("qq NNLO Real const",AsSeries(2,1.0),
                                   new qq_real(&HEFT::qq_nnlo_r_lz0_const,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qq NNLO Real log(z)",AsSeries(2,1.0),
                                   new qq_real(&HEFT::qq_nnlo_r_lz0_logz,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qq NNLO Real log(z)^2",AsSeries(2,1.0),
                                   new qq_real(&HEFT::qq_nnlo_r_lz0_logz_sq,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qq NNLO Real log(z)^3",AsSeries(2,1.0),
                                   new qq_real(&HEFT::qq_nnlo_r_lz0_logz_cube,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qq NNLO Real log(1-z)",AsSeries(2,1.0),
                                   new qq_real(&HEFT::qq_nnlo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qq NNLO Real log(1-z)^2",AsSeries(2,1.0),
                                   new qq_real(&HEFT::qq_nnlo_r_lz2,log_muf_mh_sq)));
    
}

HiggsGGFChannelQ1Q2::HiggsGGFChannelQ1Q2(const double& log_muf_mh_sq)
{
    _name = "q1q2";
    // q1 q2 channel
    _terms.push_back(new SigmaTerm("q1q2 NNLO Real const",AsSeries(2,1.0),
                                   new q1q2_real(&HEFT::q1q2_nnlo_r_lz0_const,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("q1q2 NNLO Real log(z)",AsSeries(2,1.0),
                                   new q1q2_real(&HEFT::q1q2_nnlo_r_lz0_logz,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("q1q2 NNLO Real log(z)^2",AsSeries(2,1.0),
                                   new q1q2_real(&HEFT::q1q2_nnlo_r_lz0_logz_sq,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("q1q2 NNLO Real log(z)^3",AsSeries(2,1.0),
                                   new q1q2_real(&HEFT::q1q2_nnlo_r_lz0_logz_cube,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("q1q2 NNLO Real log(1-z)",AsSeries(2,1.0),
                                   new q1q2_real(&HEFT::q1q2_nnlo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("q1q2 NNLO Real log(1-z)^2",AsSeries(2,1.0),
                                   new q1q2_real(&HEFT::q1q2_nnlo_r_lz2,log_muf_mh_sq)));
    
}

HiggsGGFChannelGGExactNLOReal::HiggsGGFChannelGGExactNLOReal(const double& log_muf_mh_sq)
{
    _name = "gg exact NLO real";
    // g g exact NLO real
    _terms.push_back(new SigmaTerm("gg exact NLO real",AsSeries(1,1.0),
                                   new qg_real(&HEFT::qg_nlo_r_lz0,log_muf_mh_sq)));
    
}



InclusiveProcess::InclusiveProcess(const UserInterface& UI)
{
    _UI=UI;
    _scale_variation=false;
    _is_central_scale = true;
    _is_enhanced_eft = false;
    _int_qcd_perturbative_order = 0;
    if (UI.qcd_perturbative_order=="LO") _int_qcd_perturbative_order=2;
    if (UI.qcd_perturbative_order=="NLO") _int_qcd_perturbative_order=3;
    if (UI.qcd_perturbative_order=="NNLO") _int_qcd_perturbative_order=4;
    if (UI.qcd_perturbative_order=="N3LO") _int_qcd_perturbative_order=5;

    if (UI.matrix_element_approximation != "pure_eft"
        and UI.matrix_element_approximation != "enhanced_eft")
    {
        cout<<"\n we can't do "<<UI.matrix_element_approximation<<" yet";
        cout<<endl;
        exit(0);
    }
    _lumi = new NewLuminosity(UI);
    
    _tau = pow(UI.m_higgs,2.)/pow(UI.Etot,2.);
    
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
    //: constructing the channels
    _channels.push_back(new HiggsGGFChannelGG(_log_muf_mh_sq));
    _channels.push_back(new HiggsGGFChannelQG(_log_muf_mh_sq));
    _channels.push_back(new HiggsGGFChannelQQBAR(_log_muf_mh_sq));
    _channels.push_back(new HiggsGGFChannelQQ(_log_muf_mh_sq));
    _channels.push_back(new HiggsGGFChannelQ1Q2(_log_muf_mh_sq));
    //: truncating the channels to the required order in a^s
    //: note that the channel terms start at a^0 at this stage
    //: since a^2 is in the Wilson Coefficient which is not yet
    //: multiplied through
    for (int i=0;i<_channels.size();i++)
    {
        _channels[i]->Truncate(_int_qcd_perturbative_order-2);
        //for (int j=0;j<_channels[i]->size();j++) cout<< *(_channels[i]->Term(j));
    }
    
    if (UI.matrix_element_approximation == "enhanced_eft")
    {
        //: the bottom is removed below for comparisons within the HXSWG
        _model.RemoveParticle("bottom");
        // we compute the LO exact matrix element
        GluonFusionExactCoefficients exactLO(_model);
        _exact_LO_coefficient = exactLO.LO_epsilon(0);
        _is_enhanced_eft = true;
    }
    
    if (UI.matrix_element_approximation == "exact")
    {
        //: the bottom is removed below for comparisons within the HXSWG
        _model.RemoveParticle("bottom");
        // we need to compute:
        //the LO exact matrix element in gg channel
        GluonFusionExactCoefficients exact(_model);
        _exact_LO_coefficient = exact.LO_epsilon(0);
        // the NLO delta exact matrix element in gg channel
//        _exact_NLO_delta_gg = exact.NLO_epsilon(0);
        // we need to add three extra channels
        // the exact real gluon gluon
//        _extra_channels.push_back(new HiggsGGFChannelGGExactNLOReal(_log_muf_mh_sq));
        // the exact real quark gluon
//        _extra_channels.push_back(new HiggsGGFChannelGQExactNLOReal(_log_muf_mh_sq));
        // the exact real quark antiquark
//        _extra_channels.push_back(new HiggsGGFChannelQQBARExactNLOReal(_log_muf_mh_sq));
        // we set the _is_exact flag to true
//        _is_exact = true;
    }
}



void InclusiveProcess::Evaluate()
{
    for (int i=0;i<_channels.size();i++)
    {
        for (int j=0;j<_channels[i]->size();j++)
            Evaluate(_channels[i]->Term(j));
    }
    if (_scale_variation)
    {
        for (int i=0;i<_mur_vector.size();i++)
        {
            _current_mur=_mur_vector[i];
            _is_central_scale = false;
            
            SetMurDependentParameters(_current_mur);
            for (int j=0;j<_channels.size();j++)
            {
                for (int k=0;j<_channels[j]->size();k++)
                {
                    SigmaTerm* curterm = _channels[j]->Term(k);
                    curterm->RewindToPostVegas();
                    Evaluate(curterm);
                }
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
    term->ConfigureLumi(_lumi,_tau,_UI);
    term->CallVegas();
    term->multiply(_prefactor);
    term->wc_expansion(_wc);
    term->evolve_from_muf_to_mur(_log_mur_over_muf_sq);
    term->multiply_by_as_pi(_as_pi);
    term->Truncate(_int_qcd_perturbative_order);
    if (_is_enhanced_eft)
    {
        term->multiply(_exact_LO_coefficient);
    }
    
    if (_is_central_scale) term->SaveCentral(_current_mur);
    else term->Save(_current_mur);
    
    

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

ResultPair InclusiveProcess::CoefficientAlphaSResult(int as_order)
{
    ResultPair res(0.0,0.0);
    for (int i=0;i<_channels.size();i++) res = res + _channels[i]->CoeffAs(as_order);
    return res;
}

ResultPair InclusiveProcess::TotalCentral()
{
    ResultPair res(0.0,0.0);
    for (int i=0;i<_channels.size();i++) res = res + _channels[i]->Result();
    return res;
}

double InclusiveProcess::CoefficientAlphaS(int as_order)
{
    return CoefficientAlphaSResult(as_order).val();
}

double InclusiveProcess::CoefficientAlphaSError(int as_order)
{
    return CoefficientAlphaSResult(as_order).err();
}

string InclusiveProcess::ChannelBreakdown()
{
    stringstream st;
    for (int i=0;i<_channels.size();i++)
    {
        st<<setw(6)<<_channels[i]->Name()<<":"<<_channels[i]->Sum()<<endl;
    }
    return st.str();
}


SigmaTerm* InclusiveProcess::find_term(const string& type_id)
{
    for (int k=0;k<_channels.size();k++)
    {
        for (int j=0;j<_channels[k]->size();j++)
        {
            if (type_id==_channels[k]->Term(j)->type())
            {
                return _channels[k]->Term(j);
            }
            else
            {
            //cout<<"\n current term is "<<_sigma[k]->type()<<" and doesn't match"<<endl;
            }
        }
    }
    cout<<"\nThe term "<<type_id<<" you asked for was not found!"<<endl;
    exit(0);
}






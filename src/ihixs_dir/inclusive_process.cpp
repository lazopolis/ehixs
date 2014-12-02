#include "inclusive_process.h"
#include "gluon_fusion_exact_coefficients.h"





ostream& operator<<(ostream& stream, const InclusiveProcess& ip)
{
   string pordername[4]={"LO","NLO","NNLO","N3LO"};

    for (int i=0;i<ip._channels.size();i++)
    {
        stream<< *(ip._channels[i]);
    }
    
    return stream;
}



InclusiveProcess::InclusiveProcess(const UserInterface& UI)
{
    _UI=UI;
    _scale_variation=true;
    _is_central_scale = true;
    _is_enhanced_eft = false;
    _is_exact = false;
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
    int number_of_murs = 9;// including the central scale
    double mur_ratio[9]={1./16,1./8.,1./4.,1./3.,1./2.,3./4.,1.,3./2.,2.};
    for (int i=0;i<number_of_murs;i++)
    {
        _mur_vector.push_back(UI.mur*mur_ratio[i]);
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
            cout<<"\n New Scale :"<<_current_mur<<endl;
            _is_central_scale = false;
            
            SetMurDependentParameters(_current_mur);
            for (int j=0;j<_channels.size();j++)
            {
                for (int k=0;k<_channels[j]->size();k++)
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
        st<<setw(6)<<left<<_channels[i]->Name()<<":"
            <<_channels[i]->Result()<<" = "
            <<_channels[i]->Sum()
            <<endl;
    }
    return st.str();
}

string InclusiveProcess::ScaleVariation()
{
    stringstream st;
    int result_characters=19;
    if (_scale_variation)
    {
        st<<setw(16)<<"Scale Variation:";
        for (int k=0;k<_mur_vector.size();k++)
            st<<setw(result_characters)<<_mur_vector[k];
        st<<endl;
        for (int i=0;i<_channels.size();i++)
        {
            st<<setw(16)<<left<<_channels[i]->Name()<<":";
            for (int k=0;k<_mur_vector.size();k++)
               st<<setw(result_characters)<<_channels[i]->Result(_mur_vector[k]);
            st<<endl;
        }
        st<<setw(16)<<" total"<<":";
        for (int k=0;k<_mur_vector.size();k++)
        {
            ResultPair res(0.0,0.0);
            for (int i=0;i<_channels.size();i++)
                res = res+ _channels[i]->Result(_mur_vector[k]);
            st<<setw(result_characters)<<res;
        }
        st<<endl;
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






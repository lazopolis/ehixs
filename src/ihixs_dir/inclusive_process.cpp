#include "inclusive_process.h"
#include "gluon_fusion_exact_coefficients.h"
#include "nlo_exact_matrix_elements.h"




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
    //_is_central_scale = true;
    //_is_enhanced_eft = false;
    //_is_exact = false;
    _int_qcd_perturbative_order = 0;
    
    cout<<endl<<"[ehixs] scales mur="<<UI.mur<<" muf="<<UI.muf<<endl;
    
    if (UI.qcd_perturbative_order=="LO")  _int_qcd_perturbative_order=2;
    if (UI.qcd_perturbative_order=="NLO") _int_qcd_perturbative_order=3;
    if (UI.qcd_perturbative_order=="NNLO") _int_qcd_perturbative_order=4;
    if (UI.qcd_perturbative_order=="N3LO") _int_qcd_perturbative_order=5;

    /*if (UI.matrix_element_approximation != "pure_eft"
        and UI.matrix_element_approximation != "enhanced_eft")
    {
        cout<<"\n we can't do "<<UI.matrix_element_approximation<<" yet";
        cout<<endl;
        exit(0);
    }
     */
    _lumi = new Luminosity(UI.pdf_set);
    
    _tau = pow(UI.m_higgs,2.)/pow(UI.Etot,2.);
    
    //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
    //: Gf = 1.16637*10^{-5} * 0.389379*10^9
    _prefactor = 35.0309;
    // computing log(muf^2/mh^2)
    _log_muf_mh_sq = 2. * log(UI.muf/_model.higgs.m());
    // setting up the model and as_pi for the central scale
    _current_mur = _UI.mur;
    SetMurDependentParameters(_current_mur);
    //: constructing the channels
    _channels.push_back(new HiggsGGFChannelGG(_log_muf_mh_sq));
    _channels.push_back(new HiggsGGFChannelQG(_log_muf_mh_sq));
    _channels.push_back(new HiggsGGFChannelQQBAR(_log_muf_mh_sq));
    _channels.push_back(new HiggsGGFChannelQQ(_log_muf_mh_sq));
    _channels.push_back(new HiggsGGFChannelQ1Q2(_log_muf_mh_sq));
    
    // constructing terms
    // gg channel
    AsSeries Delta = HEFT::n_delta_at_mh();
    AsSeries D0 = HEFT::n_D0_at_mh();
    AsSeries D1 = HEFT::n_D1_at_mh();
    AsSeries D2 = HEFT::n_D2_at_mh();
    AsSeries D3 = HEFT::n_D3_at_mh();
    AsSeries D4 = HEFT::n_D4_at_mh();
    AsSeries D5 = HEFT::n_D5_at_mh();
    AsSeries a_s(1,1.);
    AsSeries a_s_sq(2,1.);
    AsSeries a_s_cube(3,1.);
    AddTerm("gg","delta",Delta,new gg_delta);
    
    AddTerm("gg","D0",D0,new gg_plus_0);
    AddTerm("gg","D1",D1,new gg_plus_1);
    AddTerm("gg","D2",D2,new gg_plus_2);
    AddTerm("gg","D3",D3,new gg_plus_3);
    AddTerm("gg","D4",D4,new gg_plus_4);
    AddTerm("gg","D5",D5,new gg_plus_5);
    
    AddTerm("gg","delta L",HEFT::n_delta_log_muf(_log_muf_mh_sq),new gg_delta);
    AddTerm("gg","D0 L",HEFT::n_D0_log_muf(_log_muf_mh_sq),new gg_plus_0);
    AddTerm("gg","D1 L",HEFT::n_D1_log_muf(_log_muf_mh_sq),new gg_plus_1);
    AddTerm("gg","D2 L",HEFT::n_D2_log_muf(_log_muf_mh_sq),new gg_plus_2);
    AddTerm("gg","D3 L",HEFT::n_D3_log_muf(_log_muf_mh_sq),new gg_plus_3);
    AddTerm("gg","D4 L",HEFT::n_D4_log_muf(_log_muf_mh_sq),new gg_plus_4);
    
    
    AsSeries DeltaTrunc = Delta;DeltaTrunc.Truncate(2);
    AsSeries D0Trunc = D0;D0Trunc.Truncate(2);
    AsSeries D1Trunc = D1;D1Trunc.Truncate(2);
    AsSeries D2Trunc = D2;D2Trunc.Truncate(2);
    AsSeries D3Trunc = D3;D3Trunc.Truncate(2);
    AsSeries D2test(3,D2.term_of_order(3).val());
    AddTerm("gg","delta to O(a^2)",DeltaTrunc,new gg_delta,"excluded");
    AddTerm("gg","D0 to O(a^2)",D0Trunc,new gg_plus_0,"excluded");
    AddTerm("gg","D1 to O(a^2)",D1Trunc,new gg_plus_1,"excluded");
    AddTerm("gg","D2 to O(a^2)",D2Trunc,new gg_plus_2,"excluded");
    AddTerm("gg","D3 to O(a^2)",D3Trunc,new gg_plus_3,"excluded");
    AddTerm("gg","D2 check sum",D2test,new gg_plus_2,"excluded");
    
    
    AddTerm("gg","NLO Real const",a_s,
                    new gg_real(&HEFT::nlo_r_lz0,_log_muf_mh_sq));
    AddTerm("gg","NLO Real log(1-z)",a_s,
                    new gg_real(&HEFT::nlo_r_lz1,_log_muf_mh_sq));
    
    AddTerm("gg","NNLO Real const",a_s_sq,
                    new gg_real(&HEFT::nnlo_r_lz0_const,_log_muf_mh_sq));
    AddTerm("gg","NNLO Real log(z)",a_s_sq,
                    new gg_real(&HEFT::nnlo_r_lz0_logz,_log_muf_mh_sq));
    AddTerm("gg","NNLO Real log(z)^2",a_s_sq,
                    new gg_real(&HEFT::nnlo_r_lz0_logz_sq,_log_muf_mh_sq));
    AddTerm("gg","NNLO Real log(z)^3",a_s_sq,
                    new gg_real(&HEFT::nnlo_r_lz0_logz_cube,_log_muf_mh_sq));
    AddTerm("gg","NNLO Real log(1-z)",a_s_sq,
                    new gg_real(&HEFT::nnlo_r_lz1,_log_muf_mh_sq));
    AddTerm("gg","NNLO Real log(1-z)^2",a_s_sq,
                    new gg_real(&HEFT::nnlo_r_lz2,_log_muf_mh_sq));
    AddTerm("gg","NNLO Real log(1-z)^3",a_s_sq,
                    new gg_real(&HEFT::nnlo_r_lz3,_log_muf_mh_sq));
    
    
    AddTerm("gg","N3LO Real log(1-z)^0 L=0",a_s_cube,
                    new gg_real(&HEFT::gg_n3lo_r_lz0,0.0));
    AddTerm("gg","N3LO Real log(1-z)^1 L=0",a_s_cube,
                    new gg_real(&HEFT::gg_n3lo_r_lz1,0.0));
    AddTerm("gg","N3LO Real log(1-z)^2 L=0",a_s_cube,
                    new gg_real(&HEFT::gg_n3lo_r_lz2,0.0));
    AddTerm("gg","N3LO Real log(1-z)^3 L=0",a_s_cube,
                    new gg_real(&HEFT::gg_n3lo_r_lz3,0.0));
    AddTerm("gg","N3LO Real log(1-z)^4 L=0",a_s_cube,
                    new gg_real(&HEFT::gg_n3lo_r_lz4,0.0));
    AddTerm("gg","N3LO Real log(1-z)^5 L=0",a_s_cube,
                    new gg_real(&HEFT::gg_n3lo_r_lz5,0.0));
    
    AddTerm("gg","N3LO Real log(1-z)^3 NS L=0",a_s_cube,
                    new gg_real(&HEFT::gg_n3lo_r_lz3_NS,0.0),"excluded");
    AddTerm("gg","N3LO Real log(1-z)^4 NS L=0",a_s_cube,
                    new gg_real(&HEFT::gg_n3lo_r_lz4_NS,0.0),"excluded");
    AddTerm("gg","N3LO Real log(1-z)^5 NS L=0",a_s_cube,
                    new gg_real(&HEFT::gg_n3lo_r_lz5_NS,0.0),"excluded");
    
    AddTerm("gg","N3LO Reg Log Ste",a_s_cube,
                    new gg_real(&HEFT::LEggNNNLOregSte ,_log_muf_mh_sq),"excluded");
    AddTerm("gg","N3LO Reg Log Falko",a_s_cube,
                    new gg_real(&HEFT::LEggN3LOregFalko ,_log_muf_mh_sq));
    
    AddTerm("gg","N3LO L(z)*a^5",a_s_cube,
            new gg_real(&HEFT::one ,0.0),"excluded");
    AddTerm("gg","N3LO L(z)*(1-z)*a^5",a_s_cube,
            new gg_real(&HEFT::one_minus_z ,0.0),"excluded");
    AddTerm("gg","N3LO L*(F - NS)*a^5",a_s_cube,
            new gg_real(&HEFT::nFull_minus_nS ,0.0),"excluded");
    
    // q g channel
    AddTerm("qg","qg NLO Real const",a_s,
                    new qg_real(&HEFT::qg_nlo_r_lz0,_log_muf_mh_sq));
    AddTerm("qg","qg NLO Real log(1-z)",a_s,
                    new qg_real(&HEFT::qg_nlo_r_lz1,_log_muf_mh_sq));
    AddTerm("qg","qg NNLO Real const",a_s_sq,
                    new qg_real(&HEFT::qg_nnlo_r_lz0_const,_log_muf_mh_sq));
    AddTerm("qg","qg NNLO Real log(z)",a_s_sq,
                    new qg_real(&HEFT::qg_nnlo_r_lz0_logz,_log_muf_mh_sq));
    AddTerm("qg","qg NNLO Real log(z)^2",a_s_sq,
                    new qg_real(&HEFT::qg_nnlo_r_lz0_logz_sq,_log_muf_mh_sq));
    AddTerm("qg","qg NNLO Real log(z)^3",a_s_sq,
                    new qg_real(&HEFT::qg_nnlo_r_lz0_logz_cube,_log_muf_mh_sq));
    AddTerm("qg","qg NNLO Real log(1-z)",a_s_sq,
                    new qg_real(&HEFT::qg_nnlo_r_lz1,_log_muf_mh_sq));
    AddTerm("qg","qg NNLO Real log(1-z)^2",a_s_sq,
                    new qg_real(&HEFT::qg_nnlo_r_lz2,_log_muf_mh_sq));
    AddTerm("qg","qg NNLO Real log(1-z)^3",a_s_sq,
                    new qg_real(&HEFT::qg_nnlo_r_lz3,_log_muf_mh_sq));
    
    AddTerm("qg","qg N3LO Real const NS L=0",a_s_cube,
                    new qg_real(&HEFT::qg_n3lo_r_lz0,0.0));
    AddTerm("qg","qg N3LO Real log(1-z) NS L=0",a_s_cube,
                    new qg_real(&HEFT::qg_n3lo_r_lz1,0.0));
    AddTerm("qg","qg N3LO Real log(1-z)^2 NS L=0",a_s_cube,
                    new qg_real(&HEFT::qg_n3lo_r_lz2,0.0));
    AddTerm("qg","qg N3LO Real log(1-z)^3 L=0",a_s_cube,
                    new qg_real(&HEFT::qg_n3lo_r_lz3,0.0));
    AddTerm("qg","qg N3LO Real log(1-z)^3 NS L=0",a_s_cube,
                    new qg_real(&HEFT::qg_n3lo_r_lz3_NS,0.0),"excluded");
    AddTerm("qg","qg N3LO Real log(1-z)^4 L=0",a_s_cube,
                    new qg_real(&HEFT::qg_n3lo_r_lz4,0.0));
    AddTerm("qg","qg N3LO Real log(1-z)^4 NS L=0",a_s_cube,
                    new qg_real(&HEFT::qg_n3lo_r_lz4_NS,0.0),"excluded");
    AddTerm("qg","qg N3LO Real log(1-z)^5 L=0",a_s_cube,
                    new qg_real(&HEFT::qg_n3lo_r_lz5,0.0));
    AddTerm("qg","qg N3LO Real log(1-z)^5 NS L=0",a_s_cube,
                    new qg_real(&HEFT::qg_n3lo_r_lz5_NS,0.0),"excluded");
    
    AddTerm("qg","qg N3LO Reg Log Ste",a_s_cube,
                    new qg_real(&HEFT::LEqgNNNLOregSte,_log_muf_mh_sq),"excluded");
    AddTerm("qg","qg N3LO Reg Log Falko",a_s_cube,
                    new qg_real(&HEFT::LEqgN3LOregFalko,_log_muf_mh_sq));
    
    AddTerm("qg","qg N3LO L(z)*a^5",a_s_cube,
            new qg_real(&HEFT::one ,0.0),"excluded");
    AddTerm("qg","qg N3LO L(z)*(1-z)*a^5",a_s_cube,
            new qg_real(&HEFT::one_minus_z ,0.0),"excluded");
    AddTerm("qg","qg N3LO L*(F - NS)*a^5",a_s_cube,
            new qg_real(&HEFT::qg_nFull_minus_nS ,0.0),"excluded");
    
    // q qbar channel
    AddTerm("q qbar","qqb NLO Real const",a_s,
                    new qqb_real(&HEFT::qqb_nlo_r_lz0,_log_muf_mh_sq));
    AddTerm("q qbar","qqb NNLO Real const",a_s_sq,
                    new qqb_real(&HEFT::qqb_nnlo_r_lz0_const,_log_muf_mh_sq));
    AddTerm("q qbar","qqb NNLO Real log(z)",a_s_sq,
                    new qqb_real(&HEFT::qqb_nnlo_r_lz0_logz,_log_muf_mh_sq));
    AddTerm("q qbar","qqb NNLO Real log(z)^2",a_s_sq,
                    new qqb_real(&HEFT::qqb_nnlo_r_lz0_logz_sq,_log_muf_mh_sq));
    AddTerm("q qbar","qqb NNLO Real log(z)^3",a_s_sq,
                    new qqb_real(&HEFT::qqb_nnlo_r_lz0_logz_cube,_log_muf_mh_sq));
    AddTerm("q qbar","qqb NNLO Real log(1-z)",a_s_sq,
                    new qqb_real(&HEFT::qqb_nnlo_r_lz1,_log_muf_mh_sq));
    AddTerm("q qbar","qqb NNLO Real log(1-z)^2",a_s_cube,
                    new qqb_real(&HEFT::qqb_nnlo_r_lz2,_log_muf_mh_sq));
    
    
    AddTerm("q qbar","qqb Reg Log Falko",a_s_sq,
            new qqb_real(&HEFT::LEqqbN3LOregFalko,_log_muf_mh_sq));
    // q q channel
    AddTerm("qq","qq NNLO Real const",a_s_sq,
                    new qq_real(&HEFT::qq_nnlo_r_lz0_const,_log_muf_mh_sq));
    AddTerm("qq","qq NNLO Real log(z)",a_s_sq,
                    new qq_real(&HEFT::qq_nnlo_r_lz0_logz,_log_muf_mh_sq));
    AddTerm("qq","qq NNLO Real log(z)^2",a_s_sq,
                    new qq_real(&HEFT::qq_nnlo_r_lz0_logz_sq,_log_muf_mh_sq));
    AddTerm("qq","qq NNLO Real log(z)^3",a_s_sq,
                    new qq_real(&HEFT::qq_nnlo_r_lz0_logz_cube,_log_muf_mh_sq));
    AddTerm("qq","qq NNLO Real log(1-z)",a_s_sq,
                    new qq_real(&HEFT::qq_nnlo_r_lz1,_log_muf_mh_sq));
    AddTerm("qq","qq NNLO Real log(1-z)^2",a_s_sq,
                    new qq_real(&HEFT::qq_nnlo_r_lz2,_log_muf_mh_sq));
    AddTerm("qq","qq Reg Log Falko",a_s_cube,
            new qq_real(&HEFT::LEqqN3LOregFalko,_log_muf_mh_sq));
    // q1 q2 channel
    AddTerm("q1q2","q1q2 NNLO Real const",a_s_sq,
                    new q1q2_real(&HEFT::q1q2_nnlo_r_lz0_const,_log_muf_mh_sq));
    AddTerm("q1q2","q1q2 NNLO Real log(z)",a_s_sq,
                    new q1q2_real(&HEFT::q1q2_nnlo_r_lz0_logz,_log_muf_mh_sq));
    AddTerm("q1q2","q1q2 NNLO Real log(z)^2",a_s_sq,
                    new q1q2_real(&HEFT::q1q2_nnlo_r_lz0_logz_sq,_log_muf_mh_sq));
    AddTerm("q1q2","q1q2 NNLO Real log(z)^3",a_s_sq,
                    new q1q2_real(&HEFT::q1q2_nnlo_r_lz0_logz_cube,_log_muf_mh_sq));
    AddTerm("q1q2","q1q2 NNLO Real log(1-z)",a_s_sq,
                    new q1q2_real(&HEFT::q1q2_nnlo_r_lz1,_log_muf_mh_sq));
    AddTerm("q1q2","q1q2 NNLO Real log(1-z)^2",a_s_sq,
                    new q1q2_real(&HEFT::q1q2_nnlo_r_lz2,_log_muf_mh_sq));
    AddTerm("q1q2","q1q2 Reg Log Falko",a_s_cube,
            new qq_real(&HEFT::LEqqN3LOregFalko,_log_muf_mh_sq));
            
    
    //: truncating the channels to the required order in a^s
    //: note that the channel terms start at a^0 at this stage
    //: since a^2 is in the Wilson Coefficient which is not yet
    //: multiplied through
    for (int i=0;i<_channels.size();i++)
    {
        _channels[i]->Truncate(_int_qcd_perturbative_order-2);
        //for (int j=0;j<_channels[i]->size();j++) cout<< *(_channels[i]->Term(j));
    }
    
    //: top only LO and NLO
    //: the bottom and charm are removed below to compute the top_only LO exact xs
    _model.bottom.set_Y(0.0);
    _model.charm.set_Y(0.0);
    // we compute the LO exact matrix element
    GluonFusionExactCoefficients top_only(_model);
    _top_only_LO_coefficient = top_only.LO_epsilon(0);
    // the LO delta exact matrix element in gg channel
    AddTerm("gg","LO exact top only",a_s_sq*top_only.LO_epsilon(0),
            new gg_delta,
            "excluded","exact");
    // the NLO delta exact matrix element in gg channel
    AddTerm("gg","NLO virt exact top only",a_s_cube*top_only.NLO_epsilon(0),
                new gg_delta,
                "excluded","exact");
    // the NLO D0 exact matrix element in gg channel (which is the same as in the effective but multiplied by the exact born)
    AddTerm("gg","NLO D0 exact top only",a_s_sq*top_only.LO_epsilon(0)
            *( HEFT::n_D0_log_muf(_log_muf_mh_sq) + HEFT::n_D0_at_mh()),
                new gg_plus_0,
                "excluded","exact");
    // the NLO D1 exact matrix element in gg channel (which is the same as in the effective but multiplied by the exact born)
    AddTerm("gg","NLO D1 exact top only",a_s_sq*top_only.LO_epsilon(0)
            *( HEFT::n_D1_log_muf(_log_muf_mh_sq) + HEFT::n_D1_at_mh()),
                new gg_plus_1,
                "excluded","exact");
    // the exact real gluon gluon
    AddTerm("gg","NLO reg exact top only",a_s_cube,
                new gg_real_exact(&h_exact::gg_reg_exact_nlo,_log_muf_mh_sq,&_model),
                "excluded","exact");
    // the exact real quark gluon
    AddTerm("qg","qg NLO reg exact top only",a_s_cube,
                new qg_real_exact(&h_exact::qg_reg_exact_nlo,_log_muf_mh_sq,&_model),
                "excluded","exact");
    // the exact real quark antiquark
    AddTerm("q qbar","qqb NLO reg exact top only",a_s_cube,
                new qqb_real_exact(&h_exact::qqb_reg_exact_nlo,_log_muf_mh_sq,&_model),
                "excluded","exact");
    
    // full exact with bottom and charm
    // setting bottom and charm's Y back to 1.
    _model.bottom.set_Y(1.0);
    _model.charm.set_Y(1.0);
    //_is_enhanced_eft = true;
    
    // computing the exact LO
    GluonFusionExactCoefficients exact(_model);

    cout<<"\n sigma LO top only = "<<_top_only_LO_coefficient<<endl;
    cout<<"sigma LO t+b+c = "<<exact.LO_epsilon(0)<<endl;
    // the exact LO delta in gg channel
    // the NLO delta exact matrix element in gg channel
    AddTerm("gg","LO exact",a_s_sq*exact.LO_epsilon(0),
            new gg_delta,
            "excluded","exact");
    if (_int_qcd_perturbative_order>2)
    {
    // the NLO delta exact matrix element in gg channel
    AddTerm("gg","NLO virt exact",a_s_cube*exact.NLO_epsilon(0),
            new gg_delta,
            "excluded","exact");
    // the NLO D0 exact matrix element in gg channel (which is the same as in the effective but multiplied by the exact born)
    AddTerm("gg","NLO D0 exact",a_s_sq*exact.LO_epsilon(0)
            *( HEFT::n_D0_log_muf(_log_muf_mh_sq) + HEFT::n_D0_at_mh()),
            new gg_plus_0,
            "excluded","exact");
    // the NLO D1 exact matrix element in gg channel (which is the same as in the effective but multiplied by the exact born)
    AddTerm("gg","NLO D1 exact",a_s_sq*exact.LO_epsilon(0)
            *( HEFT::n_D1_log_muf(_log_muf_mh_sq) + HEFT::n_D1_at_mh()),
            new gg_plus_1,
            "excluded","exact");
    // the exact real gluon gluon
    AddTerm("gg","NLO reg exact",a_s_cube,
            new gg_real_exact(&h_exact::gg_reg_exact_nlo,_log_muf_mh_sq,&_model),
            "excluded","exact");
    // the exact real quark gluon
    AddTerm("qg","qg NLO reg exact",a_s_cube,
            new qg_real_exact(&h_exact::qg_reg_exact_nlo,_log_muf_mh_sq,&_model),
            "excluded","exact");
    // the exact real quark antiquark
    AddTerm("q qbar","qqb NLO reg exact",a_s_cube,
            new qqb_real_exact(&h_exact::qqb_reg_exact_nlo,_log_muf_mh_sq,&_model),
            "excluded","exact");
    }
    
    
    _ew =  new GluonFusionEWCoefficients(_model);
    double lambda = abs(_ew->LO());
    cout<<"ew coeff lambda = "<<lambda<<" "<<_ew->LO()<<endl;
    _ew_lambda_series = AsSeries(1,lambda,lambda*7./6.,lambda*10.);
    cout<<"model: mh = "<<_model.higgs.m()<<endl;
}

void InclusiveProcess::AddTerm(const string& channel,
             const string& name,
             const AsSeries& coeff,
             LuminosityIntegral* lumi,
             const string& exclu,
             const string& exact)
{
    if (exclu!="excluded" or exact!="exact")
    {
        cout<<"[ihixs] error in constructing SigmaTerm: string selector "
        <<exclu<<" or "<<exact<<" not recognized."<<endl;
        exit(0);
    }
    find_channel(channel)->AddTerm(new SigmaTerm(name,coeff,lumi,exclu,exact));
}

void InclusiveProcess::AddTerm(const string& channel,
             const string& name,
             const AsSeries& coeff,
             LuminosityIntegral* lumi,
             const string& exclu)
{
    if (exclu!="excluded")
    {
        cout<<"[ihixs] error in constructing SigmaTerm: string selector "
        <<exclu<<" not recognized."<<endl;
        exit(0);
    }
    find_channel(channel)->AddTerm(new SigmaTerm(name,coeff,lumi,exclu));
}

void InclusiveProcess::AddTerm(const string& channel,
             const string& name,
             const AsSeries& coeff,
             LuminosityIntegral* lumi)
{
    find_channel(channel)->AddTerm(new SigmaTerm(name,coeff,lumi));
}

void InclusiveProcess::Evaluate()
{
    for (int i=0;i<_channels.size();i++)
    {
        for (int j=0;j<_channels[i]->size();j++)
            Evaluate(_channels[i]->Term(j));
    }
}

void InclusiveProcess::Evaluate(const string& type)
{
    SigmaTerm* term=find_term(type);
    Evaluate(term);
}

void InclusiveProcess::Evaluate(SigmaTerm* term)
{
    // initializing luminosity
    term->ConfigureLumi(_lumi,_tau,_UI);
    // performing the luminoity integral
    term->CallVegas();
    // getting the result (as a series in a_s)
    AsSeries res = term->PostVegasResult();
    // multiplying by universal prefactor (equal to effective theory born)
    res = res * _prefactor;
    
    AsSeries qcd_result;
    AsSeries ew_result;
    if (term->IsExact())
    {
        qcd_result = res;
    }
    else
    {
        // multiply by C^2
        qcd_result = res * _wc.c() * _wc.c();
        // truncate to order a_s^5
        qcd_result.Truncate(5);
        // set up the weak wilson coeff: (C+lambda*(1+a_s 7/6 + a_s^2 10)^2 -C^2)
        // and multiply with it to get the ew contribution
        ew_result = res *  (2. * _wc.c() * _ew_lambda_series
                                      + _ew_lambda_series * _ew_lambda_series);
        // truncate ew to otder a_s^5
        ew_result.Truncate(5);
    }
    
    if (abs(_log_mur_over_muf_sq) > 1e-15)
    // mu_r evolution
    {
        const double b0 = consts::beta_zero;
        const double b1 = consts::beta_one;
        const double b2 = consts::beta_two;
    
        const double L = _log_mur_over_muf_sq;
        AsSeries as_mur(1,1.0,L*b0,pow(L*b0,2.)+L*b1,pow(L*b0,3.)+2.*L*L*b0*b1+L*b2);
        qcd_result.MultiplyBySeries(as_mur);
        qcd_result.Truncate(5);
        ew_result.MultiplyBySeries(as_mur);
        ew_result.Truncate(5);
    }
    // multiply each term with a_s/pi to the proper power
    qcd_result.MultiplyAs(_as_pi);
    ew_result.MultiplyAs(_as_pi);
    //truncate to the requested power
    qcd_result.Truncate(_int_qcd_perturbative_order);
    ew_result.Truncate(_int_qcd_perturbative_order);
    
    term->SetQCDResult(qcd_result);
    term->SetEwResult(ew_result);

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
    _wc.Configure(_log_muf_over_mt_sq,_model.top.scheme());
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
            <<setw(20)<<_channels[i]->Result()*RescalingCoeff()<<" = "
            <<setw(20)<<_channels[i]->Sum().term_of_order(2)*RescalingCoeff()
            <<setw(20)<<_channels[i]->Sum().term_of_order(3)*RescalingCoeff()
        <<setw(20)<<_channels[i]->Sum().term_of_order(4)*RescalingCoeff()
        <<setw(20)<<_channels[i]->Sum().term_of_order(5)*RescalingCoeff()
            <<endl;
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

Channel* InclusiveProcess::find_channel(const string& name)
{
    for (int k=0;k<_channels.size();k++)
        if (_channels[k]->Name()==name) return _channels[k];
    cout<<"\nError in find_channel: no channel found with name "<<name<<endl;
    exit(0);
}

ResultPair InclusiveProcess::NextToSoftLog345_at_L_0()
{
    ResultPair res(0.0,0.0);
    if (_int_qcd_perturbative_order==5)
    {
    res = res + find_term("N3LO Real log(1-z)^3 NS L=0")->Result().term_of_order(5);
    res = res + find_term("N3LO Real log(1-z)^4 NS L=0")->Result().term_of_order(5);
    res = res + find_term("N3LO Real log(1-z)^5 NS L=0")->Result().term_of_order(5);
    
    res = res + find_term("qg N3LO Real log(1-z)^3 NS L=0")->Result().term_of_order(5);
    res = res + find_term("qg N3LO Real log(1-z)^4 NS L=0")->Result().term_of_order(5);
    res = res + find_term("qg N3LO Real log(1-z)^5 NS L=0")->Result().term_of_order(5);
    }
    return res;
}

ResultPair InclusiveProcess::NextToSoftLog012_at_L_0()
{
    ResultPair res(0.0,0.0);
    if (_int_qcd_perturbative_order==5)
    {
        res = res + find_term("N3LO Real log(1-z)^0 L=0")->Result().term_of_order(5);
        res = res + find_term("N3LO Real log(1-z)^1 L=0")->Result().term_of_order(5);
        res = res + find_term("N3LO Real log(1-z)^2 L=0")->Result().term_of_order(5);
        
        res = res + find_term("qg N3LO Real const NS L=0")->Result().term_of_order(5);
        res = res + find_term("qg N3LO Real log(1-z) NS L=0")->Result().term_of_order(5);
        res = res + find_term("qg N3LO Real log(1-z)^2 NS L=0")->Result().term_of_order(5);
    }
    return res;
}

ResultPair InclusiveProcess::FullLog345_at_L_0()
{
    ResultPair res(0.0,0.0);
    if (_int_qcd_perturbative_order==5)
    {
    res = res + find_term("N3LO Real log(1-z)^3 L=0")->Result().term_of_order(5);
    res = res + find_term("N3LO Real log(1-z)^4 L=0")->Result().term_of_order(5);
    res = res + find_term("N3LO Real log(1-z)^5 L=0")->Result().term_of_order(5);
    
    res = res + find_term("qg N3LO Real log(1-z)^3 L=0")->Result().term_of_order(5);
    res = res + find_term("qg N3LO Real log(1-z)^4 L=0")->Result().term_of_order(5);
    res = res + find_term("qg N3LO Real log(1-z)^5 L=0")->Result().term_of_order(5);
    }
    return res;
}

ResultPair InclusiveProcess::DeltaSoft()
{
    ResultPair res(0.0,0.0);
    if (_int_qcd_perturbative_order==5)
    {
        res = res + find_term("delta")->Result().term_of_order(5);
        res = res + find_term("D0")->Result().term_of_order(5);
        res = res + find_term("D1")->Result().term_of_order(5);
        res = res + find_term("D2")->Result().term_of_order(5);
        res = res + find_term("D3")->Result().term_of_order(5);
        res = res + find_term("D4")->Result().term_of_order(5);
        res = res + find_term("D5")->Result().term_of_order(5);

        res = res - find_term("delta to O(a^2)")->Result().term_of_order(5);
        res = res - find_term("D0 to O(a^2)")->Result().term_of_order(5);
        res = res - find_term("D1 to O(a^2)")->Result().term_of_order(5);
        res = res - find_term("D2 to O(a^2)")->Result().term_of_order(5);
        res = res - find_term("D3 to O(a^2)")->Result().term_of_order(5);
        
        
    }
    return res;
}

AsSeries InclusiveProcess::EwCorrections()
{
    AsSeries res;
    for (int k=0;k<_channels.size();k++)
    {
        for (int j=0;j<_channels[k]->size();j++)
        {
            if (_channels[k]->Term(j)->IsIncluded())
                res=res+_channels[k]->Term(j)->EwResult();
        }
    }
    return res;
}


ResultPair InclusiveProcess::ExactQCDEffectsLO()
{    return find_term("LO exact")->Result().term_of_order(2);
}

ResultPair InclusiveProcess::ExactQCDEffectsNLO()
{
    ResultPair res(0.0,0.0);
    if (_int_qcd_perturbative_order>2)
    {
        res = res + find_term("LO exact")->Result().term_of_order(3);
        res = res + find_term("NLO virt exact")->Result().term_of_order(3);
        res = res + find_term("NLO reg exact")->Result().term_of_order(3);
        res = res + find_term("NLO D0 exact")->Result().term_of_order(3);
        res = res + find_term("NLO D1 exact")->Result().term_of_order(3);
        res = res + find_term("qg NLO reg exact")->Result().term_of_order(3);
        res = res + find_term("qqb NLO reg exact")->Result().term_of_order(3);
    }
    return res;
}

ResultPair InclusiveProcess::ExactQCDEffectsLO_toponly()
{    return find_term("LO exact top only")->Result().term_of_order(2);
}

ResultPair InclusiveProcess::ExactQCDEffectsNLO_toponly()
{
    ResultPair res(0.0,0.0);
    if (_int_qcd_perturbative_order>2)
    {
        res = res + find_term("LO exact top only")->Result().term_of_order(3);
        res = res + find_term("NLO virt exact top only")->Result().term_of_order(3);
        res = res + find_term("NLO reg exact top only")->Result().term_of_order(3);
        res = res + find_term("NLO D0 exact top only")->Result().term_of_order(3);
        res = res + find_term("NLO D1 exact top only")->Result().term_of_order(3);
        res = res + find_term("qg NLO reg exact top only")->Result().term_of_order(3);
        res = res + find_term("qqb NLO reg exact top only")->Result().term_of_order(3);
    }
    return res;
}

ResultPair InclusiveProcess::DeltaQCDExact()
{
    ResultPair res =  ExactQCDEffectsLO()
                    + ExactQCDEffectsNLO()
                    - RescalingCoeff()*
                        (CoefficientAlphaSResult(2)
                         + CoefficientAlphaSResult(3));
    return res;
}

ResultPair InclusiveProcess::DeltaQCDTopOnly()
{
    ResultPair res =  ExactQCDEffectsLO_toponly()
    + ExactQCDEffectsNLO_toponly()
    - RescalingCoeff()*
    (CoefficientAlphaSResult(2)
     + CoefficientAlphaSResult(3));
    return res;
}


ResultPair InclusiveProcess::EffectiveRescaledLO()
{
    ResultPair res =  RescalingCoeff()*
    (CoefficientAlphaSResult(2)
     );
    return res;
}

ResultPair InclusiveProcess::EffectiveRescaledNLO()
{
    ResultPair res =  RescalingCoeff()*
    (CoefficientAlphaSResult(2)
     + CoefficientAlphaSResult(3)
     );
    return res;
}

ResultPair InclusiveProcess::EffectiveRescaledNNLO()
{
    ResultPair res =  RescalingCoeff()*
    (CoefficientAlphaSResult(2)
     + CoefficientAlphaSResult(3)
     + CoefficientAlphaSResult(4));
    return res;
}

ResultPair InclusiveProcess::EffectiveRescaledN3LOLowerAndScale()
{
    ResultPair res =  RescalingCoeff()*
    ( CoefficientAlphaSResult(5)
     - FullLog345_at_L_0()
     - NextToSoftLog012_at_L_0()
     - DeltaSoft()
     );
    return res;
}

string InclusiveProcess::DetailedResults()
{
    stringstream st;
    
    ResultPair deltaEffRescaledLO = CoefficientAlphaSResult(2) * RescalingCoeff();
    ResultPair deltaEffRescaledNLO = CoefficientAlphaSResult(3) * RescalingCoeff();
    ResultPair deltaEffRescaledNNLO = CoefficientAlphaSResult(4) * RescalingCoeff();
    
    ResultPair N3LO_EFFR_NS =EffectiveRescaledNNLO()
                        +EffectiveRescaledN3LOLowerAndScale()
                        +DeltaSoft()
                        +NextToSoftLog012_at_L_0()
                        +NextToSoftLog345_at_L_0();
    ResultPair N3LO_EFFR_FULL = EffectiveRescaledNNLO()
                                +EffectiveRescaledN3LOLowerAndScale()
                                +DeltaSoft()
                                +NextToSoftLog012_at_L_0()
                                +FullLog345_at_L_0();
    ResultPair TotalFull = N3LO_EFFR_FULL + DeltaQCDExact()+EwCorrections().AddUp();
    ResultPair TotalNS = N3LO_EFFR_NS + DeltaQCDExact()+EwCorrections().AddUp();
    
    ResultPair TotalNNLO = EffectiveRescaledNNLO() + DeltaQCDExact()
                            +EwCorrections().term_of_order(2)
                            +EwCorrections().term_of_order(3)
                            +EwCorrections().term_of_order(4);
    
    
    st<<"------------------------ * mur = "<<_current_mur<<" muf = "<<_UI.muf<<endl;
    st<<"K = "<<RescalingCoeff()<<endl;
    st<< *this<<endl;
    st<<"--------"<<endl;
    st<<"Channel Breakdown "<<endl<<ChannelBreakdown()<<endl;
    st<<"--------"<<endl;
    st<<"Results at mur="<<_current_mur<<endl;
    st<<"K = "<<RescalingCoeff()<<endl;
    st<<setw(35)<<"descr"<<setw(24)<<"sum"<<setw(24)<<"delta"<<"% of FULL NNLO = "
    <<setw(24)<<TotalNNLO<<endl;
    st<<setprecision(4)<<left
    <<setw(35)<<"Effective Rescaled LO"
    <<setw(24)<<EffectiveRescaledLO()
    <<setw(24)<<deltaEffRescaledLO
    <<setw(24)<<deltaEffRescaledLO/TotalNNLO*100.<<"%"<<endl;
    st<<setw(35)<<"Effective Rescaled NLO"
    <<setw(24)<<EffectiveRescaledNLO()
    <<setw(24)<<deltaEffRescaledNLO
    <<setw(24)<<deltaEffRescaledNLO/TotalNNLO*100.<<"%"<<endl;
    st<<setw(35)<<"Effective Rescaled NNLO"
    <<setw(24)<<EffectiveRescaledNNLO()
    <<setw(24)<<deltaEffRescaledNNLO
    <<setw(24)<<deltaEffRescaledNNLO/TotalNNLO*100.<<"%"
    <<endl;
    st<<setw(35)<<"Effective Rescaled N3LO FLS"
    <<setw(25)<<EffectiveRescaledNNLO()+EffectiveRescaledN3LOLowerAndScale()
    <<setw(25)<<EffectiveRescaledN3LOLowerAndScale()
    <<setw(25)<<EffectiveRescaledN3LOLowerAndScale()/TotalNNLO*100.<<"%"
    <<endl;
    st<<setw(35)<<"Effective Rescaled N3LO Soft"
    <<setw(25)<<EffectiveRescaledNNLO()+EffectiveRescaledN3LOLowerAndScale()+DeltaSoft()
    <<setw(25)<<DeltaSoft()
    <<setw(25)<<DeltaSoft()/TotalNNLO*100.<<"%"
    <<endl;
    st<<setw(35)<<"Effective Rescaled N3LO NS012"
    <<setw(25)<<EffectiveRescaledNNLO()+EffectiveRescaledN3LOLowerAndScale()+DeltaSoft()+NextToSoftLog012_at_L_0()
    <<setw(25)<<NextToSoftLog012_at_L_0()
    <<setw(25)<<NextToSoftLog012_at_L_0()/TotalNNLO*100.<<"%"
    <<endl;
    
    st<<setw(35)<<"Effective Rescaled N3LO NS345"
    <<setw(25)<<N3LO_EFFR_NS
    <<setw(25)<<NextToSoftLog345_at_L_0()
    <<setw(25)<<NextToSoftLog345_at_L_0()/TotalNNLO*100.<<"%"
    <<endl;
    
    
    st<<setw(35)<<"Effective Rescaled N3LO Full 345"
    <<setw(25)<<N3LO_EFFR_FULL
    <<setw(25)<<FullLog345_at_L_0()
    <<setw(25)<<FullLog345_at_L_0()/TotalNNLO*100.<<"%"
    <<endl;
    
    st<<setw(35)<<"Exact QCD"
    <<setw(25)<<N3LO_EFFR_FULL + DeltaQCDExact()
    <<setw(25)<<DeltaQCDExact()
    <<setw(25)<<DeltaQCDExact()/TotalNNLO*100.<<"%"
    <<setw(35)<<"NS+deltaQCDExact"
    <<setw(25)<<N3LO_EFFR_NS + DeltaQCDExact()<<endl;

    st<<setw(35)<<"Electroweak"
    <<setw(24)<<TotalFull
    <<setw(24)<<EwCorrections().AddUp()
    <<setw(24)<<EwCorrections().AddUp()/TotalNNLO*100.<<"%"
    <<setw(35)<<"NS+QCD+EW"
    <<setw(24)<<TotalNS
    <<endl;
    st<<endl<<endl;
    
    return st.str();

}

string InclusiveProcess::OneLineResults()
{
    stringstream st;
    
    ResultPair LO = CoefficientAlphaSResult(2);
    ResultPair NLO = CoefficientAlphaSResult(3);
    ResultPair NNLO = CoefficientAlphaSResult(4);
    ResultPair N3LO = CoefficientAlphaSResult(5);
    ResultPair N3LO_NS = NextToSoftLog345_at_L_0();
    ResultPair N3LO_BS = FullLog345_at_L_0();
    ResultPair ExactQCDLO = ExactQCDEffectsLO();
    ResultPair ExactQCDNLO = ExactQCDEffectsNLO();
    AsSeries Ew = EwCorrections();
    ResultPair EwCorrectionsXs = Ew.AddUp();
    
    
    ResultPair EffRescaledLO = LO * RescalingCoeff();
    ResultPair EffRescaledNLO = NLO * RescalingCoeff();
    ResultPair EffRescaledNNLO = NNLO * RescalingCoeff();
    ResultPair EffRescaledN3LO = N3LO * RescalingCoeff();
    ResultPair EffRescaledN3LO_NSL345_L0 = N3LO_NS * RescalingCoeff();
    ResultPair EffRescaledN3LO_FLL345_L0 = N3LO_BS * RescalingCoeff();
    
    ResultPair TotalEffRescaled =TotalCentral()*RescalingCoeff();
    
    ResultPair TotalQCDExact = TotalEffRescaled - EffRescaledLO - EffRescaledNLO
    + ExactQCDLO + ExactQCDNLO;
    ResultPair TotalQCDPlusEW = TotalQCDExact + EwCorrectionsXs;
    

    
//    int width=25;
    st<<ExactQCDLO+Ew.term_of_order(2);
    st<<ExactQCDNLO+Ew.term_of_order(3);
    st<<EffRescaledNNLO+Ew.term_of_order(4);
    st<<EffRescaledN3LO+Ew.term_of_order(5);
    st<<TotalQCDPlusEW;
    st<<EffRescaledN3LO_FLL345_L0-EffRescaledN3LO_NSL345_L0;
        
    return st.str();
    
}

string InclusiveProcess::OneLineResultsTopOnly()
{
    stringstream st;
    
    ResultPair LO = CoefficientAlphaSResult(2);
    ResultPair NLO = CoefficientAlphaSResult(3);
    ResultPair NNLO = CoefficientAlphaSResult(4);
    ResultPair N3LO = CoefficientAlphaSResult(5);
    ResultPair N3LO_NS = NextToSoftLog345_at_L_0();
    ResultPair N3LO_BS = FullLog345_at_L_0();
    
    ResultPair ExactQCDLO = ExactQCDEffectsLO_toponly();
    ResultPair ExactQCDNLO = ExactQCDEffectsNLO_toponly();
    //AsSeries Ew = EwCorrections();
    //ResultPair EwCorrectionsXs = Ew.AddUp();
    
    
    ResultPair EffRescaledLO = LO * RescalingCoeff();
    ResultPair EffRescaledNLO = NLO * RescalingCoeff();
    ResultPair EffRescaledNNLO = NNLO * RescalingCoeff();
    ResultPair EffRescaledN3LO = N3LO * RescalingCoeff();
    ResultPair EffRescaledN3LO_NSL345_L0 = N3LO_NS * RescalingCoeff();
    ResultPair EffRescaledN3LO_FLL345_L0 = N3LO_BS * RescalingCoeff();
    
    ResultPair TotalEffRescaled =TotalCentral()*RescalingCoeff();
    
    ResultPair TotalQCDExact = TotalEffRescaled - EffRescaledLO - EffRescaledNLO
    + ExactQCDLO + ExactQCDNLO;
    //ResultPair TotalQCDPlusEW = TotalQCDExact + EwCorrectionsXs;
    
    
    
    //    int width=25;
    st<<ExactQCDLO;
    st<<ExactQCDNLO;
    st<<EffRescaledNNLO;
    st<<EffRescaledN3LO;
    st<<TotalQCDExact;
    
    return st.str();
    
}



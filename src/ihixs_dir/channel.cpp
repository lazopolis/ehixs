#include "channel.h"


ostream& operator<<(ostream& stream, const Channel& ch)
{
    for (int i=0;i<ch.size();i++)
        stream<<*(ch.Term(i));
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
            //cout<<" <- survived !"<<endl;
        }
        //else cout<<" <- didn't make it"<<endl;
    }
    _terms = new_terms;
}

AsSeries Channel::Sum()
{
    AsSeries sum(2,0.0);
    for (int i=0;i<_terms.size();i++)
    {
        if (_terms[i]->IsIncluded())
            sum=sum+_terms[i]->ResultCentral();
    }
    return sum;
}

AsSeries Channel::Sum(const double& mur)
{
    AsSeries sum(2,0.0);
    for (int i=0;i<_terms.size();i++)
    {
        if (_terms[i]->IsIncluded())
            sum=sum+_terms[i]->Result(mur);
    }
    return sum;
}

ResultPair Channel::Result()
{
    return Sum().AddUp();
}

ResultPair Channel::Result(const double& mur)
{
    return Sum(mur).AddUp();
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
    
    _terms.push_back(new SigmaTerm("N3LO Real log(1-z)^0",AsSeries(3,1.0),
                                   new gg_real(&HEFT::gg_n3lo_r_lz0,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("N3LO Real log(1-z)^1",AsSeries(3,1.0),
                                   new gg_real(&HEFT::gg_n3lo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("N3LO Real log(1-z)^2",AsSeries(3,1.0),
                                   new gg_real(&HEFT::gg_n3lo_r_lz2,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("N3LO Real log(1-z)^3",AsSeries(3,1.0),
                                   new gg_real(&HEFT::gg_n3lo_r_lz3,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("N3LO Real log(1-z)^4",AsSeries(3,1.0),
                                   new gg_real(&HEFT::gg_n3lo_r_lz4,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("N3LO Real log(1-z)^5",AsSeries(3,1.0),
                                   new gg_real(&HEFT::gg_n3lo_r_lz5,log_muf_mh_sq)));
    
    _terms.push_back(new SigmaTerm("N3LO Real log(1-z)^3 NS",AsSeries(3,1.0),
                                   new gg_real(&HEFT::gg_n3lo_r_lz3_NS,log_muf_mh_sq),"excluded"));
    _terms.push_back(new SigmaTerm("N3LO Real log(1-z)^4 NS",AsSeries(3,1.0),
                                   new gg_real(&HEFT::gg_n3lo_r_lz4_NS,log_muf_mh_sq),"excluded"));
    _terms.push_back(new SigmaTerm("N3LO Real log(1-z)^5 NS",AsSeries(3,1.0),
                                   new gg_real(&HEFT::gg_n3lo_r_lz5_NS,log_muf_mh_sq),"excluded"));
}

// --- exact NLO gg
HiggsGGFChannelGGExactNLOReal::HiggsGGFChannelGGExactNLOReal(const double& log_muf_mh_sq)
{
    _name = "gg Real Exact";
    cout<<"\nExact QCD effects at NLO not yet defined! "<<endl;
    exit(0);
}

HiggsGGFChannelGQExactNLOReal::HiggsGGFChannelGQExactNLOReal(const double& log_muf_mh_sq)
{
    _name = "qg Real Exact";
    cout<<"\nExact QCD effects at NLO not yet defined! "<<endl;
    exit(0);
}

HiggsGGFChannelQQBARExactNLOReal::HiggsGGFChannelQQBARExactNLOReal(const double& log_muf_mh_sq)
{
    _name = "qqbar Real Exact";
    cout<<"\nExact QCD effects at NLO not yet defined! "<<endl;
    exit(0);
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
    
    _terms.push_back(new SigmaTerm("qg N3LO Real const NS",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz0,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z) NS",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz1,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^2 NS",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz2,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^3",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz3,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^3 NS",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz3_NS,log_muf_mh_sq),"excluded"));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^4",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz4,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^4 NS",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz4_NS,log_muf_mh_sq),"excluded"));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^5",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz5,log_muf_mh_sq)));
    _terms.push_back(new SigmaTerm("qg N3LO Real log(1-z)^5 NS",AsSeries(3,1.0),
                                   new qg_real(&HEFT::qg_n3lo_r_lz5_NS,log_muf_mh_sq),"excluded"));
    
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



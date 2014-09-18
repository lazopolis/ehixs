class GammaGamma_Decay_Cut_LeadingPhotonPt : public CCut
{
public:
    GammaGamma_Decay_Cut_LeadingPhotonPt(const double& cutvalue)
    : CCut("LeadingPhotonPt",cutvalue){};
    bool operator()(Event* the_event)
    {
        const double pt1 = the_event->p[1].T();
        const double pt2 = the_event->p[2].T();

        double leading_gamma_pt = pt1;
        if (pt2>pt1) leading_gamma_pt = pt2;

        if (leading_gamma_pt > min) return true;
        return false;
    }


};



class GammaGamma_Decay_Veto_LeadingPhotonPt : public CCut
{
public:
    GammaGamma_Decay_Veto_LeadingPhotonPt(const double& cutvalue)
    : CCut("LeadingPhotonPtVeto",cutvalue){};
    bool operator()(Event* the_event)
    {
        const double pt1 = the_event->p[1].T();
        const double pt2 = the_event->p[2].T();

        double leading_gamma_pt = pt1;
        if (pt2>pt1) leading_gamma_pt = pt2;

        if (leading_gamma_pt < min) return true;
        return false;
    }
    
    
};


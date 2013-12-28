class GammaGamma_Decay_Cut_LeadingPhotonPt : public CCut
{
public:
    GammaGamma_Decay_Cut_LeadingPhotonPt(const double& cutvalue)
    : CCut("LeadingPhotonPt",cutvalue){};
    bool operator()(Event* the_event)
    {
    double* gamma1 = the_event->ParticleMomentum(1);
    double* gamma2 = the_event->ParticleMomentum(2);
    
    double pt1 = sqrt(gamma1[1]*gamma1[1]+gamma1[2]*gamma1[2]);
    double pt2 = sqrt(gamma2[1]*gamma2[1]+gamma2[2]*gamma2[2]);

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
    double* gamma1 = the_event->ParticleMomentum(1);
    double* gamma2 = the_event->ParticleMomentum(2);
    
    double pt1 = sqrt(gamma1[1]*gamma1[1]+gamma1[2]*gamma1[2]);
    double pt2 = sqrt(gamma2[1]*gamma2[1]+gamma2[2]*gamma2[2]);
    
    double leading_gamma_pt = pt1;
    if (pt2>pt1) leading_gamma_pt = pt2;
    
    if (leading_gamma_pt < min) return true;
    return false;
    }
    
    
};


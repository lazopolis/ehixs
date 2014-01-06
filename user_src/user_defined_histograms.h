//CHistogram(unsigned numbins_, unsigned firstbin_,
//           const double& lowend_, const double& highend_,
//           const std::string& name_, bool adapt_)


class HistogramHiggsPt : public CHistogram
{
public:
    HistogramHiggsPt(unsigned numbins,const double& lowend,
                     const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    
    double determine_xval(const CombinedEvent& ce)
    {

    double* ph = ce.production->ParticleMomentum(5);
    double ptH = sqrt(ph[1]*ph[1]+ph[2]*ph[2]);
    return ptH;
    }

};




class HistogramLeadingPhotonPt : public CHistogram
{
public:
    HistogramLeadingPhotonPt(unsigned numbins,const double& lowend,
                     const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    
    double determine_xval(const CombinedEvent& ce)
    {
    
    double* gamma1 = ce.decay->ParticleMomentum(1);
    double* gamma2 = ce.decay->ParticleMomentum(2);
    //cout<<"\n gamma1 = "<<gamma1[0]<<" "<<gamma1[1]<<" "<<gamma1[2]<<" "<<gamma1[3];
    double pt1 = sqrt(gamma1[1]*gamma1[1]+gamma1[2]*gamma1[2]);
    double pt2 = sqrt(gamma2[1]*gamma2[1]+gamma2[2]*gamma2[2]);
    if (pt1>pt2) return pt1;
    else return pt2;
    
    }
    
};



class HistogramGstarLeadingPhotonPt : public CHistogram
{
public:
    HistogramGstarLeadingPhotonPt(unsigned numbins,const double& lowend,
                             const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    
    double determine_xval(const CombinedEvent& ce)
    {
        
        double* gamma1 = ce.production->ParticleMomentum(3);
        double* gamma2 = ce.production->ParticleMomentum(4);
        //cout<<"\n gamma1 = "<<gamma1[0]<<" "<<gamma1[1]<<" "<<gamma1[2]<<" "<<gamma1[3];
        double pt1 = sqrt(gamma1[1]*gamma1[1]+gamma1[2]*gamma1[2]);
        double pt2 = sqrt(gamma2[1]*gamma2[1]+gamma2[2]*gamma2[2]);
        if (pt1>pt2) return pt1;
        else return pt2;
        
    }
    
};



class HistogramGstarPhoton1Pt : public CHistogram
{
public:
    HistogramGstarPhoton1Pt(unsigned numbins,const double& lowend,
                                  const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    
    double determine_xval(const CombinedEvent& ce)
    {
        
        double* gamma1 = ce.production->ParticleMomentum(3);
        double pt1 = sqrt(gamma1[1]*gamma1[1]+gamma1[2]*gamma1[2]);
        return pt1;
        
    }
    
};

class HistogramGstarPhoton1Y : public CHistogram
{
public:
    HistogramGstarPhoton1Y(unsigned numbins,const double& lowend,
                            const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    
    double determine_xval(const CombinedEvent& ce)
    {
        
        double* gamma1 = ce.production->ParticleMomentum(3);
        const double E=gamma1[0];
        const double pz=gamma1[3];
        //cout<<"\t  "<<pz;
        double y = 0.5*log((E+pz)/(E-pz));
        return y;
        
    }
    
};

class HistogramGstarX1 : public CHistogram
{
public:
    HistogramGstarX1(unsigned numbins,const double& lowend,
                           const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    
    double determine_xval(const CombinedEvent& ce)
    {
        
        return ce.production->ParticleMomentum(1)[0]/13000*2.0;
        
    }
    
};

class HistogramGstarX2 : public CHistogram
{
public:
    HistogramGstarX2(unsigned numbins,const double& lowend,
                     const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    
    double determine_xval(const CombinedEvent& ce)
    {
        
        return ce.production->ParticleMomentum(2)[0]/13000*2.0;
        
    }
    
};






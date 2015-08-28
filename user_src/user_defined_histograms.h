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
        return ce.production->p[3].T();
    }

};

class HistogramHiggsY : public CHistogram
{
public:
    HistogramHiggsY(unsigned numbins,const double& lowend,
                           const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    
    double determine_xval(const CombinedEvent& ce)
    {
        return ce.production->p[3].Y();
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
        const double pt1 = ce.production->p[3].T();
        const double pt2 = ce.production->p[4].T();
        if (pt1>pt2) return pt1;
        else return pt2;
    }
};

class HistogramDiphotonMass : public CHistogram
{
public:
    HistogramDiphotonMass(unsigned numbins,const double& lowend,
                             const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    double determine_xval(const CombinedEvent& ce)
    {
        return sqrt(square(ce.production->p[3]+ce.production->p[4]));
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
        const double pt1 = ce.production->p[1].T();
        const double pt2 = ce.production->p[2].T();
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
        return ce.production->p[3].T();
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
        return ce.production->p[3].Y();
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
        
        return ce.production->p[1][0]/13000*2.0;
        
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
        return ce.production->p[2][0]/13000*2.0;
    }
    
};

class HistogramGstarGLUONPT : public CHistogram
{
public:
    HistogramGstarGLUONPT(unsigned numbins,const double& lowend,
                            const double& highend,const std::string& name)
    : CHistogram(numbins,0,lowend,highend,name,false){};
    
    double determine_xval(const CombinedEvent& ce)
    {
        return ce.production->p[5].T();
    }
    
};





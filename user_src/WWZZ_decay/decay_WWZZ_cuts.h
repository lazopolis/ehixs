//class CCut
//{
//public:
//	virtual bool operator()(Event *) = 0;
//    CCut(string _name, const double& _v){name=_name;min=_v;}
//    string name;
//    string give_name(){return name;}
//    void set_value(const double & _v){min=_v;}
//    double min;
//    string info(){stringstream stream;stream<<name<<" : "<<min;return stream.str();}
//};


class WWZZ_Decay_Cut_LeptonPairInvariantMass : public CCut
{
public:
    WWZZ_Decay_Cut_LeptonPairInvariantMass(const double& cutvalue)
    : CCut("LeptonPairInvariantMass",cutvalue){};
    bool operator()(Event* the_event)
    {
    double* lep1 = the_event->ParticleMomentum(1);
    double* lep2 = the_event->ParticleMomentum(2);
    
    double e = lep1[0]+lep2[0];
    double x = lep1[1]+lep2[1];
    double y = lep1[2]+lep2[2];
    double z =   lep1[3]+lep2[3];
    const double invariant_mass = sqrt(e*e-x*x-y*y-z*z);
    if (invariant_mass > min) return true;
    return false;
    }
    
    
};

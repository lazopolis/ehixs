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
    HiggsTo4LeptonsEvent* my_event = (HiggsTo4LeptonsEvent*)(the_event);
    FourMomentum* lep1 = my_event->lept1();
    FourMomentum* lep2 = my_event->lept2();
    
    FourMomentum lep12(lep1->E()+lep2->E(),
                       lep1->px()+lep2->px(),
                       lep1->py()+lep2->py(),
                       lep1->pz()+lep2->pz());
    const double invariant_mass = sqrt(lep12.square());
    if (invariant_mass > min) return true;
    return false;
    }
};

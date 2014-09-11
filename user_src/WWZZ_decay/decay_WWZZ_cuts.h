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
        if (square(the_event->p[1]+the_event->p[2]) > min*min) return true;
        return false;
    }

};

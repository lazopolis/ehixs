#ifndef GLUON_FUSION_CUT_H
#define GLGLUON_FUSION_CUT_HUON

#include "momenta.h"
#include "cut.h"
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


class GluonFusionCut_Higgs_pt : public CCut
{
public:
    GluonFusionCut_Higgs_pt(const double& cutvalue)
    : CCut("CutHiggsPT",cutvalue){};
    bool operator()(Event* the_event)
        {
        GluonFusionEvent* my_event = (GluonFusionEvent*)(the_event);
        FourMomentum* ph = my_event->pH();
        const double pth = sqrt(ph->px() * ph->px() + ph->py()*ph->py());
        if (pth>min) return true;
        return false;
        }
};


#endif

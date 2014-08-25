#ifndef BBH_CUT_H
#define BBH_CUT_H

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


class BbhCut_Higgs_pt : public CCut
{
public:
    BbhCut_Higgs_pt(const double& cutvalue)
    : CCut("CutBBhHiggsPT",cutvalue){};
    bool operator()(Event* the_event)
    {
        double* pH = the_event->ParticleMomentum(3);
        const double pth = sqrt(pH[1] * pH[1] + pH[2]*pH[2]);
        if (pth>min)
        {
            //cout<<"\n event accepted with ptH = "<<pth;
            return true;
        }
        
        return false;
    }
};

#endif
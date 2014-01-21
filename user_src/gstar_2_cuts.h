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


class GStar2Cut_GAMMA1_pt : public CCut
{
public:
    GStar2Cut_GAMMA1_pt(const double& cutvalue)
    : CCut("CutGAMMA1PT",cutvalue){};
    bool operator()(Event* the_event)
        {
        double* pgamma1 = the_event->ParticleMomentum(3);
        const double pth = sqrt(pgamma1[1] * pgamma1[1] + pgamma1[2]*pgamma1[2]);
        if (pth>min)
            {
            //cout<<"\n event accepted with ptH = "<<pth;
            return true;
            }

        return false;
        }
};


#endif
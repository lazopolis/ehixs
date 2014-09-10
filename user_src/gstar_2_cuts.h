#ifndef GSTAR_CUT_H
#define GSTAR_CUT_H

//#include "momenta.h"
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

class GStar2Cut_GAMMA2_pt : public CCut
{
public:
    GStar2Cut_GAMMA2_pt(const double& cutvalue)
    : CCut("CutGAMMA2PT",cutvalue){};
    bool operator()(Event* the_event)
    {
        double* pgamma2 = the_event->ParticleMomentum(4);
        const double pth = sqrt(pgamma2[1] * pgamma2[1] + pgamma2[2]*pgamma2[2]);
        if (pth>min)
        {
            //cout<<"\n event accepted with ptH = "<<pth;
            return true;
        }
        
        return false;
    }
};


class GStar2Cut_GLUONSTAR_pt : public CCut
{
public:
    GStar2Cut_GLUONSTAR_pt(const double& cutvalue)
    : CCut("CutGLUONSTARPT",cutvalue){};
    bool operator()(Event* the_event)
    {
        double* gstar = the_event->ParticleMomentum(5);
        const double pth = sqrt(gstar[1] * gstar[1] + gstar[2]*gstar[2]);
        if (pth>min)
        {
            //cout<<"\n event accepted with ptH = "<<pth;
            return true;
        }
        
        return false;
    }
};


class GStar2Cut_GLUONSTAR_IM : public CCut
{
public:
    GStar2Cut_GLUONSTAR_IM(const double& cutvalue)
    : CCut("CutGLUONSTARIM",cutvalue){};
    bool operator()(Event* the_event)
    {
        double* gstar = the_event->ParticleMomentum(5);
        const double psq = (gstar[0] * gstar[0]-gstar[1] * gstar[1] - gstar[2]*gstar[2]-gstar[3] * gstar[3]);
        if (psq>min)
        {
            //cout<<"\n event accepted with ptH = "<<pth;
            return true;
        }
        
        return false;
    }
};

#endif

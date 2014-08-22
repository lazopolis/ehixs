#ifndef GLUON_FUSION_CUT_H
#define GLUON_FUSION_CUT_H

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
        double* ph = the_event->ParticleMomentum(5);
        const double pth = sqrt(ph[1] * ph[1] + ph[2]*ph[2]);
        if (pth>min)
            {
            //cout<<"\n event accepted with ptH = "<<pth;
            return true;
            }

        return false;
        }
};



class GluonFusionCut_Higgs_pt_veto : public CCut
{
public:
    GluonFusionCut_Higgs_pt_veto(const double& cutvalue)
    : CCut("CutHiggsPTVeto",cutvalue){};
    bool operator()(Event* the_event)
    {
        double* ph = the_event->ParticleMomentum(5);
        const double pth = sqrt(ph[1] * ph[1] + ph[2]*ph[2]);
        if (pth<min)
        {
            //cout<<"\n event accepted with ptH = "<<pth;
            return true;
        }
        
        return false;
    }
};



double transverse_momentum(double* p)
{
    return sqrt(p[1]*p[1]+p[2]*p[2]);
}

double zrap(double* p)
{
    return 0.5*log((p[0]+p[3])/(p[0]-p[3]));
}

double phi(double* p)
{
    // pT along the y-axis
    if (p[1]==0.0)
        {
        if (p[2]<0.0) return -1.570796326794897;
        else return 1.570796326794897;
        }
    // pT in the second or third quadrant
    else if (p[1]<0.0) return atan(p[2]/p[1]) + 3.141592653589793;
    // pT in the first quadrant
    else if (p[1]>0.0 and p[2]>=0.0) return atan(p[2]/p[1]);
    // pT in the fourth quadrant
    else return atan(p[2]/p[1]) + 6.283185307179586;
        

}

class JetAlgorithm
{
public:
    JetAlgorithm(double* p3,double* p4)
    {
    const double ptjetmin = 15.0;
    const double R = 0.4;
    const double ptbuf=1e-12;
    //
    jetnumber_ = 0;
    
    double pt3 = transverse_momentum(p3);
    double pt4 = transverse_momentum(p4);
    //: p3 and p4 are ultra soft
    if (pt3<=ptbuf and pt4<=ptbuf)  //0-jet bin
        {
        //:nothing needs to be done here
        }
    //: p3 ultra soft
    if (pt3<=ptbuf and pt4>ptbuf)
        {
        if (pt4>ptjetmin) //: p4 is hard enough to be a jet
            {
            //all_momenta.pjet1=all_momenta["pf4"];
            jetnumber_ = 1;
            }
        }
    //: p4 ultra soft
    if (pt4<=ptbuf and pt3>ptbuf)
        {
        if (pt3>ptjetmin) //: p3 is hard enough to be a jet
            {
            //all_momenta.pjet1=all_momenta["pf3"];
            jetnumber_ = 1;
            }
        }
    //: none ultra soft, this is an event coming from honest RR
    if (pt3>ptbuf and pt4>ptbuf)
        {
        // both hard
        if (pt3>ptjetmin and pt4>ptjetmin)
            {
            //: now we have to calculate angular seperation
            const double delta_y=zrap(p3)-zrap(p4);
            const double delta_phi = phi(p3) -  phi(p4);
            const double d12 = pow(delta_y,2.0) + pow(delta_phi,2.0);
        
            if (d12>R*R)//: potential 2-jet case / no parton merging
                {
                //all_momenta.pjet1=all_momenta["pf3"];
                //all_momenta.pjet2=all_momenta["pf4"];
                jetnumber_ = 2;
                }
            else //: we need to merge the two partons in one jet
                {
                //all_momenta.pjet1 = all_momenta["pf3"]+all_momenta["pf4"];
                jetnumber_ = 1;
                }
            }
        // p3 soft (but not ultrasoft)
        else if (pt3<ptjetmin and pt4>ptjetmin)
            {
            jetnumber_ = 1;
            }
        // p4 soft
        else if (pt4<ptjetmin and pt3>ptjetmin)
            {
            jetnumber_ = 1;
            }
        // both soft
        else if (pt3<ptjetmin and pt4<ptjetmin)
            {
            jetnumber_ = 0;
            }
        }

    }
    int number(){return jetnumber_;}
private:
    int jetnumber_;
};

class GluonFusionZeroJetveto : public CCut
{
public:
    GluonFusionZeroJetveto(const double& cutvalue)
        : CCut("ZeroJetVeto",cutvalue){};
bool operator()(Event* the_event)
{
    JetAlgorithm my_jets(the_event->ParticleMomentum(3),the_event->ParticleMomentum(4));
    if (my_jets.number()==0) return true;
    return false;
}
};

class GluonFusionOneJetveto : public CCut
{
public:
    GluonFusionOneJetveto(const double& cutvalue)
    : CCut("OneJetVeto",cutvalue){};
    bool operator()(Event* the_event)
    {
    JetAlgorithm my_jets(the_event->ParticleMomentum(3),the_event->ParticleMomentum(4));
    if (my_jets.number()==1) return true;
    return false;
    }
};

/*
 if(jetalgorithm)
 {
 //jet algorithm definitions
 const double ptjetmin=20.0;
 //const double yjetmax=2.5;
 const double R=0.4;
 
 all_momenta.jetnumber=0;
 
 
 //: p3 and p4 are ultra soft
 if (pt3<=ptbuf and pt4<=ptbuf)  //0-jet bin
 {
 //:nothing needs to be done here
 }
 //: p3 ultra soft
 if (pt3<=ptbuf and pt4>ptbuf)
 {
 //const double y4=p4.zrap();
 if (pt4>ptjetmin) //: p4 is hard enough to be a jet
 {
 all_momenta.pjet1=all_momenta["pf4"];
 all_momenta.jetnumber=1;
 }
 }
 //: p4 ultra soft
 if (pt4<=ptbuf and pt3>ptbuf)
 {
 //const double y3=p3.zrap();
 if (pt3>ptjetmin) //: p3 is hard enough to be a jet
 {
 all_momenta.pjet1=all_momenta["pf3"];
 all_momenta.jetnumber=1;
 }
 }
 //: none ultra soft, this is an event coming from honest RR
 if (pt3>ptbuf and pt4>ptbuf)
 {
 //const double pi = 3.141592653589793;
 //: now we have to calculate angular seperation
 const double y3=all_momenta["pf3"].zrap();
 const double y4=all_momenta["pf4"].zrap();
 //cout<<"\n phi3 "<<acos(0.7);
 //cout<<"\n phi4 "<<acos(-0.7);
 const double delta_phi = all_momenta["pf3"].phi() -  all_momenta["pf4"].phi()  ;
 //: taking care of the dphi>Pi case
 //if (delta_phi>pi) {delta_phi = 2*pi -delta_phi;cout<<"\n it happened";}
 const double d12 = pow(y3-y4,2.0) + pow(delta_phi,2.0);
 //cout<<"\n-----\nd12="<<d12<<"\tRR="<<R<<"\t delta_phi^2="<<delta_phi
 //<<"\t"<<X->p3.phi()<<"\t"<<X->p4.phi();
 //cout<<"\np3="<<X->p3<<"\tp4="<<X->p4;
 if (d12>R*R)//: potential 2-jet case / no parton merging
 {
 all_momenta.pjet1=all_momenta["pf3"];
 all_momenta.pjet2=all_momenta["pf4"];
 all_momenta.jetnumber=2;
 }
 else //: we need to merge the two partons in one jet
 {
 all_momenta.pjet1 = all_momenta["pf3"]+all_momenta["pf4"];
 all_momenta.jetnumber=1;
 
 }
 }
 }
 */




#endif

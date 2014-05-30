#include "gluon_fusion_event_reconstructor.h"
#include "constants.h"
#include "math.h"
#include<iostream>
using namespace std;
void EventReconstructor::Configure(const double& Etotal,EventBox* evbox,const double& mh)
{
    Etot=Etotal;
    event_box=evbox;
    mh_=mh;
    ptbuf=1e-10;
}


void EventReconstructor::LO_event_kinematics(const double& sigma,
                                             const double & x1,
                                             const double & x2)
{
    event_box->AddNewEvent(sigma);
    event_box->SetP(1,x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
    event_box->SetP(2,x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
    event_box->SetP(3,0.0,0.0,0.0,0.0);
    event_box->SetP(4,0.0,0.0,0.0,0.0);
    event_box->SetP(5,(x1+x2)*Etot/2.0,0.0,0.0,(x1-x2)*Etot/2.0);
}

void EventReconstructor::NLO_event_kinematics(const double& sigma,
                                              const double & x1,
                                              const double & x2,
                                              const double & z,
                                              const double & s13,
                                              const double & s23,
                                              const double & phi)
{
    if (s13==0.0 and s23==0.0)
    {
        //: actually LO kinematics
        LO_event_kinematics(sigma,x1,x2);
    }
    else
    {
        
        
        
        //     ----------------- Higgs and gluon momenta ----------------------
        const double shat = pow(Etot,2.0)*x1*x2;
        const double pt3 = sqrt(s13*s23/shat); //gluon 1
        
        const double s1H = shat-s13;
        const double s2H = shat-s23;
        const double En3 = 0.5*(s13/x1/Etot+s23/x2/Etot);
        const double pz3 = 0.5*(-s13/x1/Etot+s23/x2/Etot);
        const double En =  0.5*(s1H/x1/Etot+s2H/x2/Etot);
        const double pZ =  0.5*(-s1H/x1/Etot+s2H/x2/Etot);
        
        
        const double sinphi = sin(2.0*consts::Pi*phi);
        const double cosphi = cos(2.0*consts::Pi*phi);
        event_box->AddNewEvent(sigma);
        event_box->SetP(1,x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
        event_box->SetP(2,x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
        event_box->SetP(3,En3,-pt3*sinphi,-pt3*cosphi,pz3);
        event_box->SetP(4,0.0,0.0,0.0,0.0);
        event_box->SetP(5,En,pt3*sinphi,pt3*cosphi,pZ);
    }
    
}

void EventReconstructor::NNLO_event_kinematics( const double& sigma,
                                               const double & x1,
                                               const double & x2,
                                               const double & z,
                                               const double & s13,
                                               const double & s23,
                                               const double & s14,
                                               const double & s24,
                                               const double & s34,
                                               const double& phi)
{
    //: p1+p2 -> pH + p3 + p4 -> X1+X2+... + p3 + p4
    //: the kinematical variables defined are
    //: s_ij = 2*p_i*p_j
    //: s_iH = 2*p_i*p_H
    const double shat = pow(Etot,2.0)*x1*x2;
    
    const double s1H = shat-s13-s14;
    const double s2H = shat-s23-s24;
    const double s3H = s13+s23-s34;
    //const double s4H = s14+s24-s34;
    event_box->AddNewEvent(sigma);
    event_box->SetP(1,x1*Etot/2.0,0.0,0.0,x1*Etot/2.0);
    event_box->SetP(2,x2*Etot/2.0,0.0,0.0,-x2*Etot/2.0);
    //     ----------------- Higgs and gluon momenta ----------------------
    
    double ptsq=s1H*s2H/shat-pow(mh_,2.0);
    if (ptsq<0.0 and fabs(ptsq)<1e-5) {ptsq=0.0;}
    //: taking the absolute value cares
    //: for the case ptsq= -1e-16 which can happen due to roundoff
    const double pt3 = sqrt(s13*s23/shat); //gluon 1
    const double pt4 = sqrt(s14*s24/shat); //gluon 2
    const double pT  = sqrt(ptsq); // Higgs
    const double En3 = 0.5*(s13/x1/Etot+s23/x2/Etot);
    const double En4 = 0.5*(s14/x1/Etot+s24/x2/Etot);
    const double En =  0.5*(s1H/x1/Etot+s2H/x2/Etot);
    const double pz3 = 0.5*(-s13/x1/Etot+s23/x2/Etot);
    const double pz4 = 0.5*(-s14/x1/Etot+s24/x2/Etot);
    const double pZ =  0.5*(-s1H/x1/Etot+s2H/x2/Etot);
    
    
    //: LO kinematics : Higgs + 0 hard partons, with HpT=0
    if ((pt3<=ptbuf and pt4<=ptbuf) )
    {
        // icase="only Higgs";
        event_box->SetP(3,En3,0.0,0.0,pz3);
        event_box->SetP(4,En4,0.0,0.0,pz4);
        event_box->SetP(5,En,0.0,0.0,pZ);
        
    }
    //: NLO real kinematics : H + hard parton
    else if (pt3>ptbuf and pt4<ptbuf)
    {
        // icase=" Higgs + p3";
        const double sinphi = sin(2.0*consts::Pi*phi);
        const double cosphi = cos(2.0*consts::Pi*phi);
        
        event_box->SetP(3,En3,-pt3*sinphi,-pt3*cosphi,pz3);
        event_box->SetP(4,En4,0.0,0.0,pz4);
        event_box->SetP(5,En,pT*sinphi,pT*cosphi,pZ);
        
    }
    else if (pt4>ptbuf and pt3<ptbuf)
    {
        //  icase=" Higgs + p4";
        const double sinphi = sin(2.0*consts::Pi*phi);
        const double cosphi = cos(2.0*consts::Pi*phi);
        
        event_box->SetP(3,En3,0.0,0.0,pz3);
        event_box->SetP(4,En4,-pt4*sinphi,-pt4*cosphi,pz4);
        event_box->SetP(5,En,pT*sinphi,pT*cosphi,pZ);
        
    }
    //: NNLO double real kinematics : H + 2 hard partons
    else if (pt3>ptbuf and pt4>ptbuf)
    {
        if (pT>=0.1*ptbuf)//:the Higgs is not extra soft
        {
            //   icase=" Higgs + p3 + p4";
            //: Higgs is temporary phi-reference
            //: fixing the phi3
            double cos3H = (En3*En-pz3*pZ-0.5*s3H)/pt3/pT;
            if (cos3H<-1.0) cos3H=-1.0;
            else if (cos3H>1.0) cos3H=1.0;
            double phi3 = acos(cos3H);//: acos returns in [0,Pi]
                                      //: we now decide if p3T is in the lower or upper semicircle
                                      //: in the pT plane
            if (rand()<0.5) phi3 = 2.0*consts::Pi - phi3;
            //: if lower, phi -> 2*Pi-phi
            const double sinphi = sin(2.0*consts::Pi*phi);
            const double cosphi = cos(2.0*consts::Pi*phi);
            
            
            event_box->SetP(3,
                            En3,
                            pt3*cos(phi3)*sinphi + pt3*sin(phi3)*cosphi,
                            pt3*cos(phi3)*cosphi + pt3*sin(phi3)*sinphi,
                            pz3);
            event_box->SetP(4,
                            -En-En3,
                            -pt3*cos(phi3)*sinphi - pt3*sin(phi3)*cosphi -pT*sinphi,
                            -pt3*cos(phi3)*cosphi - pt3*sin(phi3)*sinphi-pT*cosphi,
                            -pZ-pz3);
            event_box->SetP(5,En,pT*sinphi,pT*cosphi,pZ);
        }
        else if (pT<0.1*ptbuf)
            //: soft Higgs accidentally, two jets back to back
        {
            //  icase="  p3 + p4";
            //: p3 is the phi-reference along the x-axis
            const double sinphi = sin(2.0*consts::Pi*phi);
            const double cosphi = cos(2.0*consts::Pi*phi);
            event_box->SetP(3,En3,pt3*sinphi,pt3*cosphi,pz3);
            event_box->SetP(4,En4,pt4*sinphi,pt4*cosphi,pz4);
            event_box->SetP(5,En,0.0,0.0,pZ);
        }
        else
        {
            cout<<"\n["<<__func__<<"]\tERROR : if you are at this fork, there must be a NAN among the pTs!"<<endl;
            cout<<"\n info on kinematics: ptH="<<pT<<"\tpt3"<<pt3<<"\tpt4="<<pt4
            <<"\t"<<s1H<<" "<<s2H<<" x1="<<x1<<" x2="<<x2;
        }
    }
    else
    {
        cout<<"\n["<<__func__<<"]\tERROR : if you are at this fork, there must be a NAN among the pTs!"<<endl;
        cout<<"\n info on kinematics: ptH="<<pT<<"\tpt3"<<pt3<<"\tpt4="<<pt4
        <<"\t"<<s1H<<" "<<s2H<<" x1="<<x1<<" x2="<<x2;
    }
    
    //: momentum conservation check
    //
    // fvector PM = p1+p2-p3-p4-pH;
    //for (int i=0;i<4;i++) 
    // {
    //       if (abs(PM[i])>1e-1) 
    //       {
    //           cout<<"\n****";
    //           cout<<"\n case = "<<icase;
    //           cout<<"\nerror in momentum conservation, PM="
    //           <<PM<<"\t"<<p1<<"\t"<<p2<<"\t"<<p3<<"\t"<<p4<<"\t"<<pH;
    //           cout<<"\nptsq="<<s1H*s2H/shat-pow(Model.higgs.m,2.0);
    //           cout<<"\ns1H="<<s1H<<"\ts2H="<<s2H<<"\ts3H="<<s3H<<"\ts4H="<<s4H<<"\ts13="<<s13<<"\ts23="<<s23<<"\ts14="<<s14<<"\ts24="<<s24;
    //           cout<<"\npt3="<<pt3<<"\tpt4="<<pt4<<"\tptbuf="<<ptbuf;
    //       }
    // 
    //}
    
    
    
}




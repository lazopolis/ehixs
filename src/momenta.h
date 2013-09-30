
#ifndef MOMENTA_H
#define MOMENTA_H
#include<vector>
#include "fvector.h"
#include <stdlib.h> //: for exit()

using namespace std;

class Momenta
{
public:
     //fvector p1,p2,p3,p4,pH,pVB1,pVB2,pL1a,pL1b,pL2a,pL2b,pjet1,pjet2,phbb1,phbb2;
     vector<fvector> P;
     vector<string> names;
     void flush(){for (it=P.begin();it!=P.end();it++) (*it).flush();}
     void init_fvector(const string & key){P.push_back(fvector());names.push_back(key);}
     int jetnumber;
     fvector& operator[](const string key){for (unsigned i=0;i<P.size();i++){if (names[i]==key){return P[i];}} cout<<"\n momentum "<<key<<" not found! I exit"<<endl;exit(1);}
     fvector& operator[](int i){return P[i];}
     void merge(const Momenta& TT){for (int i=0;i<TT.P.size();i++){P.push_back(TT.P[i]);names.push_back(TT.names[i]);}}
private:
     vector<fvector>::iterator it;
     
};



class Event{
public:
     Event(const double & ww,Momenta pp,double * xx_vegas_){w=ww;p=pp;xx_vegas=xx_vegas_;}
     Momenta p;
     double w;
     void merge(const Event& TT){w=w*TT.w;p.merge(TT.p);}
     double * xx_vegas;
     friend ostream& operator<<(ostream& stream, const Event& E)
          {
          //stream <<"w="<< E.w << " x1= " <<E.xx_vegas[0] <<" x2="<<E.xx_vegas[1] ;
          
          //for(unsigned i=0; i<E.p.P.size(); ++i) {stream << "\n p"<<i+1<<" = "<<E.p.P[i] << endl;}
          double s13=E.p.P[0]*E.p.P[2]*2.0;
          double s23=E.p.P[1]*E.p.P[2]*2.0;
          double s12=E.p.P[0]*E.p.P[1]*2.0;
          double costh3=0.0;
          if (E.p.P[2][0]!=0.0) costh3 = E.p.P[2][3]/E.p.P[2][0];
          stream<<pow(125.0,2.0)/s12<<" "<<costh3;
          return stream;
          }
};

          
struct LightEvent{
public:
    LightEvent(const double & _x1,const double & _z,
               const double & _lambda, const double & _w)
    :x1(_x1),lambda(_lambda),z(_z),w(_w){};
    
     double w,x1,z,lambda;
};
          
#endif



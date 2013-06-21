
#ifndef MOMENTA_H
#define MOMENTA_H
#include<vector>
#include "fvector.h"
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
          stream <<"w="<< E.w << " x1= " <<E.xx_vegas[0] <<" x2="<<E.xx_vegas[1] ;
          
          for(unsigned i=0; i<E.p.P.size(); ++i) {stream << "\n p"<<i+1<<" = "<<E.p.P[i] << endl;}
          return stream;
          }
};

          
struct SmallEvent{
     double w,x1,x2,z,s13,s14,s23,s24,s34;
};
          
#endif



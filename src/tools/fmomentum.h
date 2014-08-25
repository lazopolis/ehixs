#ifndef FMOMENTUM_H
#define FMOMENTUM_H
#include <iostream>      // std::stringstream
using namespace std;

class FMomentum{
public:
    FMomentum(){for (int i=0;i<4;i++) p[i]=0.0;}
    double p[4];
    void Set(const double& E,const double& px,const double& py, const double& pz){p[0]=E;p[1]=px;p[2]=py;p[3]=pz;}
    void equal(const FMomentum& k)
    {p[0]=k[0];p[1]=k[1];p[2]=k[2];p[3]=k[3];}
    double operator[](int i) const {return p[i];}
    void zboost(const double& b);
    void boost(const double& bx,const double& by,const double& bz);
    double operator*(const FMomentum& Q){return p[0]*Q[0]-p[1]*Q[1]-p[2]*Q[2]-p[3]*Q[3];}
    friend ostream& operator<<(ostream&, const FMomentum&);
};



#endif

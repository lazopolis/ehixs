#include "fmomentum.h"



void FMomentum::zboost(const double& bb)
{
    //    if (bb>1.0-1e-16) cout<<"\nzboost too close to light cone, bb = "<<bb;
    const double gb = 1.0/sqrt(1.0-bb*bb);
    const double E  =    gb*p[0]   -bb*gb*p[3];
    const double pz = -bb*gb*p[0]  +gb*p[3];
    p[0]=E;
    p[3]=pz;
}

void FMomentum::boost(const double& bx,const double& by,const double& bz)
{
    const double bsq = bx*bx+by*by+bz*bz;
    if (1.-bsq<0.0)
    {
        cout<<"\n Error in boost: bsq>1 : "<<bx<<","<<by<<","<<bz;
        exit(1);
    }
    const double g = 1.0/sqrt(1.0-bsq);
    const double d = g*g/(g+1.0);
    const double L[4][4] = {
        {g,     -g*bx,          -g*by,      -g*bz},
        {-g*bx, 1.0+d*bx*bx,    d*bx*by,     d*bx*bz},
        {-g*by, d*by*bx,        1.0+d*by*by, d*by*bz},
        {-g*bz, d*bz*bx,        d*bz*by,     1.0+d*bz*bz}
    };
    double newp[4];
    for (int i=0;i<4;i++)
    {
        newp[i]=0.0;
        for (int j=0;j<4;j++)
            newp[i] += L[i][j]*p[j];
    }
    for (int i=0;i<4;i++) p[i] = newp[i];
}




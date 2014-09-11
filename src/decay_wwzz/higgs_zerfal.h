#ifndef HIGGS_ZERFAL_INTERFACE
#define HIGGS_ZERFAL_INTERFACE

#include "fourvector.h"

#ifdef __cplusplus
extern "C" {
#endif
   
//subroutine HiggsZerfall(
//            &     pH,mw,mz,mt,gamW,gamZ,GF,alpha,imode,x  !Input
//            &     ,GAMMA,p1,p2,p3,p4)                     !Output
    
    
    void higgszerfall_(double*,double*,double*,double*,double*,
                       double*,double*,double*,
                       int*,     // decay mode 0:
                       double*,// passing vegas variables
                       double*,// the weight of the decay will be set here
                       double*,
                       double*,
                       double*,
                       double*
                       );
    
#ifdef __cplusplus
}
#endif
//
inline void higgszerfall(
                         FourVector& pH,
                         const double& mw,const double& mz,const double& mt,
                         const double& gamw,
                         const double& gamz,
                         const double& GF,
                         const double& alpha,
                         const int& decaymode,     // decay mode 0:
                         double* decay_xx_vegas,// passing vegas variables
                         double& decay_weight,  // the weight of the decay will be set here
                         vector<FourVector> p
                         )
{
    double _pH[4] = {pH[0],pH[1],pH[2],pH[3]};
    double _p1[4], _p2[4], _p3[4], _p4[4];
    higgszerfall_(
                  (double*) _pH,(double*) &mw,(double*) &mz,(double*) &mt,
                  (double*) &gamw,
                  (double*) &gamz,
                  (double*) &GF,
                  (double*) &alpha,
                  (int*) &decaymode,     // decay mode 0:
                  (double*) decay_xx_vegas,// passing vegas variables
                  (double*) &decay_weight,  // the weight of the decay will be set here
                  (double*) _p1, (double*) _p2, (double*) _p3, (double*) _p4
                  );
    pH = FourVector(_pH);
    p.push_back(FourVector(_p1));
    p.push_back(FourVector(_p2));
    p.push_back(FourVector(_p3));
    p.push_back(FourVector(_p4));
}


#endif

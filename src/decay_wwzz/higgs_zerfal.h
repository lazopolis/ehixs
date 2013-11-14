#ifndef HIGGS_ZERFAL_INTERFACE
#define HIGGS_ZERFAL_INTERFACE

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
                         double* pH,
                         const double& mw,const double& mz,const double& mt,
                         const double& gamw,
                         const double& gamz,
                         const double& GF,
                         const double& alpha,
                         const int& decaymode,     // decay mode 0:
                         double* decay_xx_vegas,// passing vegas variables
                         double& decay_weight,  // the weight of the decay will be set here
                         double* p1,double* p2,double* p3,double* p4
                         )
{
    higgszerfall_(
                  (double*) pH,(double*) &mw,(double*) &mz,(double*) &mt,
                  (double*) &gamw,
                  (double*) &gamz,
                  (double*) &GF,
                  (double*) &alpha,
                  (int*) &decaymode,     // decay mode 0:
                  (double*) decay_xx_vegas,// passing vegas variables
                  (double*) &decay_weight,  // the weight of the decay will be set here
                  (double*) p1,
                  (double*) p2,(double*) p3,(double*) p4
                  );
}


#endif

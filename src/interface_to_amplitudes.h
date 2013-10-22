/* Header file for the inclusion of fortran  routines related to 
the NLO amplitudes  in C/C++ programs.
   
   They must have been compiled with gfortran or a compiler that have
   the same external symbols convention.

   Based on chaplin.h by:
   
   2011, Romain Mueller, muellrom@phys.ethz.ch
   */

#ifndef INTERFACETOAMPLITUDES_H
#define INTERFACETOAMPLITUDES_H



/* C99 complex numbers */
#ifdef __cplusplus
  /* C++ */
  #include <complex>
  #define complex_double std::complex<double>
#else
  /* C */
  #include <complex.h>
  #define complex_double {_Complex double}
#endif

/* Fortran declarations 
   ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif



complex_double aq1_( double*, double*, complex_double*,double*);
complex_double aq2a_( double*, double*, complex_double*,double*);
complex_double aq2b_( double*, double*, complex_double*,double*);
complex_double aq2c_( double*, double*, complex_double*,double*);
complex_double aqqgh_( double*, double*, complex_double*,complex_double*);

void ffjet_(double*);
double fjet_(double*,double*,double *s12,double *s13,double *s23,double *s14,double *s24,double *s34,double *w);

#ifdef __cplusplus
}
#endif

/* C declarations */



inline complex_double Aq1(const double& z, const double& lambda,  const complex_double& M,const double& QQQ)
{ return aq1_((double*)&z, (double*)&lambda,  (complex_double*)&M,(double*)&QQQ); }

inline complex_double Aq2a(const double& z, const double& lambda,  const complex_double& M,const double& QQQ)
{ return aq2a_((double*)&z, (double*)&lambda,  (complex_double*)&M,(double*)&QQQ); }

inline complex_double Aq2b(const double& z, const double& lambda,  const complex_double& M,const double& QQQ)
{ return aq2b_((double*)&z, (double*)&lambda,  (complex_double*)&M,(double*)&QQQ); }

inline complex_double Aq2c(const double& z, const double& lambda,  const complex_double& M,const double& QQQ)
{ return aq2c_((double*)&z, (double*)&lambda,  (complex_double*)&M,(double*)&QQQ); }

inline complex_double Aqqgh(const double& z, const double& mh,  const complex_double& tau,const complex_double& mq)
{ return aqqgh_((double*)&z, (double*)&mh,  (complex_double*)&tau,(complex_double*)&mq); }




#define fortran_definition_macro(x) void x(int*,int*,double*,double*, double*,double*,double*, double*, double*, double*, double*,double*, double*)
#define cpp_definition_macro(XXX,YYY) inline void XXX(const int& sector, const int & pole,const double &s,const double &x1,const double &x2,const double &z,const double & lh,const double & wd,const double & lambda1, const double & lambda2, const double & lambda3,const double & lambda4,const double &res_z){YYY((int*)&sector, (int*)&pole, (double*)&s,(double*)&x1,(double*)&x2,(double*)&z,(double*)&lh,(double*)&wd, (double*)&lambda1, (double*)&lambda2, (double*)&lambda3, (double*)&lambda4,(double*)&res_z );}

// macro_nf: (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1,fff)
#define fortran_definition_macro_nf(x) void x(int*,int*,double*,double*, double*,double*,double*, double*, double*, double*, double*)
#define cpp_definition_macro_nf(XXX,YYY) inline void XXX(const int& sector, const int & pole,const double &s,const double &x1,const double &x2,const double &z,const double & lh,const double & wd,const double& nf,const double & lambda1,const double &res_z){YYY((int*)&sector, (int*)&pole, (double*)&s,(double*)&x1,(double*)&x2,(double*)&z,(double*)&lh,(double*)&wd, (double*)&nf, (double*)&lambda1,(double*)&res_z );}

//:----------------------------------------------   b bar -> g g H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif

//void bbggh1n_(int*,int*,double*,double*, double*,double*,double*, double*, double*, double*, double*,double*, double*);

fortran_definition_macro(bbggh1n_);
fortran_definition_macro(bbggh2n_);
fortran_definition_macro(bbggh3n_);
fortran_definition_macro(bbggh4n_);
fortran_definition_macro(bbggh5n_);
fortran_definition_macro(bbggh6n_);
fortran_definition_macro(bbggh7n_);
fortran_definition_macro(bbggh8n_);
fortran_definition_macro(bbggh9n_);
#ifdef __cplusplus
}
#endif

cpp_definition_macro(bbggH1n,bbggh1n_)
cpp_definition_macro(bbggH2n,bbggh2n_)
cpp_definition_macro(bbggH3n,bbggh3n_)
cpp_definition_macro(bbggH4n,bbggh4n_)
cpp_definition_macro(bbggH5n,bbggh5n_)
cpp_definition_macro(bbggH6n,bbggh6n_)
cpp_definition_macro(bbggH7n,bbggh7n_)
cpp_definition_macro(bbggH8n,bbggh8n_)
cpp_definition_macro(bbggH9n,bbggh9n_)
		
//:----------------------------------------------   b bbar -> b bbar H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(bbarbbarh1n_);
fortran_definition_macro(bbarbbarh2n_);
fortran_definition_macro(bbarbbarh3n_);
fortran_definition_macro(bbarbbarh4n_);
fortran_definition_macro(bbarbbarh5n_);
fortran_definition_macro(bbarbbarh6n_);
fortran_definition_macro(bbarbbarh7n_);
fortran_definition_macro(bbarbbarh8n_);
fortran_definition_macro(bbarbbarh9n_);
#ifdef __cplusplus
}
#endif

cpp_definition_macro(bbarbbarH1n,bbarbbarh1n_)
cpp_definition_macro(bbarbbarH2n,bbarbbarh2n_)
cpp_definition_macro(bbarbbarH3n,bbarbbarh3n_)
cpp_definition_macro(bbarbbarH4n,bbarbbarh4n_)
cpp_definition_macro(bbarbbarH5n,bbarbbarh5n_)
cpp_definition_macro(bbarbbarH6n,bbarbbarh6n_)
cpp_definition_macro(bbarbbarH7n,bbarbbarh7n_)
cpp_definition_macro(bbarbbarH8n,bbarbbarh8n_)
cpp_definition_macro(bbarbbarH9n,bbarbbarh9n_)

 //:----------------------------------------------   b bbar -> q qbar H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(bbarqqbarh1n_);
fortran_definition_macro(bbarqqbarh2n_);
fortran_definition_macro(bbarqqbarh3n_);


#ifdef __cplusplus
}
#endif
cpp_definition_macro(bbarqqbarH1n,bbarqqbarh1n_)
cpp_definition_macro(bbarqqbarH2n,bbarqqbarh2n_)
cpp_definition_macro(bbarqqbarH3n,bbarqqbarh3n_)


 //:----------------------------------------------   b b -> b b H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(bbbbh1n_);
fortran_definition_macro(bbbbh2n_);
fortran_definition_macro(bbbbh3n_);
fortran_definition_macro(bbbbh4n_);
fortran_definition_macro(bbbbh5n_);
fortran_definition_macro(bbbbh6n_);
fortran_definition_macro(bbbbh7n_);
fortran_definition_macro(bbbbh8n_);

#ifdef __cplusplus
}
#endif
cpp_definition_macro(bbbbH1n,bbbbh1n_)
cpp_definition_macro(bbbbH2n,bbbbh2n_)
cpp_definition_macro(bbbbH3n,bbbbh3n_)
cpp_definition_macro(bbbbH4n,bbbbh4n_)
cpp_definition_macro(bbbbH5n,bbbbh5n_)
cpp_definition_macro(bbbbH6n,bbbbh6n_)
cpp_definition_macro(bbbbH7n,bbbbh7n_)
cpp_definition_macro(bbbbH8n,bbbbh8n_)

//:----------------------------------------------   b g -> b g H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(bggbh1n_);
fortran_definition_macro(bggbh2n_);
fortran_definition_macro(bggbh3n_);
fortran_definition_macro(bggbh4n_);
fortran_definition_macro(bggbh5n_);
fortran_definition_macro(bggbh6n_);
fortran_definition_macro(bggbh7n_);
fortran_definition_macro(bggbh8n_);
fortran_definition_macro(bggbh9n_);
#ifdef __cplusplus
}
#endif

cpp_definition_macro(bggbH1n,bggbh1n_)
cpp_definition_macro(bggbH2n,bggbh2n_)
cpp_definition_macro(bggbH3n,bggbh3n_)
cpp_definition_macro(bggbH4n,bggbh4n_)
cpp_definition_macro(bggbH5n,bggbh5n_)
cpp_definition_macro(bggbH6n,bggbh6n_)
cpp_definition_macro(bggbH7n,bggbh7n_)
cpp_definition_macro(bggbH8n,bggbh8n_)
cpp_definition_macro(bggbH9n,bggbh9n_)

//:----------------------------------------------   b q -> b q H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(bqqbh1n_);
fortran_definition_macro(bqqbh2n_);
fortran_definition_macro(bqqbh3n_);
fortran_definition_macro(bqqbh4n_);
fortran_definition_macro(bqqbh5n_);
fortran_definition_macro(bqqbh6n_);

#ifdef __cplusplus
}
#endif

cpp_definition_macro(bqqbH1n,bqqbh1n_)
cpp_definition_macro(bqqbH2n,bqqbh2n_)
cpp_definition_macro(bqqbH3n,bqqbh3n_)
cpp_definition_macro(bqqbH4n,bqqbh4n_)
cpp_definition_macro(bqqbH5n,bqqbh5n_)
cpp_definition_macro(bqqbH6n,bqqbh6n_)

//:----------------------------------------------   g b -> b g H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(gbgbh1n_);
fortran_definition_macro(gbgbh2n_);
fortran_definition_macro(gbgbh3n_);
fortran_definition_macro(gbgbh4n_);
fortran_definition_macro(gbgbh5n_);
fortran_definition_macro(gbgbh6n_);
fortran_definition_macro(gbgbh7n_);
fortran_definition_macro(gbgbh8n_);
fortran_definition_macro(gbgbh9n_);
#ifdef __cplusplus
}
#endif

cpp_definition_macro(gbgbH1n,gbgbh1n_)
cpp_definition_macro(gbgbH2n,gbgbh2n_)
cpp_definition_macro(gbgbH3n,gbgbh3n_)
cpp_definition_macro(gbgbH4n,gbgbh4n_)
cpp_definition_macro(gbgbH5n,gbgbh5n_)
cpp_definition_macro(gbgbH6n,gbgbh6n_)
cpp_definition_macro(gbgbH7n,gbgbh7n_)
cpp_definition_macro(gbgbH8n,gbgbh8n_)
cpp_definition_macro(gbgbH9n,gbgbh9n_)

//:----------------------------------------------   q b -> b q H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(qbqbh1n_);
fortran_definition_macro(qbqbh2n_);
fortran_definition_macro(qbqbh3n_);
fortran_definition_macro(qbqbh4n_);
fortran_definition_macro(qbqbh5n_);
fortran_definition_macro(qbqbh6n_);

#ifdef __cplusplus
}
#endif

cpp_definition_macro(qbqbH1n,qbqbh1n_)
cpp_definition_macro(qbqbH2n,qbqbh2n_)
cpp_definition_macro(qbqbH3n,qbqbh3n_)
cpp_definition_macro(qbqbH4n,qbqbh4n_)
cpp_definition_macro(qbqbH5n,qbqbh5n_)
cpp_definition_macro(qbqbH6n,qbqbh6n_)


 //:----------------------------------------------   q qbar -> b bbar H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(qqbarbbarh1n_);

#ifdef __cplusplus
}
#endif
cpp_definition_macro(qqbarbbbarH1n,qqbarbbarh1n_)

 //:----------------------------------------------   g g -> b bbar H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(ggbbh1n_);
fortran_definition_macro(ggbbh2n_);
fortran_definition_macro(ggbbh3n_);
fortran_definition_macro(ggbbh4n_);
fortran_definition_macro(ggbbh5n_);

#ifdef __cplusplus
}
#endif
cpp_definition_macro(ggbbH1n,ggbbh1n_)
cpp_definition_macro(ggbbH2n,ggbbh2n_)
cpp_definition_macro(ggbbH3n,ggbbh3n_)
cpp_definition_macro(ggbbH4n,ggbbh4n_)
cpp_definition_macro(ggbbH5n,ggbbh5n_)

//:----------------------------------------------   RV b bbar -> g g H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro(rvbbargh1n_);
fortran_definition_macro(rvbbargh2n_);
fortran_definition_macro(rvbbargh3n_);
fortran_definition_macro(rvbbargh4n_);

#ifdef __cplusplus
}
#endif
cpp_definition_macro(RVbbbarggH1n,rvbbargh1n_)
cpp_definition_macro(RVbbbarggH2n,rvbbargh2n_)
cpp_definition_macro(RVbbbarggH3n,rvbbargh3n_)
cpp_definition_macro(RVbbbarggH4n,rvbbargh4n_)

//:----------------------------------------------   RV b g -> b g H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
extern "C" {
#endif
    fortran_definition_macro(rvbgbh1n_);
    fortran_definition_macro(sigma_gbbh_rv_);
    
#ifdef __cplusplus
}
#endif
cpp_definition_macro(RVbggH,rvbgbh1n_)
cpp_definition_macro(RVgbgH,sigma_gbbh_rv_)
//:----------------------------------------------   renormalization b bbar -> g g H  --------------------------------------------------
/* Fortran declarations ! gfortran external symbols convention */
#ifdef __cplusplus
  extern "C" {
#endif
fortran_definition_macro_nf(rvbbarghren0n_);
#ifdef __cplusplus
}
#endif
cpp_definition_macro_nf(RVbbbarggHrenorm,rvbbarghren0n_)

#endif

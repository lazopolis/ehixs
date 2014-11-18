/* Header file for the inclusion of CHAPLIN routines in C/C++ programs.
   
   CHAPLIN must have been compiled with gfortran or a compiler that have
   the same external symbols convention.

   2011, Romain Mueller, muellrom@phys.ethz.ch
   */

/* C99 complex numbers */

#ifndef CHAPLIN_H_ONCE
#define CHAPLIN_H_ONCE

#ifdef __cplusplus
  /* C++ */
  #include <complex>
  #define complex_double std::complex<double>
  typedef struct{
      double re; double im;
      inline operator complex_double() { return complex_double(re,im); }
  } ccomplex_double;
#else
  /* C */
  #include <complex.h>
  #define complex_double {_Complex double}
#endif


#ifdef __cplusplus
  extern "C" {
#endif

/* Fortran declarations 
   ! gfortran external symbols convention */
ccomplex_double hpl1_(int*, ccomplex_double*);
ccomplex_double hpl2_(int*, int*, ccomplex_double*);
ccomplex_double hpl3_(int*, int*, int*, ccomplex_double*);
ccomplex_double hpl4_(int*, int*, int*, int*, ccomplex_double*);

double hpl2real_(int*, int*, double*, double*);
double hpl2im_  (int*, int*, double*, double*);
double hpl3real_(int*, int*, int*, double*, double*);
double hpl3im_  (int*, int*, int*, double*, double*);
double hpl4real_(int*, int*, int*, int*, double*, double*);
double hpl4im_  (int*, int*, int*, int*, double*, double*);

#ifdef __cplusplus
}
#endif

/* C declarations */

inline complex_double HPL(const int& n1, const complex_double& z)
{ return hpl1_((int*)&n1, (ccomplex_double*)&z); }

inline complex_double HPL(const int& n1, const int& n2, const complex_double& z)
{ return hpl2_((int*)&n1, (int*)&n2, (ccomplex_double*)&z); }

inline complex_double HPL(const int& n1, const int& n2, const int& n3, const complex_double& z)
{ return hpl3_((int*)&n1, (int*)&n2, (int*)&n3, (ccomplex_double*)&z); }

inline complex_double HPL(const int& n1, const int& n2, const int& n3, const int& n4, const complex_double& z)
{ return hpl4_((int*)&n1, (int*)&n2, (int*)&n3, (int*)&n4, (ccomplex_double*)&z); }




inline complex_double HPL1(const int& n1, const complex_double& z)
{ return hpl1_((int*)&n1, (ccomplex_double*)&z); }

inline complex_double HPL2(const int& n1, const int& n2, const complex_double& z)
{ return hpl2_((int*)&n1, (int*)&n2, (ccomplex_double*)&z); }

inline complex_double HPL3(const int& n1, const int& n2, const int& n3, const complex_double& z)
{ return hpl3_((int*)&n1, (int*)&n2, (int*)&n3, (ccomplex_double*)&z); }

inline complex_double HPL4(const int& n1, const int& n2, const int& n3, const int& n4, const complex_double& z)
{ return hpl4_((int*)&n1, (int*)&n2, (int*)&n3, (int*)&n4, (ccomplex_double*)&z); }

inline double HPL2real(const int& n1, const int& n2, const double& xr, const double& xi)
{ return hpl2real_((int*)&n1, (int*)&n2, (double*)&xr, (double*)&xi); }

inline double HPL2im(const int& n1, const int& n2, const double& xr, const double& xi)
{ return hpl2im_((int*)&n1, (int*)&n2, (double*)&xr, (double*)&xi); }

inline double HPL3real(const int& n1, const int& n2, const int& n3, const double& xr, const double& xi)
{ return hpl3real_((int*)&n1, (int*)&n2, (int*)&n3, (double*)&xr, (double*)&xi); }

inline double HPL3im(const int& n1, const int& n2, const int& n3, const double& xr, const double& xi)
{ return hpl3im_((int*)&n1, (int*)&n2, (int*)&n3, (double*)&xr, (double*)&xi); }

inline double HPL4real(const int& n1, const int& n2, const int& n3, const int& n4, const double& xr, const double& xi)
{ return hpl4real_((int*)&n1, (int*)&n2, (int*)&n3, (int*)&n4, (double*)&xr, (double*)&xi); }

inline double HPL4im(const int& n1, const int& n2,const int& n3, const int& n4,  const double& xr, const double& xi)
{ return hpl4im_((int*)&n1, (int*)&n2, (int*)&n3, (int*)&n4, (double*)&xr, (double*)&xi); }

#endif

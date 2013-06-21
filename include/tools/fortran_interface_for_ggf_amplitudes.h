/* Header file for the inclusion of fortran  routines related to
 the (N)NLO amplitudes  in C/C++ programs.
 
 They must have been compiled with gfortran or a compiler that have
 the same external symbols convention.
 
 Based on a similar file from chaplin.h by:
 
 2011, Romain Mueller, muellrom@phys.ethz.ch
 */

#ifndef FORTRAN_INTERFACE_TO_GGF_AMPLITUDES_H
#define FORTRAN_INTERFACE_TO_GGF_AMPLITUDES_H

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




#define fortran_definition_macro_for_ggf(x) void x(int*/*sector*/,int*/*pole*/,double*/*shat*/,double*/*x1*/, double*/*x2*/,double*/*z*/,double*/*log(mh^2 / muf^2)*/, double*/*wd (the weight) */, double*/*nf (casted to double!)*/, double*/*lambda1*/, double*/*lambda2*/,double*/*lambda3*/, double*/*lambda4*/,double*/*fff (dummy array that Franz might be filling with inclusive pieces)*/)

#define cpp_definition_macro_for_ggf(XXX,YYY) inline void XXX(const int& sector, const int & pole,const double &s,const double &x1,const double &x2,const double &z,const double & lh,const double & wd,const double & nf, const double & lambda1, const double & lambda2, const double & lambda3,const double & lambda4,const double &res_z){YYY((int*)&sector, (int*)&pole, (double*)&s,(double*)&x1,(double*)&x2,(double*)&z,(double*)&lh,(double*)&wd, (double*)&nf,(double*)&lambda1, (double*)&lambda2, (double*)&lambda3, (double*)&lambda4,(double*)&res_z );}



/* Fortran declarations
 ! gfortran external symbols convention */
#ifdef __cplusplus
extern "C" {
#endif
     //: NLO real
     fortran_definition_macro_for_ggf(rgg2ght1_);
     fortran_definition_macro_for_ggf(rqg2qht1_);
     fortran_definition_macro_for_ggf(rgq2qht1_);
     fortran_definition_macro_for_ggf(rqqbar2ght1_);
     
     //: NNLO RR & RV
     //: q1 q2 channel
     fortran_definition_macro_for_ggf(rrq1q22q1q2ht1_);
     fortran_definition_macro_for_ggf(rrq1q22q1q2ht2_);
     //: q q channel
     fortran_definition_macro_for_ggf(rrqq2qqht1_);
     fortran_definition_macro_for_ggf(rrqq2qqht2_);
     fortran_definition_macro_for_ggf(rrqq2qqht3_);
     
     //: q qbar channel
     fortran_definition_macro_for_ggf(rvqqbar2ght1_);
     fortran_definition_macro_for_ggf(rrqqbar2qqbarht1_);
     fortran_definition_macro_for_ggf(rrqqbar2qqbarht2_);
     fortran_definition_macro_for_ggf(rrqqbar2qqbarht3_);
     fortran_definition_macro_for_ggf(rrqqbar2qqbarht4_);
     fortran_definition_macro_for_ggf(rrqqbar2qqbarht5_);
     fortran_definition_macro_for_ggf(rrqqbar2qqbarht6_);
     
     fortran_definition_macro_for_ggf(rrqqbar2gght1_);
     fortran_definition_macro_for_ggf(rrqqbar2gght2_);
     fortran_definition_macro_for_ggf(rrqqbar2gght3_);
     fortran_definition_macro_for_ggf(rrqqbar2gght4_);
     fortran_definition_macro_for_ggf(rrqqbar2gght5_);
     fortran_definition_macro_for_ggf(rrqqbar2gght6_);

     fortran_definition_macro_for_ggf(rrq1q1bar2q2q2barht1_);
     fortran_definition_macro_for_ggf(rrq1q1bar2q2q2barht2_);
     
     //: gluon quark channel
     fortran_definition_macro_for_ggf(rrgq2qght1_);
     fortran_definition_macro_for_ggf(rrgq2qght2_);
     fortran_definition_macro_for_ggf(rrgq2qght3_);
     fortran_definition_macro_for_ggf(rrgq2qght4_);
     fortran_definition_macro_for_ggf(rrgq2qght5_);
     fortran_definition_macro_for_ggf(rrgq2qght6_);
     fortran_definition_macro_for_ggf(rrgq2qght7_);
     fortran_definition_macro_for_ggf(rrgq2qght8_);
     fortran_definition_macro_for_ggf(rrgq2qght9_);
     fortran_definition_macro_for_ggf(rrgq2qght10_);
     fortran_definition_macro_for_ggf(rrgq2qght11_);
     fortran_definition_macro_for_ggf(rrgq2qght12_);
     fortran_definition_macro_for_ggf(rrgq2qght13_);
     fortran_definition_macro_for_ggf(rrgq2qght14_);
     fortran_definition_macro_for_ggf(rrgq2qght15_);

     fortran_definition_macro_for_ggf(rvgq2qht1_);

     //: quark gluon channel
     fortran_definition_macro_for_ggf(rrqg2qght1_);
     fortran_definition_macro_for_ggf(rrqg2qght2_);
     fortran_definition_macro_for_ggf(rrqg2qght3_);
     fortran_definition_macro_for_ggf(rrqg2qght4_);
     fortran_definition_macro_for_ggf(rrqg2qght5_);
     fortran_definition_macro_for_ggf(rrqg2qght6_);
     fortran_definition_macro_for_ggf(rrqg2qght7_);
     fortran_definition_macro_for_ggf(rrqg2qght8_);
     fortran_definition_macro_for_ggf(rrqg2qght9_);
     fortran_definition_macro_for_ggf(rrqg2qght10_);
     fortran_definition_macro_for_ggf(rrqg2qght11_);
     fortran_definition_macro_for_ggf(rrqg2qght12_);
     fortran_definition_macro_for_ggf(rrqg2qght13_);
     fortran_definition_macro_for_ggf(rrqg2qght14_);
     fortran_definition_macro_for_ggf(rrqg2qght15_);
     
     fortran_definition_macro_for_ggf(rvqg2qht1_);
     
     //: gluon gluon channel
     fortran_definition_macro_for_ggf(rrgg2gght1_);
     fortran_definition_macro_for_ggf(rrgg2gght2_);
     fortran_definition_macro_for_ggf(rrgg2gght3_);
     fortran_definition_macro_for_ggf(rrgg2gght4_);
     fortran_definition_macro_for_ggf(rrgg2gght5_);
     fortran_definition_macro_for_ggf(rrgg2gght6_);
     fortran_definition_macro_for_ggf(rrgg2gght7_);
     fortran_definition_macro_for_ggf(rrgg2gght8_);
     fortran_definition_macro_for_ggf(rrgg2gght9_);
     fortran_definition_macro_for_ggf(rrgg2gght10_);
     fortran_definition_macro_for_ggf(rrgg2gght11_);
     fortran_definition_macro_for_ggf(rrgg2gght12_);
     fortran_definition_macro_for_ggf(rrgg2gght13_);
     fortran_definition_macro_for_ggf(rrgg2gght14_);
     fortran_definition_macro_for_ggf(rrgg2gght15_);
     fortran_definition_macro_for_ggf(rrgg2gght16_);

     fortran_definition_macro_for_ggf(rrgg2qqbarht1_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht2_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht3_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht4_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht5_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht6_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht7_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht8_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht9_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht10_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht11_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht12_);
     fortran_definition_macro_for_ggf(rrgg2qqbarht13_);
     fortran_definition_macro_for_ggf(rvgg2ght1_);
     fortran_definition_macro_for_ggf(rvgg2ght2_);
     fortran_definition_macro_for_ggf(rvgg2ght4_);

     
     
     


#ifdef __cplusplus
}
#endif

cpp_definition_macro_for_ggf(rgg2ght1,rgg2ght1_)
cpp_definition_macro_for_ggf(rqg2qht1,rqg2qht1_)
cpp_definition_macro_for_ggf(rgq2qht1,rgq2qht1_)
cpp_definition_macro_for_ggf(rqqbar2ght1,rqqbar2ght1_)

cpp_definition_macro_for_ggf(rrq1q22q1q2ht1,rrq1q22q1q2ht1_)
cpp_definition_macro_for_ggf(rrq1q22q1q2ht2,rrq1q22q1q2ht2_)
cpp_definition_macro_for_ggf(rrqq2qqht1,rrqq2qqht1_)
cpp_definition_macro_for_ggf(rrqq2qqht2,rrqq2qqht2_)
cpp_definition_macro_for_ggf(rrqq2qqht3,rrqq2qqht3_)

cpp_definition_macro_for_ggf(rvqqbar2ght1,rvqqbar2ght1_)
cpp_definition_macro_for_ggf(rrqqbar2qqbarht1,rrqqbar2qqbarht1_)
cpp_definition_macro_for_ggf(rrqqbar2qqbarht2,rrqqbar2qqbarht2_)
cpp_definition_macro_for_ggf(rrqqbar2qqbarht3,rrqqbar2qqbarht3_)
cpp_definition_macro_for_ggf(rrqqbar2qqbarht4,rrqqbar2qqbarht4_)
cpp_definition_macro_for_ggf(rrqqbar2qqbarht5,rrqqbar2qqbarht5_)
cpp_definition_macro_for_ggf(rrqqbar2qqbarht6,rrqqbar2qqbarht6_)
cpp_definition_macro_for_ggf(rrqqbar2gght1,rrqqbar2gght1_)
cpp_definition_macro_for_ggf(rrqqbar2gght2,rrqqbar2gght2_)
cpp_definition_macro_for_ggf(rrqqbar2gght3,rrqqbar2gght3_)
cpp_definition_macro_for_ggf(rrqqbar2gght4,rrqqbar2gght4_)
cpp_definition_macro_for_ggf(rrqqbar2gght5,rrqqbar2gght5_)
cpp_definition_macro_for_ggf(rrqqbar2gght6,rrqqbar2gght6_)
cpp_definition_macro_for_ggf(rrq1q1bar2q2q2barht1,rrq1q1bar2q2q2barht1_)
cpp_definition_macro_for_ggf(rrq1q1bar2q2q2barht2,rrq1q1bar2q2q2barht2_)

cpp_definition_macro_for_ggf(rrqg2qght1,rrqg2qght1_)
cpp_definition_macro_for_ggf(rrqg2qght2,rrqg2qght2_)
cpp_definition_macro_for_ggf(rrqg2qght3,rrqg2qght3_)
cpp_definition_macro_for_ggf(rrqg2qght4,rrqg2qght4_)
cpp_definition_macro_for_ggf(rrqg2qght5,rrqg2qght5_)
cpp_definition_macro_for_ggf(rrqg2qght6,rrqg2qght6_)
cpp_definition_macro_for_ggf(rrqg2qght7,rrqg2qght7_)
cpp_definition_macro_for_ggf(rrqg2qght8,rrqg2qght8_)
cpp_definition_macro_for_ggf(rrqg2qght9,rrqg2qght9_)
cpp_definition_macro_for_ggf(rrqg2qght10,rrqg2qght10_)
cpp_definition_macro_for_ggf(rrqg2qght11,rrqg2qght11_)
cpp_definition_macro_for_ggf(rrqg2qght12,rrqg2qght12_)
cpp_definition_macro_for_ggf(rrqg2qght13,rrqg2qght13_)
cpp_definition_macro_for_ggf(rrqg2qght14,rrqg2qght14_)
cpp_definition_macro_for_ggf(rrqg2qght15,rrqg2qght15_)

cpp_definition_macro_for_ggf(rvqg2qht1,rvqg2qht1_)


cpp_definition_macro_for_ggf(rrgq2qght1,rrgq2qght1_)
cpp_definition_macro_for_ggf(rrgq2qght2,rrgq2qght2_)
cpp_definition_macro_for_ggf(rrgq2qght3,rrgq2qght3_)
cpp_definition_macro_for_ggf(rrgq2qght4,rrgq2qght4_)
cpp_definition_macro_for_ggf(rrgq2qght5,rrgq2qght5_)
cpp_definition_macro_for_ggf(rrgq2qght6,rrgq2qght6_)
cpp_definition_macro_for_ggf(rrgq2qght7,rrgq2qght7_)
cpp_definition_macro_for_ggf(rrgq2qght8,rrgq2qght8_)
cpp_definition_macro_for_ggf(rrgq2qght9,rrgq2qght9_)
cpp_definition_macro_for_ggf(rrgq2qght10,rrgq2qght10_)
cpp_definition_macro_for_ggf(rrgq2qght11,rrgq2qght11_)
cpp_definition_macro_for_ggf(rrgq2qght12,rrgq2qght12_)
cpp_definition_macro_for_ggf(rrgq2qght13,rrgq2qght13_)
cpp_definition_macro_for_ggf(rrgq2qght14,rrgq2qght14_)
cpp_definition_macro_for_ggf(rrgq2qght15,rrgq2qght15_)

cpp_definition_macro_for_ggf(rvgq2qht1,rvgq2qht1_)



cpp_definition_macro_for_ggf(rrgg2gght1,rrgg2gght1_)
cpp_definition_macro_for_ggf(rrgg2gght2,rrgg2gght2_)
cpp_definition_macro_for_ggf(rrgg2gght3,rrgg2gght3_)
cpp_definition_macro_for_ggf(rrgg2gght4,rrgg2gght4_)
cpp_definition_macro_for_ggf(rrgg2gght5,rrgg2gght5_)
cpp_definition_macro_for_ggf(rrgg2gght6,rrgg2gght6_)
cpp_definition_macro_for_ggf(rrgg2gght7,rrgg2gght7_)
cpp_definition_macro_for_ggf(rrgg2gght8,rrgg2gght8_)
cpp_definition_macro_for_ggf(rrgg2gght9,rrgg2gght9_)
cpp_definition_macro_for_ggf(rrgg2gght10,rrgg2gght10_)
cpp_definition_macro_for_ggf(rrgg2gght11,rrgg2gght11_)
cpp_definition_macro_for_ggf(rrgg2gght12,rrgg2gght12_)
cpp_definition_macro_for_ggf(rrgg2gght13,rrgg2gght13_)
cpp_definition_macro_for_ggf(rrgg2gght14,rrgg2gght14_)
cpp_definition_macro_for_ggf(rrgg2gght15,rrgg2gght15_)
cpp_definition_macro_for_ggf(rrgg2gght16,rrgg2gght16_)

cpp_definition_macro_for_ggf(rrgg2qqbarht1,rrgg2qqbarht1_)
cpp_definition_macro_for_ggf(rrgg2qqbarht2,rrgg2qqbarht2_)
cpp_definition_macro_for_ggf(rrgg2qqbarht3,rrgg2qqbarht3_)
cpp_definition_macro_for_ggf(rrgg2qqbarht4,rrgg2qqbarht4_)
cpp_definition_macro_for_ggf(rrgg2qqbarht5,rrgg2qqbarht5_)
cpp_definition_macro_for_ggf(rrgg2qqbarht6,rrgg2qqbarht6_)
cpp_definition_macro_for_ggf(rrgg2qqbarht7,rrgg2qqbarht7_)
cpp_definition_macro_for_ggf(rrgg2qqbarht8,rrgg2qqbarht8_)
cpp_definition_macro_for_ggf(rrgg2qqbarht9,rrgg2qqbarht9_)
cpp_definition_macro_for_ggf(rrgg2qqbarht10,rrgg2qqbarht10_)
cpp_definition_macro_for_ggf(rrgg2qqbarht11,rrgg2qqbarht11_)
cpp_definition_macro_for_ggf(rrgg2qqbarht12,rrgg2qqbarht12_)
cpp_definition_macro_for_ggf(rrgg2qqbarht13,rrgg2qqbarht13_)

cpp_definition_macro_for_ggf(rvgg2ght1,rvgg2ght1_)
cpp_definition_macro_for_ggf(rvgg2ght2,rvgg2ght2_)
cpp_definition_macro_for_ggf(rvgg2ght4,rvgg2ght4_)








#endif
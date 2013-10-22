#ifndef EHIXS_GLUON_FUSION_NLOEXACTMATRIXELEMENTS_H
#define EHIXS_GLUON_FUSION_NLOEXACTMATRIXELEMENTS_H
complex<double> ggf_exact_virtual_ep0(const complex<double> & x,
                                      const double& scheme_dependent_coeff);
complex<double> F2lb(const complex<double> & x);




typedef complex<double> (*ptr_to_Aq)(const double& z,
                                    const double& lambda,
                                    const complex<double>& M,
                                    const double& QQQ);

double abs_sq_of_sum_over_quarks_of(ptr_to_Aq,const double & z,
                                    const double & lambda,CModel* Model);
double sum_of_abs_sq_of_Aqi(const double &z,const double & lambda,CModel* Model);

complex<double> born_exact_summed_over_quarks(CModel* Model);
complex<double> born(complex<double> x);


double sum_of_abs_sq_of_Aqqgh(const double& z,CModel* Model);

complex<double> bubble(const complex<double>& x);
complex<double> bubf(const complex<double>& x1,const complex<double>& x2);
complex<double> triaf(const complex<double>& x);
complex<double> Aqqgh_cpp(const double& z,
                          const complex<double>&x );
#endif
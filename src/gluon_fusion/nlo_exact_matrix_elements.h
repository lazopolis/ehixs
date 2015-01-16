#ifndef EHIXS_GLUON_FUSION_NLOEXACTMATRIXELEMENTS_H
#define EHIXS_GLUON_FUSION_NLOEXACTMATRIXELEMENTS_H

#include <complex>
using namespace std;
class CModel;

namespace h_exact {
    complex<double> ggf_exact_virtual_ep0(const complex<double> & x,
                                      const double& scheme_dependent_coeff);
    double gg_reg_exact_nlo(const double& z, const double& lambda,
                            const double& L,CModel* model);
    double qg_reg_exact_nlo(const double& z, const double& lambda,
                            const double& L,CModel* model);
    double qqb_reg_exact_nlo(const double& z, const double& lambda,
                            const double& L,CModel* model);
    complex<double> F2lb(const complex<double> & x);




    typedef complex<double> (*ptr_to_Aq)(const double& z,
                                    const double& lambda,
                                    const complex<double>& M,
                                    const double& QQQ);

    double abs_sq_of_sum_over_quarks_of(ptr_to_Aq,const double & z,
                                    const double & lambda,CModel* Model);
    double sum_of_abs_sq_of_Aqi(const double &z,const double & lambda,
                                CModel* Model);
    double sum_of_abs_sq_of_Aqqgh(const double& z,CModel* Model);
    complex<double> sum_of_Aqqgh(const double& y,CModel* Model);
    
    complex<double> Aqqgh_cpp(const double& z,
                              const complex<double>&x );
    
    
    complex<double> born_exact_summed_over_quarks(CModel* Model);
    complex<double> born(complex<double> x);


    complex<double> Aq1(const double& z, const double& lambda,
                        const complex<double> & m, const double& mh);
    complex<double> Aq2a(const double& z,const double& lambda,
                         const complex<double>& m,const double& mh);
    complex<double> Aq2b(const double& z,const double& lambda,
                         const complex<double>& m,const double& mh);
    complex<double> Aq2c(const double& z,const double& lambda,
                         const complex<double>& m,const double& mh);
    complex<double> Aq2(const double& s,const double& t,const double& u,const complex<double>& m);

    // coefficients of Aq1
    double CCbox(const double& s, const double& t, const double& u);
    complex<double> CCtri(const complex<double>& m, const double& s,
                          const double& t, const double& u);
    double CCbub(const double& s, const double& t, const double& u);

    // masters
    complex<double> boxf(const complex<double>& m,const double& s,const double& t,const double& u);
    complex<double> triaf(const complex<double>& m, const double& s);
    complex<double> spec_triaf(const complex<double>& x);

    complex<double> bubf(const complex<double>& m, const double& s1, const double& s2);
    complex<double> bubble(const complex<double>& m, const double& s);
    complex<double> bub_aux(const complex<double>& x);
    
    complex<double> Box_Intv2(const double& s,const double& t,const double & u,
                              const complex<double> & m,const double& Q);
    
    
    
};
#endif
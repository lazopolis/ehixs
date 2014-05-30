
#ifndef GLUON_FUSION_EXACT_COEFFICIENTS
#define GLUON_FUSION_EXACT_COEFFICIENTS 

#include "model.h"
#include<complex>
#include<vector>
using namespace std;


class GluonFusionExactCoefficients
{
public://methods
    GluonFusionExactCoefficients(const CModel&);
    double LO_epsilon(int m){return LO_exact_coefficient[m];}
    double NLO_epsilon(int m){return NLO_soft_exact_coefficient[m];}
    
private://data
    vector<double> LO_exact_coefficient;
    vector<double> NLO_soft_exact_coefficient;
    CModel Model;
private://emthods
    double NLO_soft_exact_e0();
    complex<double> born_e2(complex<double> x);
    complex<double> born_e(complex<double> x);
    complex<double> born(complex<double> x);
    double LO_exact_e2();
    double LO_exact_e1();
    double LO_exact_e0();
};

#endif



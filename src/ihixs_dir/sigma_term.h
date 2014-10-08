

#ifndef SIGMA_TERM_H
#define SIGMA_TERM_H

#include "string"
#include "vector"
using namespace std;
#include "luminosity.h"
#include "luminosity_integrals.h"
#include "wilson_coefficients.h"

class SigmaTerm{
public:
    SigmaTerm(const string& thetype, const vector<double>& val,
              LuminosityIntegral* lumi_int)
    :_type(thetype),_val(val),_err(val),_lumi_int(lumi_int){};
    
    void ConfigureLumi(NewLuminosity* lumi,const double& tau);
    void CallVegas();
    double operator[](int i);
    void multiply(const double& c);
    void wc_expansion(const WilsonCoefficient& wc);
    
    void multiply_by_as_pi(const double& as_pi);
    
    friend ostream& operator<<(ostream&, const SigmaTerm&);
    string type();
private:
    string _type;//delta, plus, reg
    vector<double> _val;
    vector<double> _err;
    LuminosityIntegral* _lumi_int;
};

#endif

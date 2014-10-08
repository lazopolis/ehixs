#ifndef INCLUSIVE_PROCESS_H
#define INCLUSIVE_PROCESS_H

#include "user_interface.h"
//#include "vegas_adaptor.h"
#include "luminosity.h"
#include "wilson_coefficients.h"
#include "sigma_term.h"
//#include "constants.h"
#include "model.h"
#include <vector>
using namespace std;

#include "higgs_eft.h"


class InclusiveProcess
{
public:
    InclusiveProcess(const UserInterface& UI);
    void Evaluate();
    
    double CoefficientAlphaS(int i);
    friend ostream& operator<<(ostream&, const InclusiveProcess&);

    
    double sigma(const string& type_id,int i);

private:
    
private:
    CModel _model;
    double _as_pi;
    NewLuminosity* _lumi;
    
    WilsonCoefficient _wc;
    double _prefactor;
    
    double _log_muf_over_mt_sq;
    double _log_muf_mh_sq;
    double _log_mur_over_muf_sq;
    
    vector<SigmaTerm*> _sigma;

    
    vector<double> evolve_xs_to_mur(const vector<double>& x);
    
    
    vector<double> wc_x_n(const WilsonCoefficient& wc, const vector<double> x);

};


#endif
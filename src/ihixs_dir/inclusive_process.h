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
#include "luminosity_integrals.h"

class InclusiveProcess
{
public:
    InclusiveProcess(const UserInterface& UI);
    void Evaluate();
    void Evaluate(const string& type);
    void Evaluate(SigmaTerm* term);
    double CoefficientAlphaS(int i);
    double CoefficientAlphaSError(int i);

    ResultPair TotalCentral();
    friend ostream& operator<<(ostream&, const InclusiveProcess&);

    


    
private:
    UserInterface _UI;
    bool _scale_variation;
    CModel _model;
    double _as_pi;
    NewLuminosity* _lumi;
    
    WilsonCoefficient _wc;
    double _prefactor;
    double _tau;
    vector<double> _mur_vector;
    double _current_mur;
    bool _is_central_scale;
    
    double _log_muf_over_mt_sq;
    double _log_muf_mh_sq;
    double _log_mur_over_muf_sq;
    
    vector<SigmaTerm*> _sigma;

    int _int_qcd_perturbative_order;
    
    SigmaTerm* find_term(const string& type);
    
    void SetMurDependentParameters(const double& mur);
    void Truncate();

};


#endif
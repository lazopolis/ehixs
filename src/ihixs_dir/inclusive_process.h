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
class Channel
{
public:
    int size() const {return _terms.size();}
    SigmaTerm* Term(int i) const {return _terms[i];}
    void Truncate(int i);
    AsSeries Sum();
    ResultPair Result();
    ResultPair CoeffAs(int m){return Sum().term_of_order(m);}
    string Name(){return _name;}
    friend ostream& operator<<(ostream& stream, const Channel& ch);
protected:
    vector<SigmaTerm*> _terms;
    string _name;
};

class HiggsGGFChannelGG: public Channel
{
public:
    HiggsGGFChannelGG(const double& L);
};

class HiggsGGFChannelQG: public Channel
{
public:
    HiggsGGFChannelQG(const double& L);
};

class HiggsGGFChannelQQBAR: public Channel
{
public:
    HiggsGGFChannelQQBAR(const double& L);
};

class HiggsGGFChannelQQ: public Channel
{
public:
    HiggsGGFChannelQQ(const double& L);
};

class HiggsGGFChannelQ1Q2: public Channel
{
public:
    HiggsGGFChannelQ1Q2(const double& L);
};

class InclusiveProcess
{
public:
    InclusiveProcess(const UserInterface& UI);
    void Evaluate();
    void Evaluate(const string& type);
    void Evaluate(SigmaTerm* term);
    double CoefficientAlphaS(int i);
    double CoefficientAlphaSError(int i);
    ResultPair CoefficientAlphaSResult(int as_order);
    string ChannelBreakdown();
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
    
    vector<Channel*> _channels;
   // vector<SigmaTerm*> _sigma;

    int _int_qcd_perturbative_order;
    
    SigmaTerm* find_term(const string& type);
    
    void SetMurDependentParameters(const double& mur);
    void Truncate();
    
    bool _is_enhanced_eft;
    double _exact_LO_coefficient;

};


#endif
#ifndef INCLUSIVE_PROCESS_H
#define INCLUSIVE_PROCESS_H

#include "user_interface.h"
//#include "vegas_adaptor.h"
#include "luminosity.h"
#include "wilson_coefficients.h"
#include "gluon_fusion_ew_coefficients.h"
#include "sigma_term.h"
//#include "constants.h"
#include "model.h"
#include <vector>
using namespace std;

#include "higgs_eft.h"
#include "luminosity_integrals.h"
#include "channel.h"


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
    string DetailedResults();
    string OneLineResults();
    string OneLineResultsTopOnly();
    ResultPair TotalCentral();
    friend ostream& operator<<(ostream&, const InclusiveProcess&);

    ResultPair EffectiveRescaledLO();
    ResultPair EffectiveRescaledNLO();
    ResultPair EffectiveRescaledNNLO();
    ResultPair EffectiveRescaledN3LOLowerAndScale();

    ResultPair NextToSoftLog345_at_L_0();
    ResultPair NextToSoftLog012_at_L_0();
    ResultPair FullLog345_at_L_0();
    ResultPair BeyondSoft();
    ResultPair DeltaSoft();
    ResultPair DeltaQCDExact();
    ResultPair DeltaQCDTopOnly();
    AsSeries EwCorrections();
    ResultPair ExactQCDEffectsLO();
    ResultPair ExactQCDEffectsNLO();
    ResultPair ExactQCDEffectsLO_toponly();
    ResultPair ExactQCDEffectsNLO_toponly();
    double RescalingCoeff(){return _top_only_LO_coefficient;}
    
    SigmaTerm* find_term(const string& type);
private:
    UserInterface _UI;
    CModel _model;
    double _as_pi;
    Luminosity* _lumi;
    
    WilsonCoefficient _wc;
    GluonFusionEWCoefficients* _ew;
    AsSeries _ew_lambda_series;
    
    double _prefactor;
    double _tau;
    double _current_mur;
    
    double _log_muf_over_mt_sq;
    double _log_muf_mh_sq;
    double _log_mur_over_muf_sq;
    
    vector<Channel*> _channels;

    void AddTerm(const string&,
                 const string&,
                 const AsSeries&,
                 LuminosityIntegral*,
                 const string&,
                 const string&);
    
    void AddTerm(const string&,
                 const string&,
                 const AsSeries&,
                 LuminosityIntegral*,
                 const string&);
    
    void AddTerm(const string&,
                 const string&,
                 const AsSeries&,
                 LuminosityIntegral*);
    
    int _int_qcd_perturbative_order;
    
    
    Channel* find_channel(const string& type);
    void SetMurDependentParameters(const double& mur);
    void Truncate();
    
    double _top_only_LO_coefficient;

};


#endif
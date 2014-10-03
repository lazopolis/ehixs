#include "bottom_fusion_me.h"


// BottomFusion_bb_LO

void BottomFusion_bb_LO::generateEvents(vector<double>& randoms)
{
    _pg(randoms);
    const double w = _prefactor * _factor;
    _eventBox->push_back(Event(w,_p));
    return;
}

template<>
const SectorInfo XSectionMaker<BottomFusion_bb_LO>::_info(
                                                   "Born",
                                                    InitialStateFlavors(QCD::b, QCD::bbar),
                                                    0,
                                                    1
                                                   );

// BottomFusion_bb_NLO_real

void BottomFusion_bb_NLO_real::generateEvents(vector<double>& randoms)
{
    // Setting up convenient variable names
    double& lambdaR = randoms[randoms.size()-2];
    const double& z = randoms.back();
    const double lambda = lambdaR;
    const double w = _prefactor * _factor * regularME(z);
    // Generating main event
    _pg(randoms);
    _eventBox->push_back(Event(w/(lambda*(1.-lambda)),_p));
    // Generating collinear counterterms
    lambdaR = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(-w/(1.-lambda),_p));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(-w/lambda,_p));
    // There should be no variables left, although not checking for efficiency
    // Cleanup for safety reasons
    randoms.clear();
    return;
}

template<>
const SectorInfo XSectionMaker<BottomFusion_bb_NLO_real>::_info(
                                                          "NLO real",
                                                          InitialStateFlavors(QCD::b, QCD::bbar),
                                                          1,
                                                          4
                                                          );

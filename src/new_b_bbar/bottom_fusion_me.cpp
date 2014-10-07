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
    double& lambda = randoms[randoms.size()-2];
    const double z = _tau/(_x.x1*_x.x2);
    const double w = _prefactor * _factor * regularME(z);
    const double w_l = w/lambda;
    const double w_llbar = w_l/(1.-lambda);
    // Hacking known analytical zero by hand
    const double w_lbar = w_llbar-w_l;
    // Generating main event
    _pg(randoms);
    _eventBox->push_back(Event(w_llbar,_p));
    // Generating collinear counterterms
    lambda = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(-w_l,_p));
    lambda = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(-w_lbar,_p));
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

// BottomFusion_bb_NLO_soft

void BottomFusion_bb_NLO_soft::generateEvents(vector<double>& randoms)
{
    // Generating main event
    const double z = _tau/(_x.x1*_x.x2);
    double w = //(1. - consts::pi_square/3.)
                     (f0(z)-f0(1.))/(1.-z) //- log(1.-_tau)
    + (f1(z)-f1(1.))*log(1.-z)/(1.-z) ;//- 0.5*pow(log(1.-_tau),2);
    w *= _prefactor * _factor;
    cout << "z = " << z << ", w = " << w << endl;
    _pg(randoms);
    _eventBox->push_back(Event(w,_p));
    // There should be no variables in randoms, although not checking for efficiency
    return;
}

template<>
const SectorInfo XSectionMaker<BottomFusion_bb_NLO_soft>::_info(
                                                                "NLO soft",
                                                                InitialStateFlavors(QCD::b, QCD::bbar),
                                                                1,
                                                                1
                                                                );

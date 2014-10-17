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

// BottomFusion_bb_NLO_hard

const double BottomFusion_bb_NLO_hard::_cutoff = 100*DBL_EPSILON;

void BottomFusion_bb_NLO_hard::generateEvents(vector<double>& randoms)
{
    // Setting up convenient variable names
    double& lambdaR = randoms[randoms.size()-2];
    const double lambda = lambdaR;
    const double z = _tau/(_x.x1*_x.x2);
    const double w = _prefactor * _factor * regularME(z);
    // Technical cutoff
    if ( lambda < _cutoff || 1.-lambda < _cutoff || 1.-z < _cutoff )
    {
        _eventBox->push_back(Event());
        return;
    }
    // Generating main event
    _pg(randoms);
    _eventBox->push_back(Event(w/(lambda*(1.-lambda)),_p));
    // Generating collinear counterterms
    lambdaR = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(-w/lambda,_p));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(-w/(1.-lambda),_p));
    // There should be no variables left, although not checking for efficiency
    // Cleanup for safety reasons
    randoms.clear();
    return;
}

template<>
const SectorInfo XSectionMaker<BottomFusion_bb_NLO_hard>::_info(
                                                          "NLO hard",
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
double BottomFusion_bb_NNLO_RV::lambdat=0.5;

// BottomFusion_bb_NNLO_RV

void BottomFusion_bb_NNLO_RV::generateEvents(vector<double>& randoms)
{
    // Setting up convenient variable names
    double& lambdaR = randoms[randoms.size()-2];
    const double lambda = lambdaR;
    const double z = _tau/(_x.x1*_x.x2);
    const double w = _prefactor * _factor;
    const double myFull = full(z,lambda);
    const double myColl1 = coll(z,lambda);
    const double myColl2 = coll(z,1.-lambda);
    // Sanity check
    if (!isfinite(myFull) || !isfinite(myColl1) || !isfinite(myColl2))
    {
        cout.precision(16);
        cout << "\n\n[ERROR] Values \tlambda = " << lambda << ",\tz = " << z << "\t produced a non-finite weight.\n";
        throw;
    }
    // Generating main event
    _pg(randoms);
    _eventBox->push_back(Event(w*full(z,lambda),_p));
    // Generating collinear counterterms
    lambdaR = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*coll(z,lambda),_p));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*coll(z,1.-lambda),_p));
    /*if (z>lambdat)
    {
        cout.precision(15);
        cout << lambda << "\t" << z << "\t" << myFull << "\t" << myColl1 << "\t" << myColl2
            << "\t" << myFull-myColl1-myColl2 << endl;
        lambdat=z;
    }*/

    double _1mz = 0.1;
    const double foolambda = 0.234;
    while (_1mz>DBL_EPSILON)
    {
        _1mz/=2.;
        cout << _1mz << "\t" << (full(1.-_1mz,foolambda)-coll(1.-_1mz,foolambda)-coll(1.-_1mz,1.-foolambda))*_1mz/log(_1mz) << endl;
    }
    exit(0);

    // There should be no variables left, although not checking for efficiency
    // Cleanup for safety reasons
    randoms.clear();
    return;
}

template<>
const SectorInfo XSectionMaker<BottomFusion_bb_NNLO_RV>::_info(
                                                                "NNLO real-virtual",
                                                                InitialStateFlavors(QCD::b, QCD::bbar),
                                                                2,
                                                                4
                                                                );


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
//double minlambda = 1.;

// BottomFusion_bb_NLO_hard

void BottomFusion_bb_NLO_hard::generateEvents(vector<double>& randoms)
{
    // Setting up convenient variable names
    double& lambdaR = randoms[randoms.size()-2];
    const double lambda = lambdaR;
    const double z = _tau/(_x.x1*_x.x2);
    const double w = _prefactor * _factor;
    // Generating main event
    _pg(randoms);
    _eventBox->push_back(Event(w*bb2Hg<0,0>(z,lambda),_p));
    // Generating collinear counterterms
    lambdaR = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*bb2H<0,0>()*(CounterForge::Pqq<0>(z,lambda)).getCoefficient(0),_p));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*bb2H<0,0>()*(CounterForge::Pqq<0>(z,1.-lambda)).getCoefficient(0),_p));
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

// BottomFusion_bb_NNLO_RV

void BottomFusion_bb_NNLO_RV::generateEvents(vector<double>& randoms)
{
    // Setting up convenient variable names
    double& lambdaR = randoms[randoms.size()-2];
    const double lambda = lambdaR;
    const double z = _tau/(_x.x1*_x.x2);
    const double w = _prefactor * _factor;
    // Generating main event
    _pg(randoms);
    _eventBox->push_back(Event(w*bb2Hg<1,0>(z,lambda),_p));
    // Subtracting collinear counterterms
    lambdaR = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*coll(z,lambda),_p));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*coll(z,1.-lambda),_p));
    // Subtracting soft counterterm
    lambdaR = lambda;
    _x.x1 *= sqrt(z);
    _x.x2 *= sqrt(z);
    _pg(randoms);
    _eventBox->push_back(Event(-w*soft(z,lambda),_p));
    // Adding back double limit
    lambdaR = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(w*softcoll(z,lambda),_p));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(w*softcoll(z,1.-lambda),_p));
    // Restoring original Bjorkens
    _x.x1 /= sqrt(z);
    _x.x2 /= sqrt(z);
    // No variables left, although not checking for efficiency
    // Cleanup for safety reasons
    randoms.clear();
    return;
}

double BottomFusion_bb_NNLO_RV::coll(const double& z, const double& lambda)
{
    // Unjustified factor of -1!!!
    // Speed issue: about 2 times slower than direct expression
    return -2.*(
                Expansion<Parameter::epsilon,double>::exp(-log(lambda*(1.-z)/z))*
                CounterTerm->fastPqq<1>(z,lambda)*bb2H<0>()+
                CounterForge::Pqq<0>(z,lambda)*bb2H<1>()
                ).getCoefficient(0);
}

double BottomFusion_bb_NNLO_RV::soft(const double& z, const double& lambda)
{
    const double _1ml = 1.-lambda;
    const double l1 = log(_1ml);
    const double l2 = log(lambda);
    const double l3 = log(1.-z);
    return 32.*QCD::CA*(
                        6.*l1*l2 + 12.*(l1+l2)*l3 + 3.*l1*l1 + 3.*l2*l2
                        + 12.*l3*l3 - 2.*consts::pi_square
                        ) / (3.*_1ml*lambda*(1.-z));
}

double BottomFusion_bb_NNLO_RV::softcoll(const double& z, const double& lambda)
{
    const double l1 = log(lambda);
    const double l2 = log(1.-z);
    return 32.*QCD::CA*(
                        12.*l1*l2 + 3.*l1*l1 + 12.*l2*l2
                        - 2.*consts::pi_square
                        ) / (3.*lambda*(1.-z));
}

template<>
const SectorInfo XSectionMaker<BottomFusion_bb_NNLO_RV>::_info(
                                                                "NNLO real-virtual",
                                                                InitialStateFlavors(QCD::b, QCD::bbar),
                                                                2,
                                                                4
                                                                );


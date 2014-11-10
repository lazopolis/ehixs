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

const double BottomFusion_bb_NLO_hard::_cutoff = 100*DBL_EPSILON;

void BottomFusion_bb_NLO_hard::generateEvents(vector<double>& randoms)
{
    // Setting up convenient variable names
    double& lambdaR = randoms[randoms.size()-2];
    const double lambda = lambdaR;
    const double z = _tau/(_x.x1*_x.x2);
    const double w = _prefactor * _factor;
    // Technical cutoff
    if ( lambda < _cutoff || 1.-lambda < _cutoff || 1.-z < _cutoff )
    {
        _eventBox->push_back(Event());
        return;
    }
    // Generating main event
    _pg(randoms);
    _eventBox->push_back(Event(w*bb2Hg(z,lambda),_p));
    // Generating collinear counterterms
    lambdaR = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*bb2H()*(CounterForge::Pqq<0>(z,lambda)).getCoefficient(0),_p));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*bb2H()*(CounterForge::Pqq<0>(z,1.-lambda)).getCoefficient(0),_p));
//    if (lambda<minlambda) {
//    cout << lambda << "\t"
//        << w*bb2Hg(z,lambda)/(bb2H()*(CounterForge::Split<0>(z,lambda)).getCoefficient(0))
//        << "\t"
//        << -w*bb2H()*(CounterForge::Split<0>(z,1.-lambda)).getCoefficient(0)
//        << endl;
//        minlambda=lambda;
//    }
    cout << CounterForge::f2() << endl;
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
    _eventBox->push_back(Event(w*full(z,lambda),_p));
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

double BottomFusion_bb_NNLO_RV::full(const double& z, const double& lambda)
{
    const double pl1 = HPL2(0,1, 1./(1. + (z*pow(1.-z,-2))/((1.-lambda)*lambda)) ).real();
    const double pl2 = HPL2(0,1,1.-lambda).real();
    const double pl3 = HPL2(0,1,(1.-lambda)*(1.-z)).real();
    const double pl4 = HPL2(0,1,lambda).real();
    const double pl5 = HPL2(0,1,lambda*(1.-z)).real();
    const double pl6 = HPL2(0,1,lambda/(z + lambda*(1.-z))).real();
    const double pl7 = HPL2(0,1,(1.-lambda)/(1. - lambda*(1.-z))).real();
    const double l1 = log(1.-z);
    const double l2 = log(z);
    const double l3 = log(1.-lambda);
    const double l4 = log(lambda);
    const double l5 = log(1.- lambda*(1.-z));
    const double l6 = log(z + lambda*(1.-z));
    const double compr = 18.*l3 + 18.*l4 + 18.*l3*l4 - 2.*l2*(17. + 9.*l3 + 9.*l4) - 16.*l3*l5 + 2.*l4*l5
    + 2.*l3*l6 - 16.*l4*l6 + 18.*l5*l6
    - 4.*l1*(-9. + 9.*l2 - 9.*l3 - 9.*l4 + 4.*l5 + 4.*l6)
    + 18.*pl1 + 2.*pl2 + 20.*pl3 + 2.*pl4 + 20.*pl5 - 2.*pl6 - 2.*pl7
    + 36.*l1*l1 + 17.*l2*l2 + 9.*l3*l3 + 9.*l4*l4 + 8.*l5*l5 + 8.*l6*l6 - 14.*consts::pi_square;
    return 16. * (
                  (16.+compr)
                  - 2. * (-5. + 36.*l1 - 34.*l2 + 18.*l3 + 18.*l4)*z
                  + (6.+compr)*z*z
                  ) / (3.*(1.-lambda)*lambda*(1.-z));
}

double BottomFusion_bb_NNLO_RV::coll(const double& z, const double& lambda)
{
    const double lz = log(z);
    const double l1z = log(1.-z);
    /// \warning 1-z BUG?!?! check!!
    const double dilog = HPL2(0, 1, 1.-z).real();
    const double ll = log(lambda);
    const double z2 = z*z;
    //Nasty, should move back to CA and CF...
    return -16*
    (
     -16 + 14*consts::pi_square - 10*z - 6*z2 + 14*consts::pi_square*z2
     -36*(1.+z2)*l1z*l1z + 34*lz - 68*z*lz + 34*z2*lz - 25*lz*lz
     -25*z2*lz*lz - 18*ll + 36*z*ll - 18*z2*ll + 34*lz*ll + 34*z2*lz*ll - 9*ll*ll*(1.+z2)
     +4*l1z*(13*(1.+z2)*lz - 9*((1.-z)*(1.-z) + (1.+z2)*ll))
     - 20*(1.+z2)*dilog
     )/(3.*(1.-z)*lambda);
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


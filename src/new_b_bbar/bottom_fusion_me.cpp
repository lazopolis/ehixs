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

void BottomFusion_bb_NLO_hard::generateEvents(vector<double>& randoms)
{
    // Setting up convenient variable names
    double& lambdaR = randoms[randoms.size()-2];
    const double lambda = lambdaR;
    const double z = _tau/(_x.x1*_x.x2);
    const double w = _prefactor * _factor;
    test();
    // Generating main event
    _pg(randoms);
    _eventBox->push_back(Event(w*bb2Hg<0,0>(z,lambda),_p));
    // Generating collinear counterterms
    const double wcoll = w*bb2H<0,0>()*(CounterForge::Pqq<0>(z)).getCoefficient(0);
    lambdaR = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(-wcoll/lambda,_p));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(-wcoll/(1.-lambda),_p));
    // There should be no variables left, although not checking for efficiency
    // Cleanup for safety reasons
    randoms.clear();
    return;
}

bool BottomFusion_bb_NLO_hard::test()
{
    // Auxiliary variables
    double z, zbar, lambda;

    // Testing collinear limit
    z=0.3;
    for (lambda=0.5;lambda>1e-10;lambda*=0.5)
        cout
        << lambda << "\t"
        << bb2Hg<0,0>(z,lambda) << "\t"
        << bb2H<0,0>()*(CounterForge::Pqq<0>(z)).getCoefficient(0)/lambda << "\t"
        << bb2Hg<0,0>(z,lambda)/(bb2H<0,0>()*(CounterForge::Pqq<0>(z)).getCoefficient(0)/lambda) << "\n";
    cout<<endl;
    z=0.6;
    for (lambda=0.5;lambda>1e-10;lambda*=0.5)
        cout
        << lambda << "\t"
        << bb2Hg<0,0>(z,lambda) << "\t"
        << bb2H<0,0>()*(CounterForge::Pqq<0>(z)).getCoefficient(0)/lambda << "\t"
        << bb2Hg<0,0>(z,lambda)/(bb2H<0,0>()*(CounterForge::Pqq<0>(z)).getCoefficient(0)/lambda) << "\n";
    cout<<endl;

    // Testing soft limit
    lambda=0.3;
    for (zbar=0.5;zbar>1e-10;zbar*=0.5)
        cout
        << zbar << "\t"
        << bb2Hg<0,0>(1.-zbar,lambda) << "\t"
        << bb2H<0,0>()*(CounterForge::soft<0>(1.-zbar,lambda)).getCoefficient(0) << "\t"
        << bb2Hg<0,0>(1.-zbar,lambda)/(bb2H<0,0>()*CounterForge::soft<0>(1.-zbar,lambda)).getCoefficient(0) << "\n";
    cout<<endl;
    lambda=0.6;
    for (zbar=0.5;zbar>1e-10;zbar*=0.5)
        cout
        << zbar << "\t"
        << bb2Hg<0,0>(1.-zbar,lambda) << "\t"
        << bb2H<0,0>()*(CounterForge::soft<0>(1.-zbar,lambda)).getCoefficient(0) << "\t"
        << bb2Hg<0,0>(1.-zbar,lambda)/(bb2H<0,0>()*CounterForge::soft<0>(1.-zbar,lambda)).getCoefficient(0) << "\n";
    cout<<endl;

    return true;
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
    test();
    // Generating main event
    _pg(randoms);
    _eventBox->push_back(Event(w*bb2Hg<1,0>(z,lambda),_p));
    // Subtracting collinear counterterms
    lambdaR = 0.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*fastcoll(z,lambda),_p));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(-w*fastcoll(z,1.-lambda),_p));
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

double BottomFusion_bb_NNLO_RV::fastcoll(const double& z, const double& lambda)
{
    // Unjustified factor of -1!!!
    // Speed issue: about 2 times slower than direct expression
    return -2.*(
                Expansion<Parameter::epsilon,double>::exp(-log(lambda*(1.-z)/z))*
                CounterTerm->fastPqq<1>(z)*bb2H<0>()/lambda+
                CounterForge::Pqq<0>(z)*bb2H<1>()/lambda
                ).getCoefficient(0);
}

double BottomFusion_bb_NNLO_RV::coll(const double& z, const double& lambda)
{
    // Unjustified factor of -1!!!
    // Speed issue: about 3 times slower than direct expression
//    cout << "Coll\n" << -2.*Expansion<Parameter::epsilon,double>::exp(-log(lambda*(1.-z)/z/*muR*/))*
//    CounterForge::Pqq<1>(z)*bb2H<0>()/lambda << ",\t" <<
//    -2.*CounterForge::Pqq<0>(z)*bb2H<1>()/lambda << endl;
    return -2.*(
                Expansion<Parameter::epsilon,double>::exp(-log(lambda*(1.-z)/z/*muR*/))*
                CounterForge::Pqq<1>(z)*bb2H<0>()/lambda+
                2.*CounterForge::Pqq<0>(z)*bb2H<1>()/lambda
                ).getCoefficient(0);
}

double BottomFusion_bb_NNLO_RV::soft(const double& z, const double& lambda)
{
    // This is NOT the full soft limit, only the one-loop soft current term:
    // The tree soft current always cancels between soft and soft-collinear
    return (CounterForge::soft<1>(z, lambda)*bb2H<0>()).getCoefficient(0);
}

double BottomFusion_bb_NNLO_RV::softcoll(const double& z, const double& lambda)
{
    // This is NOT the full soft-collinear limit, only the one-loop soft-collinear current term:
    // The tree soft-collinear current always cancels between soft and soft-collinear
    return (CounterForge::softcoll<1>(z, lambda)*bb2H<0>()).getCoefficient(0);
}

double BottomFusion_bb_NNLO_RV::explicitcoll(const double& z, const double& lambda)
{
    const double lz = log(z);
    const double l1z = log(1.-z);
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

bool BottomFusion_bb_NNLO_RV::test()
{
    // Auxiliary variables
    double z, zbar, lambda;

    // Testing coefficients of masters expression
    z=0.4;
    zbar = 1.-z;
    for (lambda=0.50139485723094857;lambda>1e-1;lambda*=0.5)
        cout << lambda << "\t"
        << zbar*16.*bb2Hgbis(1./z,-zbar*lambda/z,-zbar*(1.-lambda)/z)/bb2Hg<1,0>(z,lambda) << "\n";
    cout<<endl;

    // Testing collinear limit
    z=0.3;
    for (lambda=0.5;lambda>1e-30;lambda*=0.5)
        cout
        << lambda << "\t"
        << bb2Hg<1,0>(z,lambda) << "\t"
        << coll(z,lambda) << "\t"
        << explicitcoll(z,lambda) << "\t"
        << bb2Hg<1,0>(z,lambda)/coll(z,lambda) << "\n";
    cout<<endl;
    z=0.6;
    for (lambda=0.5;lambda>1e-30;lambda*=0.5)
        cout
        << lambda << "\t"
        << bb2Hg<1,0>(z,lambda) << "\t"
        << coll(z,lambda) << "\t"
        << explicitcoll(z,lambda) << "\t"
        << bb2Hg<1,0>(z,lambda)/coll(z,lambda) << "\n";
    cout<<endl;
    
    // Testing soft limit
    lambda = 0.002;
    for (double zbar = 0.5; zbar>1.e-16; zbar/=2.)
    {
        cout << bb2Hg<1,0>(1.-zbar,lambda) << "\t"
        << soft(1.-zbar,lambda) << "\t"
        << (CounterForge::soft<0>(1.-zbar, lambda)*bb2H<1>()).getCoefficient(0) << "\t"
        << (CounterForge::soft<1>(1.-zbar, lambda)*bb2H<0>()).getCoefficient(0) << "\t"
        << (bb2Hg<1,0>(1.-zbar,lambda)-(CounterForge::soft<1>(1.-zbar, lambda)*bb2H<0>()).getCoefficient(0)) /
            (CounterForge::soft<0>(1.-zbar, lambda)*bb2H<1>()).getCoefficient(0)
        << "\n";
    }
    cout << endl;

    // Testing soft&collinear limit
    zbar = 0.002;
    for (double lambda = 0.5; lambda>1.e-16; lambda/=2.)
    {
        cout << soft(1.-zbar,lambda) << "\t"
        << softcoll(1.-zbar,lambda)+softcoll(1.-zbar,1.-lambda)
        << "\n";
    }
    cout << endl;

    // Testing all together
    zbar = 0.002;
    for (double lambda = 0.5; lambda>1.e-16; lambda/=2.)
    {
        cout
        << bb2Hg<1,0>(1.-zbar,lambda)-coll(1.-zbar,lambda)-coll(1.-zbar,1.-lambda) << "\t"
        << -soft(1.-zbar,lambda)+softcoll(1.-zbar,lambda)+softcoll(1.-zbar,1.-lambda)
        << "\n";
    }
    lambda = 0.002;
    for (double zbar = 0.5; zbar>1.e-16; zbar/=2.)
    {
        cout
        << bb2Hg<1,0>(1.-zbar,lambda)-coll(1.-zbar,lambda)-coll(1.-zbar,1.-lambda) << "\t"
        << -soft(1.-zbar,lambda)+softcoll(1.-zbar,lambda)+softcoll(1.-zbar,1.-lambda) << "\t"
        << (CounterForge::soft<0>(1.-zbar, lambda)*bb2H<1>()).getCoefficient(0)/(
            (CounterForge::softcoll<0>(1.-zbar, lambda)*bb2H<1>()).getCoefficient(0)+
            (CounterForge::softcoll<0>(1.-zbar, 1.-lambda)*bb2H<1>()).getCoefficient(0))
        << "\n";
    }

    exit(0);
    return true;
}

template<>
const SectorInfo XSectionMaker<BottomFusion_bb_NNLO_RV>::_info(
                                                                "NNLO real-virtual",
                                                                InitialStateFlavors(QCD::b, QCD::bbar),
                                                                2,
                                                                4
                                                                );


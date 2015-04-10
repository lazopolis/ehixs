/**
 *
 * \file    gammagamma_me.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#include "gammagamma_me.h"
#include <iostream>
#include <cfloat> // DBL_EPSILON

/// \fn    Rdist

double Rdist(const FourVector& p1, const FourVector& p2)
{
    return sqrt(pow(p1.eta()-p2.eta(),2)+pow(p1.phi()-p2.phi(),2));
}

/// \class GammaGamma_qq_LO

void GammaGamma_qq_LO::generateEvents(vector<double>& randoms)
{
    _pg(randoms);
    const double w = _prefactor * _factor;
    _eventBox->push_back(
                         Event(
                               w*qq2gammagamma<0,0>(square(_p[1]-_p[3])/square(_p[1]-_p[4])),
                               _p
                               )
                         );
    return;
}

template<>
const SectorInfo XSectionMaker<GammaGamma_qq_LO>::_info(
                                                   "Born",
                                                    InitialStateFlavors::quarks,
                                                    0,
                                                    4
                                                        );

/// \class GammaGamma_qq_NLO_real

void GammaGamma_qq_NLO_real::generateEvents(vector<double>& randoms)
{
    test(randoms);
    // Defining auxiliary names
    double& lambdaR = randoms.back();
    const double lambda = randoms.back();
    const double& z = randoms[3];
    // Generating momenta
    const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
    // Photon isolation criterion: measurement function
    if (_cone.inside(_p[3],_p[5])||_cone.inside(_p[4],_p[5])||z*square(_p[1]+_p[2])<20.||_p[3].T()<20.||_p[4].T()<20.)
    {
        _eventBox->push_back(Event(0.,_p));
        return;
    } else {
    // Pushing back main event
    _eventBox->push_back(Event(
                                w*qq2gammagammag<0,0>(
                                                     square(_p[1]+_p[2]),
                                                     square(_p[1]-_p[3]),
                                                     square(_p[1]-_p[4]),
                                                     square(_p[2]-_p[3]),
                                                     square(_p[2]-_p[4])
                                                     ),
                               _p
                               ));
    // Pushing back collinear counterterms
    lambdaR = 0.;
    const double cw = _prefactor * _factor * (CounterForge::Pqq<0>(z)).getCoefficient(0) * _pg(randoms);
    _eventBox->push_back(Event(
                               -cw*qq2gammagamma<0,0>(square(_p[1]-_p[3])/square(_p[1]-_p[4]))/
                               (z*square(_p[1]+_p[2])*lambda),
                               _p
                               ));
    lambdaR = 1.;
    _pg(randoms);
    _eventBox->push_back(Event(
                               -cw*qq2gammagamma<0,0>(square(_p[2]-_p[3])/square(_p[2]-_p[4]))/
                               (z*square(_p[1]+_p[2])*(1.-lambda)),
                               _p
                               ));
    randoms.clear();
    return;
    }
}

void GammaGamma_qq_NLO_real::test(vector<double>& randoms)
{
    // Settings
    std::cout.width(12);
    std::cout.precision(10);

    // Aliases
    double& z = randoms[3];
    double& lambda = randoms.back();

    // Test of collinear limit
    cout << "Testing collinear limit\n\n";
    z=0.23187456;
    cout << "z = " << z << endl << endl;
    for (lambda = 0.05123419384701234; lambda > 5.e-17; lambda*=0.7) {

        // Generating momenta
        const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
        double s12 = square(_p[1]+_p[2]);
        const double s13 = square(_p[1]-_p[3])/s12;
        const double s14 = square(_p[1]-_p[4])/s12;
        const double s23 = square(_p[2]-_p[3])/s12;
        const double s24 = square(_p[2]-_p[4])/s12;
        s12 = 1.;

        // Testing collinear limit
        cout << lambda << "\t"
        << qq2gammagammag<0,0>(s12,s13,s14,s23,s24) << "\t"
        << qq2gammagamma<0,0>(s13/s14)*(CounterForge::Pqq<0>(z)).getCoefficient(0)/(-lambda*z*(1-z)) << "\t"
        << qq2gammagammag<0,0>(s12,s13,s14,s23,s24)/
            (qq2gammagamma<0,0>(s13/s14)*(CounterForge::Pqq<0>(z)).getCoefficient(0)/(-lambda*z*(1-z))) << "\n";

    }

    // Test of soft limit
    cout << "Testing soft limit\n\n";
    lambda=0.63184672;
    cout << "lambda = " << lambda << endl << endl;
    for (double zbar = 0.05123419384701234; zbar>1.e-15; zbar*=0.7) {

        // Generating momenta
        z=1-zbar;
        if (z==1) exit(1234);
        const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
        double s12 = square(_p[1]+_p[2]);
        const double s13 = square(_p[1]-_p[3])/s12;
        const double s14 = square(_p[1]-_p[4])/s12;
        const double s23 = square(_p[2]-_p[3])/s12;
        const double s24 = square(_p[2]-_p[4])/s12;
        s12 = 1.;

        // Testing collinear limit
        cout << z << "\t"
        << qq2gammagammag<0,0>(s12,s13,s14,s23,s24) << "\t"
        << productCoeff(qq2gammagamma<0>(s13/s14),CounterForge::soft<0>(z,lambda),0)/(-z*(1-z)) << "\t"
        << qq2gammagammag<0,0>(s12,s13,s14,s23,s24)/
        productCoeff(qq2gammagamma<0>(s13/s14),CounterForge::soft<0>(z,lambda),0)*(-z*(1-z)) << "\n";

    }
    
    exit(0);
    return;
}


template<>
const SectorInfo XSectionMaker<GammaGamma_qq_NLO_real>::_info(
                                                        "NLO real",
                                                        InitialStateFlavors::quarks,
                                                        1,
                                                        7
                                                        );

/// \class GammaGamma_qq_NNLO_RV

void GammaGamma_qq_NNLO_RV::generateEvents(vector<double>& randoms)
{
    if (_hackIsFirstEvent) {
        _hackIsFirstEvent = false;
        return;
    }
    test(randoms);
    // Defining auxiliary names
    double& lambdaR = randoms.back();
    const double lambda = randoms.back();
    const double& z = randoms[3];
    // Generating momenta
    const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
    // Photon isolation criterion: measurement function
    if (_cone.inside(_p[3],_p[5])||_cone.inside(_p[4],_p[5])||z*square(_p[1]+_p[2])<20.||_p[3].T()<20.||_p[4].T()<20.)
    {
        _eventBox->push_back(Event(0.,_p));
        return;
    } else {
        // Pushing back main event
        _eventBox->push_back(Event(
                                   w*qq2yyg6col(
                                            square(_p[1]+_p[2]),
                                            square(_p[1]-_p[3]),
                                            square(_p[1]-_p[4]),
                                            square(_p[2]-_p[3]),
                                            square(_p[2]-_p[4])
                                            ),
                                   _p
                                   ));
        return;
    }
}


void GammaGamma_qq_NNLO_RV::test(vector<double>& randoms)
{
    std::cout.width(12);
    std::cout.precision(10);
    double& z = randoms[3];
    double& lambda = randoms[4];

    // Loop on lambda
    cout << "Testing collinear limit\n\n";
    z=0.52984766/2.;
    cout << "z = " << z << endl << endl;
    for (lambda = 0.05123419384701234; false && lambda > 5.e-7; lambda*=0.95) {

        // Generating momenta
        const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
        double s12 = square(_p[1]+_p[2]);
        const double s13 = square(_p[1]-_p[3])/s12;
        const double s14 = square(_p[1]-_p[4])/s12;
        const double s23 = square(_p[2]-_p[3])/s12;
        const double s24 = square(_p[2]-_p[4])/s12;
        s12 = 1.;

        // RV full

        // Printing general information
        if (true) {
            cout << "\n ----- lambda = " << lambda << " ----- \n";
            cout << s12 << "\t" << s13 << "\t" << s14 << "\t" << s23 << "\t" << s24 << endl;
            //cout << square(_p[1]-_p[5]) << "\t" << lambda*(1.-z)*s12 << "\t" << -s12-s13-s14 << endl;
        }
        // Printing 6 components to check 1<-->2 symmetry
        if (false) {
            double res12;
            double res21;
            res12 = qq2yyg4CAbub(s12,s13,s14,s23,s24)+qq2yyg4CAbub(s12,s14,s13,s24,s23);
            res21 = qq2yyg4CAbub(s12,s23,s24,s13,s14)+qq2yyg4CAbub(s12,s24,s23,s14,s13);
            cout << "CA bubbles " << "\t"
            << res12 << "\t" << res21 << "\t" << 2*(res12-res21)/(res12+res21) << "\n";
            res12 = qq2yyg4CAbox(s12,s13,s14,s23,s24)+qq2yyg4CAbox(s12,s14,s13,s24,s23);
            res21 = qq2yyg4CAbox(s12,s23,s24,s13,s14)+qq2yyg4CAbox(s12,s24,s23,s14,s13);
            cout << "CA boxes   " << "\t"
            << res12 << "\t" << res21 << "\t" << 2*(res12-res21)/(res12+res21) << "\n";
            res12 = qq2yyg4CFbub(s12,s13,s14,s23,s24)+qq2yyg4CFbub(s12,s14,s13,s24,s23);
            res21 = qq2yyg4CFbub(s12,s23,s24,s13,s14)+qq2yyg4CFbub(s12,s24,s23,s14,s13);
            cout << "CF bubbles " << "\t"
            << res12 << "\t" << res21 << "\t" << 2*(res12-res21)/(res12+res21) << "\n";
            res12 = qq2yyg4CFbox(s12,s13,s14,s23,s24)+qq2yyg4CFbox(s12,s14,s13,s24,s23);
            res21 = qq2yyg4CFbox(s12,s23,s24,s13,s14)+qq2yyg4CFbox(s12,s24,s23,s14,s13);
            cout << "CF boxes   " << "\t"
            << res12 << "\t" << res21 << "\t" << 2*(res12-res21)/(res12+res21) << "\n";
            res12 = qq2yyg4AFbub(s12,s13,s14,s23,s24)+qq2yyg4AFbub(s12,s14,s13,s24,s23);
            res21 = qq2yyg4AFbub(s12,s23,s24,s13,s14)+qq2yyg4AFbub(s12,s24,s23,s14,s13);
            cout << "AF bubbles " << "\t"
            << res12 << "\t" << res21 << "\t" << 2*(res12-res21)/(res12+res21) << "\n";
            res12 = qq2yyg4AFbox(s12,s13,s14,s23,s24)+qq2yyg4AFbox(s12,s14,s13,s24,s23);
            res21 = qq2yyg4AFbox(s12,s23,s24,s13,s14)+qq2yyg4AFbox(s12,s24,s23,s14,s13);
            cout << "AF boxes   " << "\t"
            << res12 << "\t" << res21 << "\t" << 2*(res12-res21)/(res12+res21) << "\n";
            res12 = qq2yyg4(s12,s13,s14,s23,s24)+qq2yyg4(s12,s14,s13,s24,s23);
            res21 = qq2yyg4(s12,s23,s24,s13,s14)+qq2yyg4(s12,s24,s23,s14,s13);
            cout << "Total      " << "\t"
            << res12 << "\t" << res21 << "\t" << 2*(res12-res21)/(res12+res21) << "\n";
        }

        // Printing 6 components for plotting against lambda
        if (false) {
            cout << lambda << "\t";
            cout << qq2yyg4CAbub(s12,s13,s14,s23,s24)+qq2yyg4CAbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4CFbub(s12,s13,s14,s23,s24)+qq2yyg4CFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4AFbub(s12,s13,s14,s23,s24)+qq2yyg4AFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4CAbox(s12,s13,s14,s23,s24)+qq2yyg4CAbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4CFbox(s12,s13,s14,s23,s24)+qq2yyg4CFbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4AFbox(s12,s13,s14,s23,s24)+qq2yyg4AFbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6CAbub(s12,s13,s14,s23,s24)+qq2yyg6CAbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6CFbub(s12,s13,s14,s23,s24)+qq2yyg6CFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6AFbub(s12,s13,s14,s23,s24)+qq2yyg6AFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6CAbox(s12,s13,s14,s23,s24)+qq2yyg6CAbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6CFbox(s12,s13,s14,s23,s24)+qq2yyg6CFbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6AFbox(s12,s13,s14,s23,s24)+qq2yyg6AFbox(s12,s14,s13,s24,s23) << endl;
        }

        // Checking 4D against 6D
        if (false) {
            cout << lambda << "\t";
            cout << (
                     qq2yyg4CAbub(s12,s13,s14,s23,s24)+qq2yyg4CAbub(s12,s14,s13,s24,s23)+
                     qq2yyg4CAbox(s12,s13,s14,s23,s24)+qq2yyg4CAbox(s12,s14,s13,s24,s23)
                     )/(
                     qq2yyg6CAbub(s12,s13,s14,s23,s24)+qq2yyg6CAbub(s12,s14,s13,s24,s23)+
                     qq2yyg6CAbox(s12,s13,s14,s23,s24)+qq2yyg6CAbox(s12,s14,s13,s24,s23)
                     )
                     << "\t";
            cout << (
                     qq2yyg4CFbub(s12,s13,s14,s23,s24)+qq2yyg4CFbub(s12,s14,s13,s24,s23)+
                     qq2yyg4CFbox(s12,s13,s14,s23,s24)+qq2yyg4CFbox(s12,s14,s13,s24,s23)
                     )/(
                     qq2yyg6CFbub(s12,s13,s14,s23,s24)+qq2yyg6CFbub(s12,s14,s13,s24,s23)+
                     qq2yyg6CFbox(s12,s13,s14,s23,s24)+qq2yyg6CFbox(s12,s14,s13,s24,s23)
                     )
                     << "\t";
            cout << (
                     qq2yyg4AFbub(s12,s13,s14,s23,s24)+qq2yyg4AFbub(s12,s14,s13,s24,s23)+
                     qq2yyg4AFbox(s12,s13,s14,s23,s24)+qq2yyg4AFbox(s12,s14,s13,s24,s23)
                     )/(
                     qq2yyg6AFbub(s12,s13,s14,s23,s24)+qq2yyg6AFbub(s12,s14,s13,s24,s23)+
                     qq2yyg6AFbox(s12,s13,s14,s23,s24)+qq2yyg6AFbox(s12,s14,s13,s24,s23)
                     )
                     << endl;
        }
        if (false) {
            cout << lambda << "\t";
            cout << (
                     qq2yyg4LCbub(s12,s13,s14,s23,s24)+qq2yyg4LCbub(s12,s14,s13,s24,s23)+
                     qq2yyg4LCbox(s12,s13,s14,s23,s24)+qq2yyg4LCbox(s12,s14,s13,s24,s23)
                     )/(
                     qq2yyg6LCbub(s12,s13,s14,s23,s24)+qq2yyg6LCbub(s12,s14,s13,s24,s23)+
                     qq2yyg6LCbox(s12,s13,s14,s23,s24)+qq2yyg6LCbox(s12,s14,s13,s24,s23)
                     )
            << "\t";
            cout << (
                     qq2yyg4SCbub(s12,s13,s14,s23,s24)+qq2yyg4SCbub(s12,s14,s13,s24,s23)+
                     qq2yyg4SCbox(s12,s13,s14,s23,s24)+qq2yyg4SCbox(s12,s14,s13,s24,s23)
                     )/(
                     qq2yyg6SCbub(s12,s13,s14,s23,s24)+qq2yyg6SCbub(s12,s14,s13,s24,s23)+
                     qq2yyg6SCbox(s12,s13,s14,s23,s24)+qq2yyg6SCbox(s12,s14,s13,s24,s23)
                     )
            << endl;
        }

        // Checking color-decomposed matrix elements
        if (false) {
            cout << (qq2yyg4CAbox(s12,s13,s14,s23,s24)+qq2yyg4CAbox(s12,s14,s13,s24,s23)) << "\t"
                 << 0.5*QCD::CA/QCD::CF*(qq2yyg4CFbox(s12,s13,s14,s23,s24)+qq2yyg4CFbox(s12,s14,s13,s24,s23)) << "\t"
                 << (qq2yyg6LCbox(s12,s13,s14,s23,s24)+qq2yyg6LCbox(s12,s14,s13,s24,s23)) << "\t";
            cout << (qq2yyg4CAbub(s12,s13,s14,s23,s24)+qq2yyg4CAbub(s12,s14,s13,s24,s23)) << "\t"
                 << 0.5*QCD::CA/QCD::CF*(qq2yyg4CFbub(s12,s13,s14,s23,s24)+qq2yyg4CFbub(s12,s14,s13,s24,s23)) << "\t"
                 << (qq2yyg6LCbub(s12,s13,s14,s23,s24)+qq2yyg6LCbub(s12,s14,s13,s24,s23)) << "\t\t";
            cout << (qq2yyg6AFbox(s12,s13,s14,s23,s24)+qq2yyg6AFbox(s12,s14,s13,s24,s23)) << "\t"
                 << -0.5/(QCD::Nc*QCD::CF)*(qq2yyg6CFbox(s12,s13,s14,s23,s24)+qq2yyg6CFbox(s12,s14,s13,s24,s23)) << "\t"
                 << (qq2yyg6SCbox(s12,s13,s14,s23,s24)+qq2yyg6SCbox(s12,s14,s13,s24,s23)) << "\t";
            cout << (qq2yyg6AFbub(s12,s13,s14,s23,s24)+qq2yyg6AFbub(s12,s14,s13,s24,s23)) << "\t"
                 << -0.5/(QCD::Nc*QCD::CF)*(qq2yyg6CFbub(s12,s13,s14,s23,s24)+qq2yyg6CFbub(s12,s14,s13,s24,s23)) << "\t"
                 << (qq2yyg6SCbub(s12,s13,s14,s23,s24)+qq2yyg6SCbub(s12,s14,s13,s24,s23)) << "\n";
        }

        // Counterterms

        // Testing collinear counterterm, color decomposition
        if (false)
        {
            // Fudge factor
            /// \todo Figure out where this fudge belongs
            const double f = -16./3.; // this is 4*alphas^2*CF
            // Switches
            const bool LCon = true;
            const bool SCon = true;
            // Intermediate variables
            const double LC = qq2yyg6LCbub(s12,s13,s14,s23,s24)+qq2yyg6LCbub(s12,s14,s13,s24,s23)+
                              qq2yyg6LCbox(s12,s13,s14,s23,s24)+qq2yyg6LCbox(s12,s14,s13,s24,s23);
            const double SC = qq2yyg6SCbub(s12,s13,s14,s23,s24)+qq2yyg6SCbub(s12,s14,s13,s24,s23)+
                              qq2yyg6SCbox(s12,s13,s14,s23,s24)+qq2yyg6SCbox(s12,s14,s13,s24,s23);
            const double mycoll1 = _coll1(z,lambda,s13/s14,LCon,SCon);
            const double mycoll2 = _coll2(z,lambda,s13/s14,LCon,SCon);
            const double myfull  = f*(LCon*LC+SCon*SC);
            // Printing
            cout << lambda << "\t\t"
                 // log(lambda)/lambda piece
                 << mycoll1/myfull << "\t"
                 // residual
                 << (myfull-mycoll1)/mycoll2 << "\t"
                 // percent difference counterterm-full ME
                 << 2*(myfull-mycoll1-mycoll2)/(myfull+mycoll1+mycoll2) <<"\n";
        }

        // Plotting counterterm vs. full ME
        if (false) {
            cout << lambda << "\t"
            << _coll(z,lambda,s13/s14) << "\t"
            << -16/3.*(qq2yyg6col(s12,s13,s14,s23,s24)+qq2yyg6col(s12,s14,s13,s24,s23)) << endl;
        }

    }

    // Loop on z
    cout << "Testing soft limit" << endl;
    lambda=0.52984766/2.;
    cout << "lambda = " << lambda << endl << endl;
    for (double zbar = 0.8123419384701234; true && zbar > 5.e-6; zbar*=0.9) {

        z=1.-zbar;
        if (z==1.) exit(1234);

        // Generating momenta
        const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
        double s12 = square(_p[1]+_p[2]);
        const double s13 = square(_p[1]-_p[3])/s12;
        const double s14 = square(_p[1]-_p[4])/s12;
        const double s23 = square(_p[2]-_p[3])/s12;
        const double s24 = square(_p[2]-_p[4])/s12;
        const double s15 = square(_p[1]-_p[5])/s12;
        const double s25 = square(_p[2]-_p[5])/s12;
        const double s34 = square(_p[3]+_p[4])/s12;
        s12 = 1.;

        // Printing general information
        if (false) {
            cout << "\n ----- z = " << z << " ----- \n";
            cout << s12/s12 << "\t" << s13/s12 << "\t" << s14/s12 << "\t" << s23/s12 << "\t" << s24/s12 << endl;
            //cout << (-s12-s13-s14)/s12 << "\t" << (-s12-s23-s24)/s12 << "\t" << (-s12-s13-s14-s23-s24)/s12 << endl;
        }
        //cout << zbar << "\t\t" << qq2yyg4CA<4>(s12,s13,s14,s23,s24) << "\t\t" << bubble(s23,3) << endl;
        //cout << productCoeff(qq2yyg4CA<4>(s12,s13,s14,s23,s24),bubble(s23,3),0) << "\n";

        // Printing 6 components for plotting against zbar
        if (false) {
            cout << zbar << "\t\t";
            cout << qq2yyg4CAbub(s12,s13,s14,s23,s24)+qq2yyg4CAbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4CFbub(s12,s13,s14,s23,s24)+qq2yyg4CFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4AFbub(s12,s13,s14,s23,s24)+qq2yyg4AFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4CAbox(s12,s13,s14,s23,s24)+qq2yyg4CAbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4CFbox(s12,s13,s14,s23,s24)+qq2yyg4CFbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg4AFbox(s12,s13,s14,s23,s24)+qq2yyg4AFbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6CAbub(s12,s13,s14,s23,s24)+qq2yyg6CAbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6CFbub(s12,s13,s14,s23,s24)+qq2yyg6CFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6AFbub(s12,s13,s14,s23,s24)+qq2yyg6AFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6CAbox(s12,s13,s14,s23,s24)+qq2yyg6CAbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6CFbox(s12,s13,s14,s23,s24)+qq2yyg6CFbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yyg6AFbox(s12,s13,s14,s23,s24)+qq2yyg6AFbox(s12,s14,s13,s24,s23) << endl;
        }

        // Printing CA bubble masters
        if (false) {
            cout << zbar << "\t\t"
            << productCoeff(qq2yyg4CA<1>(s12,s13,s14,s23,s24),bubble(s13,3),0) << "\t"
            << productCoeff(qq2yyg4CA<2>(s12,s13,s14,s23,s24),bubble(s14,3),0) << "\t"
            << productCoeff(qq2yyg4CA<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0) << "\t"
            << productCoeff(qq2yyg4CA<4>(s12,s13,s14,s23,s24),bubble(s23,3),0) << "\t"
            << productCoeff(qq2yyg4CA<5>(s12,s13,s14,s23,s24),bubble(s24,3),0) << "\t"
            << productCoeff(qq2yyg4CA<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0) << "\t"
            << productCoeff(qq2yyg4CA<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0) << endl;
        }

        // Printing SC bubble masters
        if (false) {
            cout << zbar << "\t\t"
            << productCoeff(qq2yyg6SC<1>(s12,s13,s14,s23,s24),bubble(s12,3),0) << "\t"
            << productCoeff(qq2yyg6SC<2>(s12,s13,s14,s23,s24),bubble(s13,3),0) << "\t"
            << productCoeff(qq2yyg6SC<3>(s12,s13,s14,s23,s24),bubble(s14,3),0) << "\t"
            << productCoeff(qq2yyg6SC<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0) << "\t"
            << productCoeff(qq2yyg6SC<5>(s12,s13,s14,s23,s24),bubble(s23,3),0) << "\t"
            << productCoeff(qq2yyg6SC<6>(s12,s13,s14,s23,s24),bubble(s24,3),0) << "\t"
            << productCoeff(qq2yyg6SC<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0) << "\t"
            << productCoeff(qq2yyg6SC<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0) << "\t"
            << productCoeff(qq2yyg6SC<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0) << "\t"
            << productCoeff(qq2yyg6SC<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0) << endl;
        }

        // Printing LC bubble masters
        if (false) {
            cout << zbar << "\t\t"
            << productCoeff(qq2yyg6LC<1>(s12,s13,s14,s23,s24),bubble(s13,3),0) << "\t"
            << productCoeff(qq2yyg6LC<2>(s12,s13,s14,s23,s24),bubble(s14,3),0) << "\t"
            << productCoeff(qq2yyg6LC<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0) << "\t"
            << productCoeff(qq2yyg6LC<4>(s12,s13,s14,s23,s24),bubble(s23,3),0) << "\t"
            << productCoeff(qq2yyg6LC<5>(s12,s13,s14,s23,s24),bubble(s24,3),0) << "\t"
            << productCoeff(qq2yyg6LC<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0) << "\t"
            << productCoeff(qq2yyg6LC<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0) << "\t\t"
            << productCoeff(qq2yyg6ELC<1>(s12,s13,s14,s23,s24),bubble(s13,3),0) << "\t"
            << productCoeff(qq2yyg6ELC<2>(s12,s13,s14,s23,s24),bubble(s14,3),0) << "\t"
            << productCoeff(qq2yyg6ELC<3>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0) << "\t"
            << productCoeff(qq2yyg6ELC<4>(s12,s13,s14,s23,s24),bubble(s23,3),0) << "\t"
            << productCoeff(qq2yyg6ELC<5>(s12,s13,s14,s23,s24),bubble(s24,3),0) << "\t"
            << productCoeff(qq2yyg6ELC<6>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0) << "\t"
            << productCoeff(qq2yyg6ELC<7>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0) << endl;
        }

        // Printing a sample badly-behaved coefficient
        if (false) {
            cout << zbar << "\t\t"
            << qq2yyg6LC<4,1>(s12,s13,s14,s23,s24) << "\t"
            << qq2yyg6ELC<4,1>(s12,s13,s14,s23,s24) << endl;
        }

        // Plotting counterterm vs. full ME
        if (true) {
            cout << 1.-z << "\t"
            << _fullsoft(z,lambda,s13/s14) << "\t"
            << qq2yyg6ELCbub(s12,s13,s14,s23,s24) << "\t"
            << qq2yyg6ELCbox(s12,s13,s14,s23,s24) << endl;//"\t"
//            << (qq2yyg6SCbub(s12,s13,s14,s23,s24)+qq2yyg6SCbub(s12,s14,s13,s24,s23)) << "\t"
//            << (qq2yyg6SCbox(s12,s13,s14,s23,s24)+qq2yyg6SCbox(s12,s14,s13,s24,s23)) << "\t"
//            << (qq2yyg6col(s12,s13,s14,s23,s24)+qq2yyg6col(s12,s14,s13,s24,s23)) << endl;
        }

    }

    exit(1);
    return;
}

double GammaGamma_qq_NNLO_RV::_coll(
                                     const double& z,
                                     const double& lambda,
                                     const double& ratio,
                                     const bool LCf,
                                     const bool SCf
                                     )
{
    return _coll1(z,lambda,ratio,LCf,SCf) + _coll2(z,lambda,ratio,LCf,SCf);
}

double GammaGamma_qq_NNLO_RV::_coll1(
                                    const double& z,
                                    const double& lambda,
                                    const double& ratio,
                                    const bool LCf,
                                    const bool SCf
                                    )
{
    return productCoeff(
                        Expansion<Parameter::epsilon,double>::exp(-log(lambda*(1.-z)/*muR*/),3),
                        CounterForge::Pqq<1>(z,LCf,SCf,3)*qq2gammagamma<0>(ratio),
                        0
                        )/(-lambda*z*(1-z));
}

double GammaGamma_qq_NNLO_RV::_coll2(
                                     const double& z,
                                     const double& lambda,
                                     const double& ratio,
                                     const bool LCf,
                                     const bool SCf
                                     )
{
    return productCoeff(
                        Expansion<Parameter::epsilon,double>::exp(-log(z/*muR*/),3),
                        CounterForge::Pqq<0>(z,LCf,SCf,3)*qq2gammagamma<1>(ratio),
                        0
                        )/(-lambda*z*(1-z));
}

double GammaGamma_qq_NNLO_RV::_fullsoft(const double& z, const double& lambda, const double& ratio)
{
    return (
            productCoeff(CounterForge::soft<1>(z, lambda),qq2gammagamma<0>(ratio),0) +
            productCoeff(CounterForge::soft<0>(z, lambda),qq2gammagamma<1>(ratio),0)
            )/(-z*(1-z));
}

double GammaGamma_qq_NNLO_RV::_fullsoftcoll(const double& z, const double& lambda, const double& ratio)
{
    return productCoeff(CounterForge::softcoll<1>(z, lambda),qq2gammagamma<0>(ratio),0) +
           productCoeff(CounterForge::softcoll<1>(z, lambda),qq2gammagamma<0>(ratio),0);
}

double GammaGamma_qq_NNLO_RV::_soft(const double& z, const double& lambda, const double& ratio)
{
    // This is NOT the full soft limit, only the one-loop soft current term:
    // The tree soft current always cancels between soft and soft-collinear
    return productCoeff(CounterForge::soft<1>(z, lambda),qq2gammagamma<0>(ratio),0);
}

double GammaGamma_qq_NNLO_RV::_softcoll(const double& z, const double& lambda, const double& ratio)
{
    // This is NOT the full soft-collinear limit, only the one-loop soft-collinear current term:
    // The tree soft-collinear current always cancels between soft and soft-collinear
    return productCoeff(CounterForge::softcoll<1>(z, lambda),qq2gammagamma<0>(ratio),0);
}


template<>
const SectorInfo XSectionMaker<GammaGamma_qq_NNLO_RV>::_info(
                                                              "NNLO real-virtual",
                                                              InitialStateFlavors::quarks,
                                                              2,
                                                              7
                                                              );

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
                               w*qq2yy<0,0>(square(_p[1]-_p[3])/square(_p[1]-_p[4])),
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
    const double w = _prefactor * _factor * _pg(randoms);
    // Photon isolation criterion: measurement function
    if (_cone.inside(_p[3],_p[5])||_cone.inside(_p[4],_p[5])||z*square(_p[1]+_p[2])<20.||_p[3].T()<20.||_p[4].T()<20.)
    {
        _eventBox->push_back(Event(0.,_p));
        return;
    } else {
        // Computing invariants
        double s12 = square(_p[1]+_p[2]);
        const double s13 = square(_p[1]-_p[3])/s12;
        const double s14 = square(_p[1]-_p[4])/s12;
        const double s23 = square(_p[2]-_p[3])/s12;
        const double s24 = square(_p[2]-_p[4])/s12;
        s12 = 1.;
        const double s15 = -1-s13-s14;
        const double s25 = -1-s23-s24;
        const double s35 = 1+s14+s24;
        const double s45 = 1+s13+s23;
        const double zb = -s15-s25;
        const double t12 = (s15-s25)/zb;
        const double t34 = (s35-s45)/zb;
        const double u = s13-s14-s23+s24;
        // Pushing back main event, 1-z from phase space
        _eventBox->push_back(Event(w*zb*qq2yyg<0,0>(zb,t12,t34,u),_p));
        // Pushing back collinear counterterms
        const double cw = w*(CounterForge::Pqq<0>(z)).getCoefficient(0)/z*s12;
        lambdaR = 0.;
        _pg(randoms);
        _eventBox->push_back(Event(-cw*qq2yy<0,0>(s13/s14)/lambda,_p));
        lambdaR = 1.;
        _pg(randoms);
        _eventBox->push_back(Event(-cw*qq2yy<0,0>(s23/s24)/(1.-lambda),_p));
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
        const double w = _prefactor * _factor  * _pg(randoms);
        double s12 = square(_p[1]+_p[2]);
        const double s13 = square(_p[1]-_p[3])/s12;
        const double s14 = square(_p[1]-_p[4])/s12;
        const double s23 = square(_p[2]-_p[3])/s12;
        const double s24 = square(_p[2]-_p[4])/s12;
        s12 = 1.;
        const double s15 = -1-s13-s14;
        const double s25 = -1-s23-s24;
        const double s35 = 1+s14+s24;
        const double s45 = 1+s13+s23;
        const double zb = -s15-s25;
        const double t12 = (s15-s25)/zb;
        const double t34 = (s35-s45)/zb;
        const double u = s13-s14-s23+s24;

        // Testing collinear limit
        cout << lambda << "\t"
        << (1-z)*qq2yyg<0,0>(zb,t12,t34,u) << "\t"
        << qq2yy<0,0>(s13/s14)*(CounterForge::Pqq<0>(z)).getCoefficient(0)/(-lambda*z) << "\t"
        << (1-z)*qq2yyg<0,0>(zb,t12,t34,u)/
            (qq2yy<0,0>(s13/s14)*(CounterForge::Pqq<0>(z)).getCoefficient(0)/(-lambda*z)) << "\n";

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
        const double s15 = -1-s13-s14;
        const double s25 = -1-s23-s24;
        const double s35 = 1+s14+s24;
        const double s45 = 1+s13+s23;
        const double zb = -s15-s25;
        const double t12 = (s15-s25)/zb;
        const double t34 = (s35-s45)/zb;
        const double u = s13-s14-s23+s24;

        // Testing soft limit
        cout << zbar << "\t"
        << (1-z)*qq2yyg<0,0>(zb,t12,t34,u) << "\t"
        << productCoeff(qq2yy<0>(s13/s14),CounterForge::soft<0>(z,lambda),0)/(-1) << "\t"
        << (1-z)*qq2yyg<0,0>(zb,t12,t34,u)/
           productCoeff(qq2yy<0>(s13/s14),CounterForge::soft<0>(z,lambda),0)*(-1) << "\n";

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
    ++_i;
    std::cout.width(12);
    std::cout.precision(10);
    randoms[0]=0.623847728931;
    randoms[1]=0.346123429920;
    randoms[2]=0.123098470796;
    randoms[3]=0.645725573737;
//    randoms[0] = 0.1623847728931;
//    randoms[1] = 0.73461234920;
//    randoms[2] = 0.012309847096;
//    randoms[3] = 0.645725573737;
    //randoms[4]=0.809993;
    if (_hackIsFirstEvent) {
        _hackIsFirstEvent = false;
        return;
    }
    // if (_i > 30000) exit(0);
    test(randoms);
    // Defining auxiliary names
    double& lambdaR = randoms.back();
    const double lambda = randoms.back();
    const double& z = randoms[3];
    // Generating momenta
    double w = _prefactor * _factor * _pg(randoms);
    const double s12 = square(_p[1]+_p[2]);
    w /= s12;
    // Photon isolation criterion: measurement function
    if (_cut()) {
        _eventBox->push_back(Event(0.,_p));
        return;
    } else {
        // Computing invariants
        const double s13 = square(_p[1]-_p[3])/s12;
        const double s14 = square(_p[1]-_p[4])/s12;
        const double s23 = square(_p[2]-_p[3])/s12;
        const double s24 = square(_p[2]-_p[4])/s12;
        const double s15 = -1-s13-s14;
        const double s25 = -1-s23-s24;
        double qq2yyg = 0.;
        if (z>0.5) qq2yyg = qq2yygstu6col(s13,s14,s23,s24);
            else qq2yyg = qq2yygstu6colnobar(s13,s14,s23,s24);
        qq2yyg *= 16./3.;
        // Pushing back main event
        _eventBox->push_back( Event(w*qq2yyg,_p) );
        // Pushing back collinear counterterm
        lambdaR = 0.;
        _pg(randoms);
        _eventBox->push_back( Event(w*_coll(z,lambda,s13/s14),_p) );
        lambdaR = 1.;
        _pg(randoms);
        _eventBox->push_back( Event(w*_coll(z,1.-lambda,s13/s14),_p) );
        // Checking bubble cancellation
        if (false && ++_i % 50 == 0)
        {
            if (w == 0) cout << "Bad point:\t";
            const double s34 = -1-s13-s14-s23-s24;
            const double s35 = 1+s14+s24;
            const double s45 = 1+s13+s23;
            const double zb = -s15-s25;
            const double t12 = (s15-s25)/zb;
            const double t34 = (s35-s45)/zb;
            const double u = s13-s14-s23+s24;
            cout << lambda << "\t";
            cout << abs(s14-s25)/(abs(s14)+abs(s25)) << "\t";
            const double bub13 = productCoeff(qq2yygstu6SC<2>(zb,t12,t34,u),bubble(s13,3),0);
            const double bub14 = productCoeff(qq2yygstu6SC<3>(zb,t12,t34,u),bubble(s14,3),0);
            const double bub15 = productCoeff(qq2yygstu6SC<4>(zb,t12,t34,u),bubble(s15,3),0);
            const double bub23 = productCoeff(qq2yygstu6SC<5>(zb,t12,t34,u),bubble(s23,3),0);
            const double bub24 = productCoeff(qq2yygstu6SC<6>(zb,t12,t34,u),bubble(s24,3),0);
            const double bub25 = productCoeff(qq2yygstu6SC<7>(zb,t12,t34,u),bubble(s25,3),0);
//            cout << bub13+bub24 << "\t" << qq2yygstu6LCbub1324(zb,t12,u) << endl;
            cout << bub14+bub25 << "\t" << qq2yygstu6SCbub1325(zb,t12,-t34) << endl;
//            cout << s12/(13000.*13000.) << "\t" << 0.5*log(_x.x1/_x.x2) << "\t";
//            cout << s13 << "\t" << s23 << "\t" << s14 << "\t" << s24 << endl;
//            cout << s15 << "\t" << s25 << "\t" << s34 << "\t" << s35 << "\t" << s45 << endl;
//            cout << square(_p[1]-_p[5])/s12 << "\t" << square(_p[2]-_p[5])/s12 << "\t" << square(_p[3]+_p[4])/s12 << "\t" <<  square(_p[3]+_p[5])/s12 << "\t" << square(_p[4]+_p[5])/s12 << "\t";
//            cout << zb << "\t" << t12 << "\t" << t34 << "\t" << u << "\t";
//            cout << endl;
//            cout << qq2yygstu6LCbubpatch(s13,s14,s23,s24) << endl;//"\t";
//            cout << qq2yygstu6LCbox(s13,s14,s23,s24) << "\t";
//            cout << qq2yygstu6SCbub(s13,s14,s23,s24) << "\t";
//            cout << qq2yygstu6SCbox(s13,s14,s23,s24) << endl;
//            cout << qq2yyg << "\t"
//                 << _coll(z,lambda,s13/s14) << "\t"
//                 << _coll(z,1.-lambda,s13/s14) << endl;
        }
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
    //z=0.52984766/2.;
    z=0.645725573737;
    cout << "z = " << z << endl << endl;
    //for (lambda = /*1.-*/0.85123419384234; false && /*1.-*/lambda > 5.e-8/*-16*/; lambda=/*1.-0.7*(1.-lambda)*/lambda*=0.8)
    for (double zeta = -12; true && zeta <= 12; zeta+=0.1)
    {
        lambda = 1.-1./(1+exp(zeta*log(10)));

        // Generating momenta
        const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
        double s12 = square(_p[1]+_p[2]);
        const double s13 = square(_p[1]-_p[3])/s12;
        const double s14 = square(_p[1]-_p[4])/s12;
        const double s23 = square(_p[2]-_p[3])/s12;
        const double s24 = square(_p[2]-_p[4])/s12;
        s12 = 1.;
        const double s15 = -s12-s13-s14;
        const double s25 = -s12-s23-s24;
        const double zb = -(s15+s25)/s12;
        const double s15n = s15/zb;
        const double s25n = s25/zb;
        const double s35 = s12+s14+s24;
        const double s45 = s12+s13+s23;
        const double s34 = -s12-s15-s25;
        const double s35n = (s12+s14+s24)/zb;
        const double s45n = (s12+s13+s23)/zb;
        const double t12 = (s15-s25)/zb;
        const double t34 = (s35-s45)/zb;
        const double u = s13-s14-s23+s24;
        const double z=1.-zb;

        // Printing general information
        if (false) {
            const double s = sqrt(square(_p[1]+_p[2]))/2.;
            cout << "lambda = " << lambda << "\n";
            cout << "x1 = " << _x.x1 << "\t";
            cout << "x2 = " << _x.x2 << "\n";
            cout << "p1 = " << _p[1]/s << "\n";
            cout << "p2 = " << _p[2]/s << "\n";
            cout << "p3 = " << _p[3]/s << "\n";
            cout << "p4 = " << _p[4]/s << "\n";
            cout << "p5 = " << _p[5]/s << "\n";
            cout << "p4.p25 = " << _p[4]*(_p[2]-_p[5])/s << "\n";
            cout << "p4.p4 = " << _p[4]*_p[4]/s << "\np5.p5 = " << _p[5]*_p[5]/s << endl;
            cout << s13 << "\t" << s14 << "\t" << s23 << "\t" << s24 << "\t";
            cout << s15 << "\t" << s25 << "\t" << s35 << "\t" << s45 << "\t" << s34 << endl;
        }

        // Counterterms

        // Testing collinear counterterm, color decomposition
        if (false)
        {
            // Fudge factor
            /// \todo Figure out where this fudge belongs
            const double f = -16./3.; // this is 4*alphas^2*CF
            // Switches
            const bool LCon = false;
            const bool SCon = true;
            // Intermediate variables
            const double LC = qq2yygstu6LCbub(s13,s14,s23,s24)+qq2yygstu6LCbox(s13,s14,s23,s24);
            const double SC = qq2yygstu6SCbub(s13,s14,s23,s24)+qq2yygstu6SCbox(s13,s14,s23,s24);
            const double mycoll1 = _coll1(z,lambda,s13/s14,LCon,SCon);
            const double mycoll2 = _coll2(z,lambda,s13/s14,LCon,SCon);
            const double myfull  = f*(LCon*LC+SCon*SC);
            // Printing
            cout << lambda << "\t\t"
                 // log(lambda)/lambda piece
                 << mycoll1/myfull << "\t"
                 // residual
                 << (myfull-mycoll1)/mycoll2 << "\t"
                 // total ratio
                 << myfull/(mycoll1+mycoll2) << "\t"
                 // percent difference counterterm-full ME
                 << 2*(myfull-mycoll1-mycoll2)/(myfull+mycoll1+mycoll2) <<"\n";
        }

        // Plotting counterterm vs. full ME
        if (true)
        {
            // Fudge factor
            /// \todo Figure out where this fudge belongs
            const double f = -16./3.; // this is 4*alphas^2*CF
            // Switches
            const bool LCon = true;
            const bool SCon = true;
            // Intermediate variables
            if (_cut(false)) {
                cout << zeta <<  "\t\t" << 0 << "\t" << 0 << "\t" << 0 << "\t";
                cout << 0 << "\t" << 0 << endl;
            } else {
                cout << zeta << "\t\t";
                // double arithmetics
                const double LCd = qq2yygstu6LCbub(s13,s14,s23,s24)+qq2yygstu6LCbox(s13,s14,s23,s24);
                const double SCd = qq2yygstu6SCbub(s13,s14,s23,s24)+qq2yygstu6SCbox(s13,s14,s23,s24);
                const double myfulld = f*(LCon*LCd+SCon*SCd);
                cout << myfulld << "\t";
                // quadruple arithmetics
                const __float128 s13q = s13;
                const __float128 s14q = s14;
                const __float128 s23q = s23;
                const __float128 s24q = s24;
                const double LCq = qq2yygstu6LCbub(s13q,s14q,s23q,s24q)+qq2yygstu6LCbox(s13q,s14q,s23q,s24q);
                const double SCq = qq2yygstu6SCbub(s13q,s14q,s23q,s24q)+qq2yygstu6SCbox(s13q,s14q,s23q,s24q);
                const double myfullq = f*(LCon*LCq+SCon*SCq);
                cout << myfullq << "\t";
                // rational arithmetics
#ifdef WITH_CLN
//                const cln::cl_RA s13r = cln::rational(s13);
//                const cln::cl_RA s14r = cln::rational(s14);
//                const cln::cl_RA s23r = cln::rational(s23);
//                const cln::cl_RA s24r = cln::rational(s24);
//                const double LCr = qq2yygstu6LCbub(s13r,s14r,s23r,s24r)+qq2yygstu6LCbox(s13r,s14r,s23r,s24r);
//                const double SCr = qq2yygstu6SCbub(s13r,s14r,s23r,s24r)+qq2yygstu6SCbox(s13r,s14r,s23r,s24r);
//                const double myfullr = f*(LCon*LCr+SCon*SCr);
//                cout << myfullr << "\t";
#endif
                // limits
                const double mycoll1 = _coll(z,lambda,s13/s14,LCon,SCon);
                const double mycoll2 = _coll(z,1.-lambda,s13/s14,LCon,SCon);
                cout << mycoll1 << "\t" << mycoll2 << endl;
            }
        }

    }

    // Loop on z
    cout << "Testing soft limit" << endl;
    lambda=0.52984766;///2.;
    cout << "lambda = " << lambda << endl << endl;
//    for (double zbar = 0.8123419384701234; true && zbar > 5.e-8; zbar*=0.9) {
    for (double zeta = -12; true && zeta <= 12; zeta+=0.1) {
        double zbar = 1.-1./(1+exp(zeta*log(10)));
        z=1.-zbar;
        if (z==1.) exit(1234);

        // Generating momenta
        const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
        double s12 = square(_p[1]+_p[2]);
        const double s13 = square(_p[1]-_p[3])/s12;
        const double s14 = square(_p[1]-_p[4])/s12;
        const double s23 = square(_p[2]-_p[3])/s12;
        const double s24 = square(_p[2]-_p[4])/s12;
        s12 = 1.;
        const double s15 = -s12-s13-s14;
        const double s25 = -s12-s23-s24;
        const double zb = -(s15+s25)/s12;
        const double s15n = s15/zb;
        const double s25n = s25/zb;
        const double s35 = s12+s14+s24;
        const double s45 = s12+s13+s23;
        const double s34 = -s12-s15-s25;
        const double s35n = (s12+s14+s24)/zb;
        const double s45n = (s12+s13+s23)/zb;
        const double t12 = (s15-s25)/zb;
        const double t34 = (s35-s45)/zb;
        const double u = s13-s14-s23+s24;
        const double z=1.-zb;

        // Printing general information
        if (false) {
            const double s = sqrt(square(_p[1]+_p[2]))/2.;
//            cout << "zbar = " << zbar << "\n";
//            cout << "x1 = " << _x.x1 << "\t";
//            cout << "x2 = " << _x.x2 << "\n";
//            cout << "p1 = " << _p[1]/s << "\n";
//            cout << "p2 = " << _p[2]/s << "\n";
//            cout << "p3 = " << _p[3]/s << "\n";
//            cout << "p4 = " << _p[4]/s << "\n";
//            cout << "p5 = " << _p[5]/s << "\n";
//            cout << "p4.p25 = " << _p[4]*(_p[2]-_p[5])/s << "\n";
//            cout << "p4.p4 = " << _p[4]*_p[4]/s << "\np5.p5 = " << _p[5]*_p[5]/s << endl;
            cout << s13 << "\t" << s14 << "\t" << s23 << "\t" << s24 << "\t";
            cout << s15 << "\t" << s25 << "\t" << s35 << "\t" << s45 << "\t" << s34 << endl;
        }

        // Plotting counterterm vs. full ME
        if (true)
        {
            // Fudge factor
            /// \todo Figure out where this fudge belongs
            const double f = -16./3.; // this is 4*alphas^2*CF
            // Switches
            const bool LCon = true;
            const bool SCon = true;
            // Intermediate variables
            if (_cut(false)) {
                cout << zeta <<  "\t\t" << 0 << "\t" << 0 << "\t" << 0 << "\t";
                cout << 0 << "\t" << 0 << endl;
            } else {
                cout << zeta << "\t";
                // double arithmetics
                const double LCd = qq2yygstu6LCbub(s13,s14,s23,s24)+qq2yygstu6LCbox(s13,s14,s23,s24);
                const double SCd = qq2yygstu6SCbub(s13,s14,s23,s24)+qq2yygstu6SCbox(s13,s14,s23,s24);
                const double myfulld = f*(LCon*LCd+SCon*SCd);
                cout << myfulld << "\t";
                // quadruple arithmetics
                const __float128 s13q = s13;
                const __float128 s14q = s14;
                const __float128 s23q = s23;
                const __float128 s24q = s24;
                const double LCq = qq2yygstu6LCbub(s13q,s14q,s23q,s24q)+qq2yygstu6LCbox(s13q,s14q,s23q,s24q);
                const double SCq = qq2yygstu6SCbub(s13q,s14q,s23q,s24q)+qq2yygstu6SCbox(s13q,s14q,s23q,s24q);
                const double myfullq = f*(LCon*LCq+SCon*SCq);
                cout << myfullq << "\t";
                // rational arithmetics
#ifdef WITH_CLN
                const cln::cl_RA s13r = cln::rational(s13);
                const cln::cl_RA s14r = cln::rational(s14);
                const cln::cl_RA s23r = cln::rational(s23);
                const cln::cl_RA s24r = cln::rational(s24);
                const double LCr = qq2yygstu6LCbub(s13r,s14r,s23r,s24r)+qq2yygstu6LCbox(s13r,s14r,s23r,s24r);
                const double SCr = qq2yygstu6SCbub(s13r,s14r,s23r,s24r)+qq2yygstu6SCbox(s13r,s14r,s23r,s24r);
                const double myfullr = f*(LCon*LCr+SCon*SCr);
                cout << myfullr << "\t";
#endif
                // limit
                cout << _fullsoft(z,lambda,s13/s14,LCon,SCon) << endl;
           }
        }

    }

    exit(1);
    return;
}

bool GammaGamma_qq_NNLO_RV::_cut(const bool verbose) const
{
    if (verbose) {
        if (_cone.inside(_p[3],_p[5])) cout << "Gluon is inside cone of photon 3." << endl;
        if (_cone.inside(_p[4],_p[5])) cout << "Gluon is inside cone of photon 4." << endl;
        if (z*square(_p[1]+_p[2])<20.) cout << "The diphoton system is too soft." << endl;
        if (_p[3].T()<20.) cout << "Photon 3 does not have enough pT." << endl;
        if (_p[4].T()<20.) cout << "Photon 4 does not have enough pT." << endl;
    }
    if (
        _cone.inside(_p[3],_p[5])||_cone.inside(_p[4],_p[5])
        ||z*square(_p[1]+_p[2])<20.||_p[3].T()<20.||_p[4].T()<20.
        )
        return true;
    else return false;

}

double GammaGamma_qq_NNLO_RV::_coll(
                                     const double& z,
                                     const double& lambda,
                                     const double& ratio,
                                     const bool LCf,
                                     const bool SCf
                                     ) const
{
    return _coll1(z,lambda,ratio,LCf,SCf) + _coll2(z,lambda,ratio,LCf,SCf);
}

double GammaGamma_qq_NNLO_RV::_coll1(
                                    const double& z,
                                    const double& lambda,
                                    const double& ratio,
                                    const bool LCf,
                                    const bool SCf
                                    ) const
{
    return productCoeff(
                        Expansion<Parameter::epsilon,double>::exp(-log(lambda*(1.-z)/*muR*/),3),
                        CounterForge::Pqq<1>(z,LCf,SCf,3)*qq2yy<0>(ratio),
                        0
                        )/(-lambda*z);
}

double GammaGamma_qq_NNLO_RV::_coll2(
                                     const double& z,
                                     const double& lambda,
                                     const double& ratio,
                                     const bool LCf,
                                     const bool SCf
                                     ) const
{
    return productCoeff(
                        Expansion<Parameter::epsilon,double>::exp(-log(z/*muR*/),3),
                        CounterForge::Pqq<0>(z,LCf,SCf,3)*qq2yy<1>(ratio),
                        0
                        )/(-lambda*z);
}

double GammaGamma_qq_NNLO_RV::_fullsoft(
                                        const double& z,
                                        const double& lambda,
                                        const double& ratio,
                                        const bool LCf,
                                        const bool SCf
                                        ) const
{
    return _fullsoft1(z,lambda,ratio)+_fullsoft2(z,lambda,ratio);
}

double GammaGamma_qq_NNLO_RV::_fullsoft1(
                                         const double& z,
                                         const double& lambda,
                                         const double& ratio,
                                         const bool LCf,
                                         const bool SCf
                                         ) const
{
    return LCf*(-2.*productCoeff(CounterForge::soft<1>(z,lambda,3),qq2yy<0>(ratio),0));
}

double GammaGamma_qq_NNLO_RV::_fullsoft2(
                                         const double& z,
                                         const double& lambda,
                                         const double& ratio,
                                         const bool LCf,
                                         const bool SCf
                                         ) const
{
    return -1.*productCoeff(CounterForge::soft<0>(z,lambda,3),qq2yy<1>(ratio),0)*
        (LCf*0.5*QCD::CA/QCD::CF+SCf*(1.-0.5*QCD::CA/QCD::CF));
}

double GammaGamma_qq_NNLO_RV::_fullsoftcoll(const double& z, const double& lambda, const double& ratio) const
{
    return productCoeff(CounterForge::softcoll<1>(z,lambda),qq2yy<0>(ratio),0) +
           productCoeff(CounterForge::softcoll<0>(z,lambda),qq2yy<1>(ratio),0);
}

double GammaGamma_qq_NNLO_RV::_soft(const double& z, const double& lambda, const double& ratio) const
{
    // This is NOT the full soft limit, only the one-loop soft current term:
    // The tree soft current always cancels between soft and soft-collinear
    return productCoeff(CounterForge::soft<1>(z,lambda),qq2yy<0>(ratio),0);
}

double GammaGamma_qq_NNLO_RV::_softcoll(const double& z, const double& lambda, const double& ratio) const
{
    // This is NOT the full soft-collinear limit, only the one-loop soft-collinear current term:
    // The tree soft-collinear current always cancels between soft and soft-collinear
    return productCoeff(CounterForge::softcoll<1>(z,lambda),qq2yy<0>(ratio),0);
}


template<>
const SectorInfo XSectionMaker<GammaGamma_qq_NNLO_RV>::_info(
                                                              "NNLO real-virtual",
                                                              InitialStateFlavors::quarks,
                                                              2,
                                                              7
                                                              );

/**
 *
 * \file    gammagamma_me.cpp
 * \ingroup gamma_gamma
 * \author  Simone Lionetti
 * \date    January 2015
 *
 */

#include "qq2yyg/qq2yyg1rescue.h"
#include "gammagamma_me.h"
#include "boxmaster.h"
#include <iostream>
#include <cfloat> // DBL_EPSILON

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
        _eventBox->push_back(Event(w*zb*qq2yyg0<0>(zb,t12,t34,u),_p));
        // Pushing back collinear counterterms
        /// \bug Correct phase-space mapping from yyg to yy!!!
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
        << (1-z)*qq2yyg0<0>(zb,t12,t34,u) << "\t"
        << qq2yy<0,0>(s13/s14)*(CounterForge::Pqq<0>(z)).getCoefficient(0)/(-lambda*z) << "\t"
        << (1-z)*qq2yyg0<0>(zb,t12,t34,u)/
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
        << (1-z)*qq2yyg0<0>(zb,t12,t34,u) << "\t"
        << productCoeff(qq2yy<0>(s13/s14),CounterForge::soft<0>(z,lambda),0)/(-1) << "\t"
        << (1-z)*qq2yyg0<0>(zb,t12,t34,u)/
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
    cout.precision(12);
    if (_discard != 0) {
        --_discard;
        return;
    }
    randoms[0]=0.938775483229;
    randoms[1]=0.728345969387;
    randoms[2]=0.123493873457;
    //test(randoms);
    // Defining auxiliary names
    double& lambdaR = randoms.back();
    const double lambda = randoms.back();
    double& zR = randoms[3];
    const double z = randoms[3];
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
        const double qq2yyg = 16./3.*qq2yyg1_rescue::eval(_p,0);
        // Pushing back main event
        _eventBox->push_back( Event(w*qq2yyg,_p) );
        // Pushing back collinear counterterms
        lambdaR = 0.;
        _pg(randoms);
        const qq2yyg1_rescue::PSpoint pc1(_p);
        _eventBox->push_back( Event(w*_coll(z,lambda,pc1.s13/pc1.s14),_p) );
        lambdaR = 1.;
        _pg(randoms);
        const qq2yyg1_rescue::PSpoint pc2(_p);
        _eventBox->push_back( Event(w*_coll(z,1.-lambda,pc2.s13/pc2.s14),_p) );
        // Pushing back soft counterterm
        lambdaR = lambda;
        zR = 1.;
        _pg(randoms);
        const qq2yyg1_rescue::PSpoint ps(_p);
        const double r = ps.s13/ps.s14;
        _eventBox->push_back( Event(w*_soft(z,lambda,r),_p) );
        // Pushing back soft-collinear counterterms
        _eventBox->push_back( Event(-w*_softcoll(z,lambda,r),_p) );
        _eventBox->push_back( Event(-w*_softcoll(z,1.-lambda,r),_p) );
    }
    return;
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
    for (double Y = -12; true && Y <= 12+1.e-7; Y+=0.1/*025*/)
    {
        const double ey = exp(Y);
        lambda = ey/(ey+1./ey);

        // Generating momenta
        const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
        const qq2yyg1_rescue::PSpoint p(_p);

        // Printing general information
        if (false) {
            const double s = p.s12/2.;
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
        }

        // Counterterms

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
            cout << Y << "\t\t";
            // Full ME
            double myfull;
            if (LCon && SCon) myfull = f*qq2yyg1_rescue::eval(_p,0);
            else {
                const qq2yyg1<rtn>::PSpoint prtn = qq2yyg1<rtn>::PSpoint(_p);
                if (LCon) myfull = f*qq2yyg1<rtn>::LC::eval(prtn,0);
                if (SCon) myfull = f*qq2yyg1<rtn>::SC::eval(prtn,0);
                if (!LCon && !SCon) myfull = f*qq2yyg1<rtn>::Nf::eval(prtn,0);
            }
            cout << myfull << "\t";
            // limits
            const double lam = lambda;
            lambda = 0.;
            _pg(randoms);
            const qq2yyg1_rescue::PSpoint pc1(_p);
            const double mycoll1 = _coll(z,lam,pc1.s13/pc1.s14,LCon,SCon);
            lambda = 1.;
            _pg(randoms);
            const qq2yyg1_rescue::PSpoint pc2(_p);
            const double mycoll2 = _coll(z,1.-lam,pc2.s13/pc2.s14,LCon,SCon);
            cout << mycoll1 << "\t" << mycoll2 << endl;
        }

    }

    // Loop on z
    cout << "Testing soft limit" << endl;
    lambda=0.852984766;
    cout << "lambda = " << lambda << endl << endl;
    for (double H = -10; true && H <= 10+1.e-7; H+=0.1/*025*/) {

        const double emh = exp(-H);
        const double eh = exp(H);
        z = emh/(emh+eh);
        double zbar = eh/(emh+eh);

        // Generating momenta
        const double w = _prefactor * _factor * zbar * _pg(randoms); // 1-z from phase space
        const qq2yyg1_rescue::PSpoint p(_p);

        // Printing general information
        if (false) {
            const double s = p.s12/2.;
            cout << "zbar = " << zbar << "\n";
            cout << "x1 = " << _x.x1 << "\t";
            cout << "x2 = " << _x.x2 << "\n";
            cout << "p1 = " << _p[1]/s << "\n";
            cout << "p2 = " << _p[2]/s << "\n";
            cout << "p3 = " << _p[3]/s << "\n";
            cout << "p4 = " << _p[4]/s << "\n";
            cout << "p5 = " << _p[5]/s << "\n";
            cout << "p4.p25 = " << _p[4]*(_p[2]-_p[5])/s << "\n";
            cout << "p4.p4 = " << _p[4]*_p[4]/s << "\np5.p5 = " << _p[5]*_p[5]/s << endl;
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
            cout << H << "\t";
            // Full ME
            double myfull;
            if (LCon && SCon) myfull = f*qq2yyg1_rescue::eval(_p,0);
            else {
                const qq2yyg1<rtn>::PSpoint prtn = qq2yyg1<rtn>::PSpoint(_p);
                if (LCon) myfull = f*qq2yyg1<rtn>::LC::eval(prtn,0);
                if (SCon) myfull = f*qq2yyg1<rtn>::SC::eval(prtn,0);
                if (!LCon && !SCon) myfull = f*qq2yyg1<rtn>::Nf::eval(prtn,0);
            }
            cout << myfull << "\t";
            // Soft limit
            z = 1.;
            _pg(randoms);
            const qq2yyg1_rescue::PSpoint ps(_p);
            cout << _fullsoft(1.-zbar,lambda,ps.s13/ps.s14,LCon,SCon) << endl;
        }

    }

    // Soft-collinear
    cout << "Testing soft-collinear limit" << endl;
    for (double H = -8; true && H <= 8+1.e-7; H+=0.1/*02*/) {

        const double emh = exp(-H);
        const double eh = exp(H);
        z = emh/(emh+eh);
        double zbar = eh/(emh+eh);
        lambda = zbar;
        double lam = lambda;

        // Generating momenta
        const double w = _prefactor * _factor * zbar * _pg(randoms); // 1-z from phase space
        const qq2yyg1_rescue::PSpoint p(_p);

        // Plotting counterterm vs. full ME
        if (false)
        {
            // Fudge factor
            /// \todo Figure out where this fudge belongs
            const double f = -16./3.; // this is 4*alphas^2*CF
            // Switches
            const bool LCon = true;
            const bool SCon = true;
            // Intermediate variables
            cout << H << "\t";
            // Full ME
            double myfull;
            if (LCon && SCon) myfull = f*qq2yyg1_rescue::eval(_p,0);
            else {
                const qq2yyg1<rtn>::PSpoint prtn = qq2yyg1<rtn>::PSpoint(_p);
                if (LCon) myfull = f*qq2yyg1<rtn>::LC::eval(prtn,0);
                if (SCon) myfull = f*qq2yyg1<rtn>::SC::eval(prtn,0);
                if (!LCon && !SCon) myfull = f*qq2yyg1<rtn>::Nf::eval(prtn,0);
            }
            cout << myfull << "\t";
            // Coll limit
            lambda = 0.;
            _pg(randoms);
            const qq2yyg1_rescue::PSpoint pc(_p);
            cout << _coll(z,lam,pc.s13/pc.s14,LCon,SCon) << "\t";
            // Soft limit
            lambda = lam;
            z = 1.;
            _pg(randoms);
            const qq2yyg1_rescue::PSpoint ps(_p);
            cout << _fullsoft(1.-zbar,lambda,ps.s13/ps.s14,LCon,SCon) << "\t";
            // Soft-collinear limit
            // (same kinematics as soft!)
            cout << _fullsoftcoll(1.-zbar,lambda,ps.s13/ps.s14) << endl;
        }
        
        // Plotting full vs. simplified soft/soft-collinear
        if (true)
        {
            cout << H << "\t";
            z = 1.;
            _pg(randoms);
            const qq2yyg1_rescue::PSpoint ps(_p);
            const double r = ps.s13/ps.s14;
            const double zz = 1.-zbar;
            cout << _fullsoft(zz,lambda,r)-_fullsoftcoll(zz,lambda,r)-_fullsoftcoll(zz,1.-lambda,r) << "\t";
            cout << _soft(zz,lambda,r)-_softcoll(zz,lambda,r)-_softcoll(zz,1.-lambda,r) << endl;
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
                                     )
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
                                        )
{
    return _fullsoft1(z,lambda,ratio,LCf,SCf)+_fullsoft2(z,lambda,ratio,LCf,SCf);
}

double GammaGamma_qq_NNLO_RV::_fullsoft1(
                                         const double& z,
                                         const double& lambda,
                                         const double& ratio,
                                         const bool LCf,
                                         const bool SCf
                                         )
{
    return LCf*(-2.*productCoeff(CounterForge::soft<1>(z,lambda,3),qq2yy<0>(ratio),0));
}

double GammaGamma_qq_NNLO_RV::_fullsoft2(
                                         const double& z,
                                         const double& lambda,
                                         const double& ratio,
                                         const bool LCf,
                                         const bool SCf
                                         )
{
    return -1.*productCoeff(CounterForge::soft<0>(z,lambda,3),qq2yy<1>(ratio),0)*
        (LCf*0.5*QCD::CA/QCD::CF+SCf*(1.-0.5*QCD::CA/QCD::CF));
}

double GammaGamma_qq_NNLO_RV::_fullsoftcoll(const double& z, const double& lambda, const double& ratio) const
{
    return -1.*(
                productCoeff(CounterForge::softcoll<1>(z,lambda,3),qq2yy<0>(ratio),0) +
                productCoeff(CounterForge::softcoll<0>(z,lambda,3),qq2yy<1>(ratio),0)
                );
}

double GammaGamma_qq_NNLO_RV::_soft(const double& z, const double& lambda, const double& ratio) const
{
    // This is NOT the full soft limit, only the one-loop soft current term:
    // The tree soft current always cancels between soft and soft-collinear
    return -2.*productCoeff(CounterForge::soft<1>(z,lambda,3),qq2yy<0>(ratio),0);
}

double GammaGamma_qq_NNLO_RV::_softcoll(const double& z, const double& lambda, const double& ratio) const
{
    // This is NOT the full soft-collinear limit, only the one-loop soft-collinear current term:
    // The tree soft-collinear current always cancels between soft and soft-collinear
    return -1.*productCoeff(CounterForge::softcoll<1>(z,lambda,3),qq2yy<0>(ratio),0);
}


template<>
const SectorInfo XSectionMaker<GammaGamma_qq_NNLO_RV>::_info(
                                                              "NNLO real-virtual",
                                                              InitialStateFlavors::quarks,
                                                              2,
                                                              7
                                                              );

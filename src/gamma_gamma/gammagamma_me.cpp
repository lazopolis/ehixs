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
                                   w*qq2yyg(
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
    for (double reflam = 0.5123419384701234; reflam > 1.e-10; reflam*=0.9) {
        // Defining auxiliary names
        randoms.back() = reflam;
        const double& z = randoms[3];
        // Generating momenta
        const double w = _prefactor * _factor * (1.-z) * _pg(randoms); // 1-z from phase space
        const double s12 = square(_p[1]+_p[2]);
        const double s13 = square(_p[1]-_p[3]);
        const double s14 = square(_p[1]-_p[4]);
        const double s23 = square(_p[2]-_p[3]);
        const double s24 = square(_p[2]-_p[4]);
        // Output
        if (false) {
            cout << "\n ----- lambda = " << reflam << " ----- \n";
            cout << s12 << "\t" << s13 << "\t" << s14 << "\t" << s23 << "\t" << s24 << endl;
            cout << square(_p[1]-_p[5]) << "\t" << reflam*(1.-z)*s12 << "\t" << -s12-s13-s14 << endl;
        }
        if (false) {
            double res12;
            double res21;
            res12 = qq2yygCAbub(s12,s13,s14,s23,s24)+qq2yygCAbub(s12,s14,s13,s24,s23);
            res21 = qq2yygCAbub(s12,s23,s24,s13,s14)+qq2yygCAbub(s12,s24,s23,s14,s13);
            cout << "CA bubbles " << "\t"
            << res12 << "\t" << res21 << "\t" << (res12-res21)/(res12+res21) << "\n";
            res12 = qq2yygCAbox(s12,s13,s14,s23,s24)+qq2yygCAbox(s12,s14,s13,s24,s23);
            res21 = qq2yygCAbox(s12,s23,s24,s13,s14)+qq2yygCAbox(s12,s24,s23,s14,s13);
            cout << "CA boxes   " << "\t"
            << res12 << "\t" << res21 << "\t" << (res12-res21)/(res12+res21) << "\n";
            res12 = qq2yygCFbub(s12,s13,s14,s23,s24)+qq2yygCFbub(s12,s14,s13,s24,s23);
            res21 = qq2yygCFbub(s12,s23,s24,s13,s14)+qq2yygCFbub(s12,s24,s23,s14,s13);
            cout << "CF bubbles " << "\t"
            << res12 << "\t" << res21 << "\t" << (res12-res21)/(res12+res21) << "\n";
            res12 = qq2yygCFbox(s12,s13,s14,s23,s24)+qq2yygCFbox(s12,s14,s13,s24,s23);
            res21 = qq2yygCFbox(s12,s23,s24,s13,s14)+qq2yygCFbox(s12,s24,s23,s14,s13);
            cout << "CF boxes   " << "\t"
            << res12 << "\t" << res21 << "\t" << (res12-res21)/(res12+res21) << "\n";
            res12 = qq2yygAFbub(s12,s13,s14,s23,s24)+qq2yygAFbub(s12,s14,s13,s24,s23);
            res21 = qq2yygAFbub(s12,s23,s24,s13,s14)+qq2yygAFbub(s12,s24,s23,s14,s13);
            cout << "AF bubbles " << "\t"
            << res12 << "\t" << res21 << "\t" << (res12-res21)/(res12+res21) << "\n";
            res12 = qq2yygAFbox(s12,s13,s14,s23,s24)+qq2yygAFbox(s12,s14,s13,s24,s23);
            res21 = qq2yygAFbox(s12,s23,s24,s13,s14)+qq2yygAFbox(s12,s24,s23,s14,s13);
            cout << "AF boxes   " << "\t"
            << res12 << "\t" << res21 << "\t" << (res12-res21)/(res12+res21) << "\n";
            res12 = qq2yyg(s12,s13,s14,s23,s24)+qq2yyg(s12,s14,s13,s24,s23);
            res21 = qq2yyg(s12,s23,s24,s13,s14)+qq2yyg(s12,s24,s23,s14,s13);
            cout << "Total      " << "\t"
            << res12 << "\t" << res21 << "\t" << (res12-res21)/(res12+res21) << "\n";
        }
        if (true) {
            cout << reflam << "\t";
            cout << qq2yygCAbub(s12,s13,s14,s23,s24)+qq2yygCAbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yygCFbub(s12,s13,s14,s23,s24)+qq2yygCFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yygAFbub(s12,s13,s14,s23,s24)+qq2yygAFbub(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yygCAbox(s12,s13,s14,s23,s24)+qq2yygCAbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yygCFbox(s12,s13,s14,s23,s24)+qq2yygCFbox(s12,s14,s13,s24,s23) << "\t";
            cout << qq2yygAFbox(s12,s13,s14,s23,s24)+qq2yygAFbox(s12,s14,s13,s24,s23) << endl;
        }
        if (false) {
            cout << " (" << qq2yygCAm2CF<1>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(s12,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<1>(s12,s13,s14,s23,s24),bubble(s12,3),0) << endl;
            cout << " (" << qq2yygCAm2CF<2>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(s13,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<2>(s12,s13,s14,s23,s24),bubble(s13,3),0) << endl;
            cout << " (" << qq2yygCAm2CF<3>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(s14,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<3>(s12,s13,s14,s23,s24),bubble(s14,3),0) << endl;
            cout << " (" << qq2yygCAm2CF<4>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(-s12-s13-s14,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<4>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14,3),0) << endl;
            cout << " (" << qq2yygCAm2CF<5>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(s23,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<5>(s12,s13,s14,s23,s24),bubble(s23,3),0) << endl;
            cout << " (" << qq2yygCAm2CF<6>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(s24,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<6>(s12,s13,s14,s23,s24),bubble(s24,3),0) << endl;
            cout << " (" << qq2yygCAm2CF<7>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(-s12-s23-s24,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<7>(s12,s13,s14,s23,s24),bubble(-s12-s23-s24,3),0) << endl;
            cout << " (" << qq2yygCAm2CF<8>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(-s12-s13-s14-s23-s24,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<8>(s12,s13,s14,s23,s24),bubble(-s12-s13-s14-s23-s24,3),0) << endl;
            cout << " (" << qq2yygCAm2CF<9>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(s12+s13+s23,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<9>(s12,s13,s14,s23,s24),bubble(s12+s14+s24,3),0) << endl;
            cout << " (" << qq2yygCAm2CF<10>(s12,s13,s14,s23,s24) << ") * (";
            cout << bubble(s12+s14+s24,3) << ") == ";
            cout << productCoeff(qq2yygCAm2CF<10>(s12,s13,s14,s23,s24),bubble(s12+s13+s23,3),0) << endl;
        }
    }
    exit(1);
    return;
}

template<>
const SectorInfo XSectionMaker<GammaGamma_qq_NNLO_RV>::_info(
                                                              "NNLO real-virtual",
                                                              InitialStateFlavors::quarks,
                                                              2,
                                                              7
                                                              );

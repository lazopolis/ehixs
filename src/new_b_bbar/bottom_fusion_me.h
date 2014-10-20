#ifndef BOTTOM_FUSION_ME_H
#define BOTTOM_FUSION_ME_H

#include <stdlib.h>
#include "chaplin.h"
#include "xsectionmaker.h"
#include "parametrizations.h"
#include "xgenerator.h"

/// \todo Move this to either Constants, UserInterface, Model or whatever
constexpr double yukawa_bottom = 1.0;

template<size_t N>
class BottomFusion_bb;

/**
 *
 * \class BottomFusion_bb<0>
 * \brief Mother class for subprocesses with bbar initial state and delta-like kinematics
 *
 */

template<>
class BottomFusion_bb<0> : public XSection
{

public:

    BottomFusion_bb<0>(const UserInterface& UI, const SectorInfo& info) :
    XSection(UI, info), _p(), _xg(_x), _pg(_p, _x), _tau(pow(2.*UI.m_higgs/UI.Etot,2))
    {
        _p.resize(3);
        _prefactor *= consts::Pi * pow(yukawa_bottom,2) / (2. * QCD::Nc * pow(UI.m_higgs,2));
        _xg.setParameters(_tau);
        _pg.setParameters(UI.Etot*UI.Etot*0.25, vector<double>({UI.m_higgs}));
        return;
    }

    double generateXs(vector<double>& randoms)
    {
        return _xg(randoms);
    }

protected:

    Momenta _p;
    OneXGenerator _xg;
    DeltaPG _pg;
    double _tau;

};

/**
 *
 * \class BottomFusion_bb<1>
 * \brief Mother class for subprocesses with bbar initial state and one extra particle in the final state
 *
 */

template<>
class BottomFusion_bb<1> : public XSection
{

public:

    BottomFusion_bb<1>(const UserInterface& UI, const SectorInfo& info) :
    XSection(UI, info), _p(), _xg(_x), _pg(_p, _x), _tau(pow(2.*UI.m_higgs/UI.Etot,2))
    {
        _p.resize(4);
        cout << "\nmH:\t" << UI.m_higgs << "\nEtot:\t" << UI.Etot << "\ntau:\t" << _tau << endl;
        _prefactor *= consts::Pi * pow(yukawa_bottom,2) / (2. * QCD::Nc * pow(UI.m_higgs,2));
        _xg.setParameters(_tau);
        _pg.setParameters(UI.Etot*UI.Etot*0.25, vector<double>({UI.m_higgs}));
        return;
    }

    double generateXs(vector<double>& randoms)
    {
        return _xg(randoms);
    }

protected:

    Momenta _p;
    FlatXGenerator _xg;
    ZlambdaPG _pg;
    double _tau;

};

/**
 *
 * \class BottomFusion_bb_LO
 * \brief LO sector for bbar->H
 *
 */

class BottomFusion_bb_LO : public BottomFusion_bb<0>
{

public:


    BottomFusion_bb_LO(const UserInterface& UI) :
    BottomFusion_bb<0>(UI, XSectionMaker<BottomFusion_bb_LO>::_info)
    {}

    void generateEvents(vector<double>& randoms);

};

/**
 *
 * \class BottomFusion_bb_NLO_hard
 * \brief LO sector for bbar->H
 *
 */

class BottomFusion_bb_NLO_hard : public BottomFusion_bb<1>
{

public:


    BottomFusion_bb_NLO_hard(const UserInterface& UI) :
    BottomFusion_bb<1>(UI, XSectionMaker<BottomFusion_bb_NLO_hard>::_info)
    {
        _prefactor *= 8. * consts::Pi * QCD::CF /*alphas*/;
        return;
    }

    void generateEvents(vector<double>& randoms);

private:

    double regularME(const double& z) const
    {
        return (1.+z*z)/(1.-z);
    }

    static const double _cutoff;

};

/**
 *
 * \class BottomFusion_bb_NLO_soft
 * \brief NLO soft sector for bbar->H
 * \todo  Divide stuff between soft and collinear sectors
 *
 */

class BottomFusion_bb_NLO_soft : public BottomFusion_bb<0>
{

public:


    BottomFusion_bb_NLO_soft(const UserInterface& UI) :
    BottomFusion_bb<0>(UI, XSectionMaker<BottomFusion_bb_NLO_soft>::_info)
    {
        _prefactor *= 2. * QCD::CF /*alphas*/;
        return;
    }

    void generateEvents(vector<double>& randoms);

private:

    double f0(const double& z) const
    {
        const double zbar = 1.-z;
        return zbar*zbar + log(z);
    }

    double f1(const double& z) const
    {
        return 2.*(1.+z*z);
    }

};

/**
 *
 * \class BottomFusion_bb_NNLO_RV
 * \brief LO sector for bbar->H
 *
 */

class BottomFusion_bb_NNLO_RV : public BottomFusion_bb<1>
{

public:


    BottomFusion_bb_NNLO_RV(const UserInterface& UI) :
    BottomFusion_bb<1>(UI, XSectionMaker<BottomFusion_bb_NNLO_RV>::_info)
    {
        _prefactor *= 8. * consts::Pi * QCD::CF /*alphas*/;
        return;
    }

    void generateEvents(vector<double>& randoms);

private:

    static double full(const double& z, const double& lambda)
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

    static double coll(const double& z, const double& lambda)
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

    static double soft(const double& z, const double& lambda)
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

    static double softcoll(const double& z, const double& lambda)
    {
        const double l1 = log(lambda);
        const double l2 = log(1.-z);
        return 32.*QCD::CA*(
                            12.*l1*l2 + 3.*l1*l1 + 12.*l2*l2
                            - 2.*consts::pi_square
                            ) / (3.*lambda*(1.-z));
    }

};

#endif

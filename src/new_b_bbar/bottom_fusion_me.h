#ifndef BOTTOM_FUSION_ME_H
#define BOTTOM_FUSION_ME_H

#include <stdlib.h>
#include "bb2HX.h"
#include "xsectionmaker.h"
#include "parametrizations.h"
#include "xgenerator.h"
#include "counterforge.h"
#include "bb2Hg/bb2HRVcoeffs.h"

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
        _prefactor *= 4. * consts::Pi /*alphas*/;
        test();
        return;
    }

    void generateEvents(vector<double>& randoms);

    static bool test();

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
    BottomFusion_bb<1>(UI, XSectionMaker<BottomFusion_bb_NNLO_RV>::_info),
    CounterTerm()
    {
        Expansion<Parameter::epsilon,double>::accuracy = 3;
        CounterTerm = new CounterForge(CounterForge::Scheme::CDR);
        //Check this
        _prefactor *= 8. * consts::Pi * QCD::CF /*alphas*/;
        return;
    }

    ~BottomFusion_bb_NNLO_RV()
    {
        delete CounterTerm;
    }

    void generateEvents(vector<double>& randoms);

    CounterForge* CounterTerm;

    static bool test(void);

private:

    double fastcoll(const double& z, const double& lambda);

    static double coll(const double& z, const double& lambda);

    static double soft(const double& z, const double& lambda);

    static double softcoll(const double& z, const double& lambda);

    static double explicitcoll(const double& z, const double& lambda);

};

#endif

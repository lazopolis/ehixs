#ifndef GLUON_FUSION_ME_H
#define GLUON_FUSION_ME_H

#include <stdlib.h>
#include "xsectionmaker.h"
#include "variables.h"



/**
 *
 * \class GluonFusion_gg_Delta
 * Mother class for subprocesses with gg initial state and delta-like kinematics
 *
 */

class GluonFusion_gg_Delta : public XSection
{
    
public:
    
    GluonFusion_gg_Delta(const UserInterface& UI, const SectorInfo& info);
    double generateXs(vector<double>& randoms)
    {
        return _xg(randoms);
    }
protected:
    Momenta _p;
    OneXGenerator _xg;
    DeltaPG _pg;
};

/**
 *
 * \class GluonFusion_gg_LO
 * LO matrix element for gg->H
 *
 */

class GluonFusion_gg_LO : public GluonFusion_gg_Delta
{
    
public:
    
    
    GluonFusion_gg_LO(const UserInterface& UI) :
    GluonFusion_gg_Delta(UI, XSectionMaker<GluonFusion_gg_LO>::_info)
    {}
    
    double matrixElement(const KinematicInvariants& invariants) const;
    void generateEvents(vector<double>& randoms);
};

#endif

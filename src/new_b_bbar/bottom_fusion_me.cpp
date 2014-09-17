#include "bottom_fusion_me.h"

// BottomFusion_bb_Delta

BottomFusion_bb_Delta::BottomFusion_bb_Delta(const UserInterface& UI, const SectorInfo& info)
: BottomFusionXSection(UI, info)
{
    cout << "\n *** mh = " << _mH << endl;
    _kin = new KinematicVariables<OneXGenerator,DeltaPG>();
    _kin->setParameters(_smax,vector<double>({_mH}));
    
    _prefactor *= consts::Pi * pow(yukawa_bottom,2.0)/2./QCD::Nc/pow(_mH,2.);
    return;
}

double BottomFusion_bb_LO::matrixElement(const KinematicInvariants& invariants) const
{
    return 1.0;
}

template<>
const SectorInfo XSectionMaker<BottomFusion_bb_LO>::_info(
                                                   "Born",
                                                    InitialStateFlavors(QCD::b, QCD::bbar),
                                                    0,
                                                    1
                                                   );

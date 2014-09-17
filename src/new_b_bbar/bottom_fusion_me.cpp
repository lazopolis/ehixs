#include "bottom_fusion_me.h"

// BottomFusion_bb_Delta

BottomFusion_bb_Delta::BottomFusion_bb_Delta(const UserInterface& UI, const SectorInfo& info) :
XSection(UI, info)
{
    _prefactor *= consts::Pi * pow(yukawa_bottom,2.) / (2. * QCD::Nc * pow(UI.m_higgs,2.));
    _kin = new KinematicVariables<OneXGenerator,DeltaPG>(UI.Etot*UI.Etot,{UI.m_higgs},info.dim);
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

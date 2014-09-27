#include "bottom_fusion_me.h"


// BottomFusion_bb_LO

void BottomFusion_bb_LO::generateEvents(const double* const randoms)
{
    _pg(randoms+1);
    ///////HMMMMMM?!?!?!
    const double w = _prefactor * _factor / (2. * 125.4 * 125.4);
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

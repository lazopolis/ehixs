
#include "cross_section.h"
#include <sstream>
using namespace std;



ostream& operator<<(ostream& stream, const CrossSection& XS)
{
    stream
    <<XS._info.name<<"("<<XS._info.ISF.left<<","<<XS._info.ISF.right
    <<"): a^"<<XS._info.alpha_power
    <<" ,dim="<<XS.dimension();
    
    return stream;
}

void CrossSection::initialize(const UserInterface& UI)
{
    // Getting general parameters from the user interface
    _smax = UI.Etot*UI.Etot;
    _muR = UI.mur;
    _muF = UI.muf;
    // Configuring the luminosity
    _lumi = AllocateLuminosity(UI);
    // Setting up the running
    _model.Configure(
                     _lumi->alpha_s_at_mz(),
                     UI.mur_over_mhiggs,
                     UI.perturbative_order,
                     UI.m_higgs
                     );
    _as_pi = _model.alpha_strong()/consts::Pi;
    cout << "\n[CrossSection]: a_s = " << _as_pi * consts::Pi;
    return;
}

void CrossSection::SetEventBox(EventBox& event_box)
{
   event_box_=&event_box;
}


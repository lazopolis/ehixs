
#include "cross_section.h"
#include <sstream>
using namespace std;



ostream& operator<<(ostream& stream, const CrossSection& XS)
{
    stream<<XS._info.name<<"("<<XS._info.ISF.left<<","<<XS._info.ISF.right
    <<"): a^"<<XS._info.alpha_power
    <<" ,dim="<<XS.dimension();
    
    return stream;
}

void CrossSection::startRunning(const UserInterface& UI)
{
   _model.Configure(
                    _lumi->alpha_s_at_mz(),
                    UI.mur_over_mhiggs,
                    UI.perturbative_order,
                    UI.m_higgs
                    );
   _as_pi = _model.alpha_strong()/consts::Pi;
   cout << "\n[CrossSection]: a_s(MZ) = " << _as_pi * consts::Pi;
   return;
}

void CrossSection::SetScales(const double& mur,const double& muf)
{
   _muR = mur;
   _muF = muf;
   return;
}

void CrossSection::SetEColliderSq(const double& smaximum)
{
   _smax=smaximum;
   return;
}

void CrossSection::SetEventBox(EventBox& event_box)
{
   event_box_=&event_box;
}


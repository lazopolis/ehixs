
#include "cross_section.h"
#include <sstream>
using namespace std;



ostream& operator<<(ostream& stream, const CrossSection& XS)
{
    stream<<XS.info_.name<<"("<<XS.info_.ISF.left<<","<<XS.info_.ISF.right
    <<"): a^"<<XS.info_.alpha_power
    <<" ,dim="<<XS.dimension_;
    
    return stream;
}

void CrossSection::SetAlphaStrong(const double& a_s_over_pi)
{
    a_s_over_pi_=a_s_over_pi;
    cout<<"\n[CrossSection]: a_s = "<<a_s_over_pi_* consts::Pi;
}

void CrossSection::SetScales(const double& mur,const double& muf)
{mur_=mur; muf_=muf;}

void CrossSection::SetEColliderSq(const double& smaximum)
{smax=smaximum;}

void CrossSection::SetEventBox(EventBox& event_box)
{event_box_=&event_box;}







#include "bottom_fusion_me.h"

// BottomFusion_bb

//BottomFusion_bb::BottomFusion_bb()
//{
//    info.ISF = InitialStateFlavors("b","bbar");
//}

NewLuminosity* BottomFusion_bb::AllocateLuminosity(const UserInterface& UI)
{
    NewLuminosity* foo;
    foo = new NewLuminosity(UI);
    foo->add_pair(-5,5);
    foo->add_pair(5,-5);
    // bad hack
    mh_ = UI.m_higgs;
    return foo;
}

// BottomFusion_bb_Delta

void BottomFusion_bb_Delta::Configure()
{
    //refactor: make a ConfigureBase function and move smin setting there
    cout<<"\n *** mh = "<<mh_<<endl;
    kk_.setParameters(_smax,vector<double>({mh_}));
    
    const double yukawa_bottom = 1.0;
    prefactor_ = consts::Pi * pow(yukawa_bottom,2.0)/2./QCD::Nc/pow(mh_,2.)
                *consts::convert_GeV_to_pb;
}

void BottomFusion_bb_Delta::Evaluate(double* xx_vegas)
{
    
    kk_.generate(xx_vegas);
    const double myxlumi = _lumi->give(kk_.x1,kk_.x2);
    if (myxlumi!=0.0)
    {
        const double me_sq = eval_me(kk_);
        const double sigma = prefactor_ * kk_.jacobian
        * myxlumi
        * 1.0/(2.0*kk_.s(1,2)) //flux
        * me_sq
        ;
        event_box_->add(sigma, kk_.p);
    }
    else
    {
        event_box_->add();
    }
    
}

// BottomFusion_bb_LO

//BottomFusion_bb_LO::BottomFusion_bb_LO():BottomFusion_bb_Delta()
//{
//    _info.name = "Born";
//}

double BottomFusion_bb_LO::eval_me(const KinematicInvariants& kinvar)
{
    return 1.0;
}

template<>
const SectorInfo XSectionMaker<BottomFusion_bb_LO>::_info(
                                                   "Born",
                                                    InitialStateFlavors(QCD::Flavor::b, QCD::Flavor::bbar),
                                                    0,
                                                    1
                                                   );

// BottomFusion_bb_NLO

BottomFusion_bb_NLO_Soft::BottomFusion_bb_NLO_Soft()
{}

double BottomFusion_bb_NLO_Soft::eval_me(const KinematicInvariants& kinvar)
{
    const double z = kinvar.q(3,3);
    return 0.;
}





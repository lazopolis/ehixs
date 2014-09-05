#include "bottom_fusion_me.h"

// BottomFusionCrossSection

void BottomFusionCrossSection::JF(const double& w,const vector<FMomentum>& kv)
{
    if (w!=w)
    {
        cout<<"\nerror: nan as event weight. w="<<w;
        //cout<<kv;
        cout<<endl;
        exit(1);
    }
    event_box_->AddNewEvent(w);
    for (size_t i = 0; i < kv.size(); ++i)
        event_box_->SetP(i+1,kv[i](0),kv[i](1),kv[i](2),kv[i](3));
    return;
}

void BottomFusionCrossSection::JF()
{
    event_box_->AddNewEvent(0.0);
    return;
}

// BottomFusion_bb

BottomFusion_bb::BottomFusion_bb()
{
    //refactor: move this to the LO daughter class
    number_of_particles_ = 5;
    //info_ = new NewMeExternalInfo;
    //dimension_ = 4;

    //info_.ISF = InitialStateFlavors("u","ub");
    //pdf_selection_ = "same flavor";
    info_.ISF = InitialStateFlavors("b","bbar");
}

void BottomFusion_bb::AllocateLuminosity(const UserInterface& UI)
{
    lumi = new NewLuminosity(UI);
    lumi->add_pair(-5,5);
    lumi->add_pair(5,-5);
}

double BottomFusion_bb::LL(const double& x1,const double& x2)
{
    return lumi->give(x1,x2);
}

/// \todo Destructor deleting lumi missing!!!

// BottomFusion_bb_Delta

void BottomFusion_bb_Delta::Configure()
{
    //refactor: make a ConfigureBase function and move smin setting there
    const double smin = pow(mh_,2.0);
    cout<<"\n *** mh_sq = "<<smin<<endl;
    kk_.setBoundaries(smin,smax);
    
    const double Nc = 3;
    const double yukawa_bottom = 1.0;
    prefactor_ = consts::Pi * pow(yukawa_bottom,2.0)/2./Nc/pow(mh_,2.)
                *consts::convert_GeV_to_pb;
}

void BottomFusion_bb_Delta::Evaluate(double* xx_vegas)
{
    
    kk_.generate(xx_vegas);
    const double myxlumi = LL(kk_.x1(),kk_.x2());
    if (myxlumi!=0.0)
    {
        const double me_sq = eval_me(kk_);
        const double sigma = prefactor_ * kk_.jacobian
        * myxlumi
        * 1.0/(2.0*kk_.s(1,2)) //flux
        * me_sq
        ;
        JF(sigma,kk_.p);
    }
    else
    {
        JF();
    }
    
}

// BottomFusion_bb_LO

BottomFusion_bb_LO::BottomFusion_bb_LO():BottomFusion_bb_Delta()
{
    info_.name = "Born";
}

double BottomFusion_bb_LO::eval_me(const KinematicInvariants& kinvar)
{
    return 1.0;
}

// BottomFusion_bb_NLO

BottomFusion_bb_NLO_Soft::BottomFusion_bb_NLO_Soft()
{
    info_.alpha_power = 1;
    info_.name = "NLO Soft";
}

double BottomFusion_bb_NLO_Soft::eval_me(const KinematicInvariants& kinvar)
{
    const double z = kinvar.q(3,3);
    return 0.;
}





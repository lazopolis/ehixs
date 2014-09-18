
#include "bottom_fusion_me.h"

void BottomFusionCrossSection::JF(const double& w,const BottomFusionKinematics& kv)
{
    if (w!=w)
    {
        cout<<"\nerror: nan as event weight. w="<<w;
        cout<<kv;
        cout<<endl;
        exit(1);
    }
    event_box_->AddNewEvent(w);
    event_box_->SetP(1,kv.p1.p[0],kv.p1.p[1],kv.p1.p[2],kv.p1.p[3]);
    event_box_->SetP(2,kv.p2.p[0],kv.p2.p[1],kv.p2.p[2],kv.p2.p[3]);
    event_box_->SetP(3,kv.p3.p[0],kv.p3.p[1],kv.p3.p[2],kv.p3.p[3]);
    event_box_->SetP(4,kv.p4.p[0],kv.p4.p[1],kv.p4.p[2],kv.p4.p[3]);
    event_box_->SetP(5,kv.p5.p[0],kv.p5.p[1],kv.p5.p[2],kv.p5.p[3]);
    
}

void BottomFusionCrossSection::JF()
{
    event_box_->AddNewEvent(0.0);
}


BottomFusion_bb::BottomFusion_bb()
{
    //refacator: move this to the LO daughter class
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









void BottomFusion_bb_Delta::Configure()
{
    kk_.SetNumberOfParticles(3);
    //refactor: make a ConfigureBase function and move smin setting there
    const double smin = pow(mh_,2.0);
    cout<<"\n *** mh_sq = "<<smin<<endl;
    kk_.SetBoundaries(smin,smax);
    
    const double Nc = 3;
    const double yukawa_bottom = 1.0;
    prefactor_ = consts::Pi * pow(yukawa_bottom,2.0)/2./Nc/pow(mh_,2.)
                *consts::convert_GeV_to_pb;
}


void BottomFusion_bb_Delta::Evaluate(double* xx_vegas)
{
    
    kk_.generate_kinematics(xx_vegas);
    const double myxlumi = LL(kk_.x1(),kk_.x2());
    if (myxlumi!=0.0)
    {
        const double me_sq = eval_me(kk_.invariants());
        const double sigma = prefactor_ * kk_.jacobian
        * myxlumi
        * 1.0/2.0/kk_.s(1,2)
        * me_sq
        ;
        JF(sigma,kk_);
        //cout<<kk_;
    }
    else
    {
        JF();
    }
    
}
BottomFusion_bb_LO::BottomFusion_bb_LO():BottomFusion_bb_Delta()
{
    info_.name = "Born";
}
double BottomFusion_bb_LO::eval_me(const KinematicInvariants& kinvar)
{
    return 1.0;
}

BottomFusion_bb_NLO_SOFT::BottomFusion_bb_NLO_SOFT():BottomFusion_bb_Delta()
{
    info_.name = "NLO SOFT";
}
double BottomFusion_bb_NLO_SOFT::eval_me(const KinematicInvariants& kinvar)
{
    return a_s_over_pi_ * 8./3. * (consts::pi_square/6. - 1./2.);
}



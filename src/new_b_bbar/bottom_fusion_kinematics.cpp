#include<math.h>
#include <iomanip> // setprecison()
#include "bottom_fusion_kinematics.h"

//BottomFusionKinematics

///\note Shouldn't this be in the constructor?
void BottomFusionKinematics::SetNumberOfParticles(const int num_of_particles)
{
    if (num_of_particles < 3 || num_of_particles > 5) throw;
    else {
            // the kinematic invariants class needs to know how many particles are there
            kin_inv_.SetMaxMomentumID(num_of_particles);
            num_of_particles_ = num_of_particles;
        }
    return;
}

///\note Shouldn't this be in the constructor?
void BottomFusionKinematics::SetBoundaries(const double& mh_sq,const double& S)
{
    cout<<"\n[in BottomFusionKinematics] configuration: mh_sq = "<<mh_sq;
    S_=S;
    mh_sq_=mh_sq;
    // definition of tau
    tau_= mh_sq_ / S_;
    cout<<"\t tau="<<tau_<<endl;
}

ostream& operator<<(ostream& stream, const BottomFusionKinematics& kk)
{
    
    return stream << setprecision(16)
    <<"\n-------"
    <<kk.kin_inv_
    <<"\n tau = "<<kk.tau_<<","

    <<"\n x1 = "<<kk.x1()<<","
    <<"\n x2 = "<<kk.x2()<<","
    <<"\n p1 = "<<kk.p1<<","
    <<"\n p2 = "<<kk.p2<<","
    <<"\n p3 = "<<kk.p3<<","
    <<"\n p4 = "<<kk.p4<<","
    <<"\n p5 = "<<kk.p5
    ;
}

//BottomfusionKinematicsLO

void BottomFusionKinematicsLO::generate_bjorken_xs(const double& xx0)
{
    // we are at LO so s_12 = mh^2
    // we could generate x_1 in [0,1] and then check that x1>tau
    // instead we generate x1 flat in [tau,1] which is slightly more efficient
    // actually we should generate x1 in a more efficient way
    x1_ = tau_ + (1.-tau_) * xx0;
    // x2 = tau / x1 always at LO
    x2_ = tau_ / x1_ ;
}

void BottomFusionKinematicsLO::set_up_momenta_at_lab()
{
    const double E = sqrt(S_)/2.;
    p1.Set( x1_ * E, 0., 0.,  x1_ * E);
    p2.Set( x2_ * E, 0., 0., -x2_ * E);
    p3.Set( (x1_+x2_) * E, 0., 0., (x1_-x2_) * E);
}

void BottomFusionKinematicsLO::compute_invariants()
{
    kin_inv_.Set(1,2, mh_sq_);
    kin_inv_.Set(1,3, 0.);
    kin_inv_.Set(2,3, 0.);
    kin_inv_.compute_dimensionless_invariants();
}

void BottomFusionKinematicsLO::generate_kinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas[0]);
    set_up_momenta_at_lab();
    compute_invariants();

    jacobian = (1.-tau_) / x1_ ;

}

//BottomfusionKinematicsNLO
///\todo this is again LO so far!!!!

void BottomFusionKinematicsNLO::generate_bjorken_xs(const double& xx0)
{
    // we are at LO so s_12 = mh^2
    // we could generate x_1 in [0,1] and then check that x1>tau
    // instead we generate x1 flat in [tau,1] which is slightly more efficient
    // actually we should generate x1 in a more efficient way
    x1_ = tau_ + (1.-tau_) * xx0;
    // x2 = tau / x1 always at LO
    x2_ = tau_ / x1_ ;
}

void BottomFusionKinematicsNLO::set_up_momenta_at_lab()
{
    const double E = sqrt(S_)/2.;
    p1.Set( x1_ * E, 0., 0.,  x1_ * E);
    p2.Set( x2_ * E, 0., 0., -x2_ * E);
    p3.Set( (x1_+x2_) * E, 0., 0., (x1_-x2_) * E);
}

void BottomFusionKinematicsNLO::compute_invariants()
{
    kin_inv_.Set(1,2, mh_sq_);
    kin_inv_.Set(1,3, 0.);
    kin_inv_.Set(2,3, 0.);
    kin_inv_.compute_dimensionless_invariants();
}

void BottomFusionKinematicsNLO::generate_kinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas[0]);
    set_up_momenta_at_lab();
    compute_invariants();

    jacobian = (1.-tau_) / x1_ ;

}

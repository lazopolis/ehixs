#include "kinematic_variables.h"
#include <string>       // std::string
#include <iostream>     // std::cout
#include <stdlib.h>     /* exit, EXIT_FAILURE */
#include <sstream>  
#include "math.h"
#include <iomanip>
using namespace std;

void Massive2ParticlePhaseSpace::generate(const double& Qsq,const double& s3, const double& s4,const double& v2,const double& v3)
{
    const double costheta = -1.0 + 2.0 *  v2;
    const double phi = 2.0 * consts::Pi * v3;
    jacobian_ =  4.0*consts::Pi;
    const double sintheta = sqrt(1.0-costheta*costheta);
    //Energies and z-momenta at the COM frame
    const double pp3 = sqrt(Kaellen(Qsq,s3,s4))/2.0/sqrt(Qsq);
    jacobian_ = jacobian_ * pp3/sqrt(Qsq);
    p3com_.Set((Qsq+s3-s4)/2.0/sqrt(Qsq),
               pp3 * sintheta * sin(phi),
               pp3 * sintheta * cos(phi),
               pp3 * costheta);
    p4com_.Set((Qsq+s4-s3)/2.0/sqrt(Qsq),
               -pp3 * sintheta * sin(phi),
               -pp3 * sintheta * cos(phi),
               -pp3 * costheta);
    if (p3com_[1] != p3com_[1])
    {
        cout<<"\n[Massive2ParticlePhaseSpace]: Q^2="<<Qsq
        <<" pp3 = "<<pp3;
    }
}

double Massive2ParticlePhaseSpace::Kaellen(const double& a, const double& b,const double& c)
{
    const double res = a*a+b*b+c*c-2.*a*b-2.*a*c-2.*b*c;
    const double kaellen_cutoff = 1e-5;
    if (res<1e-16 and res>-kaellen_cutoff) return 0.0;
    if (res<-kaellen_cutoff) {cout<<"\n[Kaellen] negative Kaellen K = "<<res;exit(1);}
    return res;
}

ostream& operator<<(ostream& stream, const GStar2Kinematics& kk)
{
    
    return stream << setprecision(16)
    <<"\n-------"
    <<kk.kin_inv_
    <<"\n tau = "<<kk.tau_<<","
    <<"\n smax = "<<kk.smax_<<","
    <<"\n smin = "<<kk.smin_<<","
    <<"\n x1 = "<<kk.x1()<<","
    <<"\n x2 = "<<kk.x2()<<",";
    for (int i=1;i<kk.NumberOfParticles()+1;i++)
        cout<<"\np"<<i<<" = "<<kk.P(i);
}


void GStar2Kinematics::SetNumberOfParticles(int num_of_particles)
{
    kin_inv_.SetMaxMomentumID(num_of_particles);
    num_of_particles_ = num_of_particles;
    // we initialize p_ with N+1, so that we can call p_[1]..p_[N]
    // i.e. p_[0] is a dummy FMomentum to off-set the p_ vector
    p_ = vector<FMomentum>(num_of_particles+1,FMomentum());
}

void GStar2Kinematics::SetBoundaries(const double& smin,const double& smax)
{
    smax_=smax;
    smin_=smin;
    tau_=smin/smax;
}

void GStar2Kinematics::SetMassesSquared(const double& s3, const double& s4)
{
    kin_inv_.Set(3,s3);kin_inv_.Set(4,s4);
}

void GStar2Kinematics::generate_bjorken_xs(double* xx_vegas)
{
    bjorken_.generate(tau_,xx_vegas[0],xx_vegas[1]);
    jacobian_ = bjorken_.jacobian();
    set_p1_p2_at_lab();
    kin_inv_.Set(1,2,bjorken_.x1()*bjorken_.x2()*smax_);
}

void GStar2Kinematics::set_p1_p2_at_lab()
{
    p_[1].Set(bjorken_.x1()*sqrt(smax_)/2.0,0.0,0.0,bjorken_.x1()*sqrt(smax_)/2.0);
    p_[2].Set(bjorken_.x2()*sqrt(smax_)/2.0,0.0,0.0,-bjorken_.x2()*sqrt(smax_)/2.0);
}

void GStar2Kinematics::boost_along_z_axis(const double& bb)
{
    // boosting to LAB frame (the transverse pieces are invariant)
    p_[3].equal(born_kins_.p3com());p_[3].zboost(bb);
    p_[4].equal(born_kins_.p4com());p_[4].zboost(bb);
    
}

void GStar2Kinematics::boost_to_lab()
{
    //Q=p1+p2-p5
    FMomentum Q;
    Q.Set(p_[1][0]+p_[2][0]-p_[5][0],
          p_[1][1]+p_[2][1]-p_[5][1],
          p_[1][2]+p_[2][2]-p_[5][2],
          p_[1][3]+p_[2][3]-p_[5][3]);
    const double bx = -Q[1]/Q[0];
    const double by = -Q[2]/Q[0];
    const double bz = -Q[3]/Q[0];
    
    p_[3].equal(born_kins_.p3com());
    //cout<<"\n p3com = "<<p3;
    p_[3].boost(bx,by,bz);
    //cout<<" -> boost -> "<<p3;
    p_[4].equal(born_kins_.p4com());
    p_[4].boost(bx,by,bz);
    
    
}

void GStar2Kinematics::compute_born_invariants()
{
    //kin_inv_.Set(1,2, 2.0 * (p1*p2));
    kin_inv_.Set(1,3, s(3) - 2.0 * (p_[1]*p_[3]));
    kin_inv_.Set(1,4, s(4) - 2.0 * (p_[1]*p_[4]));
    kin_inv_.Set(2,3, s(3) - 2.0 * (p_[2]*p_[3]));
    kin_inv_.Set(2,4, s(4) - 2.0 * (p_[2]*p_[4]));
    kin_inv_.Set(3,4, s(3) + s(4) + 2.0*(p_[3]*p_[4]));
    
}

//void GStar2Kinematics::compute_nlo_invariants()
//{
//    kin_inv_.Set(1,5, - 2.0* (p5*p1));
//    kin_inv_.Set(2,5,  - 2.0* (p5*p2));
//    kin_inv_.Set(3,5, s(3) + 2.0 * (p5*p3));
//    kin_inv_.Set(4,5, s(4) + 2.0 * (p5*p4));
//}



void GStar2KinematicsLO::GenerateKinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas); // also the jacobian_ is initialized there
    const double Qsq = 1. * s(1,2); // z!=1 is a N*LO configuration
    
    born_kins_.generate(Qsq,s(3),s(4),xx_vegas[2],xx_vegas[3]);
    jacobian_ = jacobian_ * born_kins_.jacobian();
    
    const double bb = bjorken_.com_rapidity_ratio();
    boost_along_z_axis(bb);
    compute_born_invariants();
    kin_inv_.compute_dimensionless_invariants();
    //check_momentum_conservation();
}

void GStar2Kinematics::check_momentum_conservation() const
{
    for (int i=0;i<4;i++)
    {
        double zero = 0.0;
        for(int n=1;n<num_of_particles_+1;n++)
        {
            double sign = 1.0;
            if (n>2) sign = -1.0;
            zero += sign * p_[n][i];
        }
        if (fabs(zero)>1e-3)
        {
            cout<<"\n\nmomentum not conserved at component "<<i
            <<":\tsum="<<zero;
            for (int n=1;n<num_of_particles_+1;n++)
            {
                double sign = 1.0;
                if (n>2) sign = -1.0;
                cout<<" "<<sign * p_[n][i];
            }
        }
    }
}




/*
void NLOKinematics::generate_kinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas);
    
    const double max_allowed_z = 1.0 -1e-15;
    z = smin_/s(1,2)+(max_allowed_z-smin_/s(1,2))*xx_vegas[4];
    
    lambda = xx_vegas[5];
    phi_g = 2.0*consts::Pi*xx_vegas[6];
    
    const double zbar = 1.0-z;
    const double lambdabar = 1.0-lambda;
    const double eq = zbar*sqrt(s(1,2)*lambda*lambdabar);
    kin_inv_.Set(1,5, -s(1,2) * zbar * lambda);
    kin_inv_.Set(2,5, -s(1,2) * zbar * lambdabar);
    // p5 is generated at LAB
    p5.Set(zbar*lambdabar*p1[0]+zbar*lambda*p2[0],
           eq*cos(phi_g),eq*sin(phi_g),
           zbar*lambdabar*p1[3]+zbar*lambda*p2[3]);
    
    //
    const double Qsq = z * s(1,2); // z!=1 is a N*LO configuration
    
    born_kins_.generate(Qsq,s(3),s(4),xx_vegas[2],xx_vegas[3]);
    jacobian = jacobian * born_kins_.jacobian();
    boost_to_lab();
    
    compute_born_invariants();
    
    //
    jacobian = jacobian * (max_allowed_z-smin_/s(1,2));//from z
    jacobian = jacobian * 2.0*consts::Pi;//from phi
    jacobian = jacobian * s(1,2) / 2.0/consts::Pi; // from splitting the phase space
    
    //compute s35 and s45 at LAB (because p3 and p4 are at LAB)
    kin_inv_.Set(5,0.0);
    kin_inv_.Set(3,5, s(3) + 2.0 * (p5*p3));
    kin_inv_.Set(4,5, s(4) + 2.0 * (p5*p4));
    kin_inv_.compute_dimensionless_invariants();
    
}


void NNLOKinematics::generate_kinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas);
    z = smin_/s(1,2)+(1.0-smin_/s(1,2))*xx_vegas[4];
    jacobian = jacobian * (1.0-smin_/s(1,2));
    lambda = xx_vegas[5];
    phi_g = 2.0*consts::Pi*xx_vegas[6];
    rho = xx_vegas[7];
    jacobian = jacobian * 2.0*consts::Pi;
    const double zbar = 1.0-z;
    const double lambdabar = 1.0-lambda;
    const double rhobar = 1. - rho;
    const double eq = zbar*sqrt(s(1,2)*lambda*lambdabar*rho);
    const double w=(1.0-rho*zbar*lambdabar)/(1.0-zbar*lambdabar);
    
    nnlo_jacobian = (1.0-smin_/s(1,2)) * 2.0*consts::Pi;
    
    // p5 is generated at LAB
    p5.Set(zbar*lambdabar*p1[0]+zbar*lambda*w*p2[0],
           eq*cos(phi_g),eq*sin(phi_g),
           zbar*lambdabar*p1[3]+zbar*lambda*w*p2[3]);
    
    const double Qsq = z * s(1,2); // z!=1 is a N*LO configuration
    
    born_kins_.generate(Qsq,s(3),s(4),xx_vegas[2],xx_vegas[3]);
    jacobian = jacobian * born_kins_.jacobian();
    boost_to_lab();
    
    compute_born_invariants();
    // computing s15, s25, s5 from parameters directly 
    // slightly improves convergence
    // and doesn't affect histograms, since the numerical agreement
    // between sij and (p_i-p_j)^2 is excellent apart from singular 
    // regions of phase space (where the exact form of unresolved
    // particles doesn't matter)
    kin_inv_.Set(1,5, -s(1,2)*(1.-z)*lambda);
    kin_inv_.Set(2,5, -s(1,2)*zbar*lambdabar
            *(1.-rhobar*zbar*lambda/(1.-zbar*lambdabar)));
    kin_inv_.Set(5, s(1,2)*zbar*zbar*lambda*lambdabar*rhobar/(1.0-zbar*lambdabar));
    
    //compute s35 and s45 at LAB (because p3 and p4 are at LAB)
    kin_inv_.Set(3,5, s(3) + s(5) + 2.0 * (p5*p3));
    kin_inv_.Set(4,5, s(4) + s(5) + 2.0 * (p5*p4));
    kin_inv_.compute_dimensionless_invariants();

}


void NNLOExclusiveKinematics::generate_kinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas);
    z = smin_/s(1,2)+(1.0-smin_/s(1,2))*xx_vegas[4];
    jacobian = jacobian * (1.0-smin_/s(1,2));
    lambda = xx_vegas[5];
    phi_g = 2.0*consts::Pi*xx_vegas[6];
    rho = xx_vegas[7];
    x3 = xx_vegas[8];
    x4 = xx_vegas[9];
    const double x3bar = 1.-x3;
    
    const double zbar = 1.0-z;
    const double lambdabar = 1.0-lambda;
    const double rhobar = 1. - rho;
    const double eq = zbar*sqrt(s(1,2)*lambda*lambdabar*rho);
    const double w=(1.0-rho*zbar*lambdabar)/(1.0-zbar*lambdabar);
    
    // 2pi explanation: the PSmeasure has 1/(2*pi)^5
    // but there is a 2*pi from the phi_g angle generation
    // and a (2*pi)^4 from gs^4 = (4*pi_as)^2 = (2*pi)^4*(a_s/pi)^2
    // so we omit the 1/(2*pi)^5 here altogether
    jacobian = jacobian * (pow(zbar,3.)*lambda*lambdabar/(1.-zbar*lambdabar))               
                            /16 * pow(s(1,2),2.);
    
    // p5 is generated at COM
    p5.Set(zbar*lambdabar+zbar*lambda*w,
           eq*cos(phi_g),eq*sin(phi_g),
           zbar*lambdabar-zbar*lambda*w);
    
    const double AA = x3*rho + x3bar*rhobar/(1.-zbar*lambdabar);
    const double AAbar = x3bar*rho + x3*rhobar/(1.-zbar*lambdabar);
    const double QQ = sqrt(lambda*lambdabar/rho);
    const double WW = sqrt(rho*rhobar*x3*x3bar/(1.-zbar*lambdabar));
    
    const double a5 = zbar*lambdabar*x3;
    const double b5 = zbar*lambda*(AA-2.*cos(consts::Pi*x4)*WW);
    const double c5 = zbar * (x3 * rho * QQ - cos(consts::Pi*x4)*QQ*WW);
    const double d5 = zbar * sin(consts::Pi*x4) * QQ * WW;
    
    const double a6 = zbar*lambdabar*x3bar;
    const double b6 = zbar*lambda*(AAbar+2.*cos(consts::Pi*x4)*WW);
    const double c6 = zbar * (x3 * rho * QQ + cos(consts::Pi*x4)*QQ*WW);
    const double d6 = -d5;
    
    const double sqrs = sqrt(s(1,2));
    
    p6.Set(sqrs*(a5+b5)/2., 
           sqrs*(c5*cos(phi_g)-d5*sin(phi_g)),
           sqrs*(c5*sin(phi_g)+d5*cos(phi_g)),
           sqrs*(a5-b5)/2.
           );
    p7.Set(sqrs*(a6+b6)/2., 
           sqrs*(c6*cos(phi_g)-d6*sin(phi_g)),
           sqrs*(c6*sin(phi_g)+d6*cos(phi_g)),
           sqrs*(a6-b6)/2.
           );
    
    const double Qsq = z * s(1,2); // z!=1 is a N*LO configuration
    
    born_kins_.generate(Qsq,s(3),s(4),xx_vegas[2],xx_vegas[3]);
    jacobian = jacobian * born_kins_.jacobian();
    boost_to_lab();
    
    compute_born_invariants();
    // computing s15, s25, s5 at COM!! from parameters directly 
    // slightly improves convergence
    // and doesn't affect histograms, since the numerical agreement
    // between sij and (p_i-p_j)^2 is excellent apart from singular 
    // regions of phase space (where the exact form of unresolved
    // particles doesn't matter)
    kin_inv_.Set(1,5, -s(1,2)*(1.-z)*lambda);
    kin_inv_.Set(2,5, -s(1,2)*zbar*lambdabar
                 *(1.-rhobar*zbar*lambda/(1.-zbar*lambdabar)));
    kin_inv_.Set(5, s(1,2)*zbar*zbar*lambda*lambdabar*rhobar/(1.0-zbar*lambdabar));
    kin_inv_.Set(1,6, -s(1,2)*zbar*lambda*(AA-2.*cos(consts::Pi*x4)*WW));
    kin_inv_.Set(2,6, -s(1,2)*zbar*lambdabar*x3);
    kin_inv_.Set(6, 0.0);
    kin_inv_.Set(1,7,-s(1,2)*zbar*lambda*(AAbar-2.*cos(consts::Pi*x4)*WW));
    kin_inv_.Set(2,7,-s(1,2)*zbar*lambdabar*x3bar);
    kin_inv_.Set(7, 0.0);
    
    
    //boosting 5,6 and 6 to lab
    double bb = -(p1[3]+p2[3])/(p1[0]+p2[0]); 
    p5.zboost(bb);
    p6.zboost(bb);
    p7.zboost(bb);
    //compute s35 and s45 at LAB (because p3 and p4 are at LAB)
    kin_inv_.Set(3,5, s(3) + s(5) + 2.0 * (p5*p3));
    kin_inv_.Set(4,5, s(4) + s(5) + 2.0 * (p5*p4));
    kin_inv_.Set(3,6, s(3)  + 2.0 * (p6*p3));
    kin_inv_.Set(4,6, s(4)  + 2.0 * (p6*p4));
    kin_inv_.Set(3,7, s(3)  + 2.0 * (p7*p3));
    kin_inv_.Set(4,7, s(4)  + 2.0 * (p7*p4));
    kin_inv_.compute_dimensionless_invariants();
    
}


void LOKinematicsShifted::generate_kinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas);
    const double Qsq = z_ * s(1,2); // z!=1 is a N*LO configuration
    
    born_kins_.generate(Qsq,s(3),s(4),xx_vegas[2],xx_vegas[3]);
    jacobian = jacobian * born_kins_.jacobian();
    shift_initial_state_particle();

    const double bb = -(p1[3]+p2[3])/(p1[0]+p2[0]);
    boost_along_z_axis(bb);
    compute_born_invariants();
    kin_inv_.compute_dimensionless_invariants();
}

void LOKinematicsShiftedLeft::shift_initial_state_particle()
{
    p1.Set(z_*p1[0],z_*p1[1],z_*p1[2],z_*p1[3]);
}

void LOKinematicsShiftedRight::shift_initial_state_particle()
{
    p2.Set(z_*p2[0],z_*p2[1],z_*p2[2],z_*p2[3]);
}

void NNLOKinematicsMueller::generate_kinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas);
    const double max_allowed_z = 1.0;
    z = smin_/s(1,2)+(max_allowed_z-smin_/s(1,2))*xx_vegas[4];
    
    //z = smin_/s(1,2)+(1.0-smin_/s(1,2))*xx_vegas[4];
    //jacobian = jacobian * (1.0-smin_/s(1,2));
    lambda = xx_vegas[5];
    phi_g = 2.0*consts::Pi*xx_vegas[6];
    u = xx_vegas[7];
    
    const double zbar = 1.0-z;
    const double lambdabar = 1.0-lambda;
    const double ubar = 1. - u;
    
    set_tbar();
    
    const double eq = tbar*sqrt(s(1,2)*lambda*lambdabar*ubar);
    
    
    // p5 is generated at LAB
    p5.Set(tbar*lambdabar*p1[0]+tbar*lambda*p2[0],
           eq*cos(phi_g),eq*sin(phi_g),
           tbar*lambdabar*p1[3]+tbar*lambda*p2[3]);
    
//    cout<<"\nrequesting born kinematics with Q^2 = "<<z*s(1,2)
//        <<" while z is in ["<<smin_/s(1,2)<<","<<max_allowed_z
//        <<"]";
    born_kins_.generate(z * s(1,2),s(3),s(4),xx_vegas[2],xx_vegas[3]);
    
    jacobian = jacobian * (max_allowed_z-smin_/s(1,2));//from z
    jacobian = jacobian * 2.0*consts::Pi;// from phi_g
    jacobian = jacobian * born_kins_.jacobian();// from born
    
    
    boost_to_lab();
    
    compute_born_invariants();
    // computing s15, s25, s5 from parameters directly 
    // slightly improves convergence
    // and doesn't affect histograms, since the numerical agreement
    // between sij and (p_i-p_j)^2 is excellent apart from singular 
    // regions of phase space (where the exact form of unresolved
    // particles doesn't matter)
    kin_inv_.Set(1,5, -s(1,2)*tbar*lambda*(1.-tbar*lambdabar*u));
    kin_inv_.Set(2,5, -s(1,2)*tbar*lambdabar*(1.-tbar*lambda*u));
    kin_inv_.Set(5, s(1,2)*tbar*tbar*lambda*lambdabar*u);
    
    //compute s35 and s45 at LAB (because p3 and p4 are at LAB)
    kin_inv_.Set(3,5, s(3) + s(5) + 2.0 * (p5*p3));
    kin_inv_.Set(4,5, s(4) + s(5) + 2.0 * (p5*p4));
    //check_momentum_conservation();
    kin_inv_.compute_dimensionless_invariants();

}

void NNLOKinematicsMueller::set_tbar()
{
    const double zbar = 1.0-z;
    const double lambdabar = 1.0-lambda;
    const double ubar = 1. - u;
    if (z>0.99 or u<0.01 or lambda<0.01 or lambdabar>0.99)
        {
        const double A = u*lambda*lambdabar;
        tbar = zbar * (1.+ A*zbar + 2.*pow(A*zbar,2.) + 5.*pow(A*zbar,3.) + 14. * pow(A*zbar,4.));   
        }
    else
        {
        tbar = (1.-sqrt(1-4*lambda*lambdabar*zbar*u))/(2.*lambda*lambdabar*u);
        }
}

double NNLOKinematicsMueller::jacobianFull()
{
    return jacobian  
        *1./(1.-2.*lambda*(1.-lambda)*u*tbar)
        *tbar/u
        *1./3./4.*consts::nf 
        /2./consts::Pi
        ;
}

double NNLOKinematicsMueller::jacobianAtLimits()
{
    return jacobian  
    *(1.-z)/u
    *1./3./4.*consts::nf 
    /2./consts::Pi
    ;
}


double NNLOKinematicsMueller::SplittingKernel()
{
    return 4./3. * 
    (1.+z*z-u*(1.-z)*(1.+z))
    /(1.-z)/(1.-u*(1.-z));
}

*/


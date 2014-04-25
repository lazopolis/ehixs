#include "kinematic_variables.h"
#include <string>       // std::string
#include <iostream>     // std::cout
#include <sstream>  
#include "math.h"
#include <iomanip>
using namespace std;
ostream& operator<<(ostream& stream, const KinematicVariables& kk)
{
    return stream << setprecision(16)
    <<"\n-------"
    <<"\n s12 = "<<kk.s(1,2)<<","
    <<"\n s13 = "<<kk.s(1,3)<<","
    <<"\n s23 = "<<kk.s(2,3)<<","
    <<"\n s14 = "<<kk.s(1,4)<<","
    <<"\n s24 = "<<kk.s(2,4)<<","
    <<"\n s34 = "<<kk.s(3,4)<<","
    <<"\n s15 = "<<kk.s(1,5)<<","
    <<"\n s25 = "<<kk.s(2,5)<<","
    <<"\n s35 = "<<kk.s(3,5)<<","
    <<"\n s45 = "<<kk.s(4,5)<<","
    <<"\n s3 = "<<kk.s(3)<<","
    <<"\n s4 = "<<kk.s(4)<<","
    <<"\n s5 = "<<kk.s(5)<<","
    <<"\n tau = "<<kk.tau_<<","
    <<"\n smax = "<<kk.smax_<<","
    <<"\n smin = "<<kk.smin_<<","
    <<"\n x1 = "<<kk.x1()<<","
    <<"\n x2 = "<<kk.x2()<<","
    <<"\n p1 = "<<kk.p1<<","
    <<"\n p2 = "<<kk.p2<<","
    <<"\n p3 = "<<kk.p3<<","
    <<"\n p4 = "<<kk.p4<<","
    <<"\n p5 = "<<kk.p5
    ;
}



void FMomentum::zboost(const double& bb)
{
    const double gb = 1.0/sqrt(1.0-bb*bb);
    const double E  =    gb*p[0]   -bb*gb*p[3];
    const double pz = -bb*gb*p[0]  +gb*p[3];
    p[0]=E;
    p[3]=pz;
}

void FMomentum::boost(const double& bx,const double& by,const double& bz)
{
    const double bsq = bx*bx+by*by+bz*bz;
    if (1.-bsq<0.0)
        {
            cout<<"\n Error in boost: bsq>1 : "<<bx<<","<<by<<","<<bz;
            exit(1);
        }
    const double g = 1.0/sqrt(1.0-bsq);
    const double d = g*g/(g+1.0);
    const double L[4][4] = {
        {g,     -g*bx,          -g*by,      -g*bz},
        {-g*bx, 1.0+d*bx*bx,    d*bx*by,     d*bx*bz},
        {-g*by, d*by*bx,        1.0+d*by*by, d*by*bz},
        {-g*bz, d*bz*bx,        d*bz*by,     1.0+d*bz*bz}
    };
    double newp[4];
    for (int i=0;i<4;i++)
    {
        newp[i]=0.0;
        for (int j=0;j<4;j++)
            newp[i] += L[i][j]*p[j];
    }
    for (int i=0;i<4;i++) p[i] = newp[i];
}



void BjorkenXs::generate(const double& tau,const double& v0, const double& v1)
{
    const double v0max = 1.-1e-6;
    const double rescaled_v0 = v0max * v0;
    const double y= log(tau) *(1.0-rescaled_v0);
    const double u = exp(y);
    const double rho = 1.0/2.0*log(u) -  log(u) * v1;
    x1_ = sqrt(u)*exp(rho);
    x2_ = sqrt(u)*exp(-rho);
    jacobian_ = -log(u)*u*(1.0-tau)*(-log(tau))*v0max;
}

void KinematicInvariants::compute_dimensionless_invariants()
{
const double s12 = s_ij_[index(1)][index(2)];
for (int i=1;i<max_+1;i++)
    {
    q_i_[index(i)] = s_i_[index(i)]/ s12;
    for (int j=1;j<i;j++)
        {
            q_ij_[index(j)][index(i)] = s_ij_[index(j)][index(i)]/s12;
        }
    }
}

int KinematicInvariants::index(int i) const
{
    check_size(i);
    return i-1;
}

int KinematicInvariants::max(int i,int j) const
{
    if (i<j) return j;
    if (j<i) return i;
    cout<<"\n Error in Kinematic Invariants: you asked for s"
        <<i<<j
        <<" which is illegal (use s("<<i<<") for masses)"
        <<endl;
    exit(1);
} 

int KinematicInvariants::min(int i,int j) const 
{
    if (i<j) return i;
    if (j<i) return j;
    cout<<"\n Error in Kinematic Invariants: you asked for s"
    <<i<<j
    <<" which is illegal (use s("<<i<<") for masses)"
    <<endl;
    exit(1);
}

void KinematicInvariants::check_size(int i) const 
{
if (i>max_)
    {
        cout<<"\nError in Kinematic Invariants: you tried to set an invariant with index "<<i<<" while the maximum allowed is "<<max_<<endl;
        exit(1);
    }
}

void KinematicVariables::generate_bjorken_xs(double* xx_vegas)
{
    bjorken_.generate(tau_,xx_vegas[0],xx_vegas[1]);
    jacobian = bjorken_.jacobian();
    set_p1_p2_at_lab();
    kin_inv_.Set(1,2,bjorken_.x1()*bjorken_.x2()*smax_);
}

void KinematicVariables::set_p1_p2_at_lab()
{
    p1.Set(bjorken_.x1()*sqrt(smax_)/2.0,0.0,0.0,bjorken_.x1()*sqrt(smax_)/2.0);
    p2.Set(bjorken_.x2()*sqrt(smax_)/2.0,0.0,0.0,-bjorken_.x2()*sqrt(smax_)/2.0);
}

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
}

double Massive2ParticlePhaseSpace::Kaellen(const double& a, const double& b,const double& c)
{
    return a*a+b*b+c*c-2.*a*b-2.*a*c-2.*b*c;
}

void LOKinematics::generate_kinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas);
    const double Qsq = 1. * s(1,2); // z!=1 is a N*LO configuration
    
    born_kins_.generate(Qsq,s(3),s(4),xx_vegas[2],xx_vegas[3]);
    jacobian = jacobian * born_kins_.jacobian();
    
    const double bb = bjorken_.com_rapidity_ratio();
    boost_along_z_axis(bb);
    compute_born_invariants();
    kin_inv_.compute_dimensionless_invariants();
}

void KinematicVariables::boost_along_z_axis(const double& bb)
{
    // boosting to LAB frame (the transverse pieces are invariant)
    p3.equal(born_kins_.p3com());p3.zboost(bb);
    p4.equal(born_kins_.p4com());p4.zboost(bb);
    
}

void KinematicVariables::boost_to_lab()
{
    //Q=p1+p2-p5
    FMomentum Q;
    Q.Set(p1[0]+p2[0]-p5[0],p1[1]+p2[1]-p5[1],p1[2]+p2[2]-p5[2],p1[3]+p2[3]-p5[3]);
    const double bx = -Q[1]/Q[0];
    const double by = -Q[2]/Q[0];
    const double bz = -Q[3]/Q[0];
    
    p3.equal(born_kins_.p3com());
    //cout<<"\n p3com = "<<p3;
    p3.boost(bx,by,bz);
    //cout<<" -> boost -> "<<p3;
    p4.equal(born_kins_.p4com());
    p4.boost(bx,by,bz);
    
    
}

void KinematicVariables::compute_born_invariants()
{
    //kin_inv_.Set(1,2, 2.0 * (p1*p2));
    kin_inv_.Set(1,3, s(3) - 2.0 * (p1*p3));
    kin_inv_.Set(1,4, s(4) - 2.0 * (p1*p4));
    kin_inv_.Set(2,3, s(3) - 2.0 * (p2*p3));
    kin_inv_.Set(2,4, s(4) - 2.0 * (p2*p4));
    kin_inv_.Set(3,4, s(3) + s(4) + 2.0*(p3*p4));
    
}

void KinematicVariables::compute_nlo_invariants()
{
    kin_inv_.Set(1,5, - 2.0* (p5*p1));
    kin_inv_.Set(2,5,  - 2.0* (p5*p2));
    kin_inv_.Set(3,5, s(3) + 2.0 * (p5*p3));
    kin_inv_.Set(4,5, s(4) + 2.0 * (p5*p4));
}

void NLOKinematics::generate_kinematics(double* xx_vegas)
{
    generate_bjorken_xs(xx_vegas);
    
    const double max_allowed_z = 1.0 -1e-10*0.0;
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

void KinematicVariables::check_momentum_conservation() const
{
    for (int i=0;i<4;i++)
    {
        const double zero = p1[i]+p2[i]-p3[i]-p4[i]-p5[i]-p6[i];
        if (fabs(zero)>1e-3)
            {
            cout<<"\n\nmomentum not conserved at component "<<i
                <<":\tsum="<<zero
                <<" "<<p1[i]<<" "<<p2[i]
                <<" "<<-p3[i]<<" "<<-p4[i]
                <<" "<<-p5[i]<<" "<<-p6[i]
                ;
            }
    }
}



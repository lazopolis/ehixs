#include "gamma_star_gamma_star_me.h"
#include "chaplin.h"


#include <iomanip>
#include <sstream>
ostream& operator<<(ostream& stream, const FMomentum& p)
{
    return stream << setprecision(8)
        <<"[ "<<p[0]<<","<<p[1]<<","<<p[2]<<","<<p[3]<<" ]";
}




GstarGstarMe::GstarGstarMe(EventBox& event_box):NewMatrixElement(event_box)
{
    info_ = new NewMeExternalInfo;
    dimension_ = 4;
    info_->name = "Born";
    info_->ISF = InitialStateFlavors("u","ub");
    pdf_selection_ = "same flavor";
    info_->alpha_power = 0;
    info_->epsilon_power_min = 0;
    info_->epsilon_power_max = 2;
    alpha_em = 1.0/137.0 ;
    kk_.s3=20.0*20.0;
    kk_.s4=30.0*30.0;
    smin = pow(sqrt(kk_.s3)+sqrt(kk_.s4),2.0);
}
void GstarGstarMe::consolidate()
{
    kk_.tau = smin/smax; // smax is a protected member of the base class
    kk_.smax = smax;
    kk_.smin= smin;
    const double averaging_factor = 1.0/6.0 * 1.0/6.0 ;
    const double color_factor = 3.0;
    
    prefactor_ = averaging_factor*color_factor* pow(alpha_em,2.0)* 0.389379*1e9;
}

void GstarGstarMe::JF(const double& w,const KinematicVariables& kv)
{
    event_box_->AddNewEvent(w);
    event_box_->SetP(1,kv.p1.p[0],kv.p1.p[1],kv.p1.p[2],kv.p1.p[3]);
    event_box_->SetP(2,kv.p2.p[0],kv.p2.p[1],kv.p2.p[2],kv.p2.p[3]);
    event_box_->SetP(3,kv.p3.p[0],kv.p3.p[1],kv.p3.p[2],kv.p3.p[3]);
    event_box_->SetP(4,kv.p4.p[0],kv.p4.p[1],kv.p4.p[2],kv.p4.p[3]);
    event_box_->SetP(5,kv.p5.p[0],kv.p5.p[1],kv.p5.p[2],kv.p5.p[3]);
    event_box_->SetP(6,kv.p6.p[0],kv.p6.p[1],kv.p6.p[2],kv.p6.p[3]);

}

void GstarGstarMe::JF()
{
    event_box_->AddNewEvent(0.0);

}

double GstarGstarMe::PP(const double& x)
{
    return 4.0/3.0 * (1.0+x*x)/(1.0-x); 
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



void KinematicVariables::generate_bjorken_xs(double* xx_vegas)
{
    const double y= log(tau) *(1.0-xx_vegas[0]);
    const double u = exp(y);
    const double rho = 1.0/2.0*log(u) -  log(u) * xx_vegas[1];
    x1 = sqrt(u)*exp(rho);
    x2 = sqrt(u)*exp(-rho);
    p1.Set(x1*sqrt(smax)/2.0,0.0,0.0,x1*sqrt(smax)/2.0);
    p2.Set(x2*sqrt(smax)/2.0,0.0,0.0,-x2*sqrt(smax)/2.0);
    s12 = x1*x2*smax;
    jacobian = -log(u)*u*(1.0-tau)*(-log(tau));
}


void KinematicVariables::SetLOKinematics(double* xx_vegas)
{
    const double Qsq = z * s12; // z!=1 is a N*LO configuration
    const double costheta = -1.0 + 2.0 * xx_vegas[2];
    const double phi = 2.0 * consts::Pi * xx_vegas[3];
    jacobian = jacobian * 4.0*consts::Pi;
    const double sintheta = sqrt(1.0-costheta*costheta);
    double A = - (Qsq-s3-s4);
    double Kaellen = Qsq*Qsq + s3*s3 + s4*s4
    -2.0*Qsq*s3-2.0*Qsq*s4-2.0*s3*s4;
    
    
    // cout<<"\n"<<varying_part_of_jacobian<<" "<<me_sq;
    //Energies and z-momenta at the COM frame
    const double pp3 = sqrt(Kaellen)/2.0/sqrt(Qsq);
    p3com.Set((Qsq+s3-s4)/2.0/sqrt(Qsq),
                  pp3 * sintheta * sin(phi),
                  pp3 * sintheta * cos(phi),
                  pp3 * costheta);
    p4com.Set((Qsq+s4-s3)/2.0/sqrt(Qsq),
                  -pp3 * sintheta * sin(phi),
                  -pp3 * sintheta * cos(phi),
                  -pp3 * costheta);
    if (z==1)// p3=(0,0,0,0) and the com frame is the rest frame of p1+p2
        {
        // we need to boost from p1=sqrt(x1*x2)*(1,0,0,1) to p1=x1*(1,0,0,1)
        // from which bb is:
        const double bb = (x2-x1)/(x2+x1);
        boost_along_z_axis(bb);
        }
    else
        {
        boost_to_lab();
        }
    compute_born_invariants();
}

void KinematicVariables::boost_along_z_axis(const double& bb)
{
    // boosting to LAB frame (the transverse pieces are invariant)
    p3.equal(p3com);p3.zboost(bb);
    p4.equal(p4com);p4.zboost(bb);

}

void KinematicVariables::boost_to_lab()
{
    //Q=p1+p2-p5
    FMomentum Q;
    Q.Set(p1[0]+p2[0]-p5[0],p1[1]+p2[1]-p5[1],p1[2]+p2[2]-p5[2],p1[3]+p2[3]-p5[3]);
    const double bx = -Q[1]/Q[0];
    const double by = -Q[2]/Q[0];
    const double bz = -Q[3]/Q[0];
    
    p3.equal(p3com);
    p3.boost(bx,by,bz);
    p4.equal(p4com);
    p4.boost(bx,by,bz);

    
}

void KinematicVariables::compute_born_invariants()
{
    s12 = 2.0 * (p1*p2);
    s13 = s3 - 2.0 * (p1*p3);
    s14 = s4 - 2.0 * (p1*p4);
    s23 = s3 - 2.0 * (p2*p3);
    s24 = s4 - 2.0 * (p2*p4);
    s34 = s3 + s4 + 2.0*(p3*p4);

}
void KinematicVariables::SetNLOKinematics(double* xx_vegas)
{
    z = smin/s12+(1.0-smin/s12)*xx_vegas[4];
    jacobian = jacobian * (1.0-smin/s12);
    lambda = xx_vegas[5];
    phi = 2.0*consts::Pi*xx_vegas[6];
    jacobian = jacobian * 2.0*consts::Pi;
    const double zbar = 1.0-z;
    const double lambdabar = 1.0-lambda;
    const double eq = zbar*sqrt(s12*lambda*lambdabar);
    s15 = -s12 * zbar * lambda;
    s25 = -s12 * zbar * lambdabar;
    p5.Set(zbar*lambdabar*p1[0]+zbar*lambda*p2[0],
              eq*cos(phi),eq*sin(phi),
              zbar*lambdabar*p1[3]+zbar*lambda*p2[3]);
    
    SetLOKinematics(xx_vegas);
    s35 = s3 + 2.0 * (p5*p3);
    s45 = s4 + 2.0 * (p5*p4);
    //cout<<"\n** p5 = ";for (int i=0;i<4;i++)cout<<p5[i]<<" ";
    //cout<<"\n** p3 = ";for (int i=0;i<4;i++)cout<<p3[i]<<" ";
}

KinematicVariables KinematicVariables::single_collinear(int i)
{
    KinematicVariables kcol;
    const double zbar = 1.0-z;
    if (i==1)
        {
        p5.Set(zbar*p1[0],0.0,0.0,zbar*p1[3]);
        }
    else if (i==2)
        {
        p5.Set(zbar*p2[0],0.0,0.0,zbar*p2[3]);
        }
    kcol.p1.equal(p1);
    kcol.p2.equal(p2);
    kcol.p3com.equal(p3com);
    kcol.p4com.equal(p4com);
    const double bb = - (kcol.p1[3]+kcol.p2[3]-kcol.p5[3])/(kcol.p1[0]+kcol.p2[0]-kcol.p5[0]);
    kcol.boost_along_z_axis(bb);
    return kcol;
}

KinematicVariables KinematicVariables::z_shifted(int i) const
{
    KinematicVariables kcol;
    
    
    // p5 = null by default
    if (i==1)
        {
        kcol.p3com.equal(p3com);
        kcol.p4com.equal(p4com);
        kcol.p2.equal(p2);
        kcol.p1.Set(z*p1[0],z*p1[1],z*p1[2],z*p1[3]);
        const double bb = -(kcol.p1[3]+kcol.p2[3])/(kcol.p1[0]+kcol.p2[0]);
        kcol.boost_along_z_axis(bb);
        kcol.s3=s3;
        kcol.s4=s4;
        kcol.compute_born_invariants();
        }
    else if (i==2)
        {
        kcol.p3com.equal(p3com);
        kcol.p4com.equal(p4com);
        kcol.p1.equal(p1);
        kcol.p2.Set(z*p2[0],z*p2[1],z*p2[2],z*p2[3]);
        const double bb = -(kcol.p1[3]+kcol.p2[3])/(kcol.p1[0]+kcol.p2[0]);
        kcol.boost_along_z_axis(bb);
        kcol.s3=s3;
        kcol.s4=s4;
        kcol.compute_born_invariants();
        }
    return kcol;
}



double GstarGstarMe::Born(const KinematicVariables& kk)
{
    return 8.0*(
                kk.s23/kk.s13 - 2.0 * (kk.s3+kk.s4)/kk.s13
                - kk.s3*kk.s4/kk.s13/kk.s13
                +kk.s13/kk.s23 - 2.0 * (kk.s3+kk.s4)/kk.s23
                - kk.s3*kk.s4/kk.s23/kk.s23
                +2.0*pow(kk.s3+kk.s4,2.0)/ (kk.s13*kk.s23)
                );
}

void GstarGstarMeDelta::Evaluate(double* xx_vegas)
{
    kk_.generate_bjorken_xs(xx_vegas);
    kk_.z=1.0;
    const double lumi = LL(kk_.x1,kk_.x2);
    if (lumi!=0.0)
        {
        kk_.SetLOKinematics(xx_vegas);
        double me_sq = eval_me(kk_);
        //cout<<"\nme_sq="<<me_sq<<" "<<kk_.s13<<" "<<kk_.s23;
        const double sigma = prefactor_ * kk_.jacobian
                        * lumi
                        * 1.0/2.0/kk_.s12
                        * me_sq
                        ;
        JF(sigma,kk_);
        }
    else
        {
        JF();
        }
    
}



double GstarGstarMELO::eval_me(const KinematicVariables& kinvar)
{
    return Born(kinvar);
}



complex<double> GstarGstarMeNLOSoft::polylog(int i,const complex<double>& z)
{
    if (i==2) return HPL2(0,1,z);
    else {cout<<"\nerror in polylog: only Li2 is defined!";return 0.0;}
}

double GstarGstarMeNLOSoft::eval_me(const KinematicVariables& kv)
{
    
    complex<double> s= complex<double>(kv.s12,0.0);
    complex<double> t = complex<double>(kv.s13,0.0);
    complex<double> u = complex<double>(kv.s23,0.0);
    complex<double> s3 = complex<double>(kv.s3,0.0);
    complex<double> s4 = complex<double>(kv.s4,0.0);
    complex<double> K = sqrt(s*s+t*t+u*u-2.0*u*s-2.0*u*t-2.0*s*t);
    
    complex<double> z  = 0.5/s * (t-u+s + K);
    complex<double> zp = 0.5/s * (t-u+s - K);
    
  
    
    complex<double> cg2 = -0.2e1 * (0.72e2 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * t * u - 0.68e2 * pow(s3, 0.5e1) * s4 * s4 * t * t * u - 0.68e2 * pow(s3, 0.5e1) * s4 * s4 * t * u * u + 0.64e2 * pow(s3, 0.5e1) * s4 * t * t * u * u - 0.36e2 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * t + 0.144e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * u - 0.36e2 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * u * u + 0.36e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) - 0.252e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * t * u - 0.252e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * u * u + 0.36e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(u, 0.3e1) + 0.86e2 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.3e1) * u + 0.372e3 * pow(s3, 0.4e1) * s4 * s4 * t * t * u * u + 0.86e2 * pow(s3, 0.4e1) * s4 * s4 * t * pow(u, 0.3e1) + 0.17e2 * pow(s3, 0.4e1) * s4 * pow(t, 0.4e1) * u - 0.77e2 * pow(s3, 0.4e1) * s4 * pow(t, 0.3e1) * u * u - 0.77e2 * pow(s3, 0.4e1) * s4 * t * t * pow(u, 0.3e1) + 0.17e2 * pow(s3, 0.4e1) * s4 * t * pow(u, 0.4e1) - 0.16e2 * pow(s3, 0.4e1) * pow(t, 0.4e1) * u * u - 0.32e2 * pow(s3, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.16e2 * pow(s3, 0.4e1) * t * t * pow(u, 0.4e1) + 0.72e2 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * t * u + 0.36e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.3e1) - 0.252e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * t * u - 0.252e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * u * u + 0.36e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(u, 0.3e1) - 0.27e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) + 0.238e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u + 0.586e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t * u * u + 0.238e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * pow(u, 0.3e1) - 0.27e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(u, 0.4e1) - 0.9e1 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.5e1) - 0.32e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.4e1) * u - 0.295e3 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.3e1) * u * u - 0.295e3 * pow(s3, 0.3e1) * s4 * s4 * t * t * pow(u, 0.3e1) - 0.32e2 * pow(s3, 0.3e1) * s4 * s4 * t * pow(u, 0.4e1) - 0.9e1 * pow(s3, 0.3e1) * s4 * s4 * pow(u, 0.5e1) - 0.25e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.5e1) * u - 0.54e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.4e1) * u * u - 0.74e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.54e2 * pow(s3, 0.3e1) * s4 * t * t * pow(u, 0.4e1) - 0.25e2 * pow(s3, 0.3e1) * s4 * t * pow(u, 0.5e1) + 0.31e2 * pow(s3, 0.3e1) * pow(t, 0.5e1) * u * u + 0.97e2 * pow(s3, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.97e2 * pow(s3, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.31e2 * pow(s3, 0.3e1) * t * t * pow(u, 0.5e1) - 0.68e2 * s3 * s3 * pow(s4, 0.5e1) * t * t * u - 0.68e2 * s3 * s3 * pow(s4, 0.5e1) * t * u * u + 0.86e2 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * u + 0.372e3 * s3 * s3 * pow(s4, 0.4e1) * t * t * u * u + 0.86e2 * s3 * s3 * pow(s4, 0.4e1) * t * pow(u, 0.3e1) - 0.9e1 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) - 0.32e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * u - 0.295e3 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * u * u - 0.295e3 * s3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.3e1) - 0.32e2 * s3 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.4e1) - 0.9e1 * s3 * s3 * pow(s4, 0.3e1) * pow(u, 0.5e1) + 0.9e1 * s3 * s3 * s4 * s4 * pow(t, 0.6e1) - 0.3e1 * s3 * s3 * s4 * s4 * pow(t, 0.5e1) * u - 0.107e3 * s3 * s3 * s4 * s4 * pow(t, 0.4e1) * u * u - 0.6e1 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.107e3 * s3 * s3 * s4 * s4 * t * t * pow(u, 0.4e1) - 0.3e1 * s3 * s3 * s4 * s4 * t * pow(u, 0.5e1) + 0.9e1 * s3 * s3 * s4 * s4 * pow(u, 0.6e1) + 0.17e2 * s3 * s3 * s4 * pow(t, 0.6e1) * u + 0.118e3 * s3 * s3 * s4 * pow(t, 0.5e1) * u * u + 0.293e3 * s3 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.293e3 * s3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.118e3 * s3 * s3 * s4 * t * t * pow(u, 0.5e1) + 0.17e2 * s3 * s3 * s4 * t * pow(u, 0.6e1) - 0.23e2 * s3 * s3 * pow(t, 0.6e1) * u * u - 0.96e2 * s3 * s3 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.146e3 * s3 * s3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.96e2 * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.23e2 * s3 * s3 * t * t * pow(u, 0.6e1) + 0.64e2 * s3 * pow(s4, 0.5e1) * t * t * u * u + 0.17e2 * s3 * pow(s4, 0.4e1) * pow(t, 0.4e1) * u - 0.77e2 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * u * u - 0.77e2 * s3 * pow(s4, 0.4e1) * t * t * pow(u, 0.3e1) + 0.17e2 * s3 * pow(s4, 0.4e1) * t * pow(u, 0.4e1) - 0.25e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u - 0.54e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * u * u - 0.74e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.54e2 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.4e1) - 0.25e2 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.5e1) + 0.17e2 * s3 * s4 * s4 * pow(t, 0.6e1) * u + 0.118e3 * s3 * s4 * s4 * pow(t, 0.5e1) * u * u + 0.293e3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.293e3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.118e3 * s3 * s4 * s4 * t * t * pow(u, 0.5e1) + 0.17e2 * s3 * s4 * s4 * t * pow(u, 0.6e1) - 0.9e1 * s3 * s4 * pow(t, 0.7e1) * u - 0.59e2 * s3 * s4 * pow(t, 0.6e1) * u * u - 0.177e3 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.326e3 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.177e3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.59e2 * s3 * s4 * t * t * pow(u, 0.6e1) - 0.9e1 * s3 * s4 * t * pow(u, 0.7e1) + 0.8e1 * s3 * pow(t, 0.7e1) * u * u + 0.38e2 * s3 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.78e2 * s3 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.78e2 * s3 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.38e2 * s3 * pow(t, 0.3e1) * pow(u, 0.6e1) + 0.8e1 * s3 * t * t * pow(u, 0.7e1) - 0.16e2 * pow(s4, 0.4e1) * pow(t, 0.4e1) * u * u - 0.32e2 * pow(s4, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.16e2 * pow(s4, 0.4e1) * t * t * pow(u, 0.4e1) + 0.31e2 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u * u + 0.97e2 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.97e2 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.31e2 * pow(s4, 0.3e1) * t * t * pow(u, 0.5e1) - 0.23e2 * s4 * s4 * pow(t, 0.6e1) * u * u - 0.96e2 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.146e3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.96e2 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.23e2 * s4 * s4 * t * t * pow(u, 0.6e1) + 0.8e1 * s4 * pow(t, 0.7e1) * u * u + 0.38e2 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.78e2 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.78e2 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.38e2 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) + 0.8e1 * s4 * t * t * pow(u, 0.7e1) - 0.7e1 * pow(t, 0.7e1) * pow(u, 0.3e1) - 0.14e2 * pow(t, 0.6e1) * pow(u, 0.4e1) - 0.14e2 * pow(t, 0.5e1) * pow(u, 0.5e1) - 0.14e2 * pow(t, 0.4e1) * pow(u, 0.6e1) - 0.7e1 * pow(t, 0.3e1) * pow(u, 0.7e1)) / (-u + s4) / (-t + s4) / (s3 - t) / (0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u) * pow(t, -0.2e1) * pow(u, -0.2e1) / (-u + s3) + 0.4e1 * (0.2e1 * s3 * s3 * t + 0.4e1 * s3 * s4 * t - 0.2e1 * s3 * s4 * u - 0.4e1 * s3 * t * t - 0.2e1 * s3 * t * u + 0.2e1 * s4 * s4 * t - 0.4e1 * s4 * t * t - 0.2e1 * s4 * t * u + 0.2e1 * pow(t, 0.3e1) + 0.2e1 * t * t * u + t * u * u) * pow(t, -0.2e1) / u * polylog(2, -(s4 / s - t / s) / t * s) + 0.4e1 * (0.2e1 * s3 * s3 * u - 0.2e1 * s3 * s4 * t + 0.4e1 * s3 * s4 * u - 0.2e1 * s3 * t * u - 0.4e1 * s3 * u * u + 0.2e1 * s4 * s4 * u - 0.2e1 * s4 * t * u - 0.4e1 * s4 * u * u + t * t * u + 0.2e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) / t * pow(u, -0.2e1) * polylog(2, -(s4 / s - u / s) / u * s) + 0.4e1 * (0.2e1 * s3 * s3 * t + 0.4e1 * s3 * s4 * t - 0.2e1 * s3 * s4 * u - 0.4e1 * s3 * t * t - 0.2e1 * s3 * t * u + 0.2e1 * s4 * s4 * t - 0.4e1 * s4 * t * t - 0.2e1 * s4 * t * u + 0.2e1 * pow(t, 0.3e1) + 0.2e1 * t * t * u + t * u * u) * pow(t, -0.2e1) / u * polylog(2, -(s3 / s - t / s) / t * s) + 0.4e1 * (0.2e1 * s3 * s3 * u - 0.2e1 * s3 * s4 * t + 0.4e1 * s3 * s4 * u - 0.2e1 * s3 * t * u - 0.4e1 * s3 * u * u + 0.2e1 * s4 * s4 * u - 0.2e1 * s4 * t * u - 0.4e1 * s4 * u * u + t * t * u + 0.2e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) / t * pow(u, -0.2e1) * polylog(2, -(s3 / s - u / s) / u * s) - 0.4e1 * (0.12e2 * pow(s3, 0.4e1) * s4 * s4 * t + 0.60e2 * pow(s3, 0.4e1) * s4 * s4 * u - 0.8e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t + 0.120e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u - 0.8e1 * pow(s3, 0.3e1) * s4 * s4 * t * t - 0.96e2 * pow(s3, 0.3e1) * s4 * s4 * t * u - 0.120e3 * pow(s3, 0.3e1) * s4 * s4 * u * u - 0.8e1 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) - 0.48e2 * pow(s3, 0.3e1) * s4 * t * t * u - 0.60e2 * pow(s3, 0.3e1) * s4 * t * u * u - 0.20e2 * pow(s3, 0.3e1) * s4 * pow(u, 0.3e1) + 0.12e2 * s3 * s3 * pow(s4, 0.4e1) * t + 0.60e2 * s3 * s3 * pow(s4, 0.4e1) * u - 0.8e1 * s3 * s3 * pow(s4, 0.3e1) * t * t - 0.96e2 * s3 * s3 * pow(s4, 0.3e1) * t * u - 0.120e3 * s3 * s3 * pow(s4, 0.3e1) * u * u + 0.12e2 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) - 0.28e2 * s3 * s3 * s4 * s4 * t * t * u - 0.20e2 * s3 * s3 * s4 * s4 * t * u * u + 0.20e2 * s3 * s3 * s4 * s4 * pow(u, 0.3e1) + 0.4e1 * s3 * s3 * s4 * pow(t, 0.4e1) + 0.68e2 * s3 * s3 * s4 * pow(t, 0.3e1) * u + 0.164e3 * s3 * s3 * s4 * t * t * u * u + 0.140e3 * s3 * s3 * s4 * t * pow(u, 0.3e1) + 0.40e2 * s3 * s3 * s4 * pow(u, 0.4e1) + 0.2e1 * s3 * s3 * pow(t, 0.5e1) + 0.10e2 * s3 * s3 * pow(t, 0.4e1) * u + 0.20e2 * s3 * s3 * pow(t, 0.3e1) * u * u + 0.20e2 * s3 * s3 * t * t * pow(u, 0.3e1) + 0.10e2 * s3 * s3 * t * pow(u, 0.4e1) + 0.2e1 * s3 * s3 * pow(u, 0.5e1) - 0.8e1 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) - 0.48e2 * s3 * pow(s4, 0.3e1) * t * t * u - 0.60e2 * s3 * pow(s4, 0.3e1) * t * u * u - 0.20e2 * s3 * pow(s4, 0.3e1) * pow(u, 0.3e1) + 0.4e1 * s3 * s4 * s4 * pow(t, 0.4e1) + 0.68e2 * s3 * s4 * s4 * pow(t, 0.3e1) * u + 0.164e3 * s3 * s4 * s4 * t * t * u * u + 0.140e3 * s3 * s4 * s4 * t * pow(u, 0.3e1) + 0.40e2 * s3 * s4 * s4 * pow(u, 0.4e1) - 0.2e1 * s3 * s4 * pow(t, 0.5e1) - 0.20e2 * s3 * s4 * pow(t, 0.4e1) * u - 0.64e2 * s3 * s4 * pow(t, 0.3e1) * u * u - 0.92e2 * s3 * s4 * t * t * pow(u, 0.3e1) - 0.62e2 * s3 * s4 * t * pow(u, 0.4e1) - 0.16e2 * s3 * s4 * pow(u, 0.5e1) - 0.2e1 * s3 * pow(t, 0.6e1) - 0.14e2 * s3 * pow(t, 0.5e1) * u - 0.40e2 * s3 * pow(t, 0.4e1) * u * u - 0.60e2 * s3 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.50e2 * s3 * t * t * pow(u, 0.4e1) - 0.22e2 * s3 * t * pow(u, 0.5e1) - 0.4e1 * s3 * pow(u, 0.6e1) + 0.2e1 * s4 * s4 * pow(t, 0.5e1) + 0.10e2 * s4 * s4 * pow(t, 0.4e1) * u + 0.20e2 * s4 * s4 * pow(t, 0.3e1) * u * u + 0.20e2 * s4 * s4 * t * t * pow(u, 0.3e1) + 0.10e2 * s4 * s4 * t * pow(u, 0.4e1) + 0.2e1 * s4 * s4 * pow(u, 0.5e1) - 0.2e1 * s4 * pow(t, 0.6e1) - 0.14e2 * s4 * pow(t, 0.5e1) * u - 0.40e2 * s4 * pow(t, 0.4e1) * u * u - 0.60e2 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.50e2 * s4 * t * t * pow(u, 0.4e1) - 0.22e2 * s4 * t * pow(u, 0.5e1) - 0.4e1 * s4 * pow(u, 0.6e1) + pow(t, 0.7e1) + 0.7e1 * pow(t, 0.6e1) * u + 0.22e2 * pow(t, 0.5e1) * u * u + 0.40e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.45e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.31e2 * t * t * pow(u, 0.5e1) + 0.12e2 * t * pow(u, 0.6e1) + 0.2e1 * pow(u, 0.7e1)) / t * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / u / s /  (z - zp) * polylog(2, 1.0-zp) + 0.4e1 * (0.12e2 * pow(s3, 0.4e1) * s4 * s4 * t + 0.60e2 * pow(s3, 0.4e1) * s4 * s4 * u - 0.8e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t + 0.120e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u - 0.8e1 * pow(s3, 0.3e1) * s4 * s4 * t * t - 0.96e2 * pow(s3, 0.3e1) * s4 * s4 * t * u - 0.120e3 * pow(s3, 0.3e1) * s4 * s4 * u * u - 0.8e1 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) - 0.48e2 * pow(s3, 0.3e1) * s4 * t * t * u - 0.60e2 * pow(s3, 0.3e1) * s4 * t * u * u - 0.20e2 * pow(s3, 0.3e1) * s4 * pow(u, 0.3e1) + 0.12e2 * s3 * s3 * pow(s4, 0.4e1) * t + 0.60e2 * s3 * s3 * pow(s4, 0.4e1) * u - 0.8e1 * s3 * s3 * pow(s4, 0.3e1) * t * t - 0.96e2 * s3 * s3 * pow(s4, 0.3e1) * t * u - 0.120e3 * s3 * s3 * pow(s4, 0.3e1) * u * u + 0.12e2 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) - 0.28e2 * s3 * s3 * s4 * s4 * t * t * u - 0.20e2 * s3 * s3 * s4 * s4 * t * u * u + 0.20e2 * s3 * s3 * s4 * s4 * pow(u, 0.3e1) + 0.4e1 * s3 * s3 * s4 * pow(t, 0.4e1) + 0.68e2 * s3 * s3 * s4 * pow(t, 0.3e1) * u + 0.164e3 * s3 * s3 * s4 * t * t * u * u + 0.140e3 * s3 * s3 * s4 * t * pow(u, 0.3e1) + 0.40e2 * s3 * s3 * s4 * pow(u, 0.4e1) + 0.2e1 * s3 * s3 * pow(t, 0.5e1) + 0.10e2 * s3 * s3 * pow(t, 0.4e1) * u + 0.20e2 * s3 * s3 * pow(t, 0.3e1) * u * u + 0.20e2 * s3 * s3 * t * t * pow(u, 0.3e1) + 0.10e2 * s3 * s3 * t * pow(u, 0.4e1) + 0.2e1 * s3 * s3 * pow(u, 0.5e1) - 0.8e1 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) - 0.48e2 * s3 * pow(s4, 0.3e1) * t * t * u - 0.60e2 * s3 * pow(s4, 0.3e1) * t * u * u - 0.20e2 * s3 * pow(s4, 0.3e1) * pow(u, 0.3e1) + 0.4e1 * s3 * s4 * s4 * pow(t, 0.4e1) + 0.68e2 * s3 * s4 * s4 * pow(t, 0.3e1) * u + 0.164e3 * s3 * s4 * s4 * t * t * u * u + 0.140e3 * s3 * s4 * s4 * t * pow(u, 0.3e1) + 0.40e2 * s3 * s4 * s4 * pow(u, 0.4e1) - 0.2e1 * s3 * s4 * pow(t, 0.5e1) - 0.20e2 * s3 * s4 * pow(t, 0.4e1) * u - 0.64e2 * s3 * s4 * pow(t, 0.3e1) * u * u - 0.92e2 * s3 * s4 * t * t * pow(u, 0.3e1) - 0.62e2 * s3 * s4 * t * pow(u, 0.4e1) - 0.16e2 * s3 * s4 * pow(u, 0.5e1) - 0.2e1 * s3 * pow(t, 0.6e1) - 0.14e2 * s3 * pow(t, 0.5e1) * u - 0.40e2 * s3 * pow(t, 0.4e1) * u * u - 0.60e2 * s3 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.50e2 * s3 * t * t * pow(u, 0.4e1) - 0.22e2 * s3 * t * pow(u, 0.5e1) - 0.4e1 * s3 * pow(u, 0.6e1) + 0.2e1 * s4 * s4 * pow(t, 0.5e1) + 0.10e2 * s4 * s4 * pow(t, 0.4e1) * u + 0.20e2 * s4 * s4 * pow(t, 0.3e1) * u * u + 0.20e2 * s4 * s4 * t * t * pow(u, 0.3e1) + 0.10e2 * s4 * s4 * t * pow(u, 0.4e1) + 0.2e1 * s4 * s4 * pow(u, 0.5e1) - 0.2e1 * s4 * pow(t, 0.6e1) - 0.14e2 * s4 * pow(t, 0.5e1) * u - 0.40e2 * s4 * pow(t, 0.4e1) * u * u - 0.60e2 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.50e2 * s4 * t * t * pow(u, 0.4e1) - 0.22e2 * s4 * t * pow(u, 0.5e1) - 0.4e1 * s4 * pow(u, 0.6e1) + pow(t, 0.7e1) + 0.7e1 * pow(t, 0.6e1) * u + 0.22e2 * pow(t, 0.5e1) * u * u + 0.40e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.45e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.31e2 * t * t * pow(u, 0.5e1) + 0.12e2 * t * pow(u, 0.6e1) + 0.2e1 * pow(u, 0.7e1)) / t * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / u / s /  (z - zp) * polylog(2, 1.0-z) + 0.4e1 * (0.60e2 * pow(s3, 0.4e1) * s4 * s4 * t + 0.12e2 * pow(s3, 0.4e1) * s4 * s4 * u + 0.120e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t - 0.8e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u - 0.120e3 * pow(s3, 0.3e1) * s4 * s4 * t * t - 0.96e2 * pow(s3, 0.3e1) * s4 * s4 * t * u - 0.8e1 * pow(s3, 0.3e1) * s4 * s4 * u * u - 0.20e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) - 0.60e2 * pow(s3, 0.3e1) * s4 * t * t * u - 0.48e2 * pow(s3, 0.3e1) * s4 * t * u * u - 0.8e1 * pow(s3, 0.3e1) * s4 * pow(u, 0.3e1) + 0.60e2 * s3 * s3 * pow(s4, 0.4e1) * t + 0.12e2 * s3 * s3 * pow(s4, 0.4e1) * u - 0.120e3 * s3 * s3 * pow(s4, 0.3e1) * t * t - 0.96e2 * s3 * s3 * pow(s4, 0.3e1) * t * u - 0.8e1 * s3 * s3 * pow(s4, 0.3e1) * u * u + 0.20e2 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) - 0.20e2 * s3 * s3 * s4 * s4 * t * t * u - 0.28e2 * s3 * s3 * s4 * s4 * t * u * u + 0.12e2 * s3 * s3 * s4 * s4 * pow(u, 0.3e1) + 0.40e2 * s3 * s3 * s4 * pow(t, 0.4e1) + 0.140e3 * s3 * s3 * s4 * pow(t, 0.3e1) * u + 0.164e3 * s3 * s3 * s4 * t * t * u * u + 0.68e2 * s3 * s3 * s4 * t * pow(u, 0.3e1) + 0.4e1 * s3 * s3 * s4 * pow(u, 0.4e1) + 0.2e1 * s3 * s3 * pow(t, 0.5e1) + 0.10e2 * s3 * s3 * pow(t, 0.4e1) * u + 0.20e2 * s3 * s3 * pow(t, 0.3e1) * u * u + 0.20e2 * s3 * s3 * t * t * pow(u, 0.3e1) + 0.10e2 * s3 * s3 * t * pow(u, 0.4e1) + 0.2e1 * s3 * s3 * pow(u, 0.5e1) - 0.20e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) - 0.60e2 * s3 * pow(s4, 0.3e1) * t * t * u - 0.48e2 * s3 * pow(s4, 0.3e1) * t * u * u - 0.8e1 * s3 * pow(s4, 0.3e1) * pow(u, 0.3e1) + 0.40e2 * s3 * s4 * s4 * pow(t, 0.4e1) + 0.140e3 * s3 * s4 * s4 * pow(t, 0.3e1) * u + 0.164e3 * s3 * s4 * s4 * t * t * u * u + 0.68e2 * s3 * s4 * s4 * t * pow(u, 0.3e1) + 0.4e1 * s3 * s4 * s4 * pow(u, 0.4e1) - 0.16e2 * s3 * s4 * pow(t, 0.5e1) - 0.62e2 * s3 * s4 * pow(t, 0.4e1) * u - 0.92e2 * s3 * s4 * pow(t, 0.3e1) * u * u - 0.64e2 * s3 * s4 * t * t * pow(u, 0.3e1) - 0.20e2 * s3 * s4 * t * pow(u, 0.4e1) - 0.2e1 * s3 * s4 * pow(u, 0.5e1) - 0.4e1 * s3 * pow(t, 0.6e1) - 0.22e2 * s3 * pow(t, 0.5e1) * u - 0.50e2 * s3 * pow(t, 0.4e1) * u * u - 0.60e2 * s3 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.40e2 * s3 * t * t * pow(u, 0.4e1) - 0.14e2 * s3 * t * pow(u, 0.5e1) - 0.2e1 * s3 * pow(u, 0.6e1) + 0.2e1 * s4 * s4 * pow(t, 0.5e1) + 0.10e2 * s4 * s4 * pow(t, 0.4e1) * u + 0.20e2 * s4 * s4 * pow(t, 0.3e1) * u * u + 0.20e2 * s4 * s4 * t * t * pow(u, 0.3e1) + 0.10e2 * s4 * s4 * t * pow(u, 0.4e1) + 0.2e1 * s4 * s4 * pow(u, 0.5e1) - 0.4e1 * s4 * pow(t, 0.6e1) - 0.22e2 * s4 * pow(t, 0.5e1) * u - 0.50e2 * s4 * pow(t, 0.4e1) * u * u - 0.60e2 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.40e2 * s4 * t * t * pow(u, 0.4e1) - 0.14e2 * s4 * t * pow(u, 0.5e1) - 0.2e1 * s4 * pow(u, 0.6e1) + 0.2e1 * pow(t, 0.7e1) + 0.12e2 * pow(t, 0.6e1) * u + 0.31e2 * pow(t, 0.5e1) * u * u + 0.45e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.40e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.22e2 * t * t * pow(u, 0.5e1) + 0.7e1 * t * pow(u, 0.6e1) + pow(u, 0.7e1)) / t * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / u / s /  (z - zp) * polylog(2, zp) - 0.4e1 * (0.60e2 * pow(s3, 0.4e1) * s4 * s4 * t + 0.12e2 * pow(s3, 0.4e1) * s4 * s4 * u + 0.120e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t - 0.8e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u - 0.120e3 * pow(s3, 0.3e1) * s4 * s4 * t * t - 0.96e2 * pow(s3, 0.3e1) * s4 * s4 * t * u - 0.8e1 * pow(s3, 0.3e1) * s4 * s4 * u * u - 0.20e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) - 0.60e2 * pow(s3, 0.3e1) * s4 * t * t * u - 0.48e2 * pow(s3, 0.3e1) * s4 * t * u * u - 0.8e1 * pow(s3, 0.3e1) * s4 * pow(u, 0.3e1) + 0.60e2 * s3 * s3 * pow(s4, 0.4e1) * t + 0.12e2 * s3 * s3 * pow(s4, 0.4e1) * u - 0.120e3 * s3 * s3 * pow(s4, 0.3e1) * t * t - 0.96e2 * s3 * s3 * pow(s4, 0.3e1) * t * u - 0.8e1 * s3 * s3 * pow(s4, 0.3e1) * u * u + 0.20e2 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) - 0.20e2 * s3 * s3 * s4 * s4 * t * t * u - 0.28e2 * s3 * s3 * s4 * s4 * t * u * u + 0.12e2 * s3 * s3 * s4 * s4 * pow(u, 0.3e1) + 0.40e2 * s3 * s3 * s4 * pow(t, 0.4e1) + 0.140e3 * s3 * s3 * s4 * pow(t, 0.3e1) * u + 0.164e3 * s3 * s3 * s4 * t * t * u * u + 0.68e2 * s3 * s3 * s4 * t * pow(u, 0.3e1) + 0.4e1 * s3 * s3 * s4 * pow(u, 0.4e1) + 0.2e1 * s3 * s3 * pow(t, 0.5e1) + 0.10e2 * s3 * s3 * pow(t, 0.4e1) * u + 0.20e2 * s3 * s3 * pow(t, 0.3e1) * u * u + 0.20e2 * s3 * s3 * t * t * pow(u, 0.3e1) + 0.10e2 * s3 * s3 * t * pow(u, 0.4e1) + 0.2e1 * s3 * s3 * pow(u, 0.5e1) - 0.20e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) - 0.60e2 * s3 * pow(s4, 0.3e1) * t * t * u - 0.48e2 * s3 * pow(s4, 0.3e1) * t * u * u - 0.8e1 * s3 * pow(s4, 0.3e1) * pow(u, 0.3e1) + 0.40e2 * s3 * s4 * s4 * pow(t, 0.4e1) + 0.140e3 * s3 * s4 * s4 * pow(t, 0.3e1) * u + 0.164e3 * s3 * s4 * s4 * t * t * u * u + 0.68e2 * s3 * s4 * s4 * t * pow(u, 0.3e1) + 0.4e1 * s3 * s4 * s4 * pow(u, 0.4e1) - 0.16e2 * s3 * s4 * pow(t, 0.5e1) - 0.62e2 * s3 * s4 * pow(t, 0.4e1) * u - 0.92e2 * s3 * s4 * pow(t, 0.3e1) * u * u - 0.64e2 * s3 * s4 * t * t * pow(u, 0.3e1) - 0.20e2 * s3 * s4 * t * pow(u, 0.4e1) - 0.2e1 * s3 * s4 * pow(u, 0.5e1) - 0.4e1 * s3 * pow(t, 0.6e1) - 0.22e2 * s3 * pow(t, 0.5e1) * u - 0.50e2 * s3 * pow(t, 0.4e1) * u * u - 0.60e2 * s3 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.40e2 * s3 * t * t * pow(u, 0.4e1) - 0.14e2 * s3 * t * pow(u, 0.5e1) - 0.2e1 * s3 * pow(u, 0.6e1) + 0.2e1 * s4 * s4 * pow(t, 0.5e1) + 0.10e2 * s4 * s4 * pow(t, 0.4e1) * u + 0.20e2 * s4 * s4 * pow(t, 0.3e1) * u * u + 0.20e2 * s4 * s4 * t * t * pow(u, 0.3e1) + 0.10e2 * s4 * s4 * t * pow(u, 0.4e1) + 0.2e1 * s4 * s4 * pow(u, 0.5e1) - 0.4e1 * s4 * pow(t, 0.6e1) - 0.22e2 * s4 * pow(t, 0.5e1) * u - 0.50e2 * s4 * pow(t, 0.4e1) * u * u - 0.60e2 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.40e2 * s4 * t * t * pow(u, 0.4e1) - 0.14e2 * s4 * t * pow(u, 0.5e1) - 0.2e1 * s4 * pow(u, 0.6e1) + 0.2e1 * pow(t, 0.7e1) + 0.12e2 * pow(t, 0.6e1) * u + 0.31e2 * pow(t, 0.5e1) * u * u + 0.45e2 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.40e2 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.22e2 * t * t * pow(u, 0.5e1) + 0.7e1 * t * pow(u, 0.6e1) + pow(u, 0.7e1)) / t * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / u / s /  (z - zp) * polylog(2, z) - 0.2e1 * (0.3e1 * pow(s3, 0.4e1) * s4 * s4 * u - 0.2e1 * pow(s3, 0.4e1) * s4 * u * u - 0.3e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t + 0.6e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u - 0.16e2 * pow(s3, 0.3e1) * s4 * s4 * u * u + pow(s3, 0.3e1) * s4 * t * u * u + 0.8e1 * pow(s3, 0.3e1) * s4 * pow(u, 0.3e1) + 0.3e1 * s3 * s3 * pow(s4, 0.4e1) * u - 0.16e2 * s3 * s3 * pow(s4, 0.3e1) * u * u + 0.3e1 * s3 * s3 * s4 * s4 * t * t * u + 0.6e1 * s3 * s3 * s4 * s4 * t * u * u + 0.22e2 * s3 * s3 * s4 * s4 * pow(u, 0.3e1) - 0.2e1 * s3 * s3 * s4 * t * t * u * u + 0.2e1 * s3 * s3 * s4 * t * pow(u, 0.3e1) - 0.8e1 * s3 * s3 * s4 * pow(u, 0.4e1) - 0.4e1 * s3 * s3 * t * pow(u, 0.4e1) - 0.2e1 * s3 * pow(s4, 0.4e1) * u * u + s3 * pow(s4, 0.3e1) * t * u * u + 0.8e1 * s3 * pow(s4, 0.3e1) * pow(u, 0.3e1) - 0.2e1 * s3 * s4 * s4 * t * t * u * u + 0.2e1 * s3 * s4 * s4 * t * pow(u, 0.3e1) - 0.8e1 * s3 * s4 * s4 * pow(u, 0.4e1) - 0.4e1 * s3 * s4 * t * t * pow(u, 0.3e1) - 0.11e2 * s3 * s4 * t * pow(u, 0.4e1) + 0.2e1 * s3 * s4 * pow(u, 0.5e1) + 0.4e1 * s3 * t * t * pow(u, 0.4e1) + 0.6e1 * s3 * t * pow(u, 0.5e1) - 0.4e1 * s4 * s4 * t * pow(u, 0.4e1) + 0.4e1 * s4 * t * t * pow(u, 0.4e1) + 0.6e1 * s4 * t * pow(u, 0.5e1) - 0.3e1 * t * t * pow(u, 0.5e1) - 0.2e1 * t * pow(u, 0.6e1)) * pow(u, -0.2e1) * pow(-u + s3, -0.2e1) * pow(-u + s4, -0.2e1) / t * log(-u) + 0.2e1 * (0.2e1 * s3 * s3 * u - 0.2e1 * s3 * s4 * t + 0.4e1 * s3 * s4 * u - 0.2e1 * s3 * t * u - 0.4e1 * s3 * u * u + 0.2e1 * s4 * s4 * u - 0.2e1 * s4 * t * u - 0.4e1 * s4 * u * u + t * t * u + 0.2e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) / t * pow(u, -0.2e1) * pow(log(-u), 0.2e1) + 0.2e1 * (0.2e1 * s3 * s3 * t + 0.4e1 * s3 * s4 * t - 0.2e1 * s3 * s4 * u - 0.4e1 * s3 * t * t - 0.2e1 * s3 * t * u + 0.2e1 * s4 * s4 * t - 0.4e1 * s4 * t * t - 0.2e1 * s4 * t * u + 0.2e1 * pow(t, 0.3e1) + 0.2e1 * t * t * u + t * u * u) * pow(t, -0.2e1) / u * pow(log(-t), 0.2e1) - 0.4e1 * (0.2e1 * s3 * s3 * u - 0.2e1 * s3 * s4 * t + 0.4e1 * s3 * s4 * u - 0.2e1 * s3 * t * u - 0.4e1 * s3 * u * u + 0.2e1 * s4 * s4 * u - 0.2e1 * s4 * t * u - 0.4e1 * s4 * u * u + t * t * u + 0.2e1 * t * u * u + 0.2e1 * pow(u, 0.3e1)) / t * pow(u, -0.2e1) * log(-s) * log(-u) + 0.2e1 * (0.4e1 * s3 * s3 * t * u - 0.2e1 * s3 * s4 * t * t + 0.8e1 * s3 * s4 * t * u - 0.2e1 * s3 * s4 * u * u - 0.6e1 * s3 * t * t * u - 0.6e1 * s3 * t * u * u + 0.4e1 * s4 * s4 * t * u - 0.6e1 * s4 * t * t * u - 0.6e1 * s4 * t * u * u + 0.3e1 * pow(t, 0.3e1) * u + 0.4e1 * t * t * u * u + 0.3e1 * t * pow(u, 0.3e1)) * pow(u, -0.2e1) * pow(t, -0.2e1) * pow(log(-s), 0.2e1) + 0.2e1 * s4 * (0.96e2 * pow(s3, 0.4e1) * pow(s4, 0.5e1) * t * u - 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * t * u - 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * u * u + 0.48e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u + 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * t * u * u + 0.48e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * pow(u, 0.3e1) - 0.32e2 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.3e1) * u * u - 0.32e2 * pow(s3, 0.4e1) * s4 * s4 * t * t * pow(u, 0.3e1) - 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.6e1) * t * t + 0.192e3 * pow(s3, 0.3e1) * pow(s4, 0.6e1) * t * u - 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.6e1) * u * u + 0.96e2 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * pow(t, 0.3e1) - 0.360e3 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * t * t * u - 0.360e3 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * t * u * u + 0.96e2 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * pow(u, 0.3e1) - 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.4e1) + 0.128e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.3e1) * u + 0.608e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * t * u * u + 0.128e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * pow(u, 0.3e1) - 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(u, 0.4e1) + 0.56e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) * u - 0.168e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u * u - 0.168e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t * pow(u, 0.3e1) + 0.56e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * pow(u, 0.4e1) - 0.24e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.5e1) * u - 0.80e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.4e1) * u * u - 0.16e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.80e2 * pow(s3, 0.3e1) * s4 * s4 * t * t * pow(u, 0.4e1) - 0.24e2 * pow(s3, 0.3e1) * s4 * s4 * t * pow(u, 0.5e1) + 0.16e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.5e1) * u * u + 0.40e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.40e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.16e2 * pow(s3, 0.3e1) * s4 * t * t * pow(u, 0.5e1) + 0.96e2 * s3 * s3 * pow(s4, 0.7e1) * t * u - 0.312e3 * s3 * s3 * pow(s4, 0.6e1) * t * t * u - 0.312e3 * s3 * s3 * pow(s4, 0.6e1) * t * u * u + 0.24e2 * s3 * s3 * pow(s4, 0.5e1) * pow(t, 0.4e1) + 0.364e3 * s3 * s3 * pow(s4, 0.5e1) * pow(t, 0.3e1) * u + 0.648e3 * s3 * s3 * pow(s4, 0.5e1) * t * t * u * u + 0.364e3 * s3 * s3 * pow(s4, 0.5e1) * t * pow(u, 0.3e1) + 0.24e2 * s3 * s3 * pow(s4, 0.5e1) * pow(u, 0.4e1) - 0.48e2 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.5e1) - 0.196e3 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.4e1) * u - 0.276e3 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * u * u - 0.276e3 * s3 * s3 * pow(s4, 0.4e1) * t * t * pow(u, 0.3e1) - 0.196e3 * s3 * s3 * pow(s4, 0.4e1) * t * pow(u, 0.4e1) - 0.48e2 * s3 * s3 * pow(s4, 0.4e1) * pow(u, 0.5e1) + 0.24e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.6e1) + 0.58e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u - 0.88e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * u * u - 0.372e3 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.88e2 * s3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.4e1) + 0.58e2 * s3 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.5e1) + 0.24e2 * s3 * s3 * pow(s4, 0.3e1) * pow(u, 0.6e1) - 0.12e2 * s3 * s3 * s4 * s4 * pow(t, 0.6e1) * u + 0.28e2 * s3 * s3 * s4 * s4 * pow(t, 0.5e1) * u * u + 0.328e3 * s3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.328e3 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.28e2 * s3 * s3 * s4 * s4 * t * t * pow(u, 0.5e1) - 0.12e2 * s3 * s3 * s4 * s4 * t * pow(u, 0.6e1) + 0.3e1 * s3 * s3 * s4 * pow(t, 0.7e1) * u + 0.20e2 * s3 * s3 * s4 * pow(t, 0.6e1) * u * u - 0.15e2 * s3 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.128e3 * s3 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.15e2 * s3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.20e2 * s3 * s3 * s4 * t * t * pow(u, 0.6e1) + 0.3e1 * s3 * s3 * s4 * t * pow(u, 0.7e1) - 0.2e1 * s3 * s3 * pow(t, 0.7e1) * u * u - 0.14e2 * s3 * s3 * pow(t, 0.6e1) * pow(u, 0.3e1) - 0.8e1 * s3 * s3 * pow(t, 0.5e1) * pow(u, 0.4e1) - 0.8e1 * s3 * s3 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.14e2 * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.2e1 * s3 * s3 * t * t * pow(u, 0.7e1) - 0.36e2 * s3 * pow(s4, 0.6e1) * pow(t, 0.3e1) * u - 0.104e3 * s3 * pow(s4, 0.6e1) * t * t * u * u - 0.36e2 * s3 * pow(s4, 0.6e1) * t * pow(u, 0.3e1) + 0.108e3 * s3 * pow(s4, 0.5e1) * pow(t, 0.4e1) * u + 0.460e3 * s3 * pow(s4, 0.5e1) * pow(t, 0.3e1) * u * u + 0.460e3 * s3 * pow(s4, 0.5e1) * t * t * pow(u, 0.3e1) + 0.108e3 * s3 * pow(s4, 0.5e1) * t * pow(u, 0.4e1) - 0.3e1 * s3 * pow(s4, 0.4e1) * pow(t, 0.6e1) - 0.130e3 * s3 * pow(s4, 0.4e1) * pow(t, 0.5e1) * u - 0.681e3 * s3 * pow(s4, 0.4e1) * pow(t, 0.4e1) * u * u - 0.1172e4 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.681e3 * s3 * pow(s4, 0.4e1) * t * t * pow(u, 0.4e1) - 0.130e3 * s3 * pow(s4, 0.4e1) * t * pow(u, 0.5e1) - 0.3e1 * s3 * pow(s4, 0.4e1) * pow(u, 0.6e1) + 0.6e1 * s3 * pow(s4, 0.3e1) * pow(t, 0.7e1) + 0.82e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.6e1) * u + 0.406e3 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u * u + 0.1058e4 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.1058e4 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.406e3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.5e1) + 0.82e2 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.6e1) + 0.6e1 * s3 * pow(s4, 0.3e1) * pow(u, 0.7e1) - 0.3e1 * s3 * s4 * s4 * pow(t, 0.8e1) - 0.24e2 * s3 * s4 * s4 * pow(t, 0.7e1) * u - 0.76e2 * s3 * s4 * s4 * pow(t, 0.6e1) * u * u - 0.308e3 * s3 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.538e3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.308e3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.76e2 * s3 * s4 * s4 * t * t * pow(u, 0.6e1) - 0.24e2 * s3 * s4 * s4 * t * pow(u, 0.7e1) - 0.3e1 * s3 * s4 * s4 * pow(u, 0.8e1) - 0.8e1 * s3 * s4 * pow(t, 0.7e1) * u * u - 0.28e2 * s3 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.28e2 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.28e2 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.28e2 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.8e1 * s3 * s4 * t * t * pow(u, 0.7e1) + s3 * pow(t, 0.8e1) * u * u + 0.14e2 * s3 * pow(t, 0.7e1) * pow(u, 0.3e1) + 0.27e2 * s3 * pow(t, 0.6e1) * pow(u, 0.4e1) + 0.28e2 * s3 * pow(t, 0.5e1) * pow(u, 0.5e1) + 0.27e2 * s3 * pow(t, 0.4e1) * pow(u, 0.6e1) + 0.14e2 * s3 * pow(t, 0.3e1) * pow(u, 0.7e1) + s3 * t * t * pow(u, 0.8e1) + 0.6e1 * pow(s4, 0.5e1) * pow(t, 0.5e1) * u + 0.20e2 * pow(s4, 0.5e1) * pow(t, 0.4e1) * u * u + 0.28e2 * pow(s4, 0.5e1) * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.20e2 * pow(s4, 0.5e1) * t * t * pow(u, 0.4e1) + 0.6e1 * pow(s4, 0.5e1) * t * pow(u, 0.5e1) - 0.18e2 * pow(s4, 0.4e1) * pow(t, 0.6e1) * u - 0.82e2 * pow(s4, 0.4e1) * pow(t, 0.5e1) * u * u - 0.156e3 * pow(s4, 0.4e1) * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.156e3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.82e2 * pow(s4, 0.4e1) * t * t * pow(u, 0.5e1) - 0.18e2 * pow(s4, 0.4e1) * t * pow(u, 0.6e1) + 0.21e2 * pow(s4, 0.3e1) * pow(t, 0.7e1) * u + 0.124e3 * pow(s4, 0.3e1) * pow(t, 0.6e1) * u * u + 0.299e3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * pow(u, 0.3e1) + 0.392e3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.4e1) + 0.299e3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.124e3 * pow(s4, 0.3e1) * t * t * pow(u, 0.6e1) + 0.21e2 * pow(s4, 0.3e1) * t * pow(u, 0.7e1) - 0.12e2 * s4 * s4 * pow(t, 0.8e1) * u - 0.86e2 * s4 * s4 * pow(t, 0.7e1) * u * u - 0.254e3 * s4 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) - 0.416e3 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) - 0.416e3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.254e3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.86e2 * s4 * s4 * t * t * pow(u, 0.7e1) - 0.12e2 * s4 * s4 * t * pow(u, 0.8e1) + 0.3e1 * s4 * pow(t, 0.9e1) * u + 0.26e2 * s4 * pow(t, 0.8e1) * u * u + 0.96e2 * s4 * pow(t, 0.7e1) * pow(u, 0.3e1) + 0.194e3 * s4 * pow(t, 0.6e1) * pow(u, 0.4e1) + 0.242e3 * s4 * pow(t, 0.5e1) * pow(u, 0.5e1) + 0.194e3 * s4 * pow(t, 0.4e1) * pow(u, 0.6e1) + 0.96e2 * s4 * pow(t, 0.3e1) * pow(u, 0.7e1) + 0.26e2 * s4 * t * t * pow(u, 0.8e1) + 0.3e1 * s4 * t * pow(u, 0.9e1) - 0.2e1 * pow(t, 0.9e1) * u * u - 0.12e2 * pow(t, 0.8e1) * pow(u, 0.3e1) - 0.32e2 * pow(t, 0.7e1) * pow(u, 0.4e1) - 0.50e2 * pow(t, 0.6e1) * pow(u, 0.5e1) - 0.50e2 * pow(t, 0.5e1) * pow(u, 0.6e1) - 0.32e2 * pow(t, 0.4e1) * pow(u, 0.7e1) - 0.12e2 * pow(t, 0.3e1) * pow(u, 0.8e1) - 0.2e1 * t * t * pow(u, 0.9e1)) * pow(-t + s4, -0.2e1) * pow(-u + s4, -0.2e1) * pow(u, -0.2e1) * pow(t, -0.2e1) * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) * log( (1.0-z)) + 0.2e1 * s4 * (0.96e2 * pow(s3, 0.4e1) * pow(s4, 0.5e1) * t * u - 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * t * u - 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * u * u + 0.48e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u + 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * t * u * u + 0.48e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * pow(u, 0.3e1) - 0.32e2 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.3e1) * u * u - 0.32e2 * pow(s3, 0.4e1) * s4 * s4 * t * t * pow(u, 0.3e1) - 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.6e1) * t * t + 0.192e3 * pow(s3, 0.3e1) * pow(s4, 0.6e1) * t * u - 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.6e1) * u * u + 0.96e2 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * pow(t, 0.3e1) - 0.360e3 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * t * t * u - 0.360e3 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * t * u * u + 0.96e2 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * pow(u, 0.3e1) - 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.4e1) + 0.128e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.3e1) * u + 0.608e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * t * u * u + 0.128e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * pow(u, 0.3e1) - 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(u, 0.4e1) + 0.56e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) * u - 0.168e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u * u - 0.168e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t * pow(u, 0.3e1) + 0.56e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * pow(u, 0.4e1) - 0.24e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.5e1) * u - 0.80e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.4e1) * u * u - 0.16e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.80e2 * pow(s3, 0.3e1) * s4 * s4 * t * t * pow(u, 0.4e1) - 0.24e2 * pow(s3, 0.3e1) * s4 * s4 * t * pow(u, 0.5e1) + 0.16e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.5e1) * u * u + 0.40e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.40e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.16e2 * pow(s3, 0.3e1) * s4 * t * t * pow(u, 0.5e1) + 0.96e2 * s3 * s3 * pow(s4, 0.7e1) * t * u - 0.312e3 * s3 * s3 * pow(s4, 0.6e1) * t * t * u - 0.312e3 * s3 * s3 * pow(s4, 0.6e1) * t * u * u + 0.24e2 * s3 * s3 * pow(s4, 0.5e1) * pow(t, 0.4e1) + 0.364e3 * s3 * s3 * pow(s4, 0.5e1) * pow(t, 0.3e1) * u + 0.648e3 * s3 * s3 * pow(s4, 0.5e1) * t * t * u * u + 0.364e3 * s3 * s3 * pow(s4, 0.5e1) * t * pow(u, 0.3e1) + 0.24e2 * s3 * s3 * pow(s4, 0.5e1) * pow(u, 0.4e1) - 0.48e2 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.5e1) - 0.196e3 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.4e1) * u - 0.276e3 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * u * u - 0.276e3 * s3 * s3 * pow(s4, 0.4e1) * t * t * pow(u, 0.3e1) - 0.196e3 * s3 * s3 * pow(s4, 0.4e1) * t * pow(u, 0.4e1) - 0.48e2 * s3 * s3 * pow(s4, 0.4e1) * pow(u, 0.5e1) + 0.24e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.6e1) + 0.58e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u - 0.88e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * u * u - 0.372e3 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.88e2 * s3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.4e1) + 0.58e2 * s3 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.5e1) + 0.24e2 * s3 * s3 * pow(s4, 0.3e1) * pow(u, 0.6e1) - 0.12e2 * s3 * s3 * s4 * s4 * pow(t, 0.6e1) * u + 0.28e2 * s3 * s3 * s4 * s4 * pow(t, 0.5e1) * u * u + 0.328e3 * s3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.328e3 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.28e2 * s3 * s3 * s4 * s4 * t * t * pow(u, 0.5e1) - 0.12e2 * s3 * s3 * s4 * s4 * t * pow(u, 0.6e1) + 0.3e1 * s3 * s3 * s4 * pow(t, 0.7e1) * u + 0.20e2 * s3 * s3 * s4 * pow(t, 0.6e1) * u * u - 0.15e2 * s3 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.128e3 * s3 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.15e2 * s3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.20e2 * s3 * s3 * s4 * t * t * pow(u, 0.6e1) + 0.3e1 * s3 * s3 * s4 * t * pow(u, 0.7e1) - 0.2e1 * s3 * s3 * pow(t, 0.7e1) * u * u - 0.14e2 * s3 * s3 * pow(t, 0.6e1) * pow(u, 0.3e1) - 0.8e1 * s3 * s3 * pow(t, 0.5e1) * pow(u, 0.4e1) - 0.8e1 * s3 * s3 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.14e2 * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.2e1 * s3 * s3 * t * t * pow(u, 0.7e1) - 0.36e2 * s3 * pow(s4, 0.6e1) * pow(t, 0.3e1) * u - 0.104e3 * s3 * pow(s4, 0.6e1) * t * t * u * u - 0.36e2 * s3 * pow(s4, 0.6e1) * t * pow(u, 0.3e1) + 0.108e3 * s3 * pow(s4, 0.5e1) * pow(t, 0.4e1) * u + 0.460e3 * s3 * pow(s4, 0.5e1) * pow(t, 0.3e1) * u * u + 0.460e3 * s3 * pow(s4, 0.5e1) * t * t * pow(u, 0.3e1) + 0.108e3 * s3 * pow(s4, 0.5e1) * t * pow(u, 0.4e1) - 0.3e1 * s3 * pow(s4, 0.4e1) * pow(t, 0.6e1) - 0.130e3 * s3 * pow(s4, 0.4e1) * pow(t, 0.5e1) * u - 0.681e3 * s3 * pow(s4, 0.4e1) * pow(t, 0.4e1) * u * u - 0.1172e4 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.681e3 * s3 * pow(s4, 0.4e1) * t * t * pow(u, 0.4e1) - 0.130e3 * s3 * pow(s4, 0.4e1) * t * pow(u, 0.5e1) - 0.3e1 * s3 * pow(s4, 0.4e1) * pow(u, 0.6e1) + 0.6e1 * s3 * pow(s4, 0.3e1) * pow(t, 0.7e1) + 0.82e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.6e1) * u + 0.406e3 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u * u + 0.1058e4 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.1058e4 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.406e3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.5e1) + 0.82e2 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.6e1) + 0.6e1 * s3 * pow(s4, 0.3e1) * pow(u, 0.7e1) - 0.3e1 * s3 * s4 * s4 * pow(t, 0.8e1) - 0.24e2 * s3 * s4 * s4 * pow(t, 0.7e1) * u - 0.76e2 * s3 * s4 * s4 * pow(t, 0.6e1) * u * u - 0.308e3 * s3 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.538e3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.308e3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.76e2 * s3 * s4 * s4 * t * t * pow(u, 0.6e1) - 0.24e2 * s3 * s4 * s4 * t * pow(u, 0.7e1) - 0.3e1 * s3 * s4 * s4 * pow(u, 0.8e1) - 0.8e1 * s3 * s4 * pow(t, 0.7e1) * u * u - 0.28e2 * s3 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.28e2 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.28e2 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.28e2 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.8e1 * s3 * s4 * t * t * pow(u, 0.7e1) + s3 * pow(t, 0.8e1) * u * u + 0.14e2 * s3 * pow(t, 0.7e1) * pow(u, 0.3e1) + 0.27e2 * s3 * pow(t, 0.6e1) * pow(u, 0.4e1) + 0.28e2 * s3 * pow(t, 0.5e1) * pow(u, 0.5e1) + 0.27e2 * s3 * pow(t, 0.4e1) * pow(u, 0.6e1) + 0.14e2 * s3 * pow(t, 0.3e1) * pow(u, 0.7e1) + s3 * t * t * pow(u, 0.8e1) + 0.6e1 * pow(s4, 0.5e1) * pow(t, 0.5e1) * u + 0.20e2 * pow(s4, 0.5e1) * pow(t, 0.4e1) * u * u + 0.28e2 * pow(s4, 0.5e1) * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.20e2 * pow(s4, 0.5e1) * t * t * pow(u, 0.4e1) + 0.6e1 * pow(s4, 0.5e1) * t * pow(u, 0.5e1) - 0.18e2 * pow(s4, 0.4e1) * pow(t, 0.6e1) * u - 0.82e2 * pow(s4, 0.4e1) * pow(t, 0.5e1) * u * u - 0.156e3 * pow(s4, 0.4e1) * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.156e3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.82e2 * pow(s4, 0.4e1) * t * t * pow(u, 0.5e1) - 0.18e2 * pow(s4, 0.4e1) * t * pow(u, 0.6e1) + 0.21e2 * pow(s4, 0.3e1) * pow(t, 0.7e1) * u + 0.124e3 * pow(s4, 0.3e1) * pow(t, 0.6e1) * u * u + 0.299e3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * pow(u, 0.3e1) + 0.392e3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.4e1) + 0.299e3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.124e3 * pow(s4, 0.3e1) * t * t * pow(u, 0.6e1) + 0.21e2 * pow(s4, 0.3e1) * t * pow(u, 0.7e1) - 0.12e2 * s4 * s4 * pow(t, 0.8e1) * u - 0.86e2 * s4 * s4 * pow(t, 0.7e1) * u * u - 0.254e3 * s4 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) - 0.416e3 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) - 0.416e3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.254e3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.86e2 * s4 * s4 * t * t * pow(u, 0.7e1) - 0.12e2 * s4 * s4 * t * pow(u, 0.8e1) + 0.3e1 * s4 * pow(t, 0.9e1) * u + 0.26e2 * s4 * pow(t, 0.8e1) * u * u + 0.96e2 * s4 * pow(t, 0.7e1) * pow(u, 0.3e1) + 0.194e3 * s4 * pow(t, 0.6e1) * pow(u, 0.4e1) + 0.242e3 * s4 * pow(t, 0.5e1) * pow(u, 0.5e1) + 0.194e3 * s4 * pow(t, 0.4e1) * pow(u, 0.6e1) + 0.96e2 * s4 * pow(t, 0.3e1) * pow(u, 0.7e1) + 0.26e2 * s4 * t * t * pow(u, 0.8e1) + 0.3e1 * s4 * t * pow(u, 0.9e1) - 0.2e1 * pow(t, 0.9e1) * u * u - 0.12e2 * pow(t, 0.8e1) * pow(u, 0.3e1) - 0.32e2 * pow(t, 0.7e1) * pow(u, 0.4e1) - 0.50e2 * pow(t, 0.6e1) * pow(u, 0.5e1) - 0.50e2 * pow(t, 0.5e1) * pow(u, 0.6e1) - 0.32e2 * pow(t, 0.4e1) * pow(u, 0.7e1) - 0.12e2 * pow(t, 0.3e1) * pow(u, 0.8e1) - 0.2e1 * t * t * pow(u, 0.9e1)) * pow(-t + s4, -0.2e1) * pow(-u + s4, -0.2e1) * pow(u, -0.2e1) * pow(t, -0.2e1) * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) * log( (1.0-zp)) + 0.2e1 * (0.6e1 * pow(s3, 0.6e1) * pow(s4, 0.4e1) * t * u - 0.8e1 * pow(s3, 0.6e1) * pow(s4, 0.3e1) * t * t * u - 0.8e1 * pow(s3, 0.6e1) * pow(s4, 0.3e1) * t * u * u + 0.3e1 * pow(s3, 0.6e1) * s4 * s4 * pow(t, 0.3e1) * u + 0.8e1 * pow(s3, 0.6e1) * s4 * s4 * t * t * u * u + 0.3e1 * pow(s3, 0.6e1) * s4 * s4 * t * pow(u, 0.3e1) - 0.2e1 * pow(s3, 0.6e1) * s4 * pow(t, 0.3e1) * u * u - 0.2e1 * pow(s3, 0.6e1) * s4 * t * t * pow(u, 0.3e1) - 0.3e1 * pow(s3, 0.5e1) * pow(s4, 0.5e1) * t * t + 0.12e2 * pow(s3, 0.5e1) * pow(s4, 0.5e1) * t * u - 0.3e1 * pow(s3, 0.5e1) * pow(s4, 0.5e1) * u * u + 0.6e1 * pow(s3, 0.5e1) * pow(s4, 0.4e1) * pow(t, 0.3e1) - 0.34e2 * pow(s3, 0.5e1) * pow(s4, 0.4e1) * t * t * u - 0.34e2 * pow(s3, 0.5e1) * pow(s4, 0.4e1) * t * u * u + 0.6e1 * pow(s3, 0.5e1) * pow(s4, 0.4e1) * pow(u, 0.3e1) - 0.3e1 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) + 0.26e2 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u + 0.74e2 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * t * t * u * u + 0.26e2 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * t * pow(u, 0.3e1) - 0.3e1 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * pow(u, 0.4e1) - 0.6e1 * pow(s3, 0.5e1) * s4 * s4 * pow(t, 0.4e1) * u - 0.42e2 * pow(s3, 0.5e1) * s4 * s4 * pow(t, 0.3e1) * u * u - 0.42e2 * pow(s3, 0.5e1) * s4 * s4 * t * t * pow(u, 0.3e1) - 0.6e1 * pow(s3, 0.5e1) * s4 * s4 * t * pow(u, 0.4e1) + 0.5e1 * pow(s3, 0.5e1) * s4 * pow(t, 0.4e1) * u * u + 0.16e2 * pow(s3, 0.5e1) * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.5e1 * pow(s3, 0.5e1) * s4 * t * t * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.4e1) * pow(s4, 0.6e1) * t * u + 0.6e1 * pow(s3, 0.4e1) * pow(s4, 0.5e1) * pow(t, 0.3e1) - 0.34e2 * pow(s3, 0.4e1) * pow(s4, 0.5e1) * t * t * u - 0.34e2 * pow(s3, 0.4e1) * pow(s4, 0.5e1) * t * u * u + 0.6e1 * pow(s3, 0.4e1) * pow(s4, 0.5e1) * pow(u, 0.3e1) - 0.12e2 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * pow(t, 0.4e1) + 0.55e2 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * pow(t, 0.3e1) * u + 0.140e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * t * u * u + 0.55e2 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * pow(u, 0.3e1) - 0.12e2 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.5e1) - 0.32e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) * u - 0.156e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u * u - 0.156e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * t * pow(u, 0.3e1) - 0.32e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(u, 0.5e1) + 0.6e1 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.5e1) * u + 0.62e2 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.4e1) * u * u + 0.100e3 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.62e2 * pow(s3, 0.4e1) * s4 * s4 * t * t * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.4e1) * s4 * s4 * t * pow(u, 0.5e1) - 0.6e1 * pow(s3, 0.4e1) * s4 * pow(t, 0.5e1) * u * u - 0.14e2 * pow(s3, 0.4e1) * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.14e2 * pow(s3, 0.4e1) * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.6e1 * pow(s3, 0.4e1) * s4 * t * t * pow(u, 0.5e1) - 0.8e1 * pow(s3, 0.4e1) * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.8e1 * pow(s3, 0.3e1) * pow(s4, 0.6e1) * t * t * u - 0.8e1 * pow(s3, 0.3e1) * pow(s4, 0.6e1) * t * u * u - 0.3e1 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * pow(t, 0.4e1) + 0.26e2 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * pow(t, 0.3e1) * u + 0.74e2 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * t * t * u * u + 0.26e2 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * t * pow(u, 0.3e1) - 0.3e1 * pow(s3, 0.3e1) * pow(s4, 0.5e1) * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.5e1) - 0.32e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.4e1) * u - 0.156e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.3e1) * u * u - 0.156e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * t * pow(u, 0.3e1) - 0.32e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(u, 0.5e1) - 0.3e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.6e1) + 0.20e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.5e1) * u + 0.119e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) * u * u + 0.184e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.119e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t * pow(u, 0.4e1) + 0.20e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * pow(u, 0.5e1) - 0.3e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(u, 0.6e1) - 0.6e1 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.6e1) * u - 0.38e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.5e1) * u * u - 0.48e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.48e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.38e2 * pow(s3, 0.3e1) * s4 * s4 * t * t * pow(u, 0.5e1) - 0.6e1 * pow(s3, 0.3e1) * s4 * s4 * t * pow(u, 0.6e1) + 0.5e1 * pow(s3, 0.3e1) * s4 * pow(t, 0.6e1) * u * u - 0.10e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.38e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.10e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.5e1 * pow(s3, 0.3e1) * s4 * t * t * pow(u, 0.6e1) + 0.18e2 * pow(s3, 0.3e1) * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.18e2 * pow(s3, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.3e1 * s3 * s3 * pow(s4, 0.6e1) * pow(t, 0.3e1) * u + 0.8e1 * s3 * s3 * pow(s4, 0.6e1) * t * t * u * u + 0.3e1 * s3 * s3 * pow(s4, 0.6e1) * t * pow(u, 0.3e1) - 0.6e1 * s3 * s3 * pow(s4, 0.5e1) * pow(t, 0.4e1) * u - 0.42e2 * s3 * s3 * pow(s4, 0.5e1) * pow(t, 0.3e1) * u * u - 0.42e2 * s3 * s3 * pow(s4, 0.5e1) * t * t * pow(u, 0.3e1) - 0.6e1 * s3 * s3 * pow(s4, 0.5e1) * t * pow(u, 0.4e1) + 0.6e1 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.5e1) * u + 0.62e2 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.4e1) * u * u + 0.100e3 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.62e2 * s3 * s3 * pow(s4, 0.4e1) * t * t * pow(u, 0.4e1) + 0.6e1 * s3 * s3 * pow(s4, 0.4e1) * t * pow(u, 0.5e1) - 0.6e1 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.6e1) * u - 0.38e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u * u - 0.48e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.48e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.38e2 * s3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.5e1) - 0.6e1 * s3 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.6e1) + 0.3e1 * s3 * s3 * s4 * s4 * pow(t, 0.7e1) * u + 0.12e2 * s3 * s3 * s4 * s4 * pow(t, 0.6e1) * u * u - 0.21e2 * s3 * s3 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.72e2 * s3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.21e2 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.12e2 * s3 * s3 * s4 * s4 * t * t * pow(u, 0.6e1) + 0.3e1 * s3 * s3 * s4 * s4 * t * pow(u, 0.7e1) - 0.2e1 * s3 * s3 * s4 * pow(t, 0.7e1) * u * u + 0.14e2 * s3 * s3 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.74e2 * s3 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.74e2 * s3 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.14e2 * s3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.2e1 * s3 * s3 * s4 * t * t * pow(u, 0.7e1) - 0.14e2 * s3 * s3 * pow(t, 0.6e1) * pow(u, 0.4e1) - 0.30e2 * s3 * s3 * pow(t, 0.5e1) * pow(u, 0.5e1) - 0.14e2 * s3 * s3 * pow(t, 0.4e1) * pow(u, 0.6e1) - 0.2e1 * s3 * pow(s4, 0.6e1) * pow(t, 0.3e1) * u * u - 0.2e1 * s3 * pow(s4, 0.6e1) * t * t * pow(u, 0.3e1) + 0.5e1 * s3 * pow(s4, 0.5e1) * pow(t, 0.4e1) * u * u + 0.16e2 * s3 * pow(s4, 0.5e1) * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.5e1 * s3 * pow(s4, 0.5e1) * t * t * pow(u, 0.4e1) - 0.6e1 * s3 * pow(s4, 0.4e1) * pow(t, 0.5e1) * u * u - 0.14e2 * s3 * pow(s4, 0.4e1) * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.14e2 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.6e1 * s3 * pow(s4, 0.4e1) * t * t * pow(u, 0.5e1) + 0.5e1 * s3 * pow(s4, 0.3e1) * pow(t, 0.6e1) * u * u - 0.10e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.38e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.10e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.5e1 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.6e1) - 0.2e1 * s3 * s4 * s4 * pow(t, 0.7e1) * u * u + 0.14e2 * s3 * s4 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.74e2 * s3 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.74e2 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.14e2 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.2e1 * s3 * s4 * s4 * t * t * pow(u, 0.7e1) - 0.4e1 * s3 * s4 * pow(t, 0.7e1) * pow(u, 0.3e1) - 0.35e2 * s3 * s4 * pow(t, 0.6e1) * pow(u, 0.4e1) - 0.68e2 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.5e1) - 0.35e2 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.6e1) - 0.4e1 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.7e1) + 0.4e1 * s3 * pow(t, 0.7e1) * pow(u, 0.4e1) + 0.16e2 * s3 * pow(t, 0.6e1) * pow(u, 0.5e1) + 0.16e2 * s3 * pow(t, 0.5e1) * pow(u, 0.6e1) + 0.4e1 * s3 * pow(t, 0.4e1) * pow(u, 0.7e1) - 0.8e1 * pow(s4, 0.4e1) * pow(t, 0.4e1) * pow(u, 0.4e1) + 0.18e2 * pow(s4, 0.3e1) * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.18e2 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.14e2 * s4 * s4 * pow(t, 0.6e1) * pow(u, 0.4e1) - 0.30e2 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.5e1) - 0.14e2 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.6e1) + 0.4e1 * s4 * pow(t, 0.7e1) * pow(u, 0.4e1) + 0.16e2 * s4 * pow(t, 0.6e1) * pow(u, 0.5e1) + 0.16e2 * s4 * pow(t, 0.5e1) * pow(u, 0.6e1) + 0.4e1 * s4 * pow(t, 0.4e1) * pow(u, 0.7e1) - 0.3e1 * pow(t, 0.7e1) * pow(u, 0.5e1) - 0.4e1 * pow(t, 0.6e1) * pow(u, 0.6e1) - 0.3e1 * pow(t, 0.5e1) * pow(u, 0.7e1)) * pow(-u + s4, -0.2e1) * pow(s3 - t, -0.2e1) * pow(-t + s4, -0.2e1) * pow(t, -0.2e1) * pow(u, -0.2e1) * pow(-u + s3, -0.2e1) * log(-s) - 0.2e1 * (0.64e2 * s * pow(s3, 0.4e1) * s4 * s4 * t * u *  z - 0.64e2 * s * pow(s3, 0.4e1) * s4 * s4 * t * u *  zp - 0.32e2 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t *  z + 0.32e2 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t *  zp + 0.128e3 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * u *  z - 0.128e3 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * u *  zp - 0.32e2 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u * u *  z + 0.32e2 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u * u *  zp - 0.96e2 * s * pow(s3, 0.3e1) * s4 * s4 * t * t * u *  z + 0.96e2 * s * pow(s3, 0.3e1) * s4 * s4 * t * t * u *  zp - 0.96e2 * s * pow(s3, 0.3e1) * s4 * s4 * t * u * u *  z + 0.96e2 * s * pow(s3, 0.3e1) * s4 * s4 * t * u * u *  zp - 0.32e2 * s * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * u *  z + 0.32e2 * s * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * u *  zp - 0.64e2 * s * pow(s3, 0.3e1) * s4 * t * t * u * u *  z + 0.64e2 * s * pow(s3, 0.3e1) * s4 * t * t * u * u *  zp - 0.32e2 * s * pow(s3, 0.3e1) * s4 * t * pow(u, 0.3e1) *  z + 0.32e2 * s * pow(s3, 0.3e1) * s4 * t * pow(u, 0.3e1) *  zp + 0.64e2 * s * s3 * s3 * pow(s4, 0.4e1) * t * u *  z - 0.64e2 * s * s3 * s3 * pow(s4, 0.4e1) * t * u *  zp - 0.96e2 * s * s3 * s3 * pow(s4, 0.3e1) * t * t * u *  z + 0.96e2 * s * s3 * s3 * pow(s4, 0.3e1) * t * t * u *  zp - 0.96e2 * s * s3 * s3 * pow(s4, 0.3e1) * t * u * u *  z + 0.96e2 * s * s3 * s3 * pow(s4, 0.3e1) * t * u * u *  zp + 0.16e2 * s * s3 * s3 * s4 * s4 * pow(t, 0.4e1) *  z - 0.16e2 * s * s3 * s3 * s4 * s4 * pow(t, 0.4e1) *  zp + 0.16e2 * s * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * u *  z - 0.16e2 * s * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * u *  zp - 0.32e2 * s * s3 * s3 * s4 * s4 * t * t * u * u *  z + 0.32e2 * s * s3 * s3 * s4 * s4 * t * t * u * u *  zp + 0.16e2 * s * s3 * s3 * s4 * s4 * t * pow(u, 0.3e1) *  z - 0.16e2 * s * s3 * s3 * s4 * s4 * t * pow(u, 0.3e1) *  zp + 0.16e2 * s * s3 * s3 * s4 * s4 * pow(u, 0.4e1) *  z - 0.16e2 * s * s3 * s3 * s4 * s4 * pow(u, 0.4e1) *  zp + 0.48e2 * s * s3 * s3 * s4 * pow(t, 0.4e1) * u *  z - 0.48e2 * s * s3 * s3 * s4 * pow(t, 0.4e1) * u *  zp + 0.144e3 * s * s3 * s3 * s4 * pow(t, 0.3e1) * u * u *  z - 0.144e3 * s * s3 * s3 * s4 * pow(t, 0.3e1) * u * u *  zp + 0.144e3 * s * s3 * s3 * s4 * t * t * pow(u, 0.3e1) *  z - 0.144e3 * s * s3 * s3 * s4 * t * t * pow(u, 0.3e1) *  zp + 0.48e2 * s * s3 * s3 * s4 * t * pow(u, 0.4e1) *  z - 0.48e2 * s * s3 * s3 * s4 * t * pow(u, 0.4e1) *  zp + 0.4e1 * s * s3 * s3 * pow(t, 0.5e1) * u *  z - 0.4e1 * s * s3 * s3 * pow(t, 0.5e1) * u *  zp + 0.16e2 * s * s3 * s3 * pow(t, 0.4e1) * u * u *  z - 0.16e2 * s * s3 * s3 * pow(t, 0.4e1) * u * u *  zp + 0.24e2 * s * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.3e1) *  z - 0.24e2 * s * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.3e1) *  zp + 0.16e2 * s * s3 * s3 * t * t * pow(u, 0.4e1) *  z - 0.16e2 * s * s3 * s3 * t * t * pow(u, 0.4e1) *  zp + 0.4e1 * s * s3 * s3 * t * pow(u, 0.5e1) *  z - 0.4e1 * s * s3 * s3 * t * pow(u, 0.5e1) *  zp - 0.32e2 * s * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * u *  z + 0.32e2 * s * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * u *  zp - 0.64e2 * s * s3 * pow(s4, 0.3e1) * t * t * u * u *  z + 0.64e2 * s * s3 * pow(s4, 0.3e1) * t * t * u * u *  zp - 0.32e2 * s * s3 * pow(s4, 0.3e1) * t * pow(u, 0.3e1) *  z + 0.32e2 * s * s3 * pow(s4, 0.3e1) * t * pow(u, 0.3e1) *  zp + 0.48e2 * s * s3 * s4 * s4 * pow(t, 0.4e1) * u *  z - 0.48e2 * s * s3 * s4 * s4 * pow(t, 0.4e1) * u *  zp + 0.144e3 * s * s3 * s4 * s4 * pow(t, 0.3e1) * u * u *  z - 0.144e3 * s * s3 * s4 * s4 * pow(t, 0.3e1) * u * u *  zp + 0.144e3 * s * s3 * s4 * s4 * t * t * pow(u, 0.3e1) *  z - 0.144e3 * s * s3 * s4 * s4 * t * t * pow(u, 0.3e1) *  zp + 0.48e2 * s * s3 * s4 * s4 * t * pow(u, 0.4e1) *  z - 0.48e2 * s * s3 * s4 * s4 * t * pow(u, 0.4e1) *  zp - 0.2e1 * s * s3 * s4 * pow(t, 0.6e1) *  z + 0.2e1 * s * s3 * s4 * pow(t, 0.6e1) *  zp - 0.24e2 * s * s3 * s4 * pow(t, 0.5e1) * u *  z + 0.24e2 * s * s3 * s4 * pow(t, 0.5e1) * u *  zp - 0.62e2 * s * s3 * s4 * pow(t, 0.4e1) * u * u *  z + 0.62e2 * s * s3 * s4 * pow(t, 0.4e1) * u * u *  zp - 0.80e2 * s * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) *  z + 0.80e2 * s * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) *  zp - 0.62e2 * s * s3 * s4 * t * t * pow(u, 0.4e1) *  z + 0.62e2 * s * s3 * s4 * t * t * pow(u, 0.4e1) *  zp - 0.24e2 * s * s3 * s4 * t * pow(u, 0.5e1) *  z + 0.24e2 * s * s3 * s4 * t * pow(u, 0.5e1) *  zp - 0.2e1 * s * s3 * s4 * pow(u, 0.6e1) *  z + 0.2e1 * s * s3 * s4 * pow(u, 0.6e1) *  zp - 0.6e1 * s * s3 * pow(t, 0.6e1) * u *  z + 0.6e1 * s * s3 * pow(t, 0.6e1) * u *  zp - 0.30e2 * s * s3 * pow(t, 0.5e1) * u * u *  z + 0.30e2 * s * s3 * pow(t, 0.5e1) * u * u *  zp - 0.60e2 * s * s3 * pow(t, 0.4e1) * pow(u, 0.3e1) *  z + 0.60e2 * s * s3 * pow(t, 0.4e1) * pow(u, 0.3e1) *  zp - 0.60e2 * s * s3 * pow(t, 0.3e1) * pow(u, 0.4e1) *  z + 0.60e2 * s * s3 * pow(t, 0.3e1) * pow(u, 0.4e1) *  zp - 0.30e2 * s * s3 * t * t * pow(u, 0.5e1) *  z + 0.30e2 * s * s3 * t * t * pow(u, 0.5e1) *  zp - 0.6e1 * s * s3 * t * pow(u, 0.6e1) *  z + 0.6e1 * s * s3 * t * pow(u, 0.6e1) *  zp + 0.4e1 * s * s4 * s4 * pow(t, 0.5e1) * u *  z - 0.4e1 * s * s4 * s4 * pow(t, 0.5e1) * u *  zp + 0.16e2 * s * s4 * s4 * pow(t, 0.4e1) * u * u *  z - 0.16e2 * s * s4 * s4 * pow(t, 0.4e1) * u * u *  zp + 0.24e2 * s * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) *  z - 0.24e2 * s * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) *  zp + 0.16e2 * s * s4 * s4 * t * t * pow(u, 0.4e1) *  z - 0.16e2 * s * s4 * s4 * t * t * pow(u, 0.4e1) *  zp + 0.4e1 * s * s4 * s4 * t * pow(u, 0.5e1) *  z - 0.4e1 * s * s4 * s4 * t * pow(u, 0.5e1) *  zp - 0.6e1 * s * s4 * pow(t, 0.6e1) * u *  z + 0.6e1 * s * s4 * pow(t, 0.6e1) * u *  zp - 0.30e2 * s * s4 * pow(t, 0.5e1) * u * u *  z + 0.30e2 * s * s4 * pow(t, 0.5e1) * u * u *  zp - 0.60e2 * s * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) *  z + 0.60e2 * s * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) *  zp - 0.60e2 * s * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) *  z + 0.60e2 * s * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) *  zp - 0.30e2 * s * s4 * t * t * pow(u, 0.5e1) *  z + 0.30e2 * s * s4 * t * t * pow(u, 0.5e1) *  zp - 0.6e1 * s * s4 * t * pow(u, 0.6e1) *  z + 0.6e1 * s * s4 * t * pow(u, 0.6e1) *  zp + 0.3e1 * s * pow(t, 0.7e1) * u *  z - 0.3e1 * s * pow(t, 0.7e1) * u *  zp + 0.16e2 * s * pow(t, 0.6e1) * u * u *  z - 0.16e2 * s * pow(t, 0.6e1) * u * u *  zp + 0.37e2 * s * pow(t, 0.5e1) * pow(u, 0.3e1) *  z - 0.37e2 * s * pow(t, 0.5e1) * pow(u, 0.3e1) *  zp + 0.48e2 * s * pow(t, 0.4e1) * pow(u, 0.4e1) *  z - 0.48e2 * s * pow(t, 0.4e1) * pow(u, 0.4e1) *  zp + 0.37e2 * s * pow(t, 0.3e1) * pow(u, 0.5e1) *  z - 0.37e2 * s * pow(t, 0.3e1) * pow(u, 0.5e1) *  zp + 0.16e2 * s * t * t * pow(u, 0.6e1) *  z - 0.16e2 * s * t * t * pow(u, 0.6e1) *  zp + 0.3e1 * s * t * pow(u, 0.7e1) *  z - 0.3e1 * s * t * pow(u, 0.7e1) *  zp + 0.72e2 * pow(s3, 0.4e1) * s4 * s4 * t * t * u + 0.72e2 * pow(s3, 0.4e1) * s4 * s4 * t * u * u + 0.112e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t * u + 0.112e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * u * u - 0.128e3 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.3e1) * u - 0.192e3 * pow(s3, 0.3e1) * s4 * s4 * t * t * u * u - 0.128e3 * pow(s3, 0.3e1) * s4 * s4 * t * pow(u, 0.3e1) - 0.28e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.4e1) * u - 0.108e3 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * u * u - 0.108e3 * pow(s3, 0.3e1) * s4 * t * t * pow(u, 0.3e1) - 0.28e2 * pow(s3, 0.3e1) * s4 * t * pow(u, 0.4e1) + 0.72e2 * s3 * s3 * pow(s4, 0.4e1) * t * t * u + 0.72e2 * s3 * s3 * pow(s4, 0.4e1) * t * u * u - 0.128e3 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * u - 0.192e3 * s3 * s3 * pow(s4, 0.3e1) * t * t * u * u - 0.128e3 * s3 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.3e1) + 0.32e2 * s3 * s3 * s4 * s4 * pow(t, 0.4e1) * u - 0.48e2 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * u * u - 0.48e2 * s3 * s3 * s4 * s4 * t * t * pow(u, 0.3e1) + 0.32e2 * s3 * s3 * s4 * s4 * t * pow(u, 0.4e1) + 0.44e2 * s3 * s3 * s4 * pow(t, 0.5e1) * u + 0.208e3 * s3 * s3 * s4 * pow(t, 0.4e1) * u * u + 0.328e3 * s3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.208e3 * s3 * s3 * s4 * t * t * pow(u, 0.4e1) + 0.44e2 * s3 * s3 * s4 * t * pow(u, 0.5e1) + 0.4e1 * s3 * s3 * pow(t, 0.6e1) * u + 0.20e2 * s3 * s3 * pow(t, 0.5e1) * u * u + 0.40e2 * s3 * s3 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.40e2 * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.20e2 * s3 * s3 * t * t * pow(u, 0.5e1) + 0.4e1 * s3 * s3 * t * pow(u, 0.6e1) - 0.28e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * u - 0.108e3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * u * u - 0.108e3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.3e1) - 0.28e2 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.4e1) + 0.44e2 * s3 * s4 * s4 * pow(t, 0.5e1) * u + 0.208e3 * s3 * s4 * s4 * pow(t, 0.4e1) * u * u + 0.328e3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.208e3 * s3 * s4 * s4 * t * t * pow(u, 0.4e1) + 0.44e2 * s3 * s4 * s4 * t * pow(u, 0.5e1) - 0.18e2 * s3 * s4 * pow(t, 0.6e1) * u - 0.82e2 * s3 * s4 * pow(t, 0.5e1) * u * u - 0.156e3 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.156e3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.82e2 * s3 * s4 * t * t * pow(u, 0.5e1) - 0.18e2 * s3 * s4 * t * pow(u, 0.6e1) - 0.6e1 * s3 * pow(t, 0.7e1) * u - 0.36e2 * s3 * pow(t, 0.6e1) * u * u - 0.90e2 * s3 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.120e3 * s3 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.90e2 * s3 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.36e2 * s3 * t * t * pow(u, 0.6e1) - 0.6e1 * s3 * t * pow(u, 0.7e1) + 0.4e1 * s4 * s4 * pow(t, 0.6e1) * u + 0.20e2 * s4 * s4 * pow(t, 0.5e1) * u * u + 0.40e2 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.40e2 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.20e2 * s4 * s4 * t * t * pow(u, 0.5e1) + 0.4e1 * s4 * s4 * t * pow(u, 0.6e1) - 0.6e1 * s4 * pow(t, 0.7e1) * u - 0.36e2 * s4 * pow(t, 0.6e1) * u * u - 0.90e2 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.120e3 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.90e2 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.36e2 * s4 * t * t * pow(u, 0.6e1) - 0.6e1 * s4 * t * pow(u, 0.7e1) + 0.3e1 * pow(t, 0.8e1) * u + 0.19e2 * pow(t, 0.7e1) * u * u + 0.53e2 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.85e2 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.85e2 * pow(t, 0.4e1) * pow(u, 0.5e1) + 0.53e2 * pow(t, 0.3e1) * pow(u, 0.6e1) + 0.19e2 * t * t * pow(u, 0.7e1) + 0.3e1 * t * pow(u, 0.8e1)) * pow(u, -0.2e1) * pow(t, -0.2e1) * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / s /  (z - zp) * log( zp) * log( (1.0-z)) - 0.2e1 * (0.16e2 * s * pow(s3, 0.3e1) * s4 * t * u *  z - 0.16e2 * s * pow(s3, 0.3e1) * s4 * t * u *  zp - 0.8e1 * s * s3 * s3 * s4 * s4 * t * t *  z + 0.8e1 * s * s3 * s3 * s4 * s4 * t * t *  zp + 0.32e2 * s * s3 * s3 * s4 * s4 * t * u *  z - 0.32e2 * s * s3 * s3 * s4 * s4 * t * u *  zp - 0.8e1 * s * s3 * s3 * s4 * s4 * u * u *  z + 0.8e1 * s * s3 * s3 * s4 * s4 * u * u *  zp - 0.24e2 * s * s3 * s3 * s4 * t * t * u *  z + 0.24e2 * s * s3 * s3 * s4 * t * t * u *  zp - 0.24e2 * s * s3 * s3 * s4 * t * u * u *  z + 0.24e2 * s * s3 * s3 * s4 * t * u * u *  zp - 0.4e1 * s * s3 * s3 * pow(t, 0.3e1) * u *  z + 0.4e1 * s * s3 * s3 * pow(t, 0.3e1) * u *  zp - 0.8e1 * s * s3 * s3 * t * t * u * u *  z + 0.8e1 * s * s3 * s3 * t * t * u * u *  zp - 0.4e1 * s * s3 * s3 * t * pow(u, 0.3e1) *  z + 0.4e1 * s * s3 * s3 * t * pow(u, 0.3e1) *  zp + 0.16e2 * s * s3 * pow(s4, 0.3e1) * t * u *  z - 0.16e2 * s * s3 * pow(s4, 0.3e1) * t * u *  zp - 0.24e2 * s * s3 * s4 * s4 * t * t * u *  z + 0.24e2 * s * s3 * s4 * s4 * t * t * u *  zp - 0.24e2 * s * s3 * s4 * s4 * t * u * u *  z + 0.24e2 * s * s3 * s4 * s4 * t * u * u *  zp + 0.2e1 * s * s3 * s4 * pow(t, 0.4e1) *  z - 0.2e1 * s * s3 * s4 * pow(t, 0.4e1) *  zp + 0.8e1 * s * s3 * s4 * pow(t, 0.3e1) * u *  z - 0.8e1 * s * s3 * s4 * pow(t, 0.3e1) * u *  zp + 0.4e1 * s * s3 * s4 * t * t * u * u *  z - 0.4e1 * s * s3 * s4 * t * t * u * u *  zp + 0.8e1 * s * s3 * s4 * t * pow(u, 0.3e1) *  z - 0.8e1 * s * s3 * s4 * t * pow(u, 0.3e1) *  zp + 0.2e1 * s * s3 * s4 * pow(u, 0.4e1) *  z - 0.2e1 * s * s3 * s4 * pow(u, 0.4e1) *  zp + 0.6e1 * s * s3 * pow(t, 0.4e1) * u *  z - 0.6e1 * s * s3 * pow(t, 0.4e1) * u *  zp + 0.18e2 * s * s3 * pow(t, 0.3e1) * u * u *  z - 0.18e2 * s * s3 * pow(t, 0.3e1) * u * u *  zp + 0.18e2 * s * s3 * t * t * pow(u, 0.3e1) *  z - 0.18e2 * s * s3 * t * t * pow(u, 0.3e1) *  zp + 0.6e1 * s * s3 * t * pow(u, 0.4e1) *  z - 0.6e1 * s * s3 * t * pow(u, 0.4e1) *  zp - 0.4e1 * s * s4 * s4 * pow(t, 0.3e1) * u *  z + 0.4e1 * s * s4 * s4 * pow(t, 0.3e1) * u *  zp - 0.8e1 * s * s4 * s4 * t * t * u * u *  z + 0.8e1 * s * s4 * s4 * t * t * u * u *  zp - 0.4e1 * s * s4 * s4 * t * pow(u, 0.3e1) *  z + 0.4e1 * s * s4 * s4 * t * pow(u, 0.3e1) *  zp + 0.6e1 * s * s4 * pow(t, 0.4e1) * u *  z - 0.6e1 * s * s4 * pow(t, 0.4e1) * u *  zp + 0.18e2 * s * s4 * pow(t, 0.3e1) * u * u *  z - 0.18e2 * s * s4 * pow(t, 0.3e1) * u * u *  zp + 0.18e2 * s * s4 * t * t * pow(u, 0.3e1) *  z - 0.18e2 * s * s4 * t * t * pow(u, 0.3e1) *  zp + 0.6e1 * s * s4 * t * pow(u, 0.4e1) *  z - 0.6e1 * s * s4 * t * pow(u, 0.4e1) *  zp - 0.3e1 * s * pow(t, 0.5e1) * u *  z + 0.3e1 * s * pow(t, 0.5e1) * u *  zp - 0.10e2 * s * pow(t, 0.4e1) * u * u *  z + 0.10e2 * s * pow(t, 0.4e1) * u * u *  zp - 0.14e2 * s * pow(t, 0.3e1) * pow(u, 0.3e1) *  z + 0.14e2 * s * pow(t, 0.3e1) * pow(u, 0.3e1) *  zp - 0.10e2 * s * t * t * pow(u, 0.4e1) *  z + 0.10e2 * s * t * t * pow(u, 0.4e1) *  zp - 0.3e1 * s * t * pow(u, 0.5e1) *  z + 0.3e1 * s * t * pow(u, 0.5e1) *  zp - 0.12e2 * pow(s3, 0.3e1) * s4 * t * t * u + 0.12e2 * pow(s3, 0.3e1) * s4 * t * u * u - 0.32e2 * s3 * s3 * s4 * s4 * t * t * u + 0.32e2 * s3 * s3 * s4 * s4 * t * u * u + 0.28e2 * s3 * s3 * s4 * pow(t, 0.3e1) * u - 0.28e2 * s3 * s3 * s4 * t * pow(u, 0.3e1) - 0.12e2 * s3 * pow(s4, 0.3e1) * t * t * u + 0.12e2 * s3 * pow(s4, 0.3e1) * t * u * u + 0.28e2 * s3 * s4 * s4 * pow(t, 0.3e1) * u - 0.28e2 * s3 * s4 * s4 * t * pow(u, 0.3e1) - 0.10e2 * s3 * s4 * pow(t, 0.4e1) * u - 0.10e2 * s3 * s4 * pow(t, 0.3e1) * u * u + 0.10e2 * s3 * s4 * t * t * pow(u, 0.3e1) + 0.10e2 * s3 * s4 * t * pow(u, 0.4e1) - 0.2e1 * s3 * pow(t, 0.5e1) * u - 0.4e1 * s3 * pow(t, 0.4e1) * u * u + 0.4e1 * s3 * t * t * pow(u, 0.4e1) + 0.2e1 * s3 * t * pow(u, 0.5e1) - 0.2e1 * s4 * pow(t, 0.5e1) * u - 0.4e1 * s4 * pow(t, 0.4e1) * u * u + 0.4e1 * s4 * t * t * pow(u, 0.4e1) + 0.2e1 * s4 * t * pow(u, 0.5e1) + pow(t, 0.6e1) * u + 0.3e1 * pow(t, 0.5e1) * u * u + 0.2e1 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.2e1 * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.3e1 * t * t * pow(u, 0.5e1) - t * pow(u, 0.6e1)) /  (z - zp) / s * pow(t, -0.2e1) / (0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u) * pow(u, -0.2e1) * log( zp) * log( (1.0-zp)) + 0.2e1 * s3 * (0.96e2 * pow(s3, 0.7e1) * s4 * s4 * t * u - 0.48e2 * pow(s3, 0.6e1) * pow(s4, 0.3e1) * t * t + 0.192e3 * pow(s3, 0.6e1) * pow(s4, 0.3e1) * t * u - 0.48e2 * pow(s3, 0.6e1) * pow(s4, 0.3e1) * u * u - 0.312e3 * pow(s3, 0.6e1) * s4 * s4 * t * t * u - 0.312e3 * pow(s3, 0.6e1) * s4 * s4 * t * u * u - 0.36e2 * pow(s3, 0.6e1) * s4 * pow(t, 0.3e1) * u - 0.104e3 * pow(s3, 0.6e1) * s4 * t * t * u * u - 0.36e2 * pow(s3, 0.6e1) * s4 * t * pow(u, 0.3e1) + 0.96e2 * pow(s3, 0.5e1) * pow(s4, 0.4e1) * t * u + 0.96e2 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) - 0.360e3 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * t * t * u - 0.360e3 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * t * u * u + 0.96e2 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * pow(u, 0.3e1) + 0.24e2 * pow(s3, 0.5e1) * s4 * s4 * pow(t, 0.4e1) + 0.364e3 * pow(s3, 0.5e1) * s4 * s4 * pow(t, 0.3e1) * u + 0.648e3 * pow(s3, 0.5e1) * s4 * s4 * t * t * u * u + 0.364e3 * pow(s3, 0.5e1) * s4 * s4 * t * pow(u, 0.3e1) + 0.24e2 * pow(s3, 0.5e1) * s4 * s4 * pow(u, 0.4e1) + 0.108e3 * pow(s3, 0.5e1) * s4 * pow(t, 0.4e1) * u + 0.460e3 * pow(s3, 0.5e1) * s4 * pow(t, 0.3e1) * u * u + 0.460e3 * pow(s3, 0.5e1) * s4 * t * t * pow(u, 0.3e1) + 0.108e3 * pow(s3, 0.5e1) * s4 * t * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.5e1) * pow(t, 0.5e1) * u + 0.20e2 * pow(s3, 0.5e1) * pow(t, 0.4e1) * u * u + 0.28e2 * pow(s3, 0.5e1) * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.20e2 * pow(s3, 0.5e1) * t * t * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.5e1) * t * pow(u, 0.5e1) - 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * t * u - 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * u * u - 0.48e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) + 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u + 0.608e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * t * u * u + 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * pow(u, 0.3e1) - 0.48e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(u, 0.4e1) - 0.48e2 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.5e1) - 0.196e3 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.4e1) * u - 0.276e3 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.3e1) * u * u - 0.276e3 * pow(s3, 0.4e1) * s4 * s4 * t * t * pow(u, 0.3e1) - 0.196e3 * pow(s3, 0.4e1) * s4 * s4 * t * pow(u, 0.4e1) - 0.48e2 * pow(s3, 0.4e1) * s4 * s4 * pow(u, 0.5e1) - 0.3e1 * pow(s3, 0.4e1) * s4 * pow(t, 0.6e1) - 0.130e3 * pow(s3, 0.4e1) * s4 * pow(t, 0.5e1) * u - 0.681e3 * pow(s3, 0.4e1) * s4 * pow(t, 0.4e1) * u * u - 0.1172e4 * pow(s3, 0.4e1) * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.681e3 * pow(s3, 0.4e1) * s4 * t * t * pow(u, 0.4e1) - 0.130e3 * pow(s3, 0.4e1) * s4 * t * pow(u, 0.5e1) - 0.3e1 * pow(s3, 0.4e1) * s4 * pow(u, 0.6e1) - 0.18e2 * pow(s3, 0.4e1) * pow(t, 0.6e1) * u - 0.82e2 * pow(s3, 0.4e1) * pow(t, 0.5e1) * u * u - 0.156e3 * pow(s3, 0.4e1) * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.156e3 * pow(s3, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.82e2 * pow(s3, 0.4e1) * t * t * pow(u, 0.5e1) - 0.18e2 * pow(s3, 0.4e1) * t * pow(u, 0.6e1) + 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.3e1) * u + 0.128e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * t * u * u + 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * pow(u, 0.3e1) + 0.56e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) * u - 0.168e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u * u - 0.168e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t * pow(u, 0.3e1) + 0.56e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * pow(u, 0.4e1) + 0.24e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.6e1) + 0.58e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.5e1) * u - 0.88e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.4e1) * u * u - 0.372e3 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.88e2 * pow(s3, 0.3e1) * s4 * s4 * t * t * pow(u, 0.4e1) + 0.58e2 * pow(s3, 0.3e1) * s4 * s4 * t * pow(u, 0.5e1) + 0.24e2 * pow(s3, 0.3e1) * s4 * s4 * pow(u, 0.6e1) + 0.6e1 * pow(s3, 0.3e1) * s4 * pow(t, 0.7e1) + 0.82e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.6e1) * u + 0.406e3 * pow(s3, 0.3e1) * s4 * pow(t, 0.5e1) * u * u + 0.1058e4 * pow(s3, 0.3e1) * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.1058e4 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.406e3 * pow(s3, 0.3e1) * s4 * t * t * pow(u, 0.5e1) + 0.82e2 * pow(s3, 0.3e1) * s4 * t * pow(u, 0.6e1) + 0.6e1 * pow(s3, 0.3e1) * s4 * pow(u, 0.7e1) + 0.21e2 * pow(s3, 0.3e1) * pow(t, 0.7e1) * u + 0.124e3 * pow(s3, 0.3e1) * pow(t, 0.6e1) * u * u + 0.299e3 * pow(s3, 0.3e1) * pow(t, 0.5e1) * pow(u, 0.3e1) + 0.392e3 * pow(s3, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.4e1) + 0.299e3 * pow(s3, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.124e3 * pow(s3, 0.3e1) * t * t * pow(u, 0.6e1) + 0.21e2 * pow(s3, 0.3e1) * t * pow(u, 0.7e1) - 0.32e2 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * u * u - 0.32e2 * s3 * s3 * pow(s4, 0.4e1) * t * t * pow(u, 0.3e1) - 0.24e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u - 0.80e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * u * u - 0.16e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.80e2 * s3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.4e1) - 0.24e2 * s3 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.5e1) - 0.12e2 * s3 * s3 * s4 * s4 * pow(t, 0.6e1) * u + 0.28e2 * s3 * s3 * s4 * s4 * pow(t, 0.5e1) * u * u + 0.328e3 * s3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.328e3 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.28e2 * s3 * s3 * s4 * s4 * t * t * pow(u, 0.5e1) - 0.12e2 * s3 * s3 * s4 * s4 * t * pow(u, 0.6e1) - 0.3e1 * s3 * s3 * s4 * pow(t, 0.8e1) - 0.24e2 * s3 * s3 * s4 * pow(t, 0.7e1) * u - 0.76e2 * s3 * s3 * s4 * pow(t, 0.6e1) * u * u - 0.308e3 * s3 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.538e3 * s3 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.308e3 * s3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.76e2 * s3 * s3 * s4 * t * t * pow(u, 0.6e1) - 0.24e2 * s3 * s3 * s4 * t * pow(u, 0.7e1) - 0.3e1 * s3 * s3 * s4 * pow(u, 0.8e1) - 0.12e2 * s3 * s3 * pow(t, 0.8e1) * u - 0.86e2 * s3 * s3 * pow(t, 0.7e1) * u * u - 0.254e3 * s3 * s3 * pow(t, 0.6e1) * pow(u, 0.3e1) - 0.416e3 * s3 * s3 * pow(t, 0.5e1) * pow(u, 0.4e1) - 0.416e3 * s3 * s3 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.254e3 * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.86e2 * s3 * s3 * t * t * pow(u, 0.7e1) - 0.12e2 * s3 * s3 * t * pow(u, 0.8e1) + 0.16e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u * u + 0.40e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.40e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.16e2 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.5e1) + 0.3e1 * s3 * s4 * s4 * pow(t, 0.7e1) * u + 0.20e2 * s3 * s4 * s4 * pow(t, 0.6e1) * u * u - 0.15e2 * s3 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.128e3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.15e2 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.20e2 * s3 * s4 * s4 * t * t * pow(u, 0.6e1) + 0.3e1 * s3 * s4 * s4 * t * pow(u, 0.7e1) - 0.8e1 * s3 * s4 * pow(t, 0.7e1) * u * u - 0.28e2 * s3 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.28e2 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.28e2 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.28e2 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.8e1 * s3 * s4 * t * t * pow(u, 0.7e1) + 0.3e1 * s3 * pow(t, 0.9e1) * u + 0.26e2 * s3 * pow(t, 0.8e1) * u * u + 0.96e2 * s3 * pow(t, 0.7e1) * pow(u, 0.3e1) + 0.194e3 * s3 * pow(t, 0.6e1) * pow(u, 0.4e1) + 0.242e3 * s3 * pow(t, 0.5e1) * pow(u, 0.5e1) + 0.194e3 * s3 * pow(t, 0.4e1) * pow(u, 0.6e1) + 0.96e2 * s3 * pow(t, 0.3e1) * pow(u, 0.7e1) + 0.26e2 * s3 * t * t * pow(u, 0.8e1) + 0.3e1 * s3 * t * pow(u, 0.9e1) - 0.2e1 * s4 * s4 * pow(t, 0.7e1) * u * u - 0.14e2 * s4 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) - 0.8e1 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) - 0.8e1 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.14e2 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.2e1 * s4 * s4 * t * t * pow(u, 0.7e1) + s4 * pow(t, 0.8e1) * u * u + 0.14e2 * s4 * pow(t, 0.7e1) * pow(u, 0.3e1) + 0.27e2 * s4 * pow(t, 0.6e1) * pow(u, 0.4e1) + 0.28e2 * s4 * pow(t, 0.5e1) * pow(u, 0.5e1) + 0.27e2 * s4 * pow(t, 0.4e1) * pow(u, 0.6e1) + 0.14e2 * s4 * pow(t, 0.3e1) * pow(u, 0.7e1) + s4 * t * t * pow(u, 0.8e1) - 0.2e1 * pow(t, 0.9e1) * u * u - 0.12e2 * pow(t, 0.8e1) * pow(u, 0.3e1) - 0.32e2 * pow(t, 0.7e1) * pow(u, 0.4e1) - 0.50e2 * pow(t, 0.6e1) * pow(u, 0.5e1) - 0.50e2 * pow(t, 0.5e1) * pow(u, 0.6e1) - 0.32e2 * pow(t, 0.4e1) * pow(u, 0.7e1) - 0.12e2 * pow(t, 0.3e1) * pow(u, 0.8e1) - 0.2e1 * t * t * pow(u, 0.9e1)) * pow(-u + s3, -0.2e1) * pow(s3 - t, -0.2e1) * pow(u, -0.2e1) * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) * pow(t, -0.2e1) * log( zp) + 0.2e1 * s3 * (0.96e2 * pow(s3, 0.7e1) * s4 * s4 * t * u - 0.48e2 * pow(s3, 0.6e1) * pow(s4, 0.3e1) * t * t + 0.192e3 * pow(s3, 0.6e1) * pow(s4, 0.3e1) * t * u - 0.48e2 * pow(s3, 0.6e1) * pow(s4, 0.3e1) * u * u - 0.312e3 * pow(s3, 0.6e1) * s4 * s4 * t * t * u - 0.312e3 * pow(s3, 0.6e1) * s4 * s4 * t * u * u - 0.36e2 * pow(s3, 0.6e1) * s4 * pow(t, 0.3e1) * u - 0.104e3 * pow(s3, 0.6e1) * s4 * t * t * u * u - 0.36e2 * pow(s3, 0.6e1) * s4 * t * pow(u, 0.3e1) + 0.96e2 * pow(s3, 0.5e1) * pow(s4, 0.4e1) * t * u + 0.96e2 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) - 0.360e3 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * t * t * u - 0.360e3 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * t * u * u + 0.96e2 * pow(s3, 0.5e1) * pow(s4, 0.3e1) * pow(u, 0.3e1) + 0.24e2 * pow(s3, 0.5e1) * s4 * s4 * pow(t, 0.4e1) + 0.364e3 * pow(s3, 0.5e1) * s4 * s4 * pow(t, 0.3e1) * u + 0.648e3 * pow(s3, 0.5e1) * s4 * s4 * t * t * u * u + 0.364e3 * pow(s3, 0.5e1) * s4 * s4 * t * pow(u, 0.3e1) + 0.24e2 * pow(s3, 0.5e1) * s4 * s4 * pow(u, 0.4e1) + 0.108e3 * pow(s3, 0.5e1) * s4 * pow(t, 0.4e1) * u + 0.460e3 * pow(s3, 0.5e1) * s4 * pow(t, 0.3e1) * u * u + 0.460e3 * pow(s3, 0.5e1) * s4 * t * t * pow(u, 0.3e1) + 0.108e3 * pow(s3, 0.5e1) * s4 * t * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.5e1) * pow(t, 0.5e1) * u + 0.20e2 * pow(s3, 0.5e1) * pow(t, 0.4e1) * u * u + 0.28e2 * pow(s3, 0.5e1) * pow(t, 0.3e1) * pow(u, 0.3e1) + 0.20e2 * pow(s3, 0.5e1) * t * t * pow(u, 0.4e1) + 0.6e1 * pow(s3, 0.5e1) * t * pow(u, 0.5e1) - 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * t * u - 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.4e1) * t * u * u - 0.48e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) + 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u + 0.608e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * t * u * u + 0.128e3 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * t * pow(u, 0.3e1) - 0.48e2 * pow(s3, 0.4e1) * pow(s4, 0.3e1) * pow(u, 0.4e1) - 0.48e2 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.5e1) - 0.196e3 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.4e1) * u - 0.276e3 * pow(s3, 0.4e1) * s4 * s4 * pow(t, 0.3e1) * u * u - 0.276e3 * pow(s3, 0.4e1) * s4 * s4 * t * t * pow(u, 0.3e1) - 0.196e3 * pow(s3, 0.4e1) * s4 * s4 * t * pow(u, 0.4e1) - 0.48e2 * pow(s3, 0.4e1) * s4 * s4 * pow(u, 0.5e1) - 0.3e1 * pow(s3, 0.4e1) * s4 * pow(t, 0.6e1) - 0.130e3 * pow(s3, 0.4e1) * s4 * pow(t, 0.5e1) * u - 0.681e3 * pow(s3, 0.4e1) * s4 * pow(t, 0.4e1) * u * u - 0.1172e4 * pow(s3, 0.4e1) * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.681e3 * pow(s3, 0.4e1) * s4 * t * t * pow(u, 0.4e1) - 0.130e3 * pow(s3, 0.4e1) * s4 * t * pow(u, 0.5e1) - 0.3e1 * pow(s3, 0.4e1) * s4 * pow(u, 0.6e1) - 0.18e2 * pow(s3, 0.4e1) * pow(t, 0.6e1) * u - 0.82e2 * pow(s3, 0.4e1) * pow(t, 0.5e1) * u * u - 0.156e3 * pow(s3, 0.4e1) * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.156e3 * pow(s3, 0.4e1) * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.82e2 * pow(s3, 0.4e1) * t * t * pow(u, 0.5e1) - 0.18e2 * pow(s3, 0.4e1) * t * pow(u, 0.6e1) + 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * pow(t, 0.3e1) * u + 0.128e3 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * t * u * u + 0.48e2 * pow(s3, 0.3e1) * pow(s4, 0.4e1) * t * pow(u, 0.3e1) + 0.56e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.4e1) * u - 0.168e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * pow(t, 0.3e1) * u * u - 0.168e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t * pow(u, 0.3e1) + 0.56e2 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * pow(u, 0.4e1) + 0.24e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.6e1) + 0.58e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.5e1) * u - 0.88e2 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.4e1) * u * u - 0.372e3 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.88e2 * pow(s3, 0.3e1) * s4 * s4 * t * t * pow(u, 0.4e1) + 0.58e2 * pow(s3, 0.3e1) * s4 * s4 * t * pow(u, 0.5e1) + 0.24e2 * pow(s3, 0.3e1) * s4 * s4 * pow(u, 0.6e1) + 0.6e1 * pow(s3, 0.3e1) * s4 * pow(t, 0.7e1) + 0.82e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.6e1) * u + 0.406e3 * pow(s3, 0.3e1) * s4 * pow(t, 0.5e1) * u * u + 0.1058e4 * pow(s3, 0.3e1) * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.1058e4 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.406e3 * pow(s3, 0.3e1) * s4 * t * t * pow(u, 0.5e1) + 0.82e2 * pow(s3, 0.3e1) * s4 * t * pow(u, 0.6e1) + 0.6e1 * pow(s3, 0.3e1) * s4 * pow(u, 0.7e1) + 0.21e2 * pow(s3, 0.3e1) * pow(t, 0.7e1) * u + 0.124e3 * pow(s3, 0.3e1) * pow(t, 0.6e1) * u * u + 0.299e3 * pow(s3, 0.3e1) * pow(t, 0.5e1) * pow(u, 0.3e1) + 0.392e3 * pow(s3, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.4e1) + 0.299e3 * pow(s3, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.124e3 * pow(s3, 0.3e1) * t * t * pow(u, 0.6e1) + 0.21e2 * pow(s3, 0.3e1) * t * pow(u, 0.7e1) - 0.32e2 * s3 * s3 * pow(s4, 0.4e1) * pow(t, 0.3e1) * u * u - 0.32e2 * s3 * s3 * pow(s4, 0.4e1) * t * t * pow(u, 0.3e1) - 0.24e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u - 0.80e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * u * u - 0.16e2 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.80e2 * s3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.4e1) - 0.24e2 * s3 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.5e1) - 0.12e2 * s3 * s3 * s4 * s4 * pow(t, 0.6e1) * u + 0.28e2 * s3 * s3 * s4 * s4 * pow(t, 0.5e1) * u * u + 0.328e3 * s3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.328e3 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.28e2 * s3 * s3 * s4 * s4 * t * t * pow(u, 0.5e1) - 0.12e2 * s3 * s3 * s4 * s4 * t * pow(u, 0.6e1) - 0.3e1 * s3 * s3 * s4 * pow(t, 0.8e1) - 0.24e2 * s3 * s3 * s4 * pow(t, 0.7e1) * u - 0.76e2 * s3 * s3 * s4 * pow(t, 0.6e1) * u * u - 0.308e3 * s3 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.538e3 * s3 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.308e3 * s3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) - 0.76e2 * s3 * s3 * s4 * t * t * pow(u, 0.6e1) - 0.24e2 * s3 * s3 * s4 * t * pow(u, 0.7e1) - 0.3e1 * s3 * s3 * s4 * pow(u, 0.8e1) - 0.12e2 * s3 * s3 * pow(t, 0.8e1) * u - 0.86e2 * s3 * s3 * pow(t, 0.7e1) * u * u - 0.254e3 * s3 * s3 * pow(t, 0.6e1) * pow(u, 0.3e1) - 0.416e3 * s3 * s3 * pow(t, 0.5e1) * pow(u, 0.4e1) - 0.416e3 * s3 * s3 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.254e3 * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.86e2 * s3 * s3 * t * t * pow(u, 0.7e1) - 0.12e2 * s3 * s3 * t * pow(u, 0.8e1) + 0.16e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.5e1) * u * u + 0.40e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.40e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.16e2 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.5e1) + 0.3e1 * s3 * s4 * s4 * pow(t, 0.7e1) * u + 0.20e2 * s3 * s4 * s4 * pow(t, 0.6e1) * u * u - 0.15e2 * s3 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) - 0.128e3 * s3 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) - 0.15e2 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.20e2 * s3 * s4 * s4 * t * t * pow(u, 0.6e1) + 0.3e1 * s3 * s4 * s4 * t * pow(u, 0.7e1) - 0.8e1 * s3 * s4 * pow(t, 0.7e1) * u * u - 0.28e2 * s3 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) + 0.28e2 * s3 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) + 0.28e2 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.28e2 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.8e1 * s3 * s4 * t * t * pow(u, 0.7e1) + 0.3e1 * s3 * pow(t, 0.9e1) * u + 0.26e2 * s3 * pow(t, 0.8e1) * u * u + 0.96e2 * s3 * pow(t, 0.7e1) * pow(u, 0.3e1) + 0.194e3 * s3 * pow(t, 0.6e1) * pow(u, 0.4e1) + 0.242e3 * s3 * pow(t, 0.5e1) * pow(u, 0.5e1) + 0.194e3 * s3 * pow(t, 0.4e1) * pow(u, 0.6e1) + 0.96e2 * s3 * pow(t, 0.3e1) * pow(u, 0.7e1) + 0.26e2 * s3 * t * t * pow(u, 0.8e1) + 0.3e1 * s3 * t * pow(u, 0.9e1) - 0.2e1 * s4 * s4 * pow(t, 0.7e1) * u * u - 0.14e2 * s4 * s4 * pow(t, 0.6e1) * pow(u, 0.3e1) - 0.8e1 * s4 * s4 * pow(t, 0.5e1) * pow(u, 0.4e1) - 0.8e1 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.14e2 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.2e1 * s4 * s4 * t * t * pow(u, 0.7e1) + s4 * pow(t, 0.8e1) * u * u + 0.14e2 * s4 * pow(t, 0.7e1) * pow(u, 0.3e1) + 0.27e2 * s4 * pow(t, 0.6e1) * pow(u, 0.4e1) + 0.28e2 * s4 * pow(t, 0.5e1) * pow(u, 0.5e1) + 0.27e2 * s4 * pow(t, 0.4e1) * pow(u, 0.6e1) + 0.14e2 * s4 * pow(t, 0.3e1) * pow(u, 0.7e1) + s4 * t * t * pow(u, 0.8e1) - 0.2e1 * pow(t, 0.9e1) * u * u - 0.12e2 * pow(t, 0.8e1) * pow(u, 0.3e1) - 0.32e2 * pow(t, 0.7e1) * pow(u, 0.4e1) - 0.50e2 * pow(t, 0.6e1) * pow(u, 0.5e1) - 0.50e2 * pow(t, 0.5e1) * pow(u, 0.6e1) - 0.32e2 * pow(t, 0.4e1) * pow(u, 0.7e1) - 0.12e2 * pow(t, 0.3e1) * pow(u, 0.8e1) - 0.2e1 * t * t * pow(u, 0.9e1)) * pow(-u + s3, -0.2e1) * pow(s3 - t, -0.2e1) * pow(u, -0.2e1) * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) * pow(t, -0.2e1) * log( z) - 0.2e1 * (0.64e2 * s * pow(s3, 0.4e1) * s4 * s4 * t * u *  z - 0.64e2 * s * pow(s3, 0.4e1) * s4 * s4 * t * u *  zp - 0.32e2 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t *  z + 0.32e2 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t *  zp + 0.128e3 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * u *  z - 0.128e3 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * u *  zp - 0.32e2 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u * u *  z + 0.32e2 * s * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u * u *  zp - 0.96e2 * s * pow(s3, 0.3e1) * s4 * s4 * t * t * u *  z + 0.96e2 * s * pow(s3, 0.3e1) * s4 * s4 * t * t * u *  zp - 0.96e2 * s * pow(s3, 0.3e1) * s4 * s4 * t * u * u *  z + 0.96e2 * s * pow(s3, 0.3e1) * s4 * s4 * t * u * u *  zp - 0.32e2 * s * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * u *  z + 0.32e2 * s * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * u *  zp - 0.64e2 * s * pow(s3, 0.3e1) * s4 * t * t * u * u *  z + 0.64e2 * s * pow(s3, 0.3e1) * s4 * t * t * u * u *  zp - 0.32e2 * s * pow(s3, 0.3e1) * s4 * t * pow(u, 0.3e1) *  z + 0.32e2 * s * pow(s3, 0.3e1) * s4 * t * pow(u, 0.3e1) *  zp + 0.64e2 * s * s3 * s3 * pow(s4, 0.4e1) * t * u *  z - 0.64e2 * s * s3 * s3 * pow(s4, 0.4e1) * t * u *  zp - 0.96e2 * s * s3 * s3 * pow(s4, 0.3e1) * t * t * u *  z + 0.96e2 * s * s3 * s3 * pow(s4, 0.3e1) * t * t * u *  zp - 0.96e2 * s * s3 * s3 * pow(s4, 0.3e1) * t * u * u *  z + 0.96e2 * s * s3 * s3 * pow(s4, 0.3e1) * t * u * u *  zp + 0.16e2 * s * s3 * s3 * s4 * s4 * pow(t, 0.4e1) *  z - 0.16e2 * s * s3 * s3 * s4 * s4 * pow(t, 0.4e1) *  zp + 0.16e2 * s * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * u *  z - 0.16e2 * s * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * u *  zp - 0.32e2 * s * s3 * s3 * s4 * s4 * t * t * u * u *  z + 0.32e2 * s * s3 * s3 * s4 * s4 * t * t * u * u *  zp + 0.16e2 * s * s3 * s3 * s4 * s4 * t * pow(u, 0.3e1) *  z - 0.16e2 * s * s3 * s3 * s4 * s4 * t * pow(u, 0.3e1) *  zp + 0.16e2 * s * s3 * s3 * s4 * s4 * pow(u, 0.4e1) *  z - 0.16e2 * s * s3 * s3 * s4 * s4 * pow(u, 0.4e1) *  zp + 0.48e2 * s * s3 * s3 * s4 * pow(t, 0.4e1) * u *  z - 0.48e2 * s * s3 * s3 * s4 * pow(t, 0.4e1) * u *  zp + 0.144e3 * s * s3 * s3 * s4 * pow(t, 0.3e1) * u * u *  z - 0.144e3 * s * s3 * s3 * s4 * pow(t, 0.3e1) * u * u *  zp + 0.144e3 * s * s3 * s3 * s4 * t * t * pow(u, 0.3e1) *  z - 0.144e3 * s * s3 * s3 * s4 * t * t * pow(u, 0.3e1) *  zp + 0.48e2 * s * s3 * s3 * s4 * t * pow(u, 0.4e1) *  z - 0.48e2 * s * s3 * s3 * s4 * t * pow(u, 0.4e1) *  zp + 0.4e1 * s * s3 * s3 * pow(t, 0.5e1) * u *  z - 0.4e1 * s * s3 * s3 * pow(t, 0.5e1) * u *  zp + 0.16e2 * s * s3 * s3 * pow(t, 0.4e1) * u * u *  z - 0.16e2 * s * s3 * s3 * pow(t, 0.4e1) * u * u *  zp + 0.24e2 * s * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.3e1) *  z - 0.24e2 * s * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.3e1) *  zp + 0.16e2 * s * s3 * s3 * t * t * pow(u, 0.4e1) *  z - 0.16e2 * s * s3 * s3 * t * t * pow(u, 0.4e1) *  zp + 0.4e1 * s * s3 * s3 * t * pow(u, 0.5e1) *  z - 0.4e1 * s * s3 * s3 * t * pow(u, 0.5e1) *  zp - 0.32e2 * s * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * u *  z + 0.32e2 * s * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * u *  zp - 0.64e2 * s * s3 * pow(s4, 0.3e1) * t * t * u * u *  z + 0.64e2 * s * s3 * pow(s4, 0.3e1) * t * t * u * u *  zp - 0.32e2 * s * s3 * pow(s4, 0.3e1) * t * pow(u, 0.3e1) *  z + 0.32e2 * s * s3 * pow(s4, 0.3e1) * t * pow(u, 0.3e1) *  zp + 0.48e2 * s * s3 * s4 * s4 * pow(t, 0.4e1) * u *  z - 0.48e2 * s * s3 * s4 * s4 * pow(t, 0.4e1) * u *  zp + 0.144e3 * s * s3 * s4 * s4 * pow(t, 0.3e1) * u * u *  z - 0.144e3 * s * s3 * s4 * s4 * pow(t, 0.3e1) * u * u *  zp + 0.144e3 * s * s3 * s4 * s4 * t * t * pow(u, 0.3e1) *  z - 0.144e3 * s * s3 * s4 * s4 * t * t * pow(u, 0.3e1) *  zp + 0.48e2 * s * s3 * s4 * s4 * t * pow(u, 0.4e1) *  z - 0.48e2 * s * s3 * s4 * s4 * t * pow(u, 0.4e1) *  zp - 0.2e1 * s * s3 * s4 * pow(t, 0.6e1) *  z + 0.2e1 * s * s3 * s4 * pow(t, 0.6e1) *  zp - 0.24e2 * s * s3 * s4 * pow(t, 0.5e1) * u *  z + 0.24e2 * s * s3 * s4 * pow(t, 0.5e1) * u *  zp - 0.62e2 * s * s3 * s4 * pow(t, 0.4e1) * u * u *  z + 0.62e2 * s * s3 * s4 * pow(t, 0.4e1) * u * u *  zp - 0.80e2 * s * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) *  z + 0.80e2 * s * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) *  zp - 0.62e2 * s * s3 * s4 * t * t * pow(u, 0.4e1) *  z + 0.62e2 * s * s3 * s4 * t * t * pow(u, 0.4e1) *  zp - 0.24e2 * s * s3 * s4 * t * pow(u, 0.5e1) *  z + 0.24e2 * s * s3 * s4 * t * pow(u, 0.5e1) *  zp - 0.2e1 * s * s3 * s4 * pow(u, 0.6e1) *  z + 0.2e1 * s * s3 * s4 * pow(u, 0.6e1) *  zp - 0.6e1 * s * s3 * pow(t, 0.6e1) * u *  z + 0.6e1 * s * s3 * pow(t, 0.6e1) * u *  zp - 0.30e2 * s * s3 * pow(t, 0.5e1) * u * u *  z + 0.30e2 * s * s3 * pow(t, 0.5e1) * u * u *  zp - 0.60e2 * s * s3 * pow(t, 0.4e1) * pow(u, 0.3e1) *  z + 0.60e2 * s * s3 * pow(t, 0.4e1) * pow(u, 0.3e1) *  zp - 0.60e2 * s * s3 * pow(t, 0.3e1) * pow(u, 0.4e1) *  z + 0.60e2 * s * s3 * pow(t, 0.3e1) * pow(u, 0.4e1) *  zp - 0.30e2 * s * s3 * t * t * pow(u, 0.5e1) *  z + 0.30e2 * s * s3 * t * t * pow(u, 0.5e1) *  zp - 0.6e1 * s * s3 * t * pow(u, 0.6e1) *  z + 0.6e1 * s * s3 * t * pow(u, 0.6e1) *  zp + 0.4e1 * s * s4 * s4 * pow(t, 0.5e1) * u *  z - 0.4e1 * s * s4 * s4 * pow(t, 0.5e1) * u *  zp + 0.16e2 * s * s4 * s4 * pow(t, 0.4e1) * u * u *  z - 0.16e2 * s * s4 * s4 * pow(t, 0.4e1) * u * u *  zp + 0.24e2 * s * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) *  z - 0.24e2 * s * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) *  zp + 0.16e2 * s * s4 * s4 * t * t * pow(u, 0.4e1) *  z - 0.16e2 * s * s4 * s4 * t * t * pow(u, 0.4e1) *  zp + 0.4e1 * s * s4 * s4 * t * pow(u, 0.5e1) *  z - 0.4e1 * s * s4 * s4 * t * pow(u, 0.5e1) *  zp - 0.6e1 * s * s4 * pow(t, 0.6e1) * u *  z + 0.6e1 * s * s4 * pow(t, 0.6e1) * u *  zp - 0.30e2 * s * s4 * pow(t, 0.5e1) * u * u *  z + 0.30e2 * s * s4 * pow(t, 0.5e1) * u * u *  zp - 0.60e2 * s * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) *  z + 0.60e2 * s * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) *  zp - 0.60e2 * s * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) *  z + 0.60e2 * s * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) *  zp - 0.30e2 * s * s4 * t * t * pow(u, 0.5e1) *  z + 0.30e2 * s * s4 * t * t * pow(u, 0.5e1) *  zp - 0.6e1 * s * s4 * t * pow(u, 0.6e1) *  z + 0.6e1 * s * s4 * t * pow(u, 0.6e1) *  zp + 0.3e1 * s * pow(t, 0.7e1) * u *  z - 0.3e1 * s * pow(t, 0.7e1) * u *  zp + 0.16e2 * s * pow(t, 0.6e1) * u * u *  z - 0.16e2 * s * pow(t, 0.6e1) * u * u *  zp + 0.37e2 * s * pow(t, 0.5e1) * pow(u, 0.3e1) *  z - 0.37e2 * s * pow(t, 0.5e1) * pow(u, 0.3e1) *  zp + 0.48e2 * s * pow(t, 0.4e1) * pow(u, 0.4e1) *  z - 0.48e2 * s * pow(t, 0.4e1) * pow(u, 0.4e1) *  zp + 0.37e2 * s * pow(t, 0.3e1) * pow(u, 0.5e1) *  z - 0.37e2 * s * pow(t, 0.3e1) * pow(u, 0.5e1) *  zp + 0.16e2 * s * t * t * pow(u, 0.6e1) *  z - 0.16e2 * s * t * t * pow(u, 0.6e1) *  zp + 0.3e1 * s * t * pow(u, 0.7e1) *  z - 0.3e1 * s * t * pow(u, 0.7e1) *  zp - 0.72e2 * pow(s3, 0.4e1) * s4 * s4 * t * t * u - 0.72e2 * pow(s3, 0.4e1) * s4 * s4 * t * u * u - 0.112e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * t * u - 0.112e3 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t * u * u + 0.128e3 * pow(s3, 0.3e1) * s4 * s4 * pow(t, 0.3e1) * u + 0.192e3 * pow(s3, 0.3e1) * s4 * s4 * t * t * u * u + 0.128e3 * pow(s3, 0.3e1) * s4 * s4 * t * pow(u, 0.3e1) + 0.28e2 * pow(s3, 0.3e1) * s4 * pow(t, 0.4e1) * u + 0.108e3 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) * u * u + 0.108e3 * pow(s3, 0.3e1) * s4 * t * t * pow(u, 0.3e1) + 0.28e2 * pow(s3, 0.3e1) * s4 * t * pow(u, 0.4e1) - 0.72e2 * s3 * s3 * pow(s4, 0.4e1) * t * t * u - 0.72e2 * s3 * s3 * pow(s4, 0.4e1) * t * u * u + 0.128e3 * s3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * u + 0.192e3 * s3 * s3 * pow(s4, 0.3e1) * t * t * u * u + 0.128e3 * s3 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.3e1) - 0.32e2 * s3 * s3 * s4 * s4 * pow(t, 0.4e1) * u + 0.48e2 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) * u * u + 0.48e2 * s3 * s3 * s4 * s4 * t * t * pow(u, 0.3e1) - 0.32e2 * s3 * s3 * s4 * s4 * t * pow(u, 0.4e1) - 0.44e2 * s3 * s3 * s4 * pow(t, 0.5e1) * u - 0.208e3 * s3 * s3 * s4 * pow(t, 0.4e1) * u * u - 0.328e3 * s3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.208e3 * s3 * s3 * s4 * t * t * pow(u, 0.4e1) - 0.44e2 * s3 * s3 * s4 * t * pow(u, 0.5e1) - 0.4e1 * s3 * s3 * pow(t, 0.6e1) * u - 0.20e2 * s3 * s3 * pow(t, 0.5e1) * u * u - 0.40e2 * s3 * s3 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.40e2 * s3 * s3 * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.20e2 * s3 * s3 * t * t * pow(u, 0.5e1) - 0.4e1 * s3 * s3 * t * pow(u, 0.6e1) + 0.28e2 * s3 * pow(s4, 0.3e1) * pow(t, 0.4e1) * u + 0.108e3 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) * u * u + 0.108e3 * s3 * pow(s4, 0.3e1) * t * t * pow(u, 0.3e1) + 0.28e2 * s3 * pow(s4, 0.3e1) * t * pow(u, 0.4e1) - 0.44e2 * s3 * s4 * s4 * pow(t, 0.5e1) * u - 0.208e3 * s3 * s4 * s4 * pow(t, 0.4e1) * u * u - 0.328e3 * s3 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.3e1) - 0.208e3 * s3 * s4 * s4 * t * t * pow(u, 0.4e1) - 0.44e2 * s3 * s4 * s4 * t * pow(u, 0.5e1) + 0.18e2 * s3 * s4 * pow(t, 0.6e1) * u + 0.82e2 * s3 * s4 * pow(t, 0.5e1) * u * u + 0.156e3 * s3 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.156e3 * s3 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.82e2 * s3 * s4 * t * t * pow(u, 0.5e1) + 0.18e2 * s3 * s4 * t * pow(u, 0.6e1) + 0.6e1 * s3 * pow(t, 0.7e1) * u + 0.36e2 * s3 * pow(t, 0.6e1) * u * u + 0.90e2 * s3 * pow(t, 0.5e1) * pow(u, 0.3e1) + 0.120e3 * s3 * pow(t, 0.4e1) * pow(u, 0.4e1) + 0.90e2 * s3 * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.36e2 * s3 * t * t * pow(u, 0.6e1) + 0.6e1 * s3 * t * pow(u, 0.7e1) - 0.4e1 * s4 * s4 * pow(t, 0.6e1) * u - 0.20e2 * s4 * s4 * pow(t, 0.5e1) * u * u - 0.40e2 * s4 * s4 * pow(t, 0.4e1) * pow(u, 0.3e1) - 0.40e2 * s4 * s4 * pow(t, 0.3e1) * pow(u, 0.4e1) - 0.20e2 * s4 * s4 * t * t * pow(u, 0.5e1) - 0.4e1 * s4 * s4 * t * pow(u, 0.6e1) + 0.6e1 * s4 * pow(t, 0.7e1) * u + 0.36e2 * s4 * pow(t, 0.6e1) * u * u + 0.90e2 * s4 * pow(t, 0.5e1) * pow(u, 0.3e1) + 0.120e3 * s4 * pow(t, 0.4e1) * pow(u, 0.4e1) + 0.90e2 * s4 * pow(t, 0.3e1) * pow(u, 0.5e1) + 0.36e2 * s4 * t * t * pow(u, 0.6e1) + 0.6e1 * s4 * t * pow(u, 0.7e1) - 0.3e1 * pow(t, 0.8e1) * u - 0.19e2 * pow(t, 0.7e1) * u * u - 0.53e2 * pow(t, 0.6e1) * pow(u, 0.3e1) - 0.85e2 * pow(t, 0.5e1) * pow(u, 0.4e1) - 0.85e2 * pow(t, 0.4e1) * pow(u, 0.5e1) - 0.53e2 * pow(t, 0.3e1) * pow(u, 0.6e1) - 0.19e2 * t * t * pow(u, 0.7e1) - 0.3e1 * t * pow(u, 0.8e1)) * pow(u, -0.2e1) * pow(t, -0.2e1) * pow(0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u, -0.2e1) / s /  (z - zp) * log( z) * log( (1.0-zp)) - 0.2e1 * (0.16e2 * s * pow(s3, 0.3e1) * s4 * t * u *  z - 0.16e2 * s * pow(s3, 0.3e1) * s4 * t * u *  zp - 0.8e1 * s * s3 * s3 * s4 * s4 * t * t *  z + 0.8e1 * s * s3 * s3 * s4 * s4 * t * t *  zp + 0.32e2 * s * s3 * s3 * s4 * s4 * t * u *  z - 0.32e2 * s * s3 * s3 * s4 * s4 * t * u *  zp - 0.8e1 * s * s3 * s3 * s4 * s4 * u * u *  z + 0.8e1 * s * s3 * s3 * s4 * s4 * u * u *  zp - 0.24e2 * s * s3 * s3 * s4 * t * t * u *  z + 0.24e2 * s * s3 * s3 * s4 * t * t * u *  zp - 0.24e2 * s * s3 * s3 * s4 * t * u * u *  z + 0.24e2 * s * s3 * s3 * s4 * t * u * u *  zp - 0.4e1 * s * s3 * s3 * pow(t, 0.3e1) * u *  z + 0.4e1 * s * s3 * s3 * pow(t, 0.3e1) * u *  zp - 0.8e1 * s * s3 * s3 * t * t * u * u *  z + 0.8e1 * s * s3 * s3 * t * t * u * u *  zp - 0.4e1 * s * s3 * s3 * t * pow(u, 0.3e1) *  z + 0.4e1 * s * s3 * s3 * t * pow(u, 0.3e1) *  zp + 0.16e2 * s * s3 * pow(s4, 0.3e1) * t * u *  z - 0.16e2 * s * s3 * pow(s4, 0.3e1) * t * u *  zp - 0.24e2 * s * s3 * s4 * s4 * t * t * u *  z + 0.24e2 * s * s3 * s4 * s4 * t * t * u *  zp - 0.24e2 * s * s3 * s4 * s4 * t * u * u *  z + 0.24e2 * s * s3 * s4 * s4 * t * u * u *  zp + 0.2e1 * s * s3 * s4 * pow(t, 0.4e1) *  z - 0.2e1 * s * s3 * s4 * pow(t, 0.4e1) *  zp + 0.8e1 * s * s3 * s4 * pow(t, 0.3e1) * u *  z - 0.8e1 * s * s3 * s4 * pow(t, 0.3e1) * u *  zp + 0.4e1 * s * s3 * s4 * t * t * u * u *  z - 0.4e1 * s * s3 * s4 * t * t * u * u *  zp + 0.8e1 * s * s3 * s4 * t * pow(u, 0.3e1) *  z - 0.8e1 * s * s3 * s4 * t * pow(u, 0.3e1) *  zp + 0.2e1 * s * s3 * s4 * pow(u, 0.4e1) *  z - 0.2e1 * s * s3 * s4 * pow(u, 0.4e1) *  zp + 0.6e1 * s * s3 * pow(t, 0.4e1) * u *  z - 0.6e1 * s * s3 * pow(t, 0.4e1) * u *  zp + 0.18e2 * s * s3 * pow(t, 0.3e1) * u * u *  z - 0.18e2 * s * s3 * pow(t, 0.3e1) * u * u *  zp + 0.18e2 * s * s3 * t * t * pow(u, 0.3e1) *  z - 0.18e2 * s * s3 * t * t * pow(u, 0.3e1) *  zp + 0.6e1 * s * s3 * t * pow(u, 0.4e1) *  z - 0.6e1 * s * s3 * t * pow(u, 0.4e1) *  zp - 0.4e1 * s * s4 * s4 * pow(t, 0.3e1) * u *  z + 0.4e1 * s * s4 * s4 * pow(t, 0.3e1) * u *  zp - 0.8e1 * s * s4 * s4 * t * t * u * u *  z + 0.8e1 * s * s4 * s4 * t * t * u * u *  zp - 0.4e1 * s * s4 * s4 * t * pow(u, 0.3e1) *  z + 0.4e1 * s * s4 * s4 * t * pow(u, 0.3e1) *  zp + 0.6e1 * s * s4 * pow(t, 0.4e1) * u *  z - 0.6e1 * s * s4 * pow(t, 0.4e1) * u *  zp + 0.18e2 * s * s4 * pow(t, 0.3e1) * u * u *  z - 0.18e2 * s * s4 * pow(t, 0.3e1) * u * u *  zp + 0.18e2 * s * s4 * t * t * pow(u, 0.3e1) *  z - 0.18e2 * s * s4 * t * t * pow(u, 0.3e1) *  zp + 0.6e1 * s * s4 * t * pow(u, 0.4e1) *  z - 0.6e1 * s * s4 * t * pow(u, 0.4e1) *  zp - 0.3e1 * s * pow(t, 0.5e1) * u *  z + 0.3e1 * s * pow(t, 0.5e1) * u *  zp - 0.10e2 * s * pow(t, 0.4e1) * u * u *  z + 0.10e2 * s * pow(t, 0.4e1) * u * u *  zp - 0.14e2 * s * pow(t, 0.3e1) * pow(u, 0.3e1) *  z + 0.14e2 * s * pow(t, 0.3e1) * pow(u, 0.3e1) *  zp - 0.10e2 * s * t * t * pow(u, 0.4e1) *  z + 0.10e2 * s * t * t * pow(u, 0.4e1) *  zp - 0.3e1 * s * t * pow(u, 0.5e1) *  z + 0.3e1 * s * t * pow(u, 0.5e1) *  zp + 0.12e2 * pow(s3, 0.3e1) * s4 * t * t * u - 0.12e2 * pow(s3, 0.3e1) * s4 * t * u * u + 0.32e2 * s3 * s3 * s4 * s4 * t * t * u - 0.32e2 * s3 * s3 * s4 * s4 * t * u * u - 0.28e2 * s3 * s3 * s4 * pow(t, 0.3e1) * u + 0.28e2 * s3 * s3 * s4 * t * pow(u, 0.3e1) + 0.12e2 * s3 * pow(s4, 0.3e1) * t * t * u - 0.12e2 * s3 * pow(s4, 0.3e1) * t * u * u - 0.28e2 * s3 * s4 * s4 * pow(t, 0.3e1) * u + 0.28e2 * s3 * s4 * s4 * t * pow(u, 0.3e1) + 0.10e2 * s3 * s4 * pow(t, 0.4e1) * u + 0.10e2 * s3 * s4 * pow(t, 0.3e1) * u * u - 0.10e2 * s3 * s4 * t * t * pow(u, 0.3e1) - 0.10e2 * s3 * s4 * t * pow(u, 0.4e1) + 0.2e1 * s3 * pow(t, 0.5e1) * u + 0.4e1 * s3 * pow(t, 0.4e1) * u * u - 0.4e1 * s3 * t * t * pow(u, 0.4e1) - 0.2e1 * s3 * t * pow(u, 0.5e1) + 0.2e1 * s4 * pow(t, 0.5e1) * u + 0.4e1 * s4 * pow(t, 0.4e1) * u * u - 0.4e1 * s4 * t * t * pow(u, 0.4e1) - 0.2e1 * s4 * t * pow(u, 0.5e1) - pow(t, 0.6e1) * u - 0.3e1 * pow(t, 0.5e1) * u * u - 0.2e1 * pow(t, 0.4e1) * pow(u, 0.3e1) + 0.2e1 * pow(t, 0.3e1) * pow(u, 0.4e1) + 0.3e1 * t * t * pow(u, 0.5e1) + t * pow(u, 0.6e1)) /  (z - zp) / s * pow(t, -0.2e1) / (0.4e1 * s3 * s4 - t * t - 0.2e1 * t * u - u * u) * pow(u, -0.2e1) * log( z) * log( (1.0-z)) - 0.2e1 * (0.3e1 * pow(s3, 0.4e1) * s4 * s4 * t - 0.2e1 * pow(s3, 0.4e1) * s4 * t * t + 0.6e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * t - 0.3e1 * pow(s3, 0.3e1) * pow(s4, 0.3e1) * u - 0.16e2 * pow(s3, 0.3e1) * s4 * s4 * t * t + 0.8e1 * pow(s3, 0.3e1) * s4 * pow(t, 0.3e1) + pow(s3, 0.3e1) * s4 * t * t * u + 0.3e1 * s3 * s3 * pow(s4, 0.4e1) * t - 0.16e2 * s3 * s3 * pow(s4, 0.3e1) * t * t + 0.22e2 * s3 * s3 * s4 * s4 * pow(t, 0.3e1) + 0.6e1 * s3 * s3 * s4 * s4 * t * t * u + 0.3e1 * s3 * s3 * s4 * s4 * t * u * u - 0.8e1 * s3 * s3 * s4 * pow(t, 0.4e1) + 0.2e1 * s3 * s3 * s4 * pow(t, 0.3e1) * u - 0.2e1 * s3 * s3 * s4 * t * t * u * u - 0.4e1 * s3 * s3 * pow(t, 0.4e1) * u - 0.2e1 * s3 * pow(s4, 0.4e1) * t * t + 0.8e1 * s3 * pow(s4, 0.3e1) * pow(t, 0.3e1) + s3 * pow(s4, 0.3e1) * t * t * u - 0.8e1 * s3 * s4 * s4 * pow(t, 0.4e1) + 0.2e1 * s3 * s4 * s4 * pow(t, 0.3e1) * u - 0.2e1 * s3 * s4 * s4 * t * t * u * u + 0.2e1 * s3 * s4 * pow(t, 0.5e1) - 0.11e2 * s3 * s4 * pow(t, 0.4e1) * u - 0.4e1 * s3 * s4 * pow(t, 0.3e1) * u * u + 0.6e1 * s3 * pow(t, 0.5e1) * u + 0.4e1 * s3 * pow(t, 0.4e1) * u * u - 0.4e1 * s4 * s4 * pow(t, 0.4e1) * u + 0.6e1 * s4 * pow(t, 0.5e1) * u + 0.4e1 * s4 * pow(t, 0.4e1) * u * u - 0.2e1 * pow(t, 0.6e1) * u - 0.3e1 * pow(t, 0.5e1) * u * u) * pow(t, -0.2e1) / u * pow(s3 - t, -0.2e1) * pow(-t + s4, -0.2e1) * log(-t) - 0.4e1 * (0.2e1 * s3 * s3 * t + 0.4e1 * s3 * s4 * t - 0.2e1 * s3 * s4 * u - 0.4e1 * s3 * t * t - 0.2e1 * s3 * t * u + 0.2e1 * s4 * s4 * t - 0.4e1 * s4 * t * t - 0.2e1 * s4 * t * u + 0.2e1 * pow(t, 0.3e1) + 0.2e1 * t * t * u + t * u * u) * pow(t, -0.2e1) / u * log(-s) * log(-t);
    return 8.0*2.0*real( cg2);//:8 is the color factor
}


double GstarGstarMeNLOkinematics::collinear_limit(const KinematicVariables& kk,int i)
{
    if (i==1)
        return 8.0*consts::pi_square
            * PP(1.0/kk.z) * Born(kk.z_shifted(1))/kk.s15;
    else if (i==2)
        return 8.0*consts::pi_square
            * PP(1.0/kk.z) * Born(kk.z_shifted(2))/kk.s25;
    else
        {
        cout<<"\n"<<__func__<<" error: collinear limit asked with id different than 1 or 2"<<endl;
        return 0.0;
        }
}

void GstarGstarMeNLOkinematics::Evaluate(double* xx_vegas)
{
    kk_.generate_bjorken_xs(xx_vegas);
    const double lumi = LL(kk_.x1,kk_.x2);
    if (lumi!=0.0)
        {
        //cout<<"\n";for (int j=0;j<7;j++) cout<<xx_vegas[j]<<" ";
        kk_.SetNLOKinematics(xx_vegas);
        double me_sq = eval_me(kk_);
        const double collinear1 = collinear_limit(kk_,1);
        const double collinear2 = collinear_limit(kk_,2);
        const double total_factor = lumi * prefactor_ * kk_.jacobian;
                
        const double ds_R =  total_factor * (1.0-kk_.z)* me_sq/2.0/kk_.s12;
        const double ds_c1 = total_factor * (1.0-kk_.z)*collinear1 /2.0/ kk_.s12;
        const double ds_c2 = total_factor * (1.0-kk_.z)*collinear2 /2.0/ kk_.s12;
        KinematicVariables kshifted = kk_.z_shifted(2);
        //cout<<"\nR " <<ds_R<<" "<<ds_c1<<" "<<ds_c2<<" "<<kk_.lambda
        //    <<"\t\t"<<Born(kshifted)<<" "<<kshifted.s13<<" "<<kshifted.s23
        //<<" "<<kshifted.s14<<" "<<kshifted.s24<<" "<<kshifted.s34;
        JF(ds_R , kk_);
        JF(-ds_c1, kk_.single_collinear(1));
        JF(-ds_c2, kk_.single_collinear(2));
        }
    else
        {
        event_box_->AddNewEvent(0.0);
        }
}

double Power(const double x,int i)
{
    double res=1.0;
    for (int j=0;j<i;j++)
        {
        res=res*x;
        }
    return res;
}

double GstarGstarMeNLOHard::eval_me(const KinematicVariables& kk)
{
    const double s3 = kk.s3;
    const double s4 = kk.s4;
    const double s12 = kk.s12;
    const double s31 = kk.s13;
    const double s41 = kk.s14;
    const double s51 = kk.s15;
    const double s23 = kk.s23;
    const double s24 = kk.s24;
    const double s25 = kk.s25;
    const double s34 = kk.s34;
    const double s35 = kk.s35;
    const double s45 = kk.s45;
    
    const double Vgs2 = 4.0/3.0 * 2.0 * (4.0 * consts::pi_square);
    return Vgs2*((-2*(-4*Power(s12,3)*Power(s23,2)*s24*s3*Power(s31,2)*s4*s41 + 8*Power(s12,2)*Power(s23,3)*s24*s3*Power(s31,2)*s4*s41 -
         4*Power(s12,2)*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s4*s41 + 4*s12*Power(s23,3)*Power(s24,2)*s3*Power(s31,2)*s4*s41 +
         4*s12*Power(s23,2)*Power(s24,3)*s3*Power(s31,2)*s4*s41 - 4*Power(s12,2)*Power(s23,2)*s24*s25*s3*Power(s31,2)*s4*s41 +
         4*s12*Power(s23,3)*s24*s25*s3*Power(s31,2)*s4*s41 - 12*s12*Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*s4*s41 +
         4*s12*Power(s23,2)*s24*Power(s25,2)*s3*Power(s31,2)*s4*s41 - 16*Power(s12,2)*Power(s23,2)*s24*Power(s3,2)*Power(s31,2)*s4*s41 -
         8*s12*s23*Power(s24,3)*Power(s3,2)*Power(s31,2)*s4*s41 + 16*s12*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s4*s41 -
         8*s12*s23*s24*Power(s25,2)*Power(s3,2)*Power(s31,2)*s4*s41 - 4*Power(s12,2)*Power(s23,3)*s24*Power(s31,3)*s4*s41 -
         2*s12*Power(s23,3)*Power(s24,2)*Power(s31,3)*s4*s41 - 2*s12*Power(s23,2)*Power(s24,3)*Power(s31,3)*s4*s41 -
         2*s12*Power(s23,3)*s24*s25*Power(s31,3)*s4*s41 + 4*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,3)*s4*s41 -
         2*s12*Power(s23,2)*s24*Power(s25,2)*Power(s31,3)*s4*s41 + 8*Power(s12,2)*Power(s23,2)*s24*s3*Power(s31,3)*s4*s41 -
         4*s12*Power(s23,2)*Power(s24,2)*s3*Power(s31,3)*s4*s41 + 4*s12*s23*Power(s24,3)*s3*Power(s31,3)*s4*s41 -
         4*s12*Power(s23,2)*s24*s25*s3*Power(s31,3)*s4*s41 - 8*s12*s23*Power(s24,2)*s25*s3*Power(s31,3)*s4*s41 +
         4*s12*s23*s24*Power(s25,2)*s3*Power(s31,3)*s4*s41 + 2*s12*Power(s23,2)*Power(s24,2)*Power(s31,4)*s4*s41 +
         2*s12*Power(s23,2)*s24*s25*Power(s31,4)*s4*s41 - 2*Power(s12,2)*Power(s23,3)*s24*Power(s31,2)*s34*s4*s41 +
         2*Power(s12,2)*Power(s23,2)*Power(s24,2)*Power(s31,2)*s34*s4*s41 - 2*Power(s12,2)*Power(s23,2)*s24*s25*Power(s31,2)*s34*s4*s41 -
         2*s12*Power(s23,3)*s24*s25*Power(s31,2)*s34*s4*s41 + 2*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s34*s4*s41 -
         2*s12*Power(s23,2)*s24*Power(s25,2)*Power(s31,2)*s34*s4*s41 + 8*Power(s12,2)*Power(s23,2)*s24*s3*Power(s31,2)*s34*s4*s41 -
         4*Power(s12,2)*s23*Power(s24,2)*s3*Power(s31,2)*s34*s4*s41 + 4*Power(s12,2)*s23*s24*s25*s3*Power(s31,2)*s34*s4*s41 -
         4*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*s34*s4*s41 + 4*s12*s23*s24*Power(s25,2)*s3*Power(s31,2)*s34*s4*s41 -
         2*Power(s12,2)*Power(s23,2)*s24*Power(s31,3)*s34*s4*s41 + 2*s12*Power(s23,2)*s24*s25*Power(s31,3)*s34*s4*s41 -
         2*Power(s12,2)*Power(s23,3)*s24*Power(s31,2)*s35*s4*s41 - 2*Power(s12,2)*Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*s4*s41 -
         2*s12*Power(s23,3)*Power(s24,2)*Power(s31,2)*s35*s4*s41 - 2*s12*Power(s23,2)*Power(s24,3)*Power(s31,2)*s35*s4*s41 +
         2*Power(s12,2)*Power(s23,2)*s24*s25*Power(s31,2)*s35*s4*s41 + 2*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s35*s4*s41 +
         8*Power(s12,2)*Power(s23,2)*s24*s3*Power(s31,2)*s35*s4*s41 + 4*Power(s12,2)*s23*Power(s24,2)*s3*Power(s31,2)*s35*s4*s41 +
         4*s12*s23*Power(s24,3)*s3*Power(s31,2)*s35*s4*s41 - 4*Power(s12,2)*s23*s24*s25*s3*Power(s31,2)*s35*s4*s41 -
         4*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*s35*s4*s41 - 2*Power(s12,2)*Power(s23,2)*s24*Power(s31,3)*s35*s4*s41 +
         2*s12*Power(s23,2)*Power(s24,2)*Power(s31,3)*s35*s4*s41 - 4*Power(s12,2)*Power(s23,2)*s24*Power(s31,2)*s34*s35*s4*s41 +
         2*Power(s12,2)*Power(s23,3)*s24*Power(s31,2)*Power(s4,2)*s41 - 4*s12*Power(s23,4)*s24*Power(s31,2)*Power(s4,2)*s41 -
         2*Power(s12,2)*Power(s23,2)*Power(s24,2)*Power(s31,2)*Power(s4,2)*s41 - 4*s12*Power(s23,3)*Power(s24,2)*Power(s31,2)*Power(s4,2)*s41 +
         2*Power(s12,2)*Power(s23,2)*s24*s25*Power(s31,2)*Power(s4,2)*s41 - 2*s12*Power(s23,3)*s24*s25*Power(s31,2)*Power(s4,2)*s41 -
         10*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*Power(s4,2)*s41 + 2*s12*Power(s23,2)*s24*Power(s25,2)*Power(s31,2)*Power(s4,2)*s41 -
         16*Power(s12,2)*Power(s23,2)*s24*s3*Power(s31,2)*Power(s4,2)*s41 - 4*s12*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s4,2)*s41 -
         16*Power(s12,2)*s23*s24*s25*s3*Power(s31,2)*Power(s4,2)*s41 + 4*s12*Power(s23,2)*s24*s25*s3*Power(s31,2)*Power(s4,2)*s41 +
         8*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*Power(s4,2)*s41 - 4*Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*Power(s4,2)*s41 -
         8*s12*s23*s24*Power(s25,2)*s3*Power(s31,2)*Power(s4,2)*s41 + 4*Power(s23,2)*s24*Power(s25,2)*s3*Power(s31,2)*Power(s4,2)*s41 +
         16*s12*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s4,2)*s41 + 16*s12*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s4,2)*s41 +
         16*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s4,2)*s41 - 16*Power(s24,2)*s25*Power(s3,3)*Power(s31,2)*Power(s4,2)*s41 +
         2*Power(s12,2)*Power(s23,2)*s24*Power(s31,3)*Power(s4,2)*s41 + 8*s12*Power(s23,3)*s24*Power(s31,3)*Power(s4,2)*s41 +
         4*s12*Power(s23,2)*Power(s24,2)*Power(s31,3)*Power(s4,2)*s41 + 2*s12*Power(s23,2)*s24*s25*Power(s31,3)*Power(s4,2)*s41 -
         8*s12*s23*Power(s24,2)*s3*Power(s31,3)*Power(s4,2)*s41 - 8*s12*s23*s24*s25*s3*Power(s31,3)*Power(s4,2)*s41 -
         12*s23*Power(s24,2)*s25*s3*Power(s31,3)*Power(s4,2)*s41 - 4*s23*s24*Power(s25,2)*s3*Power(s31,3)*Power(s4,2)*s41 +
         16*Power(s24,2)*s25*Power(s3,2)*Power(s31,3)*Power(s4,2)*s41 - 4*s12*Power(s23,2)*s24*Power(s31,4)*Power(s4,2)*s41 +
         4*Power(s12,2)*Power(s23,2)*s24*Power(s31,2)*s35*Power(s4,2)*s41 - 4*s12*s23*Power(s24,2)*s3*Power(s31,2)*s35*Power(s4,2)*s41 +
         4*s12*s23*s24*s25*s3*Power(s31,2)*s35*Power(s4,2)*s41 - 2*Power(s12,2)*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*Power(s41,2) +
         2*Power(s12,2)*s23*Power(s24,3)*Power(s3,2)*s31*Power(s41,2) - 4*s12*Power(s23,2)*Power(s24,3)*Power(s3,2)*s31*Power(s41,2) -
         4*s12*s23*Power(s24,4)*Power(s3,2)*s31*Power(s41,2) + 2*Power(s12,2)*s23*Power(s24,2)*s25*Power(s3,2)*s31*Power(s41,2) -
         10*s12*Power(s23,2)*Power(s24,2)*s25*Power(s3,2)*s31*Power(s41,2) - 2*s12*s23*Power(s24,3)*s25*Power(s3,2)*s31*Power(s41,2) +
         2*s12*s23*Power(s24,2)*Power(s25,2)*Power(s3,2)*s31*Power(s41,2) + 4*Power(s12,2)*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s41,2) -
         3*s12*Power(s23,3)*Power(s24,2)*s3*Power(s31,2)*Power(s41,2) - s12*Power(s23,2)*Power(s24,3)*s3*Power(s31,2)*Power(s41,2) +
         2*s12*s23*Power(s24,4)*s3*Power(s31,2)*Power(s41,2) + 10*s12*Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*Power(s41,2) +
         2*s12*s23*Power(s24,3)*s25*s3*Power(s31,2)*Power(s41,2) - 4*Power(s23,2)*Power(s24,2)*Power(s25,2)*s3*Power(s31,2)*Power(s41,2) -
         2*Power(s12,2)*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s41,2) + 4*s12*Power(s23,2)*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s41,2) +
         4*s12*s23*Power(s24,3)*Power(s3,2)*Power(s31,2)*Power(s41,2) - 4*s12*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s41,2) -
         Power(s23,2)*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s41,2) - 2*s23*Power(s24,3)*s25*Power(s3,2)*Power(s31,2)*Power(s41,2) +
         4*s23*Power(s24,2)*Power(s25,2)*Power(s3,2)*Power(s31,2)*Power(s41,2) + 2*s12*Power(s23,3)*Power(s24,2)*Power(s31,3)*Power(s41,2) +
         2*s12*Power(s23,2)*Power(s24,3)*Power(s31,3)*Power(s41,2) + Power(s23,3)*Power(s24,2)*s25*Power(s31,3)*Power(s41,2) +
         Power(s23,2)*Power(s24,3)*s25*Power(s31,3)*Power(s41,2) + 2*Power(s23,2)*Power(s24,2)*Power(s25,2)*Power(s31,3)*Power(s41,2) -
         3*s12*Power(s23,2)*Power(s24,2)*s3*Power(s31,3)*Power(s41,2) - 2*s12*s23*Power(s24,3)*s3*Power(s31,3)*Power(s41,2) +
         Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,3)*Power(s41,2) - 2*s23*Power(s24,2)*Power(s25,2)*s3*Power(s31,3)*Power(s41,2) -
         Power(s23,2)*Power(s24,2)*s25*Power(s31,4)*Power(s41,2) + 2*Power(s12,2)*Power(s23,2)*Power(s24,2)*s3*s31*s34*Power(s41,2) -
         2*Power(s12,2)*s23*Power(s24,3)*s3*s31*s34*Power(s41,2) - 2*Power(s12,2)*s23*Power(s24,2)*s25*s3*s31*s34*Power(s41,2) +
         2*s12*Power(s23,2)*Power(s24,2)*s25*s3*s31*s34*Power(s41,2) - 2*s12*s23*Power(s24,3)*s25*s3*s31*s34*Power(s41,2) -
         2*s12*s23*Power(s24,2)*Power(s25,2)*s3*s31*s34*Power(s41,2) - 2*Power(s12,2)*Power(s23,2)*Power(s24,2)*Power(s31,2)*s34*Power(s41,2) -
         2*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s34*Power(s41,2) + Power(s23,2)*Power(s24,2)*Power(s25,2)*Power(s31,2)*s34*Power(s41,2) +
         2*Power(s12,2)*s23*Power(s24,2)*s3*Power(s31,2)*s34*Power(s41,2) + 2*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*s34*Power(s41,2) -
         2*s23*Power(s24,2)*Power(s25,2)*s3*Power(s31,2)*s34*Power(s41,2) + 2*s12*Power(s23,2)*Power(s24,3)*s3*s31*s35*Power(s41,2) +
         2*s12*s23*Power(s24,4)*s3*s31*s35*Power(s41,2) + 4*s12*Power(s23,2)*Power(s24,2)*s25*s3*s31*s35*Power(s41,2) +
         2*s12*s23*Power(s24,3)*s25*s3*s31*s35*Power(s41,2) + s12*Power(s23,3)*Power(s24,2)*Power(s31,2)*s35*Power(s41,2) +
         s12*Power(s23,2)*Power(s24,3)*Power(s31,2)*s35*Power(s41,2) - 2*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s35*Power(s41,2) -
         Power(s23,3)*Power(s24,2)*s25*Power(s31,2)*s35*Power(s41,2) - Power(s23,2)*Power(s24,3)*s25*Power(s31,2)*s35*Power(s41,2) -
         2*s12*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s35*Power(s41,2) - 2*s12*s23*Power(s24,3)*s3*Power(s31,2)*s35*Power(s41,2) +
         2*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*s35*Power(s41,2) + Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*s35*Power(s41,2) +
         2*s23*Power(s24,3)*s25*s3*Power(s31,2)*s35*Power(s41,2) + s12*Power(s23,2)*Power(s24,2)*Power(s31,3)*s35*Power(s41,2) -
         4*Power(s12,3)*s23*Power(s24,2)*s3*s31*s4*Power(s41,2) - 4*Power(s12,2)*Power(s23,2)*Power(s24,2)*s3*s31*s4*Power(s41,2) +
         4*s12*Power(s23,3)*Power(s24,2)*s3*s31*s4*Power(s41,2) + 8*Power(s12,2)*s23*Power(s24,3)*s3*s31*s4*Power(s41,2) +
         4*s12*Power(s23,2)*Power(s24,3)*s3*s31*s4*Power(s41,2) - 4*Power(s12,2)*s23*Power(s24,2)*s25*s3*s31*s4*Power(s41,2) -
         12*s12*Power(s23,2)*Power(s24,2)*s25*s3*s31*s4*Power(s41,2) + 4*s12*s23*Power(s24,3)*s25*s3*s31*s4*Power(s41,2) +
         4*s12*s23*Power(s24,2)*Power(s25,2)*s3*s31*s4*Power(s41,2) - 16*Power(s12,2)*s23*Power(s24,2)*Power(s3,2)*s31*s4*Power(s41,2) -
         4*s12*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*s4*Power(s41,2) - 16*Power(s12,2)*s23*s24*s25*Power(s3,2)*s31*s4*Power(s41,2) +
         8*s12*Power(s23,2)*s24*s25*Power(s3,2)*s31*s4*Power(s41,2) + 4*s12*s23*Power(s24,2)*s25*Power(s3,2)*s31*s4*Power(s41,2) -
         4*Power(s23,2)*Power(s24,2)*s25*Power(s3,2)*s31*s4*Power(s41,2) - 8*s12*s23*s24*Power(s25,2)*Power(s3,2)*s31*s4*Power(s41,2) +
         4*s23*Power(s24,2)*Power(s25,2)*Power(s3,2)*s31*s4*Power(s41,2) + 2*s12*Power(s23,4)*s24*Power(s31,2)*s4*Power(s41,2) +
         4*Power(s12,2)*Power(s23,2)*Power(s24,2)*Power(s31,2)*s4*Power(s41,2) - s12*Power(s23,3)*Power(s24,2)*Power(s31,2)*s4*Power(s41,2) -
         3*s12*Power(s23,2)*Power(s24,3)*Power(s31,2)*s4*Power(s41,2) + 2*s12*Power(s23,3)*s24*s25*Power(s31,2)*s4*Power(s41,2) +
         10*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s4*Power(s41,2) - 4*Power(s23,2)*Power(s24,2)*Power(s25,2)*Power(s31,2)*s4*Power(s41,2) -
         4*Power(s12,2)*Power(s23,2)*s24*s3*Power(s31,2)*s4*Power(s41,2) - 4*s12*Power(s23,3)*s24*s3*Power(s31,2)*s4*Power(s41,2) -
         4*Power(s12,2)*s23*Power(s24,2)*s3*Power(s31,2)*s4*Power(s41,2) - 4*s12*s23*Power(s24,3)*s3*Power(s31,2)*s4*Power(s41,2) -
         24*s12*Power(s23,2)*s24*s25*s3*Power(s31,2)*s4*Power(s41,2) - 24*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*s4*Power(s41,2) +
         10*Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*s4*Power(s41,2) - 8*s12*s23*s24*Power(s25,2)*s3*Power(s31,2)*s4*Power(s41,2) +
         6*Power(s23,2)*s24*Power(s25,2)*s3*Power(s31,2)*s4*Power(s41,2) + 6*s23*Power(s24,2)*Power(s25,2)*s3*Power(s31,2)*s4*Power(s41,2) -
         4*s12*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*s4*Power(s41,2) + 8*s12*s23*s24*s25*Power(s3,2)*Power(s31,2)*s4*Power(s41,2) -
         2*Power(s23,2)*s24*s25*Power(s3,2)*Power(s31,2)*s4*Power(s41,2) - 10*s12*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s4*Power(s41,2) -
         8*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s4*Power(s41,2) - 2*Power(s24,3)*s25*Power(s3,2)*Power(s31,2)*s4*Power(s41,2) -
         12*s23*s24*Power(s25,2)*Power(s3,2)*Power(s31,2)*s4*Power(s41,2) + 4*Power(s24,2)*Power(s25,2)*Power(s3,2)*Power(s31,2)*s4*Power(s41,2) +
         8*Power(s24,2)*s25*Power(s3,3)*Power(s31,2)*s4*Power(s41,2) - 2*s12*Power(s23,3)*s24*Power(s31,3)*s4*Power(s41,2) -
         s12*Power(s23,2)*Power(s24,2)*Power(s31,3)*s4*Power(s41,2) + 2*s12*Power(s23,2)*s24*s25*Power(s31,3)*s4*Power(s41,2) +
         Power(s23,2)*Power(s24,2)*s25*Power(s31,3)*s4*Power(s41,2) + 4*s12*Power(s23,2)*s24*s3*Power(s31,3)*s4*Power(s41,2) +
         4*s12*s23*Power(s24,2)*s3*Power(s31,3)*s4*Power(s41,2) + 6*s23*Power(s24,2)*s25*s3*Power(s31,3)*s4*Power(s41,2) -
         10*Power(s24,2)*s25*Power(s3,2)*Power(s31,3)*s4*Power(s41,2) - 4*Power(s12,2)*Power(s23,2)*s24*s3*s31*s34*s4*Power(s41,2) +
         8*Power(s12,2)*s23*Power(s24,2)*s3*s31*s34*s4*Power(s41,2) + 4*Power(s12,2)*s23*s24*s25*s3*s31*s34*s4*Power(s41,2) -
         4*s12*Power(s23,2)*s24*s25*s3*s31*s34*s4*Power(s41,2) + 4*s12*s23*s24*Power(s25,2)*s3*s31*s34*s4*Power(s41,2) +
         2*Power(s12,2)*Power(s23,2)*s24*Power(s31,2)*s34*s4*Power(s41,2) + 2*s12*Power(s23,2)*s24*s25*Power(s31,2)*s34*s4*Power(s41,2) -
         2*Power(s23,2)*s24*Power(s25,2)*Power(s31,2)*s34*s4*Power(s41,2) + 2*Power(s23,2)*s24*s25*s3*Power(s31,2)*s34*s4*Power(s41,2) +
         2*s23*Power(s24,2)*s25*s3*Power(s31,2)*s34*s4*Power(s41,2) + 8*s23*s24*Power(s25,2)*s3*Power(s31,2)*s34*s4*Power(s41,2) -
         2*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s34*s4*Power(s41,2) + 4*Power(s12,2)*Power(s23,2)*s24*s3*s31*s35*s4*Power(s41,2) +
         4*Power(s12,2)*s23*Power(s24,2)*s3*s31*s35*s4*Power(s41,2) + 4*Power(s12,2)*s23*s24*s25*s3*s31*s35*s4*Power(s41,2) -
         2*Power(s12,2)*Power(s23,2)*s24*Power(s31,2)*s35*s4*Power(s41,2) + 2*s12*Power(s23,3)*s24*Power(s31,2)*s35*s4*Power(s41,2) +
         2*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*s4*Power(s41,2) + 2*s12*Power(s23,2)*s24*s25*Power(s31,2)*s35*s4*Power(s41,2) -
         Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s35*s4*Power(s41,2) - 4*s12*s23*s24*s25*s3*Power(s31,2)*s35*s4*Power(s41,2) +
         2*Power(s23,2)*s24*s25*s3*Power(s31,2)*s35*s4*Power(s41,2) - 6*s23*Power(s24,2)*s25*s3*Power(s31,2)*s35*s4*Power(s41,2) +
         4*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s35*s4*Power(s41,2) - 2*s12*Power(s23,2)*s24*Power(s31,3)*s35*s4*Power(s41,2) -
         2*Power(s23,2)*s24*s25*Power(s31,2)*s34*s35*s4*Power(s41,2) - 8*s12*Power(s23,3)*s24*s3*s31*Power(s4,2)*Power(s41,2) -
         16*Power(s12,2)*s23*Power(s24,2)*s3*s31*Power(s4,2)*Power(s41,2) + 16*s12*Power(s23,2)*s24*s25*s3*s31*Power(s4,2)*Power(s41,2) -
         8*s12*s23*s24*Power(s25,2)*s3*s31*Power(s4,2)*Power(s41,2) + 16*s12*Power(s23,2)*s24*Power(s3,2)*s31*Power(s4,2)*Power(s41,2) +
         16*s12*Power(s23,2)*s25*Power(s3,2)*s31*Power(s4,2)*Power(s41,2) + 16*Power(s23,2)*s24*s25*Power(s3,2)*s31*Power(s4,2)*Power(s41,2) -
         2*Power(s12,2)*Power(s23,2)*s24*Power(s31,2)*Power(s4,2)*Power(s41,2) + 4*s12*Power(s23,3)*s24*Power(s31,2)*Power(s4,2)*Power(s41,2) +
         4*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*Power(s4,2)*Power(s41,2) - 4*s12*Power(s23,2)*s24*s25*Power(s31,2)*Power(s4,2)*Power(s41,2) -
         2*Power(s23,3)*s24*s25*Power(s31,2)*Power(s4,2)*Power(s41,2) - Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*Power(s4,2)*Power(s41,2) +
         4*Power(s23,2)*s24*Power(s25,2)*Power(s31,2)*Power(s4,2)*Power(s41,2) - 4*s12*Power(s23,2)*s24*s3*Power(s31,2)*Power(s4,2)*Power(s41,2) -
         10*s12*Power(s23,2)*s25*s3*Power(s31,2)*Power(s4,2)*Power(s41,2) - 2*Power(s23,3)*s25*s3*Power(s31,2)*Power(s4,2)*Power(s41,2) +
         8*s12*s23*s24*s25*s3*Power(s31,2)*Power(s4,2)*Power(s41,2) - 8*Power(s23,2)*s24*s25*s3*Power(s31,2)*Power(s4,2)*Power(s41,2) -
         2*s23*Power(s24,2)*s25*s3*Power(s31,2)*Power(s4,2)*Power(s41,2) + 4*Power(s23,2)*Power(s25,2)*s3*Power(s31,2)*Power(s4,2)*Power(s41,2) -
         12*s23*s24*Power(s25,2)*s3*Power(s31,2)*Power(s4,2)*Power(s41,2) + 4*Power(s23,2)*s25*Power(s3,2)*Power(s31,2)*Power(s4,2)*Power(s41,2) +
         4*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s4,2)*Power(s41,2) - 4*s12*Power(s23,2)*s24*Power(s31,3)*Power(s4,2)*Power(s41,2) -
         2*Power(s23,2)*s24*s25*Power(s31,3)*Power(s4,2)*Power(s41,2) - 2*Power(s23,2)*Power(s25,2)*Power(s31,3)*Power(s4,2)*Power(s41,2) -
         2*Power(s23,2)*s25*s3*Power(s31,2)*s34*Power(s4,2)*Power(s41,2) - 4*s12*Power(s23,2)*s24*s3*s31*s35*Power(s4,2)*Power(s41,2) -
         8*s12*Power(s23,2)*s25*s3*s31*s35*Power(s4,2)*Power(s41,2) - 4*s12*s23*s24*s25*s3*s31*s35*Power(s4,2)*Power(s41,2) -
         8*Power(s23,2)*s24*s25*s3*s31*s35*Power(s4,2)*Power(s41,2) + 2*s12*Power(s23,2)*s25*Power(s31,2)*s35*Power(s4,2)*Power(s41,2) +
         2*Power(s23,3)*s25*Power(s31,2)*s35*Power(s4,2)*Power(s41,2) + 6*Power(s23,2)*s24*s25*Power(s31,2)*s35*Power(s4,2)*Power(s41,2) -
         4*Power(s23,2)*s25*s3*Power(s31,2)*s35*Power(s4,2)*Power(s41,2) + 2*Power(s23,2)*s25*Power(s31,2)*s34*s35*Power(s4,2)*Power(s41,2) -
         16*Power(s23,2)*s25*Power(s3,2)*s31*Power(s4,3)*Power(s41,2) + 8*Power(s23,2)*s25*s3*Power(s31,2)*Power(s4,3)*Power(s41,2) +
         2*Power(s23,2)*s25*Power(s31,3)*Power(s4,3)*Power(s41,2) + 8*Power(s23,2)*s25*s3*s31*s35*Power(s4,3)*Power(s41,2) -
         4*Power(s23,2)*s25*Power(s31,2)*s35*Power(s4,3)*Power(s41,2) - 2*s12*Power(s23,3)*Power(s24,2)*s3*s31*Power(s41,3) -
         4*Power(s12,2)*s23*Power(s24,3)*s3*s31*Power(s41,3) - 2*s12*Power(s23,2)*Power(s24,3)*s3*s31*Power(s41,3) +
         4*s12*Power(s23,2)*Power(s24,2)*s25*s3*s31*Power(s41,3) - 2*s12*s23*Power(s24,3)*s25*s3*s31*Power(s41,3) -
         2*s12*s23*Power(s24,2)*Power(s25,2)*s3*s31*Power(s41,3) + 2*Power(s12,2)*s23*Power(s24,2)*Power(s3,2)*s31*Power(s41,3) +
         4*s12*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*Power(s41,3) + 8*s12*s23*Power(s24,3)*Power(s3,2)*s31*Power(s41,3) +
         2*s12*s23*Power(s24,2)*s25*Power(s3,2)*s31*Power(s41,3) + 2*s12*Power(s23,3)*Power(s24,2)*Power(s31,2)*Power(s41,3) +
         2*s12*Power(s23,2)*Power(s24,3)*Power(s31,2)*Power(s41,3) + Power(s23,3)*Power(s24,2)*s25*Power(s31,2)*Power(s41,3) +
         Power(s23,2)*Power(s24,3)*s25*Power(s31,2)*Power(s41,3) + 2*Power(s23,2)*Power(s24,2)*Power(s25,2)*Power(s31,2)*Power(s41,3) -
         s12*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s41,3) - 2*s12*s23*Power(s24,3)*s3*Power(s31,2)*Power(s41,3) +
         2*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*Power(s41,3) + Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*Power(s41,3) -
         4*s12*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s41,3) - 2*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s41,3) -
         2*Power(s24,2)*Power(s25,2)*Power(s3,2)*Power(s31,2)*Power(s41,3) + 2*Power(s24,2)*s25*Power(s3,3)*Power(s31,2)*Power(s41,3) -
         2*Power(s23,2)*Power(s24,2)*s25*Power(s31,3)*Power(s41,3) - 2*Power(s12,2)*s23*Power(s24,2)*s3*s31*s34*Power(s41,3) +
         2*s12*s23*Power(s24,2)*s25*s3*s31*s34*Power(s41,3) - 2*s12*Power(s23,2)*Power(s24,2)*s3*s31*s35*Power(s41,3) -
         4*s12*s23*Power(s24,3)*s3*s31*s35*Power(s41,3) - 2*s12*s23*Power(s24,2)*s25*s3*s31*s35*Power(s41,3) +
         s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*Power(s41,3) + 2*s12*s23*Power(s24,2)*s3*Power(s31,2)*s35*Power(s41,3) +
         2*s23*Power(s24,2)*s25*s3*Power(s31,2)*s35*Power(s41,3) - 2*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s35*Power(s41,3) +
         4*s12*Power(s23,3)*s24*s3*s31*s4*Power(s41,3) + 8*Power(s12,2)*s23*Power(s24,2)*s3*s31*s4*Power(s41,3) -
         4*s12*Power(s23,2)*Power(s24,2)*s3*s31*s4*Power(s41,3) - 8*s12*Power(s23,2)*s24*s25*s3*s31*s4*Power(s41,3) -
         4*s12*s23*Power(s24,2)*s25*s3*s31*s4*Power(s41,3) + 4*s12*s23*s24*Power(s25,2)*s3*s31*s4*Power(s41,3) -
         8*s12*Power(s23,2)*s24*Power(s3,2)*s31*s4*Power(s41,3) - 8*s12*s23*s24*s25*Power(s3,2)*s31*s4*Power(s41,3) -
         12*Power(s23,2)*s24*s25*Power(s3,2)*s31*s4*Power(s41,3) - 4*s23*s24*Power(s25,2)*Power(s3,2)*s31*s4*Power(s41,3) -
         2*s12*Power(s23,3)*s24*Power(s31,2)*s4*Power(s41,3) - 3*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s4*Power(s41,3) +
         Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s4*Power(s41,3) - 2*Power(s23,2)*s24*Power(s25,2)*Power(s31,2)*s4*Power(s41,3) +
         4*s12*Power(s23,2)*s24*s3*Power(s31,2)*s4*Power(s41,3) + 4*s12*s23*Power(s24,2)*s3*Power(s31,2)*s4*Power(s41,3) +
         6*Power(s23,2)*s24*s25*s3*Power(s31,2)*s4*Power(s41,3) + 4*s12*Power(s23,2)*s24*s3*s31*s35*s4*Power(s41,3) +
         4*s12*s23*s24*s25*s3*s31*s35*s4*Power(s41,3) + 8*Power(s23,2)*s24*s25*s3*s31*s35*s4*Power(s41,3) -
         2*s12*Power(s23,2)*s24*Power(s31,2)*s35*s4*Power(s41,3) - 2*Power(s23,2)*s24*s25*Power(s31,2)*s35*s4*Power(s41,3) +
         16*Power(s23,2)*s25*Power(s3,2)*s31*Power(s4,2)*Power(s41,3) - 10*Power(s23,2)*s25*s3*Power(s31,2)*Power(s4,2)*Power(s41,3) -
         8*Power(s23,2)*s25*s3*s31*s35*Power(s4,2)*Power(s41,3) + 2*Power(s23,2)*s25*Power(s31,2)*s35*Power(s4,2)*Power(s41,3) +
         2*s12*Power(s23,2)*Power(s24,2)*s3*s31*Power(s41,4) + 2*s12*s23*Power(s24,2)*s25*s3*s31*Power(s41,4) -
         4*s12*s23*Power(s24,2)*Power(s3,2)*s31*Power(s41,4) - Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*Power(s41,4) +
         2*s12*s23*Power(s24,2)*s3*s31*s35*Power(s41,4) + 2*s12*Power(s23,4)*s24*Power(s31,2)*s4*s41*s45 +
         2*s12*Power(s23,3)*Power(s24,2)*Power(s31,2)*s4*s41*s45 + 2*s12*Power(s23,3)*s24*s25*Power(s31,2)*s4*s41*s45 +
         4*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s4*s41*s45 + 4*Power(s12,2)*Power(s23,2)*s24*s3*Power(s31,2)*s4*s41*s45 +
         4*Power(s12,2)*s23*Power(s24,2)*s3*Power(s31,2)*s4*s41*s45 + 4*Power(s12,2)*s23*s24*s25*s3*Power(s31,2)*s4*s41*s45 -
         4*s12*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*s4*s41*s45 - 4*s12*s23*s24*s25*Power(s3,2)*Power(s31,2)*s4*s41*s45 -
         8*s12*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s4*s41*s45 - 8*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s4*s41*s45 +
         8*Power(s24,2)*s25*Power(s3,3)*Power(s31,2)*s4*s41*s45 - 4*s12*Power(s23,3)*s24*Power(s31,3)*s4*s41*s45 -
         2*s12*Power(s23,2)*Power(s24,2)*Power(s31,3)*s4*s41*s45 - 2*s12*Power(s23,2)*s24*s25*Power(s31,3)*s4*s41*s45 +
         4*s12*s23*Power(s24,2)*s3*Power(s31,3)*s4*s41*s45 + 4*s12*s23*s24*s25*s3*Power(s31,3)*s4*s41*s45 + 8*s23*Power(s24,2)*s25*s3*Power(s31,3)*s4*s41*s45 -
         8*Power(s24,2)*s25*Power(s3,2)*Power(s31,3)*s4*s41*s45 + 2*s12*Power(s23,2)*s24*Power(s31,4)*s4*s41*s45 -
         2*Power(s12,2)*Power(s23,2)*Power(s24,2)*s3*s31*Power(s41,2)*s45 - 2*s12*Power(s23,3)*Power(s24,2)*s3*s31*Power(s41,2)*s45 -
         2*Power(s12,2)*s23*Power(s24,3)*s3*s31*Power(s41,2)*s45 - 2*s12*Power(s23,2)*Power(s24,3)*s3*s31*Power(s41,2)*s45 +
         2*Power(s12,2)*s23*Power(s24,2)*s25*s3*s31*Power(s41,2)*s45 + 2*s12*Power(s23,2)*Power(s24,2)*s25*s3*s31*Power(s41,2)*s45 +
         4*Power(s12,2)*s23*Power(s24,2)*Power(s3,2)*s31*Power(s41,2)*s45 + s12*Power(s23,3)*Power(s24,2)*Power(s31,2)*Power(s41,2)*s45 +
         s12*Power(s23,2)*Power(s24,3)*Power(s31,2)*Power(s41,2)*s45 - 2*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*Power(s41,2)*s45 -
         Power(s23,3)*Power(s24,2)*s25*Power(s31,2)*Power(s41,2)*s45 - Power(s23,2)*Power(s24,3)*s25*Power(s31,2)*Power(s41,2)*s45 -
         2*Power(s12,2)*s23*Power(s24,2)*s3*Power(s31,2)*Power(s41,2)*s45 + 2*s12*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s41,2)*s45 +
         2*s12*s23*Power(s24,3)*s3*Power(s31,2)*Power(s41,2)*s45 + 2*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*Power(s41,2)*s45 -
         Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*Power(s41,2)*s45 + 2*s12*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s41,2)*s45 +
         6*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s41,2)*s45 + 2*Power(s24,3)*s25*Power(s3,2)*Power(s31,2)*Power(s41,2)*s45 -
         4*Power(s24,2)*s25*Power(s3,3)*Power(s31,2)*Power(s41,2)*s45 + s12*Power(s23,2)*Power(s24,2)*Power(s31,3)*Power(s41,2)*s45 -
         2*s12*s23*Power(s24,2)*s3*Power(s31,3)*Power(s41,2)*s45 - 2*s23*Power(s24,2)*s25*s3*Power(s31,3)*Power(s41,2)*s45 +
         2*Power(s24,2)*s25*Power(s3,2)*Power(s31,3)*Power(s41,2)*s45 - 4*Power(s12,2)*s23*Power(s24,2)*s3*s31*s34*Power(s41,2)*s45 -
         2*s23*Power(s24,2)*s25*s3*Power(s31,2)*s34*Power(s41,2)*s45 + 2*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s34*Power(s41,2)*s45 +
         4*Power(s12,2)*Power(s23,2)*s24*s3*s31*s4*Power(s41,2)*s45 + 4*s12*Power(s23,3)*s24*s3*s31*s4*Power(s41,2)*s45 +
         8*Power(s12,2)*s23*Power(s24,2)*s3*s31*s4*Power(s41,2)*s45 - 4*Power(s12,2)*s23*s24*s25*s3*s31*s4*Power(s41,2)*s45 -
         4*s12*Power(s23,2)*s24*s25*s3*s31*s4*Power(s41,2)*s45 - 4*s12*Power(s23,2)*s24*Power(s3,2)*s31*s4*Power(s41,2)*s45 +
         4*s12*s23*s24*s25*Power(s3,2)*s31*s4*Power(s41,2)*s45 - 2*s12*Power(s23,3)*s24*Power(s31,2)*s4*Power(s41,2)*s45 -
         2*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s4*Power(s41,2)*s45 + 2*s12*Power(s23,2)*s24*s25*Power(s31,2)*s4*Power(s41,2)*s45 +
         2*Power(s23,3)*s24*s25*Power(s31,2)*s4*Power(s41,2)*s45 + Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s4*Power(s41,2)*s45 -
         4*s12*s23*s24*s25*s3*Power(s31,2)*s4*Power(s41,2)*s45 - 6*Power(s23,2)*s24*s25*s3*Power(s31,2)*s4*Power(s41,2)*s45 +
         2*s23*Power(s24,2)*s25*s3*Power(s31,2)*s4*Power(s41,2)*s45 - 4*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s4*Power(s41,2)*s45 +
         2*s12*Power(s23,2)*s24*Power(s31,3)*s4*Power(s41,2)*s45 + 2*Power(s23,2)*s24*s25*Power(s31,3)*s4*Power(s41,2)*s45 +
         4*Power(s23,2)*s25*s3*Power(s31,2)*Power(s4,2)*Power(s41,2)*s45 - 2*Power(s23,2)*s25*Power(s31,3)*Power(s4,2)*Power(s41,2)*s45 -
         2*Power(s12,2)*s23*Power(s24,2)*s3*s31*Power(s41,3)*s45 + 2*s12*Power(s23,2)*Power(s24,2)*s3*s31*Power(s41,3)*s45 +
         s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*Power(s41,3)*s45 - 2*s12*s23*Power(s24,2)*s3*Power(s31,2)*Power(s41,3)*s45 +
         2*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*Power(s4,2)*s51 + 2*Power(s23,2)*Power(s24,3)*s25*Power(s31,2)*Power(s4,2)*s51 -
         10*s12*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s4,2)*s51 - 10*Power(s23,2)*Power(s24,3)*s3*Power(s31,2)*Power(s4,2)*s51 -
         8*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*Power(s4,2)*s51 - 4*Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*Power(s4,2)*s51 -
         8*s23*Power(s24,3)*s25*s3*Power(s31,2)*Power(s4,2)*s51 + 16*s12*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s4,2)*s51 +
         4*Power(s23,2)*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s4,2)*s51 + 16*s23*Power(s24,3)*Power(s3,2)*Power(s31,2)*Power(s4,2)*s51 -
         4*s12*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s4,2)*s51 - 8*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s4,2)*s51 -
         4*Power(s24,3)*s25*Power(s3,2)*Power(s31,2)*Power(s4,2)*s51 + 8*Power(s24,2)*s25*Power(s3,3)*Power(s31,2)*Power(s4,2)*s51 +
         2*Power(s23,2)*Power(s24,2)*s25*Power(s31,3)*Power(s4,2)*s51 - 2*Power(s23,2)*Power(s24,2)*s3*Power(s31,3)*Power(s4,2)*s51 +
         4*s23*Power(s24,2)*s25*s3*Power(s31,3)*Power(s4,2)*s51 - 4*Power(s24,2)*s25*Power(s3,2)*Power(s31,3)*Power(s4,2)*s51 +
         2*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s34*Power(s4,2)*s51 - 2*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s34*Power(s4,2)*s51 +
         4*s23*Power(s24,2)*s25*s3*Power(s31,2)*s34*Power(s4,2)*s51 - 4*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s34*Power(s4,2)*s51 +
         2*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*Power(s4,2)*s51 + 2*Power(s23,2)*Power(s24,3)*Power(s31,2)*s35*Power(s4,2)*s51 -
         8*s12*s23*Power(s24,2)*s3*Power(s31,2)*s35*Power(s4,2)*s51 - 4*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s35*Power(s4,2)*s51 -
         8*s23*Power(s24,3)*s3*Power(s31,2)*s35*Power(s4,2)*s51 + 2*Power(s23,2)*Power(s24,2)*Power(s31,3)*s35*Power(s4,2)*s51 +
         2*Power(s23,2)*Power(s24,2)*Power(s31,2)*s34*s35*Power(s4,2)*s51 + 2*Power(s23,3)*Power(s24,2)*Power(s31,2)*Power(s4,3)*s51 -
         4*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*Power(s4,3)*s51 + 8*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s4,3)*s51 +
         4*s23*Power(s24,2)*s25*s3*Power(s31,2)*Power(s4,3)*s51 - 16*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s4,3)*s51 +
         8*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s4,3)*s51 - 4*Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*Power(s4,3)*s51 +
         8*s23*Power(s24,2)*s3*Power(s31,2)*s35*Power(s4,3)*s51 - 8*Power(s12,3)*s23*s24*s25*s3*s31*s4*s41*s51 -
         8*Power(s12,2)*Power(s23,2)*s24*s25*s3*s31*s4*s41*s51 - 8*Power(s12,2)*s23*Power(s24,2)*s25*s3*s31*s4*s41*s51 -
         8*s12*Power(s23,2)*Power(s24,2)*s25*s3*s31*s4*s41*s51 - 16*Power(s12,2)*s23*Power(s24,2)*Power(s3,2)*s31*s4*s41*s51 +
         8*s12*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*s4*s41*s51 - 8*s12*s23*Power(s24,3)*Power(s3,2)*s31*s4*s41*s51 -
         4*s12*Power(s23,2)*s24*s25*Power(s3,2)*s31*s4*s41*s51 + 4*s12*s23*Power(s24,2)*s25*Power(s3,2)*s31*s4*s41*s51 -
         4*Power(s23,2)*Power(s24,2)*s25*Power(s3,2)*s31*s4*s41*s51 + 4*s23*Power(s24,3)*s25*Power(s3,2)*s31*s4*s41*s51 +
         2*s12*Power(s23,4)*s24*Power(s31,2)*s4*s41*s51 + 2*s12*Power(s23,3)*Power(s24,2)*Power(s31,2)*s4*s41*s51 +
         4*Power(s12,2)*Power(s23,2)*s24*s25*Power(s31,2)*s4*s41*s51 + 2*s12*Power(s23,3)*s24*s25*Power(s31,2)*s4*s41*s51 +
         8*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s4*s41*s51 - 2*Power(s23,2)*Power(s24,3)*s25*Power(s31,2)*s4*s41*s51 -
         4*Power(s12,2)*Power(s23,2)*s24*s3*Power(s31,2)*s4*s41*s51 - 4*s12*Power(s23,3)*s24*s3*Power(s31,2)*s4*s41*s51 -
         24*s12*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s4*s41*s51 - 8*s12*s23*Power(s24,3)*s3*Power(s31,2)*s4*s41*s51 +
         6*Power(s23,2)*Power(s24,3)*s3*Power(s31,2)*s4*s41*s51 - 8*Power(s12,2)*s23*s24*s25*s3*Power(s31,2)*s4*s41*s51 -
         16*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*s4*s41*s51 + 10*Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*s4*s41*s51 +
         4*s23*Power(s24,3)*s25*s3*Power(s31,2)*s4*s41*s51 + 8*s12*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*s4*s41*s51 -
         2*Power(s23,2)*Power(s24,2)*Power(s3,2)*Power(s31,2)*s4*s41*s51 - 12*s23*Power(s24,3)*Power(s3,2)*Power(s31,2)*s4*s41*s51 -
         4*s12*s23*s24*s25*Power(s3,2)*Power(s31,2)*s4*s41*s51 - 8*s12*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s4*s41*s51 -
         4*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s4*s41*s51 + 4*Power(s24,3)*s25*Power(s3,2)*Power(s31,2)*s4*s41*s51 +
         4*Power(s24,2)*s25*Power(s3,3)*Power(s31,2)*s4*s41*s51 - 2*s12*Power(s23,3)*s24*Power(s31,3)*s4*s41*s51 +
         2*s12*Power(s23,2)*Power(s24,2)*Power(s31,3)*s4*s41*s51 + 2*s12*Power(s23,2)*s24*s25*Power(s31,3)*s4*s41*s51 +
         4*s12*Power(s23,2)*s24*s3*Power(s31,3)*s4*s41*s51 + 4*s23*Power(s24,2)*s25*s3*Power(s31,3)*s4*s41*s51 -
         8*Power(s24,2)*s25*Power(s3,2)*Power(s31,3)*s4*s41*s51 + 4*Power(s12,2)*Power(s23,2)*s24*s3*s31*s34*s4*s41*s51 +
         4*Power(s12,2)*s23*Power(s24,2)*s3*s31*s34*s4*s41*s51 - 2*Power(s12,2)*Power(s23,2)*s24*Power(s31,2)*s34*s4*s41*s51 +
         2*s12*Power(s23,3)*s24*Power(s31,2)*s34*s4*s41*s51 + 2*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s34*s4*s41*s51 +
         4*s12*Power(s23,2)*s24*s25*Power(s31,2)*s34*s4*s41*s51 - 2*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*s34*s4*s41*s51 -
         4*s12*s23*Power(s24,2)*s3*Power(s31,2)*s34*s4*s41*s51 + 2*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s34*s4*s41*s51 -
         4*s23*Power(s24,2)*s25*s3*Power(s31,2)*s34*s4*s41*s51 + 4*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s34*s4*s41*s51 -
         2*s12*Power(s23,2)*s24*Power(s31,3)*s34*s4*s41*s51 - 4*Power(s12,2)*Power(s23,2)*s24*s3*s31*s35*s4*s41*s51 +
         4*Power(s12,2)*s23*Power(s24,2)*s3*s31*s35*s4*s41*s51 - 4*s12*Power(s23,2)*Power(s24,2)*s3*s31*s35*s4*s41*s51 +
         4*s12*s23*Power(s24,3)*s3*s31*s35*s4*s41*s51 + 2*Power(s12,2)*Power(s23,2)*s24*Power(s31,2)*s35*s4*s41*s51 +
         2*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*s4*s41*s51 - 2*Power(s23,2)*Power(s24,3)*Power(s31,2)*s35*s4*s41*s51 +
         2*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s35*s4*s41*s51 + 8*s23*Power(s24,3)*s3*Power(s31,2)*s35*s4*s41*s51 -
         2*Power(s23,2)*Power(s24,2)*Power(s31,2)*s34*s35*s4*s41*s51 - 16*Power(s12,2)*Power(s23,2)*s24*s3*s31*Power(s4,2)*s41*s51 -
         8*s12*Power(s23,3)*s24*s3*s31*Power(s4,2)*s41*s51 + 8*s12*Power(s23,2)*Power(s24,2)*s3*s31*Power(s4,2)*s41*s51 +
         4*s12*Power(s23,2)*s24*s25*s3*s31*Power(s4,2)*s41*s51 + 4*Power(s23,3)*s24*s25*s3*s31*Power(s4,2)*s41*s51 -
         4*s12*s23*Power(s24,2)*s25*s3*s31*Power(s4,2)*s41*s51 - 4*Power(s23,2)*Power(s24,2)*s25*s3*s31*Power(s4,2)*s41*s51 +
         2*Power(s12,2)*Power(s23,2)*s24*Power(s31,2)*Power(s4,2)*s41*s51 + 2*s12*Power(s23,3)*s24*Power(s31,2)*Power(s4,2)*s41*s51 -
         4*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*Power(s4,2)*s41*s51 - 2*Power(s23,3)*Power(s24,2)*Power(s31,2)*Power(s4,2)*s41*s51 -
         4*s12*Power(s23,2)*s24*s25*Power(s31,2)*Power(s4,2)*s41*s51 + 6*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*Power(s4,2)*s41*s51 +
         4*s12*Power(s23,2)*s24*s3*Power(s31,2)*Power(s4,2)*s41*s51 + 16*s12*s23*Power(s24,2)*s3*Power(s31,2)*Power(s4,2)*s41*s51 -
         8*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s4,2)*s41*s51 + 4*s12*s23*s24*s25*s3*Power(s31,2)*Power(s4,2)*s41*s51 -
         8*Power(s23,2)*s24*s25*s3*Power(s31,2)*Power(s4,2)*s41*s51 - 4*s23*Power(s24,2)*s25*s3*Power(s31,2)*Power(s4,2)*s41*s51 +
         16*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s4,2)*s41*s51 - 8*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s4,2)*s41*s51 -
         2*s12*Power(s23,2)*s24*Power(s31,3)*Power(s4,2)*s41*s51 - 2*Power(s23,2)*Power(s24,2)*Power(s31,3)*Power(s4,2)*s41*s51 +
         4*s23*s24*s25*s3*Power(s31,3)*Power(s4,2)*s41*s51 + 4*s12*Power(s23,2)*s24*s3*s31*s35*Power(s4,2)*s41*s51 -
         4*s12*s23*Power(s24,2)*s3*s31*s35*Power(s4,2)*s41*s51 + 6*Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*Power(s4,2)*s41*s51 -
         8*s23*Power(s24,2)*s3*Power(s31,2)*s35*Power(s4,2)*s41*s51 + 2*s12*Power(s23,2)*Power(s24,2)*s25*Power(s3,2)*Power(s41,2)*s51 +
         2*Power(s23,3)*Power(s24,2)*s25*Power(s3,2)*Power(s41,2)*s51 + 2*Power(s23,2)*Power(s24,3)*Power(s3,3)*Power(s41,2)*s51 -
         4*Power(s23,2)*Power(s24,2)*s25*Power(s3,3)*Power(s41,2)*s51 + 2*s12*Power(s23,2)*Power(s24,3)*s3*s31*Power(s41,2)*s51 +
         2*s12*s23*Power(s24,4)*s3*s31*Power(s41,2)*s51 + 4*Power(s12,2)*s23*Power(s24,2)*s25*s3*s31*Power(s41,2)*s51 +
         8*s12*Power(s23,2)*Power(s24,2)*s25*s3*s31*Power(s41,2)*s51 - 2*Power(s23,3)*Power(s24,2)*s25*s3*s31*Power(s41,2)*s51 +
         2*s12*s23*Power(s24,3)*s25*s3*s31*Power(s41,2)*s51 + 2*Power(s12,2)*s23*Power(s24,2)*Power(s3,2)*s31*Power(s41,2)*s51 -
         4*s12*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*Power(s41,2)*s51 + 2*s12*s23*Power(s24,3)*Power(s3,2)*s31*Power(s41,2)*s51 -
         2*Power(s23,2)*Power(s24,3)*Power(s3,2)*s31*Power(s41,2)*s51 - 4*s12*s23*Power(s24,2)*s25*Power(s3,2)*s31*Power(s41,2)*s51 +
         6*Power(s23,2)*Power(s24,2)*s25*Power(s3,2)*s31*Power(s41,2)*s51 - Power(s23,4)*Power(s24,2)*Power(s31,2)*Power(s41,2)*s51 -
         2*Power(s23,3)*Power(s24,3)*Power(s31,2)*Power(s41,2)*s51 - Power(s23,2)*Power(s24,4)*Power(s31,2)*Power(s41,2)*s51 -
         4*s12*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*Power(s41,2)*s51 - 2*Power(s23,3)*Power(s24,2)*s25*Power(s31,2)*Power(s41,2)*s51 -
         2*Power(s23,2)*Power(s24,3)*s25*Power(s31,2)*Power(s41,2)*s51 + 10*s12*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s41,2)*s51 +
         Power(s23,3)*Power(s24,2)*s3*Power(s31,2)*Power(s41,2)*s51 + 4*s12*s23*Power(s24,3)*s3*Power(s31,2)*Power(s41,2)*s51 +
         Power(s23,2)*Power(s24,3)*s3*Power(s31,2)*Power(s41,2)*s51 + 8*s12*s23*Power(s24,2)*s25*s3*Power(s31,2)*Power(s41,2)*s51 -
         10*s12*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s41,2)*s51 - Power(s23,2)*Power(s24,2)*Power(s3,2)*Power(s31,2)*Power(s41,2)*s51 +
         2*s12*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s41,2)*s51 + 6*s23*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*Power(s41,2)*s51 +
         2*Power(s24,3)*s25*Power(s3,2)*Power(s31,2)*Power(s41,2)*s51 - 4*Power(s24,2)*s25*Power(s3,3)*Power(s31,2)*Power(s41,2)*s51 +
         Power(s23,3)*Power(s24,2)*Power(s31,3)*Power(s41,2)*s51 + Power(s23,2)*Power(s24,3)*Power(s31,3)*Power(s41,2)*s51 -
         2*Power(s23,2)*Power(s24,2)*s25*Power(s31,3)*Power(s41,2)*s51 - 2*s23*Power(s24,2)*s25*s3*Power(s31,3)*Power(s41,2)*s51 +
         2*Power(s24,2)*s25*Power(s3,2)*Power(s31,3)*Power(s41,2)*s51 + 2*Power(s23,2)*Power(s24,2)*s25*Power(s3,2)*s34*Power(s41,2)*s51 -
         2*Power(s12,2)*s23*Power(s24,2)*s3*s31*s34*Power(s41,2)*s51 + 2*s12*Power(s23,2)*Power(s24,2)*s3*s31*s34*Power(s41,2)*s51 +
         2*s12*s23*Power(s24,3)*s3*s31*s34*Power(s41,2)*s51 + 4*s12*s23*Power(s24,2)*s25*s3*s31*s34*Power(s41,2)*s51 -
         2*Power(s23,2)*Power(s24,2)*s25*s3*s31*s34*Power(s41,2)*s51 - 2*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s34*Power(s41,2)*s51 +
         2*s12*s23*Power(s24,2)*s3*Power(s31,2)*s34*Power(s41,2)*s51 - 2*s23*Power(s24,2)*s25*s3*Power(s31,2)*s34*Power(s41,2)*s51 +
         2*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s34*Power(s41,2)*s51 - 2*Power(s23,2)*Power(s24,3)*Power(s3,2)*s35*Power(s41,2)*s51 +
         2*s12*Power(s23,2)*Power(s24,2)*s3*s31*s35*Power(s41,2)*s51 - 2*s12*s23*Power(s24,3)*s3*s31*s35*Power(s41,2)*s51 +
         2*Power(s23,2)*Power(s24,3)*s3*s31*s35*Power(s41,2)*s51 - 2*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*Power(s41,2)*s51 +
         4*s12*s23*Power(s24,2)*s3*Power(s31,2)*s35*Power(s41,2)*s51 + Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s35*Power(s41,2)*s51 -
         Power(s23,2)*Power(s24,2)*Power(s31,3)*s35*Power(s41,2)*s51 - 10*s12*Power(s23,2)*Power(s24,2)*Power(s3,2)*s4*Power(s41,2)*s51 -
         10*Power(s23,3)*Power(s24,2)*Power(s3,2)*s4*Power(s41,2)*s51 - 8*s12*Power(s23,2)*s24*s25*Power(s3,2)*s4*Power(s41,2)*s51 -
         8*Power(s23,3)*s24*s25*Power(s3,2)*s4*Power(s41,2)*s51 - 4*Power(s23,2)*Power(s24,2)*s25*Power(s3,2)*s4*Power(s41,2)*s51 +
         8*Power(s23,2)*Power(s24,2)*Power(s3,3)*s4*Power(s41,2)*s51 + 4*Power(s23,2)*s24*s25*Power(s3,3)*s4*Power(s41,2)*s51 -
         8*s12*Power(s23,3)*s24*s3*s31*s4*Power(s41,2)*s51 - 4*Power(s12,2)*s23*Power(s24,2)*s3*s31*s4*Power(s41,2)*s51 -
         24*s12*Power(s23,2)*Power(s24,2)*s3*s31*s4*Power(s41,2)*s51 + 6*Power(s23,3)*Power(s24,2)*s3*s31*s4*Power(s41,2)*s51 -
         4*s12*s23*Power(s24,3)*s3*s31*s4*Power(s41,2)*s51 - 8*Power(s12,2)*s23*s24*s25*s3*s31*s4*Power(s41,2)*s51 -
         16*s12*Power(s23,2)*s24*s25*s3*s31*s4*Power(s41,2)*s51 + 4*Power(s23,3)*s24*s25*s3*s31*s4*Power(s41,2)*s51 +
         10*Power(s23,2)*Power(s24,2)*s25*s3*s31*s4*Power(s41,2)*s51 + 16*s12*Power(s23,2)*s24*Power(s3,2)*s31*s4*Power(s41,2)*s51 +
         4*s12*s23*Power(s24,2)*Power(s3,2)*s31*s4*Power(s41,2)*s51 - 8*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*s4*Power(s41,2)*s51 +
         4*s12*s23*s24*s25*Power(s3,2)*s31*s4*Power(s41,2)*s51 - 4*Power(s23,2)*s24*s25*Power(s3,2)*s31*s4*Power(s41,2)*s51 -
         8*s23*Power(s24,2)*s25*Power(s3,2)*s31*s4*Power(s41,2)*s51 + 4*s12*Power(s23,3)*s24*Power(s31,2)*s4*Power(s41,2)*s51 +
         10*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s4*Power(s41,2)*s51 + Power(s23,3)*Power(s24,2)*Power(s31,2)*s4*Power(s41,2)*s51 +
         Power(s23,2)*Power(s24,3)*Power(s31,2)*s4*Power(s41,2)*s51 + 8*s12*Power(s23,2)*s24*s25*Power(s31,2)*s4*Power(s41,2)*s51 -
         12*s12*Power(s23,2)*s24*s3*Power(s31,2)*s4*Power(s41,2)*s51 - 12*s12*s23*Power(s24,2)*s3*Power(s31,2)*s4*Power(s41,2)*s51 +
         10*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s4*Power(s41,2)*s51 - 8*s12*s23*s24*s25*s3*Power(s31,2)*s4*Power(s41,2)*s51 +
         10*Power(s23,2)*s24*s25*s3*Power(s31,2)*s4*Power(s41,2)*s51 + 10*s23*Power(s24,2)*s25*s3*Power(s31,2)*s4*Power(s41,2)*s51 -
         4*s23*Power(s24,2)*Power(s3,2)*Power(s31,2)*s4*Power(s41,2)*s51 - 4*s23*s24*s25*Power(s3,2)*Power(s31,2)*s4*Power(s41,2)*s51 -
         4*Power(s24,2)*s25*Power(s3,2)*Power(s31,2)*s4*Power(s41,2)*s51 - 2*Power(s23,2)*Power(s24,2)*Power(s3,2)*s34*s4*Power(s41,2)*s51 +
         4*Power(s23,2)*s24*s25*Power(s3,2)*s34*s4*Power(s41,2)*s51 - 4*s12*Power(s23,2)*s24*s3*s31*s34*s4*Power(s41,2)*s51 +
         2*Power(s23,2)*Power(s24,2)*s3*s31*s34*s4*Power(s41,2)*s51 - 4*Power(s23,2)*s24*s25*s3*s31*s34*s4*Power(s41,2)*s51 +
         2*s12*Power(s23,2)*s24*Power(s31,2)*s34*s4*Power(s41,2)*s51 - 2*Power(s23,2)*s24*s25*Power(s31,2)*s34*s4*Power(s41,2)*s51 +
         4*Power(s23,2)*Power(s24,2)*Power(s3,2)*s35*s4*Power(s41,2)*s51 - 4*s12*Power(s23,2)*s24*s3*s31*s35*s4*Power(s41,2)*s51 -
         6*Power(s23,2)*Power(s24,2)*s3*s31*s35*s4*Power(s41,2)*s51 + 2*s12*Power(s23,2)*s24*Power(s31,2)*s35*s4*Power(s41,2)*s51 -
         Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*s4*Power(s41,2)*s51 + 16*s12*Power(s23,2)*s24*Power(s3,2)*Power(s4,2)*Power(s41,2)*s51 +
         16*Power(s23,3)*s24*Power(s3,2)*Power(s4,2)*Power(s41,2)*s51 + 4*Power(s23,2)*Power(s24,2)*Power(s3,2)*Power(s4,2)*Power(s41,2)*s51 -
         4*s12*Power(s23,2)*s25*Power(s3,2)*Power(s4,2)*Power(s41,2)*s51 - 4*Power(s23,3)*s25*Power(s3,2)*Power(s4,2)*Power(s41,2)*s51 -
         8*Power(s23,2)*s24*s25*Power(s3,2)*Power(s4,2)*Power(s41,2)*s51 - 16*Power(s23,2)*s24*Power(s3,3)*Power(s4,2)*Power(s41,2)*s51 +
         8*Power(s23,2)*s25*Power(s3,3)*Power(s4,2)*Power(s41,2)*s51 + 8*s12*Power(s23,2)*s24*s3*s31*Power(s4,2)*Power(s41,2)*s51 -
         12*Power(s23,3)*s24*s3*s31*Power(s4,2)*Power(s41,2)*s51 - 2*Power(s23,2)*Power(s24,2)*s3*s31*Power(s4,2)*Power(s41,2)*s51 -
         8*s12*Power(s23,2)*s25*s3*s31*Power(s4,2)*Power(s41,2)*s51 + 4*Power(s23,3)*s25*s3*s31*Power(s4,2)*Power(s41,2)*s51 -
         4*s12*s23*s24*s25*s3*s31*Power(s4,2)*Power(s41,2)*s51 - 4*Power(s23,2)*s24*s25*s3*s31*Power(s4,2)*Power(s41,2)*s51 +
         16*Power(s23,2)*s24*Power(s3,2)*s31*Power(s4,2)*Power(s41,2)*s51 - 8*Power(s23,2)*s25*Power(s3,2)*s31*Power(s4,2)*Power(s41,2)*s51 -
         10*s12*Power(s23,2)*s24*Power(s31,2)*Power(s4,2)*Power(s41,2)*s51 - Power(s23,2)*Power(s24,2)*Power(s31,2)*Power(s4,2)*Power(s41,2)*s51 +
         2*s12*Power(s23,2)*s25*Power(s31,2)*Power(s4,2)*Power(s41,2)*s51 + 2*Power(s23,3)*s25*Power(s31,2)*Power(s4,2)*Power(s41,2)*s51 +
         6*Power(s23,2)*s24*s25*Power(s31,2)*Power(s4,2)*Power(s41,2)*s51 - 4*Power(s23,2)*s24*s3*Power(s31,2)*Power(s4,2)*Power(s41,2)*s51 -
         4*Power(s23,2)*s25*s3*Power(s31,2)*Power(s4,2)*Power(s41,2)*s51 - 4*s23*s24*s25*s3*Power(s31,2)*Power(s4,2)*Power(s41,2)*s51 -
         4*Power(s23,2)*s25*Power(s3,2)*s34*Power(s4,2)*Power(s41,2)*s51 + 4*Power(s23,2)*s25*s3*s31*s34*Power(s4,2)*Power(s41,2)*s51 +
         2*Power(s23,2)*s25*Power(s31,2)*s34*Power(s4,2)*Power(s41,2)*s51 + 8*Power(s23,2)*s25*Power(s3,2)*Power(s4,3)*Power(s41,2)*s51 +
         4*Power(s23,2)*s25*s3*s31*Power(s4,3)*Power(s41,2)*s51 - 4*Power(s23,2)*s25*Power(s31,2)*Power(s4,3)*Power(s41,2)*s51 +
         2*Power(s23,2)*Power(s24,2)*s25*Power(s3,2)*Power(s41,3)*s51 + 2*s12*Power(s23,2)*Power(s24,2)*s3*s31*Power(s41,3)*s51 -
         2*s12*s23*Power(s24,3)*s3*s31*Power(s41,3)*s51 + 2*s12*s23*Power(s24,2)*s25*s3*s31*Power(s41,3)*s51 -
         2*s12*s23*Power(s24,2)*Power(s3,2)*s31*Power(s41,3)*s51 - 2*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*Power(s41,3)*s51 +
         Power(s23,3)*Power(s24,2)*Power(s31,2)*Power(s41,3)*s51 + Power(s23,2)*Power(s24,3)*Power(s31,2)*Power(s41,3)*s51 -
         2*Power(s23,2)*Power(s24,2)*s25*Power(s31,2)*Power(s41,3)*s51 - 2*s12*s23*Power(s24,2)*s3*s31*s34*Power(s41,3)*s51 +
         2*s12*s23*Power(s24,2)*s3*s31*s35*Power(s41,3)*s51 + 2*Power(s23,2)*Power(s24,2)*s3*s31*s35*Power(s41,3)*s51 -
         Power(s23,2)*Power(s24,2)*Power(s31,2)*s35*Power(s41,3)*s51 - 2*Power(s23,2)*Power(s24,2)*Power(s3,2)*s4*Power(s41,3)*s51 +
         4*Power(s23,2)*s24*s25*Power(s3,2)*s4*Power(s41,3)*s51 + 4*s12*s23*Power(s24,2)*s3*s31*s4*Power(s41,3)*s51 +
         4*Power(s23,2)*s24*s25*s3*s31*s4*Power(s41,3)*s51 + 4*s23*s24*s25*Power(s3,2)*s31*s4*Power(s41,3)*s51 -
         2*Power(s23,2)*s24*s25*Power(s31,2)*s4*Power(s41,3)*s51 - 4*Power(s23,2)*s25*Power(s3,2)*Power(s4,2)*Power(s41,3)*s51 -
         8*Power(s23,2)*s25*s3*s31*Power(s4,2)*Power(s41,3)*s51 + 2*Power(s23,2)*s25*Power(s31,2)*Power(s4,2)*Power(s41,3)*s51 -
         2*Power(s23,3)*Power(s24,2)*Power(s31,2)*Power(s4,2)*s45*s51 + 4*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s4,2)*s45*s51 +
         4*Power(s12,2)*Power(s23,2)*s24*s3*s31*s4*s41*s45*s51 + 4*s12*Power(s23,3)*s24*s3*s31*s4*s41*s45*s51 -
         4*Power(s12,2)*s23*Power(s24,2)*s3*s31*s4*s41*s45*s51 - 4*s12*Power(s23,2)*Power(s24,2)*s3*s31*s4*s41*s45*s51 -
         4*s12*Power(s23,2)*s24*Power(s3,2)*s31*s4*s41*s45*s51 + 4*s12*s23*Power(s24,2)*Power(s3,2)*s31*s4*s41*s45*s51 -
         2*s12*Power(s23,3)*s24*Power(s31,2)*s4*s41*s45*s51 + 2*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*s4*s41*s45*s51 +
         2*Power(s23,3)*Power(s24,2)*Power(s31,2)*s4*s41*s45*s51 - 4*s12*s23*Power(s24,2)*s3*Power(s31,2)*s4*s41*s45*s51 -
         6*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s4*s41*s45*s51 + 2*s12*Power(s23,2)*s24*Power(s31,3)*s4*s41*s45*s51 +
         2*Power(s23,2)*Power(s24,2)*Power(s31,3)*s4*s41*s45*s51 + 2*s12*Power(s23,2)*Power(s24,2)*Power(s3,2)*Power(s41,2)*s45*s51 +
         2*Power(s23,3)*Power(s24,2)*Power(s3,2)*Power(s41,2)*s45*s51 - 4*Power(s23,2)*Power(s24,2)*Power(s3,3)*Power(s41,2)*s45*s51 +
         2*Power(s12,2)*s23*Power(s24,2)*s3*s31*Power(s41,2)*s45*s51 + 2*s12*Power(s23,2)*Power(s24,2)*s3*s31*Power(s41,2)*s45*s51 -
         2*Power(s23,3)*Power(s24,2)*s3*s31*Power(s41,2)*s45*s51 + 6*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*Power(s41,2)*s45*s51 -
         2*s12*Power(s23,2)*Power(s24,2)*Power(s31,2)*Power(s41,2)*s45*s51 + 2*s12*s23*Power(s24,2)*s3*Power(s31,2)*Power(s41,2)*s45*s51 -
         Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s41,2)*s45*s51 - Power(s23,2)*Power(s24,2)*Power(s31,3)*Power(s41,2)*s45*s51 +
         2*Power(s23,2)*Power(s24,2)*Power(s3,2)*s34*Power(s41,2)*s45*s51 - 2*Power(s23,2)*Power(s24,2)*s3*s31*s34*Power(s41,2)*s45*s51 -
         8*s12*Power(s23,2)*s24*Power(s3,2)*s4*Power(s41,2)*s45*s51 - 8*Power(s23,3)*s24*Power(s3,2)*s4*Power(s41,2)*s45*s51 -
         4*Power(s23,2)*Power(s24,2)*Power(s3,2)*s4*Power(s41,2)*s45*s51 + 8*Power(s23,2)*s24*Power(s3,3)*s4*Power(s41,2)*s45*s51 +
         8*Power(s23,3)*s24*s3*s31*s4*Power(s41,2)*s45*s51 + 2*Power(s23,2)*Power(s24,2)*s3*s31*s4*Power(s41,2)*s45*s51 -
         8*Power(s23,2)*s24*Power(s3,2)*s31*s4*Power(s41,2)*s45*s51 + 4*s12*Power(s23,2)*s24*Power(s31,2)*s4*Power(s41,2)*s45*s51 +
         Power(s23,2)*Power(s24,2)*Power(s31,2)*s4*Power(s41,2)*s45*s51 + 2*Power(s23,2)*Power(s24,2)*Power(s3,2)*Power(s41,3)*s45*s51 -
         Power(s23,2)*Power(s24,2)*Power(s31,2)*Power(s41,3)*s45*s51 - 2*Power(s23,3)*Power(s24,2)*Power(s31,2)*Power(s4,2)*Power(s51,2) +
         4*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s4,2)*Power(s51,2) + 4*s12*Power(s23,3)*s24*s3*s31*s4*s41*Power(s51,2) -
         8*s12*Power(s23,2)*Power(s24,2)*s3*s31*s4*s41*Power(s51,2) + 4*s12*s23*Power(s24,3)*s3*s31*s4*s41*Power(s51,2) -
         8*s12*Power(s23,2)*s24*Power(s3,2)*s31*s4*s41*Power(s51,2) - 8*s12*s23*Power(s24,2)*Power(s3,2)*s31*s4*s41*Power(s51,2) -
         12*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*s4*s41*Power(s51,2) - 4*s23*Power(s24,3)*Power(s3,2)*s31*s4*s41*Power(s51,2) -
         2*s12*Power(s23,3)*s24*Power(s31,2)*s4*s41*Power(s51,2) - 2*Power(s23,2)*Power(s24,3)*Power(s31,2)*s4*s41*Power(s51,2) +
         4*s12*Power(s23,2)*s24*s3*Power(s31,2)*s4*s41*Power(s51,2) + 6*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*s4*s41*Power(s51,2) +
         4*s12*Power(s23,2)*s24*s3*s31*s34*s4*s41*Power(s51,2) + 4*s12*s23*Power(s24,2)*s3*s31*s34*s4*s41*Power(s51,2) +
         8*Power(s23,2)*Power(s24,2)*s3*s31*s34*s4*s41*Power(s51,2) - 2*s12*Power(s23,2)*s24*Power(s31,2)*s34*s4*s41*Power(s51,2) -
         2*Power(s23,2)*Power(s24,2)*Power(s31,2)*s34*s4*s41*Power(s51,2) - 8*s12*Power(s23,2)*s24*s3*s31*Power(s4,2)*s41*Power(s51,2) -
         4*Power(s23,3)*s24*s3*s31*Power(s4,2)*s41*Power(s51,2) - 8*s12*s23*Power(s24,2)*s3*s31*Power(s4,2)*s41*Power(s51,2) -
         12*Power(s23,2)*Power(s24,2)*s3*s31*Power(s4,2)*s41*Power(s51,2) + 2*s12*Power(s23,2)*s24*Power(s31,2)*Power(s4,2)*s41*Power(s51,2) +
         4*Power(s23,2)*Power(s24,2)*Power(s31,2)*Power(s4,2)*s41*Power(s51,2) + 4*Power(s23,2)*s24*s3*Power(s31,2)*Power(s4,2)*s41*Power(s51,2) -
         2*Power(s23,2)*Power(s24,3)*Power(s3,2)*Power(s41,2)*Power(s51,2) - 2*Power(s23,3)*Power(s24,2)*s3*s31*Power(s41,2)*Power(s51,2) -
         2*s12*s23*Power(s24,3)*s3*s31*Power(s41,2)*Power(s51,2) + 2*s12*s23*Power(s24,2)*Power(s3,2)*s31*Power(s41,2)*Power(s51,2) +
         4*Power(s23,2)*Power(s24,2)*Power(s3,2)*s31*Power(s41,2)*Power(s51,2) + 2*Power(s23,3)*Power(s24,2)*Power(s31,2)*Power(s41,2)*Power(s51,2) +
         2*Power(s23,2)*Power(s24,3)*Power(s31,2)*Power(s41,2)*Power(s51,2) - 4*Power(s23,2)*Power(s24,2)*s3*Power(s31,2)*Power(s41,2)*Power(s51,2) -
         2*s12*s23*Power(s24,2)*s3*s31*s34*Power(s41,2)*Power(s51,2) - 2*Power(s23,2)*Power(s24,2)*s3*s31*s34*Power(s41,2)*Power(s51,2) + 
         Power(s23,2)*Power(s24,2)*Power(s31,2)*s34*Power(s41,2)*Power(s51,2) + 4*Power(s23,2)*Power(s24,2)*Power(s3,2)*s4*Power(s41,2)*Power(s51,2) + 
         4*s12*s23*Power(s24,2)*s3*s31*s4*Power(s41,2)*Power(s51,2) + 6*Power(s23,2)*Power(s24,2)*s3*s31*s4*Power(s41,2)*Power(s51,2) + 
         4*s23*Power(s24,2)*Power(s3,2)*s31*s4*Power(s41,2)*Power(s51,2) - 4*Power(s23,2)*Power(s24,2)*Power(s31,2)*s4*Power(s41,2)*Power(s51,2)))/
    (Power(s23,2)*Power(s24,2)*s25*s3*Power(s31,2)*s4*Power(s41,2)*s51));
    
}















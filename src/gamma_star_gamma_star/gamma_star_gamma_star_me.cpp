#include "gamma_star_gamma_star_me.h"
#include "chaplin.h"


#include <iomanip>
#include <sstream>
ostream& operator<<(ostream& stream, const FMomentum& p)
{
    return stream << setprecision(8)
        <<"[ "<<p[0]<<","<<p[1]<<","<<p[2]<<","<<p[3]<<" ]";
}



double GstarGstarBorn::operator()(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double q13 = kk.q(1,3);
    const double q23 = kk.q(2,3);
    
    
    return 8.0*3.0*(
                    q23/q13 - 2.0 * (q3+q4)/q13
                    - q3*q4/q13/q13
                    +q13/q23 - 2.0 * (q3+q4)/q23
                    - q3*q4/q23/q23
                    +2.0*pow(q3+q4,2.0)/ (q13*q23)
                    );
}

double GstarGstarBorn::e(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double t = kk.q(1,3);
    const double u = kk.q(2,3);
    
    
    return 48.0*(-q3*q3/(t*u)-q4*q4/(t*u)
                 +q3*q4/pow(u,2.0)-q3*q4/(t*u)+q3*q4/pow(t,2.0)
                 +q3/u+q3/t+q4/u+q4/t
                 -t/u-1.0-u/t );
}


double GstarGstarBorn::e2(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double t = kk.q(1,3);
    const double u = kk.q(2,3);
    
    return 24.0*(-q3*q4/pow(u,2.0)
                 -2.0*q3*q4/(t*u)-q3*q4/pow(t,2.0)+t/u+2.0+u/t);
}

double GstarGstarBorn::e3(const KinematicInvariants& kk)
{
    return 0.0;
}

void GstarGstarBorn::configure(const double& charge, const double& alpha_em)
{
    const double averaging_factor = 1.0/6.0 * 1.0/6.0 ;    
    prefactor_ = averaging_factor*pow(charge,4.0)
                * pow(alpha_em,2.0)* 0.389379*1.e9;
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


GstarGstarMe::GstarGstarMe()
{
    number_of_particles_ = 6;
    //info_ = new NewMeExternalInfo;
    dimension_ = 4;
    info_.name = "Born";
    info_.ISF = InitialStateFlavors("u","ub");
    pdf_selection_ = "same flavor";
    info_.alpha_power = 0;
    alpha_em = 1.0/137.0 ;
    m3 = 40.0;
    m4 = 50.0;//9.118800e+01;
    
    //kk_.s4= m4*m4;
    smin = pow(m3+m4,2.0);
}
void GstarGstarMe::compute_averaging_charge_and_a_em_prefactor()
{
    
    const double averaging_factor = 1.0/6.0 * 1.0/6.0 ;
    const double charge = 2.0/3.0;
    //const double color_factor = 3.0;
    
    prefactor_ = averaging_factor*pow(charge,4.0)
                * pow(alpha_em,2.0)* 0.389379*1.e9;
}

void GstarGstarMeDelta::Configure()
{
    kk_.SetMassesSquared(m3*m3,m4*m4);
    kk_.SetBoundaries(smin,smax);
    compute_averaging_charge_and_a_em_prefactor();
}

void GstarGstarMeNLOConv::Configure()
{
    kk_.SetMassesSquared(m3*m3,m4*m4);
    kk_.SetBoundaries(smin,smax);
    compute_averaging_charge_and_a_em_prefactor();
}

void GstarGstarMeNLOkinematics::Configure()
{
    kk_.SetMassesSquared(m3*m3,m4*m4);
    kk_.SetBoundaries(smin,smax);
    kk_left_.SetMassesSquared(m3*m3,m4*m4);
    kk_left_.SetBoundaries(smin,smax);
    kk_right_.SetMassesSquared(m3*m3,m4*m4);
    kk_right_.SetBoundaries(smin,smax);
    compute_averaging_charge_and_a_em_prefactor();
}

void GstarGstarMeNNLOHard::Configure()
{
    kk_.SetMassesSquared(m3*m3,m4*m4);
    kk_.SetBoundaries(smin,smax);
    kk_nlo_.SetMassesSquared(m3*m3,m4*m4);
    kk_nlo_.SetBoundaries(smin,smax);
    kk_left_.SetMassesSquared(m3*m3,m4*m4);
    kk_left_.SetBoundaries(smin,smax);
    kk_right_.SetMassesSquared(m3*m3,m4*m4);
    kk_right_.SetBoundaries(smin,smax);
    compute_averaging_charge_and_a_em_prefactor();
}

void GstarGstarMeNNLOMueller::Configure()
{
    kk_.SetMassesSquared(m3*m3,m4*m4);
    kk_.SetBoundaries(smin,smax);
    kk_nlo_.SetMassesSquared(m3*m3,m4*m4);
    kk_nlo_.SetBoundaries(smin,smax);
    kk_left_.SetMassesSquared(m3*m3,m4*m4);
    kk_left_.SetBoundaries(smin,smax);
    kk_right_.SetMassesSquared(m3*m3,m4*m4);
    kk_right_.SetBoundaries(smin,smax);
    compute_averaging_charge_and_a_em_prefactor();
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

double GstarGstarMe::PPt1(const double& x,const double & rho)
{
    return 4.0/3.0 * ((1.0-x)*(1.0-x)*rho+2.0*x)/(1.0-x); 
}

double GstarGstarMe::PPt2(const double& z,const double & rho)
{
    const double zb = 1.0-z;
    const double rb = 1.0-rho;
    return 4.0/3.0 * (zb*zb*(1.0-z*rb/(1.0-zb*rb))+2.0*z)/zb; 
}

/*
double GstarGstarMe::Born(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double q13 = kk.q(1,3);
    const double q23 = kk.q(2,3);


    return 8.0*3.0*(
                q23/q13 - 2.0 * (q3+q4)/q13
                - q3*q4/q13/q13
                +q13/q23 - 2.0 * (q3+q4)/q23
                - q3*q4/q23/q23
                +2.0*pow(q3+q4,2.0)/ (q13*q23)
                );
}

double GstarGstarMe::Born_e(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double t = kk.q(1,3);
    const double u = kk.q(2,3);
    

    return 48.0*(-q3*q3/(t*u)-q4*q4/(t*u)
            +q3*q4/pow(u,2.0)-q3*q4/(t*u)+q3*q4/pow(t,2.0)
            +q3/u+q3/t+q4/u+q4/t
            -t/u-1.0-u/t );
}


double GstarGstarMe::Born_e2(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double t = kk.q(1,3);
    const double u = kk.q(2,3);
    
    return 24.0*(-q3*q4/pow(u,2.0)
                 -2.0*q3*q4/(t*u)-q3*q4/pow(t,2.0)+t/u+2.0+u/t);
}

double GstarGstarMe::Born_e3(const KinematicInvariants& kk)
{
    const double q3 = kk.q(3);
    const double q4 = kk.q(4);
    const double t = kk.q(1,3);
    const double u = kk.q(2,3);
    
    return 0.0;
}
*/

//: reminder R= |Mnlo|^2 / (4*gs^2)
double GstarGstarMe::R(const KinematicInvariants& kk)
{
    const double s3 = kk.s(3);
    const double s4 = kk.s(4);
    const double s12 = kk.s(1,2);
    const double s31 = kk.s(1,3);
    const double s41 = kk.s(1,4);
    const double s51 = kk.s(1,5);
    const double s23 = kk.s(2,3);
    const double s24 = kk.s(2,4);
    const double s25 = kk.s(2,5);
    const double s34 = kk.s(3,4);
    const double s35 = kk.s(3,5);
    const double s45 = kk.s(4,5);

    
    const double Vgs2 = 2.0 ;
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

#include "RR_from_maple.cpp"

void GstarGstarMeDelta::Evaluate(double* xx_vegas)
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

        }
    else
        {
        JF();
        }
    
}



double GstarGstarMELO::eval_me(const KinematicInvariants& kinvar)
{
    return born_(kinvar);
}


GstarGstarMeNLOSoft::GstarGstarMeNLOSoft():GstarGstarMeDelta(){
    info_.alpha_power = 1;
    info_.name = "NLOSoft";

};



double GstarGstarMeNLOSoft::eval_me(const KinematicInvariants& kv)
{
    //There is a factor of 2  from 2*Re(V*conjg(B))
    //and a compensating factor of 1/2 from the virtual being defined in a 
    //a_s/2/pi expansion (we always use a_s/pi here)
    return 1.0*V_.Evaluate(kv) 
            + 4.0/3.0*born_.e2(kv)
            + 4.0/3.0*3.0/2.0*born_.e(kv);
}

double GstarGstarMeNLOSoft::Catani(const KinematicInvariants& kv,int eps)
{
    double res=10.0;
    if (eps==-2)
        {
            const double C2 =  4.0/3.0*(-1.0)*born_(kv);
            res =  V_.EpsilonM2(kv) -C2;
        }
    else if (eps==-1)
        {
            const double C1 =  4.0/3.0*(-3.0/2.0*born_(kv) - 1.0*born_.e(kv));
            res =  V_.EpsilonM1(kv) -C1;
        }
    else
        {
        cout<<"\nError in Catani: you asked for e^"<<eps<<" that is not predicted by Catani";
        }
    return res;
}
//----------------------------------------------------

GstarGstarMeNNLOSoft::GstarGstarMeNNLOSoft():GstarGstarMeDelta(){
    info_.alpha_power = 2;
    info_.name = "NNLOSoft";

};



double GstarGstarMeNNLOSoft::eval_me(const KinematicInvariants& kv)
{
    //There is a factor of 2  from 2*Re(V*conjg(B))
    //and a compensating factor of 1/4 from the virtual being defined in a 
    //a_s/2/pi expansion (we always use a_s/pi here)
    // hence an overall 1/2
//    if ((kv.s13/kv.s12< -0.9) 
//        or (kv.s23/kv.s12<-0.9)
//        or (kv.s12<9000.)) return 0.;

    const double mu=muf_;
    
    //cout<<"\n mu="<<mu;
    //const double mu=sqrt(kv.s12);
    const double L=log(kv.s(1,2)/mu/mu);
    //const double L=log(kv.s12);
    const double B0=born_(kv);
    const double B1=born_.e(kv);
    const double B2=born_.e2(kv);
    const double NF=consts::nf;
    const double VV0 = VV_.Epsilon0(kv);
    const double V0 = V_.Epsilon0(kv);
    const double V1 = V_.EpsilonP1(kv);

    
    const double res= 1./2.*(
            -(2./27.)*NF*B0  *L*L*L
            +(19./27.)*NF*B0*L*L
            +(-(65./81.)*NF*B0+(8./27.)*NF*B1+(2./3.)*NF*B2+(1./3.)*NF*V0)*L
            +(1./54.)*NF*(-5.*consts::pi_square+28.*consts::z3)*B0
            +(1./18.)*B1*NF*consts::pi_square
            +(1./6.)*NF*(2.*V1+3.*VV0)
            );
    if (res!=res) 
    {
    //cout<<"\n"<<kv;
    cout<<"\nError: VVrenormalized is a nan: "<<res<<endl;
    return 0.0;
    }
 
//    if (kv.s13/kv.s12<-0.999 or kv.s23/kv.s12<-0.999){
//        cout<<"\n******************"<<endl;
//        cout<<kv;
//        cout<<"\n t = "<<kv.s13/kv.s12<<"\t u= "<<kv.s23/kv.s12;
//        cout<<"\n--> res="<<res;
//        }
    //return 1./2.*(-2./27.)*NF*B0  *L*L*L;
    return res;
}

double GstarGstarMeNNLOSoft::Catani(const KinematicInvariants& kv,int eps)
{

    double res=10.0;
    if (eps==-3)
    {
        const double C3 =  1.0/9.0*born_(kv) * consts::nf;
        const double pole3 =  VV_.EpsilonM3(kv) * consts::nf /2.0;
//            + consts::nf/3.0 * V_.EpsilonM2(kv)  ;
        //cout<<"\nCatani 1/e^3 : "<< pole3 <<" vs "<<C3<<endl;
        res = pole3 - C3;
    }
    else if (eps==-2)
    {
//    const double C2 = ( -4.0/27.0*born_(kv)
//                    -1.0/3.0*born_.e(kv)) * consts::nf;
    const double C2 = consts::nf*((1./9.)*born_.e(kv)+(14./27.)*born_(kv));    
        res =  VV_.EpsilonM2(kv) * consts::nf /2.0
  //      + consts::nf/3.0 * V_.EpsilonM1(kv)  
        -C2;
    }
    else if (eps==-1)
    {
        const double C1 =  consts::nf*(
                            -(1./3.)*born_.e2(kv)
                            -(4./27.)*born_.e(kv)
                            +(65./162.)*born_(kv)
                            -(1./3.)*V_.Epsilon0(kv)
                            );

        const double pole1 =  VV_.EpsilonM1(kv) * consts::nf /2.0;
        cout<<"\nCatani 1/e^1 : "<< pole1 <<" vs "<<C1<<endl;
        res = pole1-C1;
    }
    else
    {
        cout<<"\nError in Catani: you asked for e^"<<eps<<" that is not predicted by Catani";
    }
    return res;
}

double GstarGstarMeNNLOSoft::VVRenormalized(const KinematicInvariants& kbr)
{
//vv[-3]=1/9*B[0],vv[-2]=1/9*B[1]+14/27*B[0]
    const double Ce2X_ci_mi = consts::nf/2.*(VV_.Epsilon0(kbr)
                                             - 1./2.*consts::pi_square
                               //                     *VV_.EpsilonM2(kbr)
                                *2.*(1./9.*born_.e(kbr) + 14./27.*born_(kbr))
                            -14./3.*consts::z3
                            //*VV_.EpsilonM3(kbr))
                                *2.*1./9.*born_(kbr)
                            );
    
    const double CeX_ci_mi_V = V_.EpsilonP1(kbr)
    -(1./4.)*consts::pi_square
        //*V_.EpsilonM1(kbr)
        *(-4./3.*born_.e(kbr)-2.0*born_(kbr))
    -(7./3.)*consts::z3
        //*V_.EpsilonM2(kbr)
        *(-4.0/3.0)*born_(kbr);
    return Ce2X_ci_mi + consts::nf/3.*CeX_ci_mi_V;
    
    
}
          
//----------------------------------------------------


GstarGstarSoft::GstarGstarSoft():GstarGstarMeDelta(){
    //killing the prefactor_ of the sector
    //necessary if you mix orders of a_s
    info_.alpha_power = 0;
    info_.name = "Total Soft up to O(as^2)";
};



double GstarGstarSoft::eval_me(const KinematicInvariants& kv)
{
    //There is a factor of 2  from 2*Re(V*conjg(B))
    //and a compensating factor of 1/4 from the virtual being defined in a 
    //a_s/2/pi expansion (we always use a_s/pi here)
    // hence an overall 1/2
    
    //const double mu=12.5;
    const double L=log(muf_*muf_/kv.s(1,2));
    const double B0=born_(kv);
    const double B1=born_.e(kv);
    const double B2=born_.e2(kv);
    const double NF=consts::nf;
    const double VV0 = VV_.Epsilon0(kv);
    const double V0 = V_.Epsilon0(kv);
    const double V1 = V_.EpsilonP1(kv);
    
    const double soft_leading_order = B0;
    // soft includes all delta pieces from integrated collinear ccterms
    const double soft_nlo = V0 + 2.*B1 -2.*L*B0+ 4./3. * B2; 
    const double soft_nnlo = 1./2.*NF*(
                                       
                                       +(1./3.)*B0*L*L
                                       -(-(1./9.)*B0
                                         -(4./27.)*consts::pi_square*B0
                                         +(8./27.)*B1
                                         +(2./3.)*B2
                                         +(1./3.)*V0)*L
                                       +(1./54.)*(-5.*consts::pi_square)*B0
                                       +(1./18.)*B1*consts::pi_square
                                       +(1./6.)*(2.*V1+3.*VV0)
                                       +(1./54.)*28.*consts::z3*B0
                                       +(17./54.*4)*consts::z3*B0
                                       -41./81./3.*4.*B0
                                       +(1./54.)*(-5.*consts::pi_square)*B0
                                       +(35./54./3.)*consts::pi_square*B0
                                       );
    return soft_leading_order + a_s_over_pi_*soft_nlo + pow(a_s_over_pi_,2.0) * soft_nnlo;
}

//----------------------------------------------------
      
                
double GstarGstarMeNLOkinematics::collinear_limit(int i)
{
    //reminder: P(1/z) = -1/z * P(z)
    if (i==1)
        return  1.0/2.0*PP(1.0/kk_.z) * born_(kk_left_.invariants())/kk_.s(1,5);
    else if (i==2)
        return 1.0/2.0*PP(1.0/kk_.z) * born_(kk_right_.invariants())/kk_.s(2,5);
    else
        {
        cout<<"\n"<<__func__<<" error: collinear limit asked with id different than 1 or 2"<<endl;
        return 0.0;
        }
}

void GstarGstarMeNLOkinematics::Evaluate(double* xx_vegas)
{
    kk_.generate_kinematics(xx_vegas);
    kk_left_.SetZ(kk_.z);
    kk_left_.generate_kinematics(xx_vegas);
    kk_right_.SetZ(kk_.z);
    kk_right_.generate_kinematics(xx_vegas);

    const double lumi = LL(kk_.x1(),kk_.x2());
    if (lumi!=0.0)
        {
        double me_sq = eval_me(kk_.invariants());
        const double zbar = 1.0-kk_.z;
        const double collinear1 = collinear_limit(1);
        const double collinear2 = collinear_limit(2);
        const double total_factor = prefactor_ * kk_.jacobian
                                        * lumi
                                        * 1.0/2.0/kk_.s(1,2) 
                                        *rescaling_factor(kk_)  ;
                 
        const double ds_R =  total_factor * zbar * me_sq;
        const double ds_c1 = total_factor * zbar * collinear1;
        const double ds_c2 = total_factor * zbar * collinear2;
        
        JF(ds_R , kk_);
        JF(-ds_c1, kk_left_);
        JF(-ds_c2, kk_right_);
        }
    else
        {
        event_box_->AddNewEvent(0.0);
        }
}




double GstarGstarMeNLOHard::rescaling_factor(const KinematicVariables& kk)
{
    return 1.0;
}

double GstarGstarMeNLOHard::eval_me(const KinematicInvariants& kk)
{
    return R(kk);
}


GstarGstarMENLOHardQuarkGluon::GstarGstarMENLOHardQuarkGluon():GstarGstarMe(),kk_(5),kk_right_(4){
    info_.alpha_power = 1;
    dimension_ = 7;
    info_.name = "Quark Gluon NLOHard";
    info_.ISF = InitialStateFlavors("u","g");
    
};

void GstarGstarMENLOHardQuarkGluon::Configure()
{
    kk_.SetMassesSquared(m3*m3,m4*m4);
    kk_.SetBoundaries(smin,smax);
    kk_right_.SetMassesSquared(m3*m3,m4*m4);
    kk_right_.SetBoundaries(smin,smax);
    compute_averaging_charge_and_a_em_prefactor();
}

void GstarGstarMENLOHardQuarkGluon::Evaluate(double* xx_vegas)
{
    kk_.generate_kinematics(xx_vegas);
    
    kk_right_.SetZ(kk_.z);
    kk_right_.generate_kinematics(xx_vegas);
    
    const double lumi = LL(kk_.x1(),kk_.x2());
    if (lumi!=0.0)
    {
        const double correction_for_averaging = 3./8.;
        double me_sq = correction_for_averaging *Rcrossed(kk_.invariants());
        const double zbar = 1.0-kk_.z;
        const double collinear2 = 1./4.
                        *(1.-2.*kk_.z*(1.-kk_.z))/kk_.s(2,5)
                        *born_(kk_right_.invariants())/kk_.z;
        const double total_factor = prefactor_ * kk_.jacobian
        * lumi
        * 1.0/2.0/kk_.s(1,2);
        
        const double ds_R =  total_factor * zbar * me_sq;
        const double ds_c2 = total_factor * zbar * collinear2;
        
        JF(ds_R , kk_);
        JF(-ds_c2, kk_right_);
    }
    else
    {
        event_box_->AddNewEvent(0.0);
    }
}

double GstarGstarMENLOHardQuarkGluon::Rcrossed(const KinematicInvariants& kv)
{
    // crossing 2<->5
    KinematicInvariants kcrossed;
    
    kcrossed.Set(3,kv.s(3));
    kcrossed.Set(4,kv.s(4));
    kcrossed.Set(1,2,-kv.s(1,5));
    kcrossed.Set(1,3,kv.s(1,3));
    kcrossed.Set(1,4,kv.s(1,4));
    kcrossed.Set(1,5,-kv.s(1,2));
    kcrossed.Set(2,3,-kv.s(3,5));
    kcrossed.Set(2,4,-kv.s(4,5));
    kcrossed.Set(2,5,kv.s(2,5));
    kcrossed.Set(3,4,kv.s(3,4));
    kcrossed.Set(3,5,-kv.s(2,3));
    kcrossed.Set(4,5,-kv.s(2,4));
    return R(kcrossed);
}


double GstarGstarMeNNLO_R_remnant::rescaling_factor(const KinematicVariables& kk)
{
    //const double muf=12.5;
    //cout<<"\nmuf="<<muf_;
    const double zbar = 1.0-kk_.z;
    const double lambdabar = 1.-kk_.lambda;
    const double lambda = kk_.lambda;
    const double res= consts::nf/6. * ( -5./3. 
    + log(pow(zbar,2.0) * lambda*lambdabar
                / (1.0-zbar*lambdabar)
                ) 
    - log(muf_*muf_/kk_.s(1,2))
                );
    //cout<<"\nas/pi = "<<a_s_over_pi_;
 //   return 1./a_s_over_p;
    return res;
}


double GstarGstarMeNNLO_IL_Romain::rescaling_factor(const KinematicVariables& kk)
{
    //const double muf=12.5;
    //cout<<"\nmuf="<<muf_;
    const double zbar = 1.0-kk_.z;
    const double lambdabar = 1.-kk_.lambda;
    const double lambda = kk_.lambda;
    const double res= consts::nf/6. * ( -5./3. 
                                       + log(pow(zbar,2.0) * lambda*lambdabar
                                             ) 
                                       - log(muf_*muf_/kk_.s(1,2))
                                       );
    //cout<<"\nas/pi = "<<a_s_over_pi_;
    //   return 1./a_s_over_p;
    return res;
}

double GstarGstarNPlusOne::rescaling_factor(const KinematicVariables& kk)
{
    //const double muf=12.5;
    const double zbar = 1.0-kk_.z;
    const double lambdabar = 1.-kk_.lambda;
    const double lambda = kk_.lambda;
    const double nlo = 1.0;
    const double nnlo= -5./18. 
    + 1./6.*log(pow(zbar,2.0) * lambda*lambdabar
                / (1.0-zbar*lambdabar)
                ) 
    - 1./6.*log(muf_*muf_/kk_.s(1,2));
    
    return a_s_over_pi_ * nlo + pow(a_s_over_pi_,2.0)*nnlo;
}


//----------------------------------------------------

void GstarGstarMeNLOConv::Evaluate(double* xx_vegas)
{
    kk_.generate_kinematics(xx_vegas);
    const double lumi = LL(kk_.x1(),kk_.x2());
    
    //: this z does *not* affect the kinematics it's an integration variable
    //: for the convolution integral
    double z;
    double convolution_jacobian;
    //double muf = kk_.s12;
    double L =  log(muf_*muf_/kk_.s(1,2));
    double delta,D0,D1,reg;
    if (which_leg_==1)
        {
        z = kk_.x1()+(1.0-kk_.x1())*xx_vegas[4];
        const double lumi_x_over_z = LL(kk_.x1()/z,kk_.x2());
        convolution_jacobian =  (1.0-kk_.x1());//from z 
        delta = lumi/convolution_jacobian;
        const double plus = (lumi_x_over_z/z - lumi)/(1.0-z);
        D0 =  plus + delta * log(1.0-kk_.x1());
        D1 = plus * log(1.0-z) + delta * pow(log(1.0-kk_.x1()),2.0)/2.0;
        reg = lumi_x_over_z/z;
        
        }
    else if (which_leg_==2)
        {
            z = kk_.x2()+(1.0-kk_.x2())*xx_vegas[4];
            const double lumi_x_over_z = LL(kk_.x1(),kk_.x2()/z);
            convolution_jacobian =  (1.0-kk_.x2());//from z 
            delta = lumi/convolution_jacobian;
            const double plus = (lumi_x_over_z/z - lumi)/(1.0-z);
            D0 =  plus + delta * log(1.0-kk_.x2());
            D1 = plus * log(1.0-z) + delta * pow(log(1.0-kk_.x2()),2.0)/2.0;
            reg = lumi_x_over_z/z; 
        }
    else
        {
        cout<<"\n[GstarGstarMeNLOConv::Evaluate] Error: you have not defined properly the whicleg_ variable. It is set to "<<which_leg_<<" whereas it should have been 1 or 2"<<endl;
        exit(0); 
        }
    
    const double total_factor = prefactor_ * kk_.jacobian * convolution_jacobian
                * 1.0/2.0/kk_.s(1,2) * born_(kk_.invariants())   ;
    const double CF = 4.0/3.0;
    
    const double res = total_factor 
        * (-1.0/2.0) * CF 
        * ( L * (2.0 * D0  -(1.0 + z) * reg + 3.0/2.0 * delta) 
            -4.0 * D1  +(2.0* (1.0+z)*log(1.0-z) - (1.0 - z)) * reg 
            + consts::pi_square / 4.0 * delta
            ); 
    JF(res,kk_);    
        
}




//----------------------------------------------------


void GstarGstarMeNNLOHard::Evaluate(double* xx_vegas)
{
    
    kk_.generate_kinematics(xx_vegas);
    kk_nlo_.generate_kinematics(xx_vegas);
    kk_left_.SetZ(kk_.z);
    kk_left_.generate_kinematics(xx_vegas);
    kk_right_.SetZ(kk_.z);
    kk_right_.generate_kinematics(xx_vegas);
    const double lumi = LL(kk_.x1(),kk_.x2());
    if (lumi!=0.0)
        {
        const double zbar = 1.0-kk_.z;
        const double rhobar = 1.0-kk_.rho;
        const double lambdabar = 1. - kk_.lambda;
        const double total_factor = lumi * prefactor_ 
                            * kk_.jacobian
                            *zbar/rhobar
                            *1./3./4.*consts::nf 
                            /2./consts::Pi
                            ;
        double double_real = RR(kk_.invariants())/2.0 ;
        
            
        const double s15tilde = -kk_.s(1,2) * (1.-kk_.z)
                                *kk_.lambda;
        const double s25tilde = -kk_.s(1,2) * (1.-kk_.z)
            *(1.-kk_.lambda)*(1.-(1.-kk_.z)*(1.-kk_.rho));
        
//        cout<<"\n"<<kk_.s(1,5)<<" "<<s15tilde
//            <<"\t"<<kk_.s(2,5)<<" "<<s25tilde;
        double triple_col_1 = 1./2.*PPt1(kk_.z,kk_.rho)*born_(kk_left_.invariants())/kk_.z/(-s15tilde) ;
        double triple_col_2 = 1./2.*PPt2(kk_.z,kk_.rho)*born_(kk_right_.invariants())/kk_.z/(-s25tilde) ;
        double single_col = R(kk_nlo_.invariants()) ;
        
        double counter1 = 1./2.*PP(1./kk_nlo_.z) 
            * born_(kk_left_.invariants())
            /(kk_nlo_.s(1,5)) ;
        double counter2 = 1./2.*PP(1./kk_nlo_.z) 
            * born_(kk_right_.invariants())
            /(kk_nlo_.s(2,5)) ;
         
        //the real thing
        JF(double_real*total_factor   ,kk_);       
        JF(-triple_col_1*total_factor,kk_left_);
        JF(-triple_col_2*total_factor,kk_right_);
        JF(-single_col*total_factor,kk_nlo_);
        JF(+counter1*total_factor,kk_left_);
        JF(+counter2*total_factor,kk_right_);

 
    }
    else
    {
        JF();
    }
}

void GstarGstarMeNNLOMueller::Evaluate(double* xx_vegas)
{
    kk_.generate_kinematics(xx_vegas);
    kk_nlo_.generate_kinematics(xx_vegas);
    kk_left_.SetZ(kk_.z);
    kk_left_.generate_kinematics(xx_vegas);
    kk_right_.SetZ(kk_.z);
    kk_right_.generate_kinematics(xx_vegas);
    const double lumi = LL(kk_.x1(),kk_.x2());
    if (lumi!=0.0)
    {        

        const double total_factor = lumi * prefactor_ 
                                    * kk_.jacobianFull();
        const double total_factor_z = lumi * prefactor_ 
                                * kk_.jacobianAtLimits();
        
        double double_real = RR(kk_.invariants())/2.0 ;
        
        
        //note that in the Mueller param s15tilde = s15nlo
        // and s25tilde=s25nlo
        double triple_col_1 = 1./2.*kk_.SplittingKernel()
                        *born_(kk_left_.invariants())/kk_.z
                        /(-kk_nlo_.s(1,5)) ;
        double triple_col_2 = 1./2.*kk_.SplittingKernel()
                        *born_(kk_right_.invariants())/kk_.z
                        /(-kk_nlo_.s(2,5)) ;
        
        double single_col = R(kk_nlo_.invariants()) ;
        
        double counter1 = 1./2.*PP(kk_.z) 
                        * born_(kk_left_.invariants())
                        /kk_.z/(-kk_nlo_.s(1,5)) ;
        double counter2 = 1./2.*PP(kk_.z) 
                        * born_(kk_right_.invariants())
                        /kk_.z/(-kk_nlo_.s(2,5)) ;

        if (double_real!=double_real)
            {
            cout<<kk_<<"\n\t\t\t*** RR = "<<double_real;
            cout<<"\nz="<<kk_.z<<" u="<<kk_.u<<" lambda = "<<kk_.lambda;
            exit(1);
            }
        
        JF(double_real      * total_factor  ,kk_);       
        JF(-triple_col_1    * total_factor_z,kk_left_);
        JF(-triple_col_2    * total_factor_z,kk_right_);
        JF(-single_col      * total_factor_z,kk_nlo_);
        JF(+counter1        * total_factor_z,kk_left_);
        JF(+counter2        * total_factor_z,kk_right_);
        
        
    }
    else
    {
        JF();
    }
}


void GstarGstarMeNNLOConv::set_plus_coefficients(const double& xx4)
{
    const double lumi = LL(kk_.x1(),kk_.x2());
    
    set_parameters_for_plus_coeffs(xx4);
    
    convolution_jacobian =  one_minus_x;//from z 
    delta = lumi/convolution_jacobian;
    const double plus = (lumi_x_over_z/z - lumi)/(1.0-z);
    D0 =  plus + delta * log(one_minus_x);
    D1 = plus * log(1.0-z) 
    + delta * pow(log(one_minus_x),2.0)/2.0;
    D2 = plus * pow(log(1.0-z),2.0) 
    + delta * pow(log(one_minus_x),3.0)/3.0;
    reg = lumi_x_over_z/z;
    
}


double GstarGstarMeNNLOConv::log_delta()
{
    const double log3_delta =  -1./72.;
    const double log2_delta =  -5./72.;
    const double log1_delta =  -7./54.
    +(1./36.)*consts::pi_square;
    const double CF = 4.0/3.0;
    return  CF *( log3_delta * delta * pow(L_,3.)
            +log2_delta * delta * pow(L_,2.)
            +log1_delta * delta * L_ );
}

double GstarGstarMeNNLOConv::log_plus()
{
    const double log2_D0 = 1./12.;
    const double log1_D0 = 5./18.;
    const double log1_D1 = -1./3.;
    const double CF = 4.0/3.0;
    return  CF * ( log2_D0 * D0 * pow(L_,2.)
            +log1_D0 * D0 *L_
            +log1_D1 * D1 *L_ );
}

double GstarGstarMeNNLOConv::log_reg()
{
    const double zsqp1 = z*z+1.;
    const double zp1 = z+1.;
    const double logz = log(z);
    const double logzbar = log(1.-z);
    const double zbar = 1.0-z;
    
    const double log2_reg = -1./24.*zp1;
    const double log1_reg =  1./12.*zsqp1*logz/zbar
    +1./6.*zp1*logzbar
    -1./18.
    -2./9.*z;
    const double CF = 4.0/3.0;
    return CF * ( log2_reg*pow(L_,2.)+log1_reg*L_ ) * reg;
}

double GstarGstarMeNNLOConv::nonlog_delta()
{
    const double CF = 4.0/3.0;
    return CF * 
            (17./72.*consts::z3
            -41./324.
            +35./864.*consts::pi_square
            ) * delta ;
}

double GstarGstarMeNNLOConv::nonlog_plus()
{
    const double nl_D0 = -1./18.*consts::pi_square+7./27.;
    const double nl_D1 = -5./9.;
    const double nl_D2 =  1./3.;
    const double CF = 4.0/3.0;
    return CF*(nl_D0*D0+nl_D1*D1+nl_D2*D2);  
}

double GstarGstarMeNNLOConv::nonlog_reg()
{
    const double zsqp1 = z*z+1.;
    const double zp1 = z+1.;
    const double logz = log(z);
    const double logzbar = log(1.-z);
    const double zbar = 1.0-z;
    const double Li2zbar = real(HPL2(0,1,zbar));
    const double CF = 4.0/3.0;
    return reg *  CF *  ( 
    -1./48.*(zsqp1)*pow(logz,2.0)/zbar
    -1./6.*zsqp1*logzbar/zbar*logz
    +5./72.*zsqp1/zbar*logz
    -1./6.*zp1*pow(logzbar,2.0)
    +(1./9.+4./9.*z)*logzbar
    -1./12.*zsqp1*Li2zbar/zbar
    +1./36.*zp1*consts::pi_square
    -37./216.*z
    -19./216.
                     );
}

void GstarGstarMeNNLOConv::Configure()
{
    kk_.SetMassesSquared(m3*m3,m4*m4);
    kk_.SetBoundaries(smin,smax);
    compute_averaging_charge_and_a_em_prefactor();
}

double GstarGstarMeNNLOConv::
    born_times_prefactor_times_jacobians()
{
    return  prefactor_ 
            * kk_.jacobian 
            * convolution_jacobian
            * 1.0/2.0/kk_.s(1,2) * born_(kk_.invariants());
}

void GstarGstarMeNNLOConv::Evaluate(double* xx_vegas)
{
    kk_.generate_kinematics(xx_vegas);
    
    
    L_ =  log(muf_*muf_/kk_.s(1,2));
    set_plus_coefficients(xx_vegas[4]);
                         
    double res = born_times_prefactor_times_jacobians() 
                *consts::nf
                * (
                log_delta() + log_plus() + log_reg()
                + nonlog_plus() + nonlog_delta() + nonlog_reg()
                + asymmetric_reg_piece()
                  ); 

    JF(res,kk_);    
    
}


void GstarGstarMeNNLOConvLeft::set_parameters_for_plus_coeffs(const double& xx4)
{
    z = kk_.x1()+(1.0-kk_.x1())*xx4;
    lumi_x_over_z = LL(kk_.x1()/z,kk_.x2());
    one_minus_x = 1.0-kk_.x1();
}

void GstarGstarMeNNLOConvRight::set_parameters_for_plus_coeffs(const double& xx4)
{
    z = kk_.x2()+(1.0-kk_.x2())*xx4;
    lumi_x_over_z = LL(kk_.x1(),kk_.x2()/z);
    one_minus_x =  (1.0-kk_.x2());
}

double GstarGstarMeNNLOConvLeft::asymmetric_reg_piece()
{
    return 0.0;
}


double GstarGstarMeNNLOConvRight::asymmetric_reg_piece()
{
    const double logz = log(z);
    const double zbar = 1.0-z;
    const double zsqp1 = z*z+1.;
    const double Li2zbar = real(HPL2(0,1,zbar));
    const double CF = 4.0/3.0;
    return - CF/12. * (logz-zsqp1*Li2zbar/zbar+zbar) * reg;
}



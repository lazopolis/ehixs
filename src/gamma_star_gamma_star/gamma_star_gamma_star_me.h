#ifndef GSTARGSTARMEDELTA_H
#define GSTARGSTARMEDELTA_H

#include "convolutions.h" // for FF


class FMomentum{
public:
    FMomentum(){for (int i=0;i<4;i++) p[i]=0.0;}
    double p[4];
    void Set(const double& E,const double& px,const double& py, const double& pz){p[0]=E;p[1]=px;p[2]=py;p[3]=pz;}
    void equal(const FMomentum& k)
            {p[0]=k[0];p[1]=k[1];p[2]=k[2];p[3]=k[3];}
    double operator[](int i) const {return p[i];}
    void zboost(const double& b);
    void boost(const double& bx,const double& by,const double& bz);
    double operator*(const FMomentum& Q){return p[0]*Q[0]-p[1]*Q[1]-p[2]*Q[2]-p[3]*Q[3];}
    friend ostream& operator<<(ostream&, const FMomentum&);
};

class KinematicVariables{
public:
    double s12,s13,s23,s14,s24,s34,s15,s25,s35,s45;
    double x1,x2;
    double z;
    double lambda;
    double phi;
    double s3,s4;
    double tau,smax,smin;

    FMomentum p3com;
    FMomentum p4com;


    FMomentum p1;
    FMomentum p2;
    FMomentum p3;
    FMomentum p4;
    FMomentum p5;
    FMomentum p6;

    
public:
    void boost_to_lab();
    void boost_along_z_axis(const double& bb);
    void SetLOKinematics(double* xx_vegas);
    void SetNLOKinematics(double* xx_vegas);
    KinematicVariables single_collinear(int);
    KinematicVariables z_shifted(int i) const;
    void compute_born_invariants();
    void generate_bjorken_xs(double* xx_vegas);

    double jacobian;

};

class GstarGstarMe: public NewMatrixElement
{
public:
    GstarGstarMe(EventBox& event_box);
    void consolidate();

    double Born(const KinematicVariables& kk);
    double PP(const double&);
protected:
    void generate_resolved_kinematics(double* xx_vegas,const double Qsq);
    void JF(const double&,const KinematicVariables& kv);
    void JF();
    
protected:
    KinematicVariables kk_;
    
    double smin;
    double alpha_em;
    double prefactor_;
};

class GstarGstarMeDelta: public GstarGstarMe
{
public:
    GstarGstarMeDelta(EventBox& event_box):GstarGstarMe(event_box){};
    void Evaluate(double* xx_vegas);
    virtual double eval_me(const KinematicVariables&)=0;
};


class GstarGstarMELO : public GstarGstarMeDelta
{
public:
    GstarGstarMELO(EventBox& event_box):GstarGstarMeDelta(event_box){};
    double eval_me(const KinematicVariables&);
};

class GstarGstarMeNLOSoft: public GstarGstarMeDelta
{
public:
    GstarGstarMeNLOSoft(EventBox& event_box):GstarGstarMeDelta(event_box){
        info_->alpha_power = 1;
        info_->name = "NLOSoft";
        
    };
    double polylog(int i,const complex<double>& z);
    double eval_me(const KinematicVariables&);
    double Vimag(const double& s12,const double& s23,
                        const double& s31,const double& s3,
                        const double& s4,const double& z,
                        const double& zp,const double& CF,
                           const double& Nc);
    double Vreal(const double& s12,const double& s23,
                           const double& s31,const double& s3,
                           const double& s4,const double& z,
                           const double& zp,const double& CF,
                           const double& Nc);

};

class GstarGstarMeNNLOSoft: public GstarGstarMeDelta
{
public:
    GstarGstarMeNNLOSoft(EventBox& event_box):GstarGstarMeDelta(event_box){
        info_->alpha_power = 2;
        info_->name = "NNLOSoft";
        
    };
    double polylog(int i,const complex<double>& z);
    double eval_me(const KinematicVariables&);
    double QQoc(double z,double zp,double w);
    double Qoch(double z,double zp,double w);
    double Qoclow(double z,double zp,double w);
    double Qoczero(double z,double zp,double w);
    double QQec(double z,double zp,double w);
    double Qech(double z,double zp,double w);
    double Qeclow(double z,double zp,double w);
    double Qeczero(double z,double zp,double w);
    double TT37 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT36 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT35 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT34 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT33 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT32 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT31 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT30 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT29 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT28 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT27 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT26 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT25 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT24 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT23 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT22 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT21 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT20 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT19 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT18 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT17 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT16 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT15 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT14 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT13 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    double TT12 (double u,double t,double q3,double q4,double z,
                 double zp,double Nc,double Nf,double CF);
    
};

class GstarGstarMeNLOkinematics : public GstarGstarMe
{
public:
    void generate_unresolved_kinematics(double* xx_vegas);
    
    GstarGstarMeNLOkinematics(EventBox& event_box)
        :GstarGstarMe(event_box)
        {
        info_->alpha_power = 1;
        info_->name = "NLOHard";
        dimension_ = 7;
        };
    void Evaluate(double* xx_vegas);
    double collinear_limit(const KinematicVariables&,int i);

    virtual double eval_me(const KinematicVariables& )=0;
protected:
    double s15;
    double s25;
    double s35;
    double s45;
};


class GstarGstarMeNLOHard: public GstarGstarMeNLOkinematics
{
public:
    GstarGstarMeNLOHard(EventBox& event_box):GstarGstarMeNLOkinematics(event_box){
        info_->alpha_power = 1;
        info_->name = "NLOHard";
        
    };
    complex<double> polylog(int i,const double& z);
    double eval_me(const KinematicVariables&);
};






#endif

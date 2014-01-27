#ifndef GSTARGSTARMEDELTA_H
#define GSTARGSTARMEDELTA_H

#include "convolutions.h" // for FF


class FMomentum{
public:
    FMomentum(){for (int i=0;i<4;i++) p[i]=0.0;}
    double p[4];
    void Set(const double& E,const double& px,const double& py, const double& pz){p[0]=E;p[1]=px;p[2]=py;p[3]=pz;}
    double operator[](int i){return p[i];}
};

class KinematicVariables{
public:
    double s12,s13,s23,s14,s24,s34,s15,s25,s35,s45;
    double x1,x2;
    double z;
    double lambda;
    double phi;
    double s3,s4;
    double tau,smax;
    FMomentum p1com;
    FMomentum p2com;
    FMomentum p3com;
    FMomentum p4com;
    FMomentum p5com;
    FMomentum p6com;

    FMomentum p1;
    FMomentum p2;
    FMomentum p3;
    FMomentum p4;
    FMomentum p5;
    FMomentum p6;

    
    
    void boost_to_lab();
    void boost_along_z_axis(const double bb);
    void SetLOKinematics(double* xx_vegas);
    void SetNLOKinematics(double* xx_vegas);
    KinematicVariables single_collinear(int);
    void generate_bjorken_xs(double* xx_vegas);

    double jacobian;

};


class GstarGstarMe: public NewMatrixElement
{
public:
    GstarGstarMe(EventBox& event_box);
    void consolidate();

protected:
    void generate_resolved_kinematics(double* xx_vegas,const double Qsq);
    void JF(const double&,const KinematicVariables& kv);
    void JF();
    double Born(const KinematicVariables& kk);
    double PP(const double&);
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
    complex<double> polylog(int i,const double& z);
    double eval_me(const KinematicVariables&);
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

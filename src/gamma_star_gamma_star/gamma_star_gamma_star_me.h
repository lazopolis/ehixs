#ifndef GSTARGSTARMEDELTA_H
#define GSTARGSTARMEDELTA_H

#include "convolutions.h" // for FF
#include "kinematic_variables.h"


class GstarGstarBorn
{
public:
    void configure(const double& charge, const double& alpha_em);
    double operator()(const KinematicInvariants& kk);
    double e(const KinematicInvariants& kk);
    double e2(const KinematicInvariants& kk);
    double e3(const KinematicInvariants& kk);
private:
    double prefactor_;
};



class CrossSection
{
public:
    virtual void Evaluate(double*)=0;
    virtual void Configure()=0;
    void PassEColliderSq(const double& smaximum)
    {smax=smaximum;}
    void AllocateLuminosity(Luminosity* lumi)
    {
        lumi_box_.MatchPDFs(info_.ISF.left, info_.ISF.right,pdf_selection_);
        lumi_box_.AllocateLuminosity(lumi);
    }
    void PassAlphaStrong(const double& a_s_over_pi)
    {
    a_s_over_pi_=a_s_over_pi;
    cout<<"\n[CrossSection]: a_s = "<<a_s_over_pi_* consts::Pi;
    }
    void PassScales(const double& mur,const double& muf)
    {mur_=mur; muf_=muf;}
    void SetEventBox(EventBox& event_box)
    {event_box_=&event_box;}
    int Dimension(){return dimension_;}
    
    //specific to gstar^2
    virtual void PassMasses(const double& m3, const double& m4)=0;
    
    friend ostream& operator<<(ostream& stream, const CrossSection&);
protected:
    NewMeExternalInfo info_;
    int dimension_;
    EventBox* event_box_;
    
    double smax;
    string pdf_selection_;
    double a_s_over_pi_;
    double mur_;
    double muf_;
    LuminosityBox lumi_box_;
    string name_;
protected:
    double LL(const double& x1,const double& x2)
    {
        
        return lumi_box_.give(x1,x2)
        *pow(a_s_over_pi_,info_.alpha_power);
    }
};

class GstarGstarMe: public CrossSection
{
public:
    GstarGstarMe();
    void compute_averaging_charge_and_a_em_prefactor();

    //refactor: m3 -> m3_
    void PassMasses(const double& mm3, const double& mm4)
        {m3=mm3;m4=mm4;
        cout<<"\n[GstarGstarMe]:Setting masses m3="<<m3<<" m4="<<m4<<endl;}
    double R(const KinematicInvariants& kk);
    double RR(const KinematicInvariants& kk);
    double PP(const double&);
    double PPt1(const double& x,const double & rho);
    double PPt2(const double& x,const double & rho);

    // public to be accessible from tests (refactor here)
    GstarGstarBorn born_;
protected:
   // void generate_resolved_kinematics(double* xx_vegas,const double Qsq);
    void JF(const double&,const KinematicVariables& kv);
    void JF();
    
protected:
    int number_of_particles_;
    double smin;
    double m3,m4;
    double alpha_em;
    double prefactor_;
    
};

class GstarGstarMeDelta: public GstarGstarMe
{
public:
    GstarGstarMeDelta():GstarGstarMe(),kk_(4){};
    void Configure();
    void Evaluate(double* xx_vegas);
    virtual double eval_me(const KinematicInvariants&)=0;
protected:
    LOKinematics kk_;
};


class GstarGstarMELO : public GstarGstarMeDelta
{
public:
    GstarGstarMELO():GstarGstarMeDelta(){};
    double eval_me(const KinematicInvariants&);
};

typedef double (*ptr_to_coeff)(const double&,const double&,const double&,const double&);

#include "virtual_amplitude.h"

class GstarGstarMeNLOSoft: public GstarGstarMeDelta
{
public:
    GstarGstarMeNLOSoft();
    double eval_me(const KinematicInvariants&);
    double Catani(const KinematicInvariants& kv,int eps);
private:
    GstarVirtual V_;
};

                                 
class GstarGstarMeNNLOSoft: public GstarGstarMeDelta
{
public:
    GstarGstarMeNNLOSoft();
    double eval_me(const KinematicInvariants&);
    double Catani(const KinematicInvariants& kv,int eps);
    double VVRenormalized(const KinematicInvariants& kv);
private:
    GstarVirtual V_;
    GstarVirtualVirtual VV_;
  
};

class GstarGstarSoft : public GstarGstarMeDelta
{
public:
    GstarGstarSoft();
    double eval_me(const KinematicInvariants&);
private:
    GstarVirtual V_;
    GstarVirtualVirtual VV_;
};

//----------------------------------------------------

class GstarGstarMeNLOkinematics : public GstarGstarMe
{
public:
    GstarGstarMeNLOkinematics()
        :GstarGstarMe(),kk_(5),kk_left_(4),kk_right_(4)
        {
        info_.alpha_power = 1;
        info_.name = "NLOHard";
        dimension_ = 7;
        };
    void Evaluate(double* xx_vegas);
    void Configure();

    double collinear_limit(int i);

    virtual double eval_me(const KinematicInvariants& )=0;
    virtual double rescaling_factor(const KinematicVariables&)=0;
protected:
    NLOKinematics kk_;
    LOKinematicsShiftedLeft  kk_left_;
    LOKinematicsShiftedRight kk_right_;
    double s15;
    double s25;
    double s35;
    double s45;
};


class GstarGstarMeNLOHard: public GstarGstarMeNLOkinematics
{
public:
    GstarGstarMeNLOHard():GstarGstarMeNLOkinematics(){
        info_.alpha_power = 1;
        info_.name = "NLOHard";
        
    };
    complex<double> polylog(int i,const double& z);
    double eval_me(const KinematicInvariants&);
    virtual double rescaling_factor(const KinematicVariables&);
};

class GstarGstarMENLOHardQuarkGluon : public GstarGstarMe
{
public:
    GstarGstarMENLOHardQuarkGluon();
    void Evaluate(double* xx_vegas);
    void Configure();
private:
    NLOKinematics kk_;
    LOKinematicsShiftedRight kk_right_;
public:
double Rcrossed(const KinematicInvariants& kv);

};


class GstarGstarNPlusOne: public GstarGstarMeNLOHard
{
public:
    GstarGstarNPlusOne():GstarGstarMeNLOHard(){
        info_.alpha_power = 0;
        info_.name = "N+1 up to O(as^2)";
        
    };
    double rescaling_factor(const KinematicVariables&);
};

//----------------------------------------------------

class GstarGstarMeNLOConv : public GstarGstarMe
{
public:
    GstarGstarMeNLOConv(int which_leg)
    :GstarGstarMe(),kk_(4)
    {
        info_.alpha_power = 1;
        stringstream ss;
        ss << "NLOConv"<<which_leg;
        info_.name = ss.str();
        dimension_ = 5;
        which_leg_ = which_leg;
    };
    void Evaluate(double* xx_vegas);
    void Configure();
private:
    int which_leg_;
    LOKinematics kk_;
};

class GstarGstarMeNLOConvQuarkGluon : public GstarGstarMe
{
public:
    GstarGstarMeNLOConvQuarkGluon()
    :GstarGstarMe(),kk_(4)
    {
        info_.alpha_power = 1;
        stringstream ss;
        ss << "NLOConv Quark Gluon";
        info_.name = ss.str();
        dimension_ = 5;
        info_.ISF = InitialStateFlavors("q","g");
        pdf_selection_ = "crossed_charged";


    };
    void Evaluate(double* xx_vegas);
    void Configure();
private:
    LOKinematics kk_;
};


//----------------------------------------------------

class GstarGstarMeNNLOHard: public GstarGstarMe
{
public:
    GstarGstarMeNNLOHard()
    :GstarGstarMe(),kk_(5),kk_nlo_(5),kk_left_(4),kk_right_(4)
    {
        info_.alpha_power = 2;
        info_.name = "NNLOHard";
        dimension_ = 8;
    };
    void Configure();
    void Evaluate(double* xx_vegas);
protected:
    NNLOKinematics kk_;
    NLOKinematics kk_nlo_;
    LOKinematicsShiftedLeft  kk_left_;
    LOKinematicsShiftedRight kk_right_;
};

class GstarGstarMeNNLOMueller: public GstarGstarMe
{
public:
    GstarGstarMeNNLOMueller()
    :GstarGstarMe(),kk_(5),kk_nlo_(5),kk_left_(4),kk_right_(4)
    {
        info_.alpha_power = 2;
        info_.name = "NNLOHard Mueller";
        dimension_ = 8;
    };
    void Evaluate(double* xx_vegas);
    void Configure();

protected:
    NNLOKinematicsMueller kk_;
    NLOKinematics kk_nlo_;
    LOKinematicsShiftedLeft  kk_left_;
    LOKinematicsShiftedRight kk_right_;
};


class GstarGstarMeNNLOConv : public GstarGstarMe
{
public:
    GstarGstarMeNNLOConv()
    :GstarGstarMe(),kk_(4)
    {
        info_.alpha_power = 2;
        
        dimension_ = 5;
    };
    void Evaluate(double* xx_vegas);
    void Configure();
protected:
    double delta,D0,D1,D2,reg;
    double convolution_jacobian;
    //: this z does *not* affect the kinematics it's an integration variable
    //: for the convolution integral
    double z;
    double one_minus_x;
    double lumi_x_over_z;
    double L_;
    LOKinematics kk_;
protected:
    void set_plus_coefficients(const double& xx4);

    double born_times_prefactor_times_jacobians();
    double log_delta();
    double log_plus();
    double log_reg();
    double reg_partial_bfkl_log_from_plus();
    double nonlog_delta();
    double nonlog_plus();
    double nonlog_reg();
    
    void virtual set_parameters_for_plus_coeffs(const double& xx4)=0;
    double virtual asymmetric_reg_piece()=0;


};


class GstarGstarMeNNLOConvLeft : public GstarGstarMeNNLOConv
{
public:
    GstarGstarMeNNLOConvLeft()
    :GstarGstarMeNNLOConv()
    {
        stringstream ss;
        ss << "NNLOConvLeft";
        info_.name = ss.str();
    }
private:    
    void set_parameters_for_plus_coeffs(const double& xx4);
    double  asymmetric_reg_piece();

};

class GstarGstarMeNNLOConvRight : public GstarGstarMeNNLOConv
{
public:
    GstarGstarMeNNLOConvRight()
    :GstarGstarMeNNLOConv()
    {
        stringstream ss;
        ss << "NNLOConvRight";
        info_.name = ss.str();
    }
    
private:
    void set_parameters_for_plus_coeffs(const double& xx4);
    double  asymmetric_reg_piece();

};


class GstarGstarMeNNLO_R_remnant: public GstarGstarMeNLOHard
{
public:
    GstarGstarMeNNLO_R_remnant():GstarGstarMeNLOHard(){
        info_.alpha_power = 2;
        info_.name = "NNLO Single Collinear Counterterm";
        
    };
    double rescaling_factor(const KinematicVariables&);
};

class GstarGstarMeNNLO_IL_Romain: public GstarGstarMeNLOHard
{
public:
    GstarGstarMeNNLO_IL_Romain():GstarGstarMeNLOHard(){
        info_.alpha_power = 2;
        info_.name = "NNLO Infamous Log in Mueller parametrization";
        
    };
    double rescaling_factor(const KinematicVariables&);
};





#endif

#ifndef GSTARGSTARMEDELTA_H
#define GSTARGSTARMEDELTA_H

#include "convolutions.h" // for FF

double qq2gaga(const double&s12,
               const double& s13,
               const double& s23,const double& s3,const double& s4);

class GstarGstarMe: public NewMatrixElement
{
public:
    GstarGstarMe(EventBox& event_box):NewMatrixElement(event_box)
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
        const double m3=20.0;
        const double m4=30.0;
        s3=m3*m3;
        s4=m4*m4;
        smin = s3+s4+2.0*m3*m4;
        
        
    }
    void consolidate()
    {
        tau = smin/smax; // smax is a protected member of the base class
                         //cout<<"\n tau= "<<tau<<" smax = "<<smax<<endl;
        const double averaging_factor = 1.0/6.0 * 1.0/6.0 ;
        const double color_factor = 3.0;
        const double const_part_of_jacobian = 
        (1.0-tau) 
        *(1.0-log(tau))
        * 4.0 * consts::Pi;
        prefactor_ = averaging_factor*color_factor
        * pow(alpha_em,2.0) 
        * const_part_of_jacobian
        *0.389379*1e9;
    }
    void generate_bjorken_xs(double* xx_vegas)
    {
        const double y= log(tau) + (1.0-log(tau))*xx_vegas[0];
        const double u = exp(y);
        const double rho = 1.0/2.0*log(u) -  log(u) * xx_vegas[1];
        x1 = sqrt(u)*exp(rho);
        x2 = sqrt(u)*exp(-rho);
        varying_part_of_jacobian_ = -log(u)*u;
    }
    void generate_resolved_kinematics(double* xx_vegas,const double Qsq)
    {
        const double costheta = -1.0 + 2.0 * xx_vegas[2];
        const double phi = 2.0 * consts::Pi * xx_vegas[3];
        const double sintheta = sqrt(1.0-costheta*costheta);
    double A = - (Qsq-s3-s4);
    double Kaellen = Qsq*Qsq + s3*s3 + s4*s4
    -2.0*Qsq*s3-2.0*Qsq*s4-2.0*s3*s4;
    
    double s13 = A/2.0 + sqrt(Kaellen)/2.0 * costheta;
    double s23 = A/2.0 - sqrt(Kaellen)/2.0 * costheta;
    // cout<<"\n"<<varying_part_of_jacobian<<" "<<me_sq;
    //Energies and z-momenta at the COM frame
    const double E3 = (Qsq+s3-s4)/2.0/sqrt(Qsq);
    const double E4 = (Qsq+s4-s3)/2.0/sqrt(Qsq);
    const double p3 = sqrt(Kaellen)/2.0/sqrt(Qsq);
    const double p3z = p3 * costheta;
    //cout<<"\np3z="<<p3z;
    const double p4z = -p3z;
    const double p3xCOM = p3 * sintheta * sin(phi);
    const double p3yCOM = p3 * sintheta * cos(phi);
    // boost factor for boosting back to LAB frame
    const double bb = (x2-x1)/(x2+x1);
    const double gb = 1.0/sqrt(1.0-bb*bb);
    // boosting to LAB frame (the transverse pieces are invariant)
    const double E3lab = gb * E3 - bb * gb * p3z;
    const double p3zlab = gb * p3z -bb * gb * E3;
    const double E4lab = gb * E4 - bb * gb * p4z;
    const double p4zlab = gb * p4z -bb * gb * E4;
    // assigning the event
    event_box_->AddNewEvent(0.0);//we need a weight value to initialize the event: typical case for refactoring
    event_box_->SetP(1,x1*sqrt(smax)/2.0,0.0,0.0,x1*sqrt(smax)/2.0);
    event_box_->SetP(2,x2*sqrt(smax)/2.0,0.0,0.0,-x2*sqrt(smax)/2.0);
    event_box_->SetP(3,E3lab,p3xCOM,p3yCOM,p3zlab);
    event_box_->SetP(4,E4lab,-p3xCOM,-p3yCOM,p4zlab);
    event_box_->SetP(5,0.0,0.0,0.0,0.0);
    event_box_->SetP(6,0.0,0.0,0.0,0.0);
    
    }
    
    
    
protected:
    double s12;
    double s13;
    double s23;
    double s14;
    double s24;
    double s34;
    double s3;
    double s4;
    double x1;
    double x2;
    double smin;
    double tau;
    double alpha_em;
    double prefactor_;
    double varying_part_of_jacobian_;
};

class GstarGstarMeDelta: public GstarGstarMe
{
public:
    GstarGstarMeDelta(EventBox& event_box):GstarGstarMe(event_box){};
        void Evaluate(double* xx_vegas)
    {
    generate_bjorken_xs(xx_vegas);
    const double lumi = LL(x1,x2);
    if (lumi!=0.0)
        {
        const double Qsq = x1*x2*smax;// born kinematics: z=1
        s12 = Qsq;// born kinematics: z=1
        generate_resolved_kinematics(xx_vegas,Qsq);
        
        double me_sq = eval_me(Qsq,s13,s23,s3,s4);
        
        
        const double sigma = prefactor_
                            * LL(x1,x2)
                            * varying_part_of_jacobian_
                            * 1.0/2.0/s12
                            * me_sq
                            ;
        event_box_->SetWeight(sigma);
        }
    else
        {
        event_box_->AddNewEvent(0.0);
        
        }
    
    }
    virtual double eval_me(const double&,
                   const double& ,
                   const double& ,const double& ,const double& )=0;
};


class GstarGstarMELO : public GstarGstarMeDelta
{
public:
    GstarGstarMELO(EventBox& event_box):GstarGstarMeDelta(event_box){};
    double eval_me(const double&,
                          const double& ,
                          const double& ,const double& ,const double&);
};

class GstarGstarMeNLOSoft: public GstarGstarMeDelta
{
public:
    GstarGstarMeNLOSoft(EventBox& event_box):GstarGstarMeDelta(event_box){
        info_->alpha_power = 1;
        info_->name = "NLOSoft";
        
    };
    complex<double> polylog(int i,const double& z);
    double eval_me(const double&,
                   const double& ,
                   const double& ,const double& ,const double&);
};


class GstarGstarMeNLOkinematics : public GstarGstarMe
{
public:
    void generate_unresolved_kinematics(double* xx_vegas)
    {
    cout<<"\n not fixed yet"<<endl;
    }

    GstarGstarMeNLOkinematics(EventBox& event_box):GstarGstarMe(event_box)
        {
        info_->alpha_power = 1;
        info_->name = "NLOHard";
        dimension_ = 7;
        };
    void Evaluate(double* xx_vegas)
    {
    generate_bjorken_xs(xx_vegas);
    const double lumi = LL(x1,x2);
    if (lumi!=0.0)
        {
        const double z = xx_vegas[4];
        const double Qsq = z*x1*x2*smax;// nlo kinematics: z!=1
        s12 = x1*x2*smax;
        generate_resolved_kinematics(xx_vegas,Qsq);
        generate_unresolved_kinematics(xx_vegas);
        
        double me_sq = eval_me(s12,s13,s23,s14,s24,s34,s15,s25,s35,s45,s3,s4);
        
        
        const double sigma = prefactor_
        * LL(x1,x2)
        * varying_part_of_jacobian_
        * 1.0/2.0/s12
        * me_sq
        ;
        event_box_->SetWeight(sigma);
        }
    else
        {
        event_box_->AddNewEvent(0.0);
        
        }
    
    }
    virtual double eval_me(const double&,
                           const double& ,
                           const double& ,const double& ,const double&
                           ,const double& ,const double& ,const double&
                           ,const double& ,const double& ,const double&
                           ,const double& )=0;
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
    double eval_me(const double&,
                   const double& ,
                   const double& ,const double& ,const double&
                   ,const double& ,const double& ,const double&
                   ,const double& ,const double& ,const double&
                   ,const double& );
};






#endif

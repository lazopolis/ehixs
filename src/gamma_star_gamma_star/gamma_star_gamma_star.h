#ifndef GAMMA_STAR_GAMMA_STAR_H
#define GAMMA_STAR_GAMMA_STAR_H

#include "production.h"
#include "convolutions.h" // for FF


class GstarGstarMeDelta: public NewMatrixElement
{
public:
    GstarGstarMeDelta(EventBox& event_box):NewMatrixElement(event_box)
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
    void Evaluate(double* xx_vegas)
        {
        const double y= log(tau) + (1.0-log(tau))*xx_vegas[0];
        const double u = exp(y);
        //cout<<"\n u="<<u<<" tau="<<tau;
        const double rho = 1.0/2.0*log(u) -  log(u) * xx_vegas[1];
        const double x1 = sqrt(u)*exp(rho);
        const double x2 = sqrt(u)*exp(-rho);
        
        const double lumi = LL(x1,x2);
        if (lumi!=0.0)
        {
        
        const double costheta = -1.0 + 2.0 * xx_vegas[2];
        const double phi = 2.0 * consts::Pi * xx_vegas[3];
        const double sintheta = sqrt(1.0-costheta*costheta);
        const double varying_part_of_jacobian = -log(u)*u;
        
        double s12 = x1*x2*smax;
        
        double A = - (s12-s3-s4);
        double Kaellen = s12*s12 + s3*s3 + s4*s4
                        -2.0*s12*s3-2.0*s12*s4-2.0*s3*s4;
        
        double s13 = A/2.0 + sqrt(Kaellen)/2.0 * costheta;
        double s23 = A/2.0 - sqrt(Kaellen)/2.0 * costheta;
        
        double me_sq = matrix_element_squared(s12,s13,s23);
        
    
        const double sigma = prefactor_ 
                            * LL(x1,x2) 
                            * varying_part_of_jacobian
                            * 1.0/2.0/s12 
                            * me_sq
                            ;
       // cout<<"\n"<<varying_part_of_jacobian<<" "<<me_sq;
        //Energies and z-momenta at the COM frame
        const double E3 = (s12+s3-s4)/2.0/sqrt(s12);
        const double E4 = (s12+s4-s3)/2.0/sqrt(s12);
        const double p3 = sqrt(Kaellen)/2.0/sqrt(s12);
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
        event_box_->AddNewEvent(sigma);
        event_box_->SetP(1,x1*sqrt(smax)/2.0,0.0,0.0,x1*sqrt(smax)/2.0);
        event_box_->SetP(2,x2*sqrt(smax)/2.0,0.0,0.0,-x2*sqrt(smax)/2.0);
        event_box_->SetP(3,E3,p3xCOM,p3yCOM,p3z);
        event_box_->SetP(4,E4lab,-p3xCOM,-p3yCOM,p4zlab);
        event_box_->SetP(5,0.0,0.0,0.0,0.0);
        event_box_->SetP(6,0.0,0.0,0.0,0.0);
        }
        else
        {
            event_box_->AddNewEvent(0.0);
            
        }

        }

    virtual double matrix_element_squared(const double&,const double&,
                                          const double&)=0;
protected:
    double s3;
    double s4;
    double smin;
    double tau;
    double alpha_em;
    double prefactor_;
};

class GstarGstarMeLO: public GstarGstarMeDelta
{
public:
    GstarGstarMeLO(EventBox& event_box):GstarGstarMeDelta(event_box){};
    double matrix_element_squared(const double& s12,const double& s13,
                                  const double& s23)
    {
        return 8.0*(
                            s23/s13 - 2.0 * (s3+s4)/s13 - s3*s4/s13/s13
                            +s13/s23 - 2.0 * (s3+s4)/s23 - s3*s4/s23/s23
                            +2.0*pow(s3+s4,2.0)/ (s13*s23)
                            );
    }

};

class GstarGstarMeNLOSoft: public GstarGstarMeDelta
{
public:
    GstarGstarMeNLOSoft(EventBox& event_box):GstarGstarMeDelta(event_box){
        info_->alpha_power = 1;
        info_->name = "NLOSoft";

        };

    double matrix_element_squared(const double& s12,const double& s13,
                                  const double& s23)
    {
        return 8.0*(
                    s23/s13 - 2.0 * (s3+s4)/s13 - s3*s4/s13/s13
                    +s13/s23 - 2.0 * (s3+s4)/s23 - s3*s4/s23/s23
                    +2.0*pow(s3+s4,2.0)/ (s13*s23)
                    );
    }
    
};




class GammaStarGammaStar : public Production
{
public:
    GammaStarGammaStar(const UserInterface & UI);
    ~GammaStarGammaStar();
    
    //: functions
    void SetDecayParticleIdInEventBox(){event_box.SetDecayParticleId(3);}
    int number_of_necessary_sectors(){return number_of_necessary_sectors_;}
    
    void PassParametersToMatrixElements(const double& s);

    
    vector<string> give_sector_names(const string & pleft,
                                     const string & pright,
                                     const string & myorder,
                                     const int & ep_power,
                                     const string & me_approx)

        {
        string res = "we don't give no names in gamma* gamma*";
        vector<string> res2;
        res2.push_back(res);
        return res2;
        }
    
    
    void evaluate_sector();
    string sector_name(){return my_sector_name;}
    void SetNumberOfParticles() {event_box.SetNumberOfParticles(6);}
    
    //: gluon fusion specific construction:  to be removed!
    void book_production_event(const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                               const double &){};
    //: public to integrate with fortran Fjet
    
    void create_matrix_elements();
    
private:
    SectorBox* all_sectors;
    vector<NewMatrixElement*> available_matrix_elements;
    void find_sector(const UserInterface & UI);
    void allocate_luminosity();
    Sector* the_sector;
    int number_of_necessary_sectors_;

};

#endif
#ifndef GAMMA_STAR_GAMMA_STAR_H
#define GAMMA_STAR_GAMMA_STAR_H

#include "production.h"
#include "convolutions.h" // for FF


class GstarGstarMeLO: public NewMatrixElement
{
public:
    GstarGstarMeLO(EventBox& event_box):NewMatrixElement(event_box)
        {
        info_ = new NewMeExternalInfo;
        dimension_ = 3;
        info_->name = "Born";
        info_->ISF = InitialStateFlavors("up","upbar");
        pdf_selection_ = "same flavor";
        info_->alpha_power = 0;
        info_->epsilon_power_min = 0;
        info_->epsilon_power_max = 2;
        }
    void Evaluate(double* xx_vegas)
        {
        lumi_->set_cur_lumi(xx_vegas[0],xx_vegas[1]);
        double factor_for_ME =   initial_state_jacobian_
        *lumi_->LL(0)
        *prefactor_;
        
        event_box_->AddNewEvent(factor_for_ME);
//        event_box_->SetP(1,x[0]*Etot/2.0,0.0,0.0,x[0]*Etot/2.0);
//        event_box_->SetP(2,x[1]*Etot/2.0,0.0,0.0,-x[1]*Etot/2.0);
//        event_box_->SetP(3,0.0,0.0,0.0,0.0);
//        event_box_->SetP(4,0.0,0.0,0.0,0.0);
//        event_box_->SetP(5,0.0,0.0,0.0,0.0);
//        event_box_->SetP(6,0.0,0.0,0.0,0.0);
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
    
    
    
    vector<string> give_sector_names(const string & pleft,
                                     const string & pright,
                                     const string & myorder,
                                     const int & ep_power,
                                     const string & me_approx)
    {string res = "we don't give no names in gamma* gamma*"; vector<string> res2;res2.push_back(res);return res2;}
    
    
    void evaluate_sector();
    string sector_name(){return my_sector_name;}
    void SetNumberOfParticles() {event_box.SetNumberOfParticles(6);}
    
    //: gluon fusion specific construction:  to be removed!
    void book_production_event(const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                               const double &){};//: public to integrate with fortran Fjet
    
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
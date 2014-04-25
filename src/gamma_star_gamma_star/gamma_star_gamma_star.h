#ifndef GAMMA_STAR_GAMMA_STAR_H
#define GAMMA_STAR_GAMMA_STAR_H

#include "production.h"
#include "convolutions.h" // for FF
#include "gamma_star_gamma_star_me.h"



class GammaStarGammaStar : public Production
{
public:
    GammaStarGammaStar(const UserInterface & UI);
    ~GammaStarGammaStar();
    
    //: virtual obligations from production 
    void SetDecayParticleIdInEventBox(){event_box.SetDecayParticleId(3);}
    int number_of_necessary_sectors(){return available_xs_.size();}
    
    
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
    //: but it's a virtual function of the production
    //: class, so the production class has to change
    //: implementation for this one to be removed
    void book_production_event(const double &,const double &,const double &,const double &,const double &,const double &,const double &,const double &,const double &){};
    
    void create_matrix_elements();
    
private:
    vector<CrossSection*> available_xs_;
    void find_the_xs(const UserInterface & UI);
    void allocate_luminosity();
    CrossSection* the_xs_;
    //int number_of_necessary_sectors_;
private:
    void info();
    void initialize_sector(const UserInterface& UI);

};

#endif
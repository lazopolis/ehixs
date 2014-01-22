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
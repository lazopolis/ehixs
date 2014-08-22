#ifndef GAMMA_STAR_GAMMA_STAR_H
#define GAMMA_STAR_GAMMA_STAR_H

#include "production.h"
#include "convolutions.h" // for FF
#include "gamma_star_gamma_star_me.h"



class GammaStarGammaStar : public Production
{
public:
    ~GammaStarGammaStar();
    //: virtual obligations from production
    int dimension_of_integration();
    void SetNumberOfParticles() {event_box.SetNumberOfParticles(6);}
    void SetDecayParticleIdInEventBox(){event_box.SetDecayParticleId(3);}
    void evaluate_sector();
    void create_matrix_elements();
    void info();
    void SelectAndConfigureSector(const UserInterface&);
    
    
    
private:
    vector<GstarGstarMe*> available_xs_;
    void find_the_xs(const UserInterface & UI);
    void allocate_luminosity();
    GstarGstarMe* the_xs_;
    //int number_of_necessary_sectors_;
private:
    // these should be in process
    void xml_info(const char * output_fname);

};

#endif
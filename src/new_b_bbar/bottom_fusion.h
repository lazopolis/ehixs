#ifndef BOTTOM_FUSION_H
#define BOTTOM_FUSION_H

// implementing the process P P -> higgs + X @ NNLO via bottom fusion
// we can have up to 2 extra emission particles.
// The particles are numbered
// f1(p1) + f2(p2) -> h(p3) + f3(p4) + f4(p5)
//
// we agree that at NLO the extra particle has momenta p4, so p5 is not used at NLO

// including base class
#include "production.h"
#include "bottom_fusion_me.h"

class BottomFusion : public Production
{
public:
    ~BottomFusion();
    //: virtual obligations from production
    int dimension_of_integration();
    void SetNumberOfParticles() {event_box.SetNumberOfParticles(5);}
    void SetDecayParticleIdInEventBox(){event_box.SetDecayParticleId(3);}
    void evaluate_sector();
    void create_matrix_elements();
    void info();
    void SelectAndConfigureSector(const UserInterface&);
    double alpha_s_at_mz_from_lhapdfs();
    void SetModelDependentParameters();
    
private://data
    vector<BottomFusionCrossSection*> available_xs_;
    BottomFusionCrossSection* the_xs_;
private://methods
    void find_the_xs(const UserInterface & UI);
    void allocate_luminosity();
    
    //int number_of_necessary_sectors_;
private:
    // these should be in process
    void xml_info(const char * output_fname);
    void initialize_sector(const UserInterface& UI);
    
};






#endif
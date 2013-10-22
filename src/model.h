#ifndef MODEL_H
#define MODEL_H

#include "particle.h"
#include<vector>
#include <stdlib.h> //: for exit()
using namespace std;



class CModel{
public:
     CModel();
     ~CModel(){};
         
//     vector<double> alpha_strong;//: a_s (not over Pi)
//     vector<double> alpha_strong_at_mz;//: a_s @ m_z (not over Pi)!!
     
     void consolidate(const double& a_at_mz,const double & mur_over_mh,
                      int porder);
    double alpha_strong(){return alpha_s->v();}
//    void evolve_quark_masses(const double & mur,int porder);
//     void evolve_as_to_mur(const double & mur,int porder);
//     double run_alpha_strong(const double & a_prev,const double & mur,int porder);
    double mu_r(){return mu_r_;}
    
    Particle higgs;
    vector<Particle*> quarks;
    vector<Particle*> leptons;
    vector<Particle*> vector_bosons;
    Particle top;
    Particle W;
    Particle Z;
    
    Particle bottom;
private:
	double mu_r_;
    CouplingConstant* alpha_s;
    
    
    

};

#endif
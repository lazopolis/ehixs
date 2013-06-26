#ifndef MODEL_H
#define MODEL_H

#include "ParticleObject.h"
#include<vector>
#include <stdlib.h>
using namespace std;

class CModel{
public:
     CModel();
     ~CModel(){};
     vector<ParticleObject*> quarks;
     vector<ParticleObject*> leptons;
     vector<ParticleObject*> vector_bosons;
     ParticleObject higgs;
     ParticleObject W;
     ParticleObject Z;
     ParticleObject top;
     ParticleObject bottom;
     void read(const string & option , const string & value);
     void set_Xq_for_quarks()
          {
          for (int i=0;i<quarks.size();i++)
               quarks[i]->Set_Xq(higgs.m());
          }
     vector<double> alpha_strong;//: a_s (not over Pi)
     vector<double> alpha_strong_at_mz;//: a_s @ m_z (not over Pi)!!
     
     void evolve(const vector<double> & a_at_mz,const double & mur, int porder);
     void evolve_quark_masses(const double & mur,int porder);
     void evolve_as_to_mur(const double & mur,int porder);
     double run_alpha_strong(const double & a_prev,const double & mur,int porder);

private:
	
};

#endif
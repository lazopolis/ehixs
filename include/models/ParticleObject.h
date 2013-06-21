#ifndef PARTICLEOBJECT_H
#define PARTICLEOBJECT_H

#include<complex>
#include<string>
#include "CConstants.h"
using namespace std;

class ParticleObject
{
public:
	ParticleObject(){};
	~ParticleObject(){};
     ParticleObject(string,double,double,double ,double);
                      //: name, mass,width,yukawa,charge  i.e. no ref_scale and no running
	ParticleObject(string,double,double,double,double ,double);
				  //:name,mass,ref_scale, width , yukawa,charge
	double m_at_ref_scale;
     double m();
     void set_m_at_ref_scale(const double & mm)
          {
          m_at_ref_scale=mm;
          if (not(needs_evolution)) m_at_scale_mur=mm;
          }
     complex<double> complex_mass_squared_at_mur();
     double ref_scale;
	
	double charge;
	double G;
	double Y;
	string name;
	complex<double> X;
	complex<double> Wq;
	void calculate_complex_mass();
	void Set_Xq(double);
     void evolve_mass_to_mur(const double&,const double & ,int );
     bool needs_evolution;

private:
     double evolution_step();
	double m_at_scale_mur;
     complex<double> cm_sq;
     double evolved_scale;
     bool evolved;
     bool complex_mass_calculated;
	
};


#endif
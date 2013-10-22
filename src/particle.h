#ifndef PARTICLEOBJECT_H
#define PARTICLEOBJECT_H

#include<complex>
#include<string>
#include "constants.h"
using namespace std;

class CouplingConstant{
public://methods
    CouplingConstant(double v,const double ref_scale)
    : v_(v){};//,ref_scale_(ref_scale){};
    double v(){return v_;}
    void evolve(const double& mu,int porder,const double zmass);
private://data
    double v_;
    //double ref_scale_;
};


class MassParameter{
public://methods
    MassParameter(double m,const double ref_scale): m_at_ref_scale_(m),ref_scale_(ref_scale){scheme_ = "msbar"; m_=0;}
    MassParameter(double m){m_ = m;m_at_ref_scale_=0.0; ref_scale_ = 0.0; scheme_ = "on-shell";}
    double value(){return m_;}
    void evolve(CouplingConstant as,const double& mu,int porder,const double& zmass);
    string scheme(){return scheme_;}
private://data
    double m_;
    double ref_scale_;
    double m_at_ref_scale_;
    string scheme_;
};

class Particle
{
public:
	Particle(){};
	~Particle(){};
    Particle(const string&);//: name
    //
    void set_msbar_mass(const double& mass,const double & scale);
    void set_pole_mass(const double& mass);
    void set_width(const double& w){width_=w;}
    void set_charge(const double& ch){charge_ = ch;}
    void set_Y(const double& Y){Y_ = Y;}
    //
    void consolidate(CouplingConstant as,const double& mur, int porder,
                     const double& mh,const double& zmass);
    //
    double width(){return width_;}
    double m(){return m_->value();}
    complex<double> cm_sq(){return cm_sq_;}
    double Y(){return Y_;}
    complex<double> X(){return X_;}
    complex<double> Wq(){return Wq_;}
    double charge(){return charge_;}
    string name(){return name_;}
    string scheme(){return m_->scheme();}
private://data
    string name_;
    double width_;
    double charge_;
    double Y_;
    MassParameter* m_;
	
	complex<double> X_;
	complex<double> Wq_;
    double m_at_scale_mur;
    complex<double> cm_sq_;
    double evolved_scale;
    bool evolved;
    bool needs_evolution;
    bool complex_mass_calculated;
private://methods
    double evolution_step();
    void calculate_complex_mass();
	//void evolve_mass_to_mur(const double&,const double & ,int );
    void set_Xq(double);
};


#endif
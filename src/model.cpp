

#include "model.h"
#include "iostream"

using namespace::std;


CModel::CModel()
{
    top=Particle("top");
    //top.set_onshell_mass(172.7);
    top.set_msbar_mass(163.7,163.7);
    top.set_charge(2.0/3.0);
    top.set_Y(1.0);
    top.set_width(0.0);
    //
    bottom=Particle("bottom");
    //bottom.set_onshell_mass(3.63);
    bottom.set_msbar_mass(3.63,10.0);
    bottom.set_charge(-1.0/3.0);
    bottom.set_Y(1.0);
    bottom.set_width(0.0);
    //
    W=Particle("W");
    W.set_pole_mass(80.403);
    W.set_charge(1.0);
    W.set_Y(1.0);
    W.set_width(2.141);
    //
    Z=Particle("Z");
    Z.set_pole_mass(91.1876);
    Z.set_charge(0.0);
    Z.set_Y(1.0);
    Z.set_width(2.4952);
    //
    //
    higgs=Particle("higgs");
    higgs.set_pole_mass(125.0);
    higgs.set_charge(0.0);
    higgs.set_Y(1.0);
    higgs.set_width(4.36E-003);
    //
    quarks.push_back(&top);
    quarks.push_back(&bottom);
    vector_bosons.push_back(&W);
    vector_bosons.push_back(&Z);

    mu_r_=0.0;
}





void CModel::consolidate(const double & a_at_mz,
                         const double & mur_over_mh, int porder)
{
    mu_r_ = mur_over_mh * higgs.m();
    alpha_s = new CouplingConstant(a_at_mz,Z.m());
    for (int i=0;i<quarks.size();i++)
        quarks[i]->consolidate(*alpha_s,mu_r_,porder,higgs.m(),Z.m());
    alpha_s->evolve(mu_r_,porder,Z.m());
}











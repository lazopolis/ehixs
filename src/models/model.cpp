

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
    W=VectorBoson("W");
    W.set_pole_mass(80.403);
    W.set_charge(1.0);
    W.set_Y(1.0);
    W.set_width(2.141);
    
    //
    Z=VectorBoson("Z");
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
                         const double & mur_over_mh, int porder,
                         const double& mh)
{
    cout<<"[ehixs]"<<endl<<"[ehixs] couplings and masses "<<endl;
    cout<<"[ehixs]"<<endl;
    higgs.set_pole_mass(mh);
    mu_r_ = mur_over_mh * higgs.m();
    alpha_s = new CouplingConstant(a_at_mz,Z.m());
    for (int i=0;i<quarks.size();i++)
        quarks[i]->consolidate(*alpha_s,mu_r_,porder,higgs.m(),Z.m());
    for (int i=0;i<vector_bosons.size();i++)
        vector_bosons[i]->consolidate(*alpha_s,mu_r_,porder,higgs.m(),Z.m());
    alpha_s->evolve(mu_r_,porder,Z.m());
    cout<<"[ehixs] a_s(m_z) = "<<a_at_mz<<" -> a_s("<<mu_r_<<") = "<<
        alpha_s->v()<<endl;
    
    // complex masses in gw,sw,cw
    complex<double> gw = sqrt(4.0*sqrt(2.0)*consts::G_fermi * W.cm_sq());
    complex<double> cw_sq = W.cm_sq()/Z.cm_sq();
    complex<double> cw = sqrt(cw_sq);
    complex<double> sw_sq = 1.0-cw_sq;
    //real masses in gw, sw, cw
    gw = sqrt(4.0*sqrt(2.0)*consts::G_fermi * pow(W.m(),2.0));
    cw_sq = pow(W.m()/Z.m(),2.0);
    cw = sqrt(cw_sq);
    sw_sq = 1.0-cw_sq;
    
    W.cv_up = gw / sqrt(2.0);
    W.cv_down = gw / sqrt(2.0);
    W.ca_up = gw / sqrt(2.0);
    W.ca_down = gw / sqrt(2.0);
    W.lamda = 2.0;
    
    
    Z.cv_up = gw/cw*(1.0/2.0 - 4.0/3.0 * sw_sq);
    Z.cv_down = gw/cw*(-1.0/2.0 + 2.0/3.0 * sw_sq);
    Z.ca_up = gw/cw*1.0/2.0;
    Z.ca_down = gw/cw*1.0/2.0;
    Z.lamda = 2.0;
    cout<<"[ehixs] gw = "<<gw<<"\tsw^2 = "<< sw_sq
        <<"\tcw = "<< cw<<endl;
    cout<<"[ehixs] G_F = "<<consts::G_fermi<<endl;
    cout<<"[ehixs] mw = "<<W.m()<<endl;

    cout<<"[ehixs] gw_up for W = "<<pow(W.cv_up,2.0) + pow(W.ca_up,2.0)<<endl;
    cout<<"[ehixs] gw_up for Z = "<<pow(Z.cv_up,2.0) + pow(Z.ca_up,2.0)<<endl;
    cout<<"[ehixs] lambda for W = "<<W.lamda<<endl;
    cout<<"[ehixs] lambda for Z = "<<Z.lamda<<endl;
    
}











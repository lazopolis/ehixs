

#include "model.h"
#include "iostream"

using namespace::std;


CModel::CModel()
{
top=ParticleObject("top",172.7,0.0,1.0,2.0/3.0);
bottom=ParticleObject("bottom",3.63,10.0,0.0,1.0,-1.0/3.0);//: m_b defined at mu0=10GeV
W=ParticleObject("W",80.403,2.141,1.0,1.0);
Z=ParticleObject("Z",91.1876,2.4952,1.0,0.0);
higgs=ParticleObject("higgs",125.0,4.36681030806381626E-003,1.0,0.0);
quarks.push_back(&top);
quarks.push_back(&bottom);
vector_bosons.push_back(&W);
vector_bosons.push_back(&Z);

    _mu_r=0.0;
}



void CModel::read(const string & option , const string & value)
{
//--------------------- higgs
if (option=="m_higgs")
	{
	higgs.set_m_at_ref_scale(atof(value.c_str()));
	higgs.calculate_complex_mass();
	}
//--------------------- top
if (option=="m_top")
	{
	top.set_m_at_ref_scale(atof(value.c_str()));
	top.calculate_complex_mass();
	}

if (option=="G_top")
	{
	top.G=atof(value.c_str());
	top.calculate_complex_mass();
	}
if (option=="Y_top")
	{
	top.Y=atof(value.c_str());
	}
//--------------------- bottom
if (option=="m_bot")
	{
	bottom.set_m_at_ref_scale(atof(value.c_str()));
	bottom.calculate_complex_mass();
	}

if (option=="G_bot")
	{
	bottom.G=atof(value.c_str());
	bottom.calculate_complex_mass();
	}
if (option=="Y_bot")
	{
	bottom.Y=atof(value.c_str());
	}
//--------------------- W
if (option=="m_W")
	{
	W.set_m_at_ref_scale(atof(value.c_str()));
	W.calculate_complex_mass();
	}

if (option=="G_W")
	{
	W.G=atof(value.c_str());
	W.calculate_complex_mass();
	}
//--------------------- Z
if (option=="m_Z")
	{
	Z.set_m_at_ref_scale(atof(value.c_str()));
	Z.calculate_complex_mass();
	}

if (option=="G_Z")
	{
	Z.G=atof(value.c_str());
	Z.calculate_complex_mass();
	}

}


void CModel::evolve(const vector<double> & a_at_mz,const double & mur, int porder)
{
    _mu_r=mur;
     alpha_strong_at_mz = a_at_mz;
     evolve_quark_masses(mur,porder);
     evolve_as_to_mur(mur,porder);
    cout<<"\n[CModel] : received a_s[0](m_z) = "<<alpha_strong_at_mz[0]
        <<"\t mur = "<<_mu_r<<endl;
}

void CModel::evolve_quark_masses(const double & mur,int porder)
{
     for (int i=0;i<quarks.size();i++)
          {
          if (quarks[i]->needs_evolution)
               {
               double as_at_ref_scale = run_alpha_strong(alpha_strong_at_mz[0], quarks[i]->ref_scale, porder);
               quarks[i]->evolve_mass_to_mur(as_at_ref_scale, mur, porder);
               }
          }
}


void CModel::evolve_as_to_mur(const double & mur,int porder)
{
     
     for (int i=0;i<alpha_strong_at_mz.size();i++)
          {
          alpha_strong.push_back(run_alpha_strong(alpha_strong_at_mz[i],mur,porder));
          }
     for (int i=0;i<alpha_strong.size();i++)
          {
          cout<<"\nfrom Model: [order "<<porder<<" evolution] evolving a_s of member "<<i
          <<" from a(Mz)="<<alpha_strong_at_mz[i]<<" to a("<<mur<<")="<<alpha_strong[i];
          }
     
    }

double CModel::run_alpha_strong(const double & a_prev,const double & mur,int porder)
{
     const int N=100000;
     //: valid for any nf
     double      b0 = 11.0 / 4.0 - consts::nf / 6.0;
     double      b1 = (51.0 / 8.0 - 19.0 / 24.0 * consts::nf);
     double      b2 = (2857.0 / 128.0 - 5033.0 / 1152.0 * consts::nf + 325.0 / 3456.0 * pow(consts::nf,2.0));

     
     double as_prev = a_prev/consts::Pi;
     double step=log(pow(mur,2.0)/pow(Z.m(),2.0))/N;
     for (int i=0;i<N;i++)
          {
          double incr;
          if (porder==0) incr = step * pow(as_prev,2.0) * b0;
          else if (porder==1) incr = step * pow(as_prev,2.0)*(b0+b1*as_prev);
          else if (porder==2) incr = step * pow(as_prev,2.0)*(b0+b1*as_prev + b2 * pow(as_prev,2.0));
          else
               {
               cout<<"\n["<<__func__<<"]\t  : Unknown perturbative order : "<<porder<<endl;
               exit(0);
               }
          as_prev = as_prev - incr;
          }
     return as_prev*consts::Pi;
}



















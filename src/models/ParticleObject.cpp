
#include "ParticleObject.h"
#include "iostream"
using namespace std;
ParticleObject::ParticleObject(string iname,double imq,double _ref_scale,double iGq,double iYq,double ich)
{
     name=iname;
     m_at_ref_scale=imq;
     Y=iYq;
     G=iGq;
     //calculate_complex_mass();
     charge=ich;
     ref_scale = _ref_scale;
     needs_evolution=true;
     evolved=false;
     complex_mass_calculated=false;
}

ParticleObject::ParticleObject(string iname,double imq,double iGq,double iYq,double ich)
{
     name=iname;
     m_at_ref_scale=imq;
     Y=iYq;
     G=iGq;
     //calculate_complex_mass();
     charge=ich;
     ref_scale = 0.0;
     evolved=true; //: in this case there is no evolution
     needs_evolution=false;
     m_at_scale_mur = m_at_ref_scale;
     complex_mass_calculated=false;
     
}

void ParticleObject::Set_Xq(double m_higgs)
{
//: meaning virtuality of higgs
Wq = 4.0*complex_mass_squared_at_mur()/m_higgs/m_higgs;
X = - Wq/pow(sqrt(1.0-Wq)+1.0,2.0);
//     cout<<"\ncalculating Wq and X for "<<name<<" with m="<<sqrt(cm_sq)<<" and sqrt(m_h)="<<sqrt(m_higgs);

}


void ParticleObject::calculate_complex_mass()
{
cm_sq=complex<double>(m_at_scale_mur*m_at_scale_mur,-m_at_scale_mur*G);
}

double ParticleObject::m()
{
if (not(needs_evolution) or evolved)
     {
     return m_at_scale_mur;
     }
else
     {
     cout<<"\n error in ParticleObject: I am asked for mass at mur scale before any evolution";
     exit(1);
     return 0.0;
     }
}


complex<double> ParticleObject::complex_mass_squared_at_mur()
{
     if (not(needs_evolution)  or evolved)
          {
          if (complex_mass_calculated)
               {
               return cm_sq;
               }
          else
               {
               calculate_complex_mass();
               complex_mass_calculated=true;
               return cm_sq;
               }
          }
     else
          {
          cout<<"\n error in ParticleObject: I am asked for mass at mur scale before any evolution";
          exit(1);
          return 0.0;
          }
}





void ParticleObject::evolve_mass_to_mur(const double & as_at_ref_scale,const double & mur,int porder)
{
     const int N=100000;
     //: valid for any nf
     double      b0 = 11.0 / 4.0 - consts::nf / 6.0;
     double      b1 = (51.0 / 8.0 - 19.0 / 24.0 * consts::nf);
     double      b2 = (2857.0 / 128.0 - 5033.0 / 1152.0 * consts::nf + 325.0 / 3456.0 * pow(consts::nf,2.0));
     
     double gamma_0=1;
     double gamma_1 = 101.0 / 24.0 - 5.0 / 36.0 * consts::nf;
     double gamma_2 = 1249.0 / 64.0 - 2.284121493373735 * consts::nf - 35.0 / 1296.0 * pow(consts::nf,2.0);
     
     
     double as_prev= as_at_ref_scale/consts::Pi;
     double m_prev = m_at_ref_scale;

     double step=log(pow(mur,2.0)/pow(ref_scale,2.0))/N;
     for (int i=0;i<N;i++)
          {
          double incr;
          double m_incr;
          switch(porder)
               {
                    case 0:
                         incr = step * pow(as_prev,2.0) * b0;
                         m_incr = step * gamma_0 * as_prev * m_prev;
                         break;
                    case 1:
                         incr = step * pow(as_prev,2.0)*(b0+b1*as_prev);
                         m_incr = step * ( gamma_0*as_prev+ + gamma_1 * pow(as_prev,2.0))* m_prev;
                         break;
                    case 2:
                         incr = step * pow(as_prev,2.0)*(b0+b1*as_prev + b2 * pow(as_prev,2.0));
                         m_incr = step * ( gamma_0*as_prev+ + gamma_1 * pow(as_prev,2.0) + gamma_2 * pow(as_prev,3.0))* m_prev;
                         break;
                    default:cout<<"\n["<<__func__<<"]\t  : Unknown perturbative order : "<<porder<<endl;
                    exit(0);
               }
          as_prev = as_prev - incr;
          m_prev = m_prev - m_incr;
          }
     m_at_scale_mur=m_prev;
     evolved=true;
}

 




















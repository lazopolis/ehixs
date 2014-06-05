
#include "particle.h"
#include "iostream"
#include <stdlib.h> //: for exit()

using namespace std;



void CouplingConstant::evolve(const double& mur,int porder,const double zmass)
{
    const int N=100000;
    //: valid for any nf
    double      b0 = 11.0 / 4.0 - consts::nf / 6.0;
    double      b1 = (51.0 / 8.0 - 19.0 / 24.0 * consts::nf);
    double      b2 = (2857.0 / 128.0 - 5033.0 / 1152.0 * consts::nf + 325.0 / 3456.0 * pow(consts::nf,2.0));
    
    
    double as_prev = v_ / consts::Pi;
    double step=log(pow(mur,2.0)/pow(zmass,2.0))/N;
    if (porder==0)
        {
        for (int i=0;i<N;i++)
            {
            double incr = step * pow(as_prev,2.0) * b0;
            as_prev = as_prev - incr;
            }
        }
    else if (porder==1)
        {
        for (int i=0;i<N;i++)
            {
            double incr = step * pow(as_prev,2.0)*(b0+b1*as_prev);
            as_prev = as_prev - incr;
            }
        }
    else if (porder==2)
        {
        for (int i=0;i<N;i++)
            {
            double incr = step * pow(as_prev,2.0)*(b0+b1*as_prev + b2 * pow(as_prev,2.0));
            as_prev = as_prev - incr;
            }
        }
    else
        {
        cout<<"\n["<<__func__<<"]\t  : Unknown perturbative order : "<<porder<<endl;
        exit(0);
        }
    v_ = as_prev*consts::Pi;
}



//------------------------------------------------------------------------------

// the b0,b1,b2 and g0,g1,g2 are hardcoded here
void MassParameter::evolve(CouplingConstant as,const double & mur,
                           int porder,const double& zmass)
{
    
    if (scheme_=="msbar")
        {
        // evolving as to the reference scale of this mass
        as.evolve(ref_scale_,porder,zmass);
        double as_at_ref_scale = as.v();
        //
        const int N=100000;
        //: valid for any nf
        double      b0 = 11.0 / 4.0 - consts::nf / 6.0;
        double      b1 = (51.0 / 8.0 - 19.0 / 24.0 * consts::nf);
        double      b2 = (2857.0 / 128.0 - 5033.0 / 1152.0 * consts::nf
                          + 325.0 / 3456.0 * pow(consts::nf,2.0));
    
        double gamma_0=1;
        double gamma_1 = 101.0 / 24.0 - 5.0 / 36.0 * consts::nf;
        double gamma_2 = 1249.0 / 64.0 - 2.284121493373735 * consts::nf
                        - 35.0 / 1296.0 * pow(consts::nf,2.0);
    
    
        double as_prev= as_at_ref_scale/consts::Pi;
        double m_prev = m_at_ref_scale_;
    
        double step=log(pow(mur,2.0)/pow(ref_scale_,2.0))/N;
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
        m_=m_prev;

        cout<<" msbar scheme : evolved from m("<<ref_scale_<<")="
            <<m_at_ref_scale_<<" -> m("<<mur<<")="<<m_<<endl;
        }
    else
        {
        cout<<" on-shell scheme, no evolution. "<<endl;
        }
}



//------------------------------------------------------------------------------


Particle::Particle(const string& name)
{
    name_=name;
    Y_=0.0;
    width_=0.0;
    charge_ = 0.0;
    m_ = NULL;
}

void Particle::set_msbar_mass(const double& m,const double& ref_scale)
{
    m_ = new MassParameter(m,ref_scale);
}

void Particle::set_pole_mass(const double & m)
{
    m_ = new MassParameter(m);
}

void Particle::consolidate(CouplingConstant as,const double& mur, int porder,
                                 const double & mh,const double& zmass)
{
   if (m_ == NULL)
       {
       cout<<"Error in Model/Particle : mass has not been initialized before trying to evolve it"<<endl<<endl;
       exit(0);
       }
    else
        {
        
        cout<<"[ehixs] "<<name_<<" quark evolution:";
        m_->evolve(as,mur,porder,zmass);
        calculate_complex_mass();
        set_Xq(mh);
        }
}

void Particle::set_Xq(double m_higgs)
{
//: meaning virtuality of higgs
Wq_ = 4.0*cm_sq_/m_higgs/m_higgs;
X_ = - Wq_/pow(sqrt(1.0-Wq_)+1.0,2.0);
}


void Particle::calculate_complex_mass()
{
    cm_sq_=complex<double>(pow(m_->value(),2.0), -width_*m_->value());
}























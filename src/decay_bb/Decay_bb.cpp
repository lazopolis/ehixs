#include "Decay_bb.h"


Decay_bb::Decay_bb()
{
     sectors.push_back(new DecaySector<ptr_to_Decay_bb_function>(&Decay_bb::LO,2,"h->bb LO"));
     sectors.push_back(new DecaySector<ptr_to_Decay_bb_function>(&Decay_bb::NLOV,2,"h->bb NLO Virtual"));

}



void Decay_bb::init(const UserInterface &UI,TheHatch* the_hatch)
{
     Model.higgs.set_m_at_ref_scale(UI.m_higgs);
     
     my_sector=UI.decay_sector;
     if (my_sector<int(sectors.size()))
     {
          my_sector_name = sectors[my_sector]->name();

          dimension_of_integration_for_decay=sectors[my_sector]->dim();
          setup_hatch(the_hatch);
          sector_defined=true;
     }
     else
     {
     cout<<"\n There is no sector with id number "<<my_sector<<" declared! This is a critical error and I exit"<<endl;
     exit(1);
     }
     my_momenta.init_fvector("b1");
     my_momenta.init_fvector("b2");
     my_momenta.init_fvector("g1");
     my_momenta.init_fvector("g2");
}




void Decay_bb::do_decay()
{
     fvector PH_rest(Model.higgs.m(),0.0,0.0,0.0);
     do_decay(PH_rest);
}


void Decay_bb::do_decay(const fvector & inPH)
{
     decay_events.clear();
     PH=inPH;
     (this->*(sectors[my_sector]->ptr()))();

     //double decay_weight =1.0;   
     //decay_events.push_back(new Event(decay_weight,my_momenta));
}

void Decay_bb::LO()
{    
     const double mh = Model.higgs.m();
     const double flux = 1.0/2.0/mh;
     const double dPhi2 = 1.0/8.0/consts::Pi;
     const double Nc=3.0;
     const double ME_sq = y_b[0]*y_b[0]*mh*mh * 2.0 * Nc;
     const double decay_weight = flux * dPhi2  * ME_sq;
     //cout<<"\n new event with weight "<<decay_weight<<"\tyb="<<y_b[0];
     set_LO_momenta();
     decay_events.push_back(new Event(decay_weight,my_momenta,decay_xx_vegas));
}

void Decay_bb::NLOV()
{    
     const double mh = Model.higgs.m();
     const double flux = 1.0/mh;
     const double dPhi2 = 1.0/8.0/consts::Pi;
     const double Nc=3.0;
     const double ME_sq = yb*yb*mh*mh * 2.0 * Nc;
     const double decay_weight = flux * dPhi2  * ME_sq;
     set_LO_momenta();
     decay_events.push_back(new Event(decay_weight,my_momenta,decay_xx_vegas));
}



void Decay_bb::set_LO_momenta()
{
     //cout<<"\n***** "<<P;
     //:constructs 1->2 phase space factor and the two new final state vectors
     double pi=consts::Pi;
     //: generate the costheta at [-1,1] (induces a jacobian = 2.0)
     double costheta = -1.0+2.0*decay_xx_vegas[0];
     double sintheta = sqrt(1.0-costheta*costheta);
     //: generate the phi angle at [0,2*pi] (induces a jacobian = 2*pi)
     double phi=2.0*pi*decay_xx_vegas[1];
     
     //: calulate the energy of p1 at the rest frame of P (P=pH here)
     double p_sq=PH.square();
     double E1=1.0/2.0*sqrt(p_sq);
     //: the energy of p2
     double E2=E1;
     //: the |p1_vector|
     double p1v=E1;
     //: setting up the two vectors at the rest frame of P, so back to back
     my_momenta["b1"] = fvector(E1,p1v*sintheta*sin(phi),p1v*sintheta*cos(phi),p1v*costheta);
     my_momenta["b2"] = fvector(E2,-p1v*sintheta*sin(phi),-p1v*sintheta*cos(phi),-p1v*costheta);
     //cout<<"\np1,p2="<<p1<<"\t"<<p2;
     //: boosting back to the frame where P is defined
     //: i.e. with v = - Pvector / P_0
     const double EH=PH[0];
     my_momenta["b1"].boost(-PH[1]/EH,-PH[2]/EH,-PH[3]/EH);
     my_momenta["b2"].boost(-PH[1]/EH,-PH[2]/EH,-PH[3]/EH);     
}


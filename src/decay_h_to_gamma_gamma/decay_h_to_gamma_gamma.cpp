#include "decay_h_to_gamma_gamma.h"
#include "higgs_zerfal.h"
#include "decay_gamma_gamma_cuts.h"

Decay_gammagamma::Decay_gammagamma(const UserInterface & UI)
{
    dimension_of_integration_for_decay=2;
#include "decay_gamma_gamma_cut_initialization.h"
    cuts_->ParseCuts(UI);
    decay_mode_ = 7;
}

void Decay_gammagamma::do_decay()
{
    double  PH_rest[4] ={Model.higgs.m(),0.0,0.0,0.0};
    do_decay(PH_rest);
    
}


void Decay_gammagamma::do_decay(double* PH)
{
    event_box.CleanUp();
    
    int decaymode = 1;
    // decay mode 1:HZZeemm | 2: HZZllll | 3: HWWlnln | 4: HWWZZlnln
    //double pH[4]={PH->E(),PH->px(),PH->py(),PH->pz()};
    
        
    double p1[4];
    double p2[4];
    double p3[4];
    double p4[4];
    double decay_weight = 0.0;
    
    double alpha_QED = 1.0/129.0;
    higgszerfall(
                 PH,
                 Model.W.m(),Model.Z.m(),Model.top.m(),
                 Model.W.width(),Model.Z.width(),consts::G_fermi,
                 alpha_QED,
                 decay_mode_,
                 decay_xx_vegas,// passing vegas variables
                 decay_weight,  // the weight of the decay will be set here
                 p1,p2,p3,p4
                 );  // the four-momenta of the final state particles
    // as set by HiggsZerfall
    //double* gamma1 = p1;
    //cout<<"\n** gamma1 = "<<gamma1[0]<<" "<<gamma1[1]<<" "<<gamma1[2]<<" "<<gamma1[3];
    //cout<<"\n[decay]: w = "<<decay_weight<<endl;
    
    decay_weight = decay_weight / Model.higgs.width();
    
    event_box.AddNewEvent(decay_weight);
    event_box.SetP(1,p1[0],p1[1],p1[2],p1[3]);
    event_box.SetP(2,p2[0],p2[1],p2[2],p2[3]);

}

/*



#include "decay_h_to_gamma_gamma.h"

#include "decay_gamma_gamma_cuts.h"

Decay_gammagamma::Decay_gammagamma(const UserInterface & UI)
{
    // Model.higgs.set_m_at_ref_scale(UI.m_higgs);
    //     for (int i=0;i<Model.quarks.size();i++)
    //          {
    //          Model.quarks[i]->Set_Xq(Model.higgs.m());
    //          }
    //     for (int i=0;i<Model.vector_bosons.size();i++)
    //          {
    //          Model.vector_bosons[i]->Set_Xq(Model.higgs.m());
    //          }
    dimension_of_integration_for_decay=2;
    #include "decay_gamma_gamma_cut_initialization.h"
    cuts_->ParseCuts(UI);
    
    
    
}


void Decay_gammagamma::do_decay()
{
    double PH_rest[4] = {Model.higgs.m(),0.0,0.0,0.0};
    do_decay(PH_rest);
}

void Decay_gammagamma::do_decay(double* PH)
{
    double pi=consts::Pi;
    
    double* gamma1;
    double* gamma2;
    double PS = One_to_two_PSP(PH,0.0,0.0,
                               gamma1,
                               gamma2,
                               decay_xx_vegas[0],
                               decay_xx_vegas[1])
    /4.0/pow(pi,2); // 1/(8pi)
    
    // the matrix element:
    // it receives loop contributions from all quarks, all leptons and all charged
    // vector bosons. Here, we only consider the quarks (even tau contributions
    // are suppressed by (mtau/mtop)^2) and the W boson.
    //
    // the quark contribution is Nc*e_q^2*Ffermion with Ffermion = -4/3*"born"
    // with "born" from the LO gluonfusion.
    //
    // the W contribution is governed by "bosonloop".
    
    complex<double> temp(0.0,0.0);
    for(int i=0; i<Model.quarks.size();i++)
        {
        //cout << "\n quark no " << i << "\t --> \t x = " << Model.quarks[i]->X;
        temp += -4.0*born_for_gamma_gamma(Model.quarks[i]->X())*pow(Model.quarks[i]->charge(),2);
        //cout<<"\ntemp +="<<-4.0*born(Model.quarks[i]->X)*pow(Model.quarks[i]->charge,2);
        }
    
    for(int i=0; i<Model.vector_bosons.size();i++)
        {
        temp += bosonloop(Model.vector_bosons[i]->X())*pow(Model.vector_bosons[i]->charge(),2);
        }
    // the prefactor, taken from my masterthesis with PS-volume
    // and flux factor divided out
    // and alpha expressed through Gf and mw.
    // all mh are replaced by the higgs virtuality.
    double mh = Model.higgs.m();
    double alpha = 1.0/137.0; // hardcoded ATM
    double pref = consts::G_fermi*pow(mh,3)*pow(alpha,2)/sqrt(2.0)/16.0/pow(pi,2);
    double Msq = pref*real(temp*conj(temp));
    
    double decay_weight = PS*Msq*mh/pi; // *mh/pi to counter the normalisation of the Breit-Wigner.
    
    event_box.AddNewEvent(decay_weight);
    event_box.SetP(1,gamma1[0],gamma1[1],gamma1[2],gamma1[3]);
    event_box.SetP(2,gamma2[0],gamma2[1],gamma2[2],gamma2[3]);
    
}



complex<double> Decay_gammagamma::bosonloop(complex<double> x)
{
#ifdef debug
    cout<<"\n["<<__func__<<"]";
#endif
    //: the bosonic loop contribution to the gamma gamma decay.
    complex<double > res= 2.0 - 12.0*x/pow(1.0-x,2)*(1.0-(1.0+x*x)/pow(1.0-x,2)*HPL2(0,0,x));
    return res;
}

complex<double> Decay_gammagamma::born_for_gamma_gamma(complex<double> x)
{
#ifdef debug
    cout<<"\n["<<__func__<<"]";
#endif
    //: the expression below goes to 1 as mq->infty, i.e. as x->1
    complex<double > res=(-3.0)*x/pow(1.0-x,2.0)*(2.0-pow(1.0+x,2.0)/pow(1.0-x,2.0)*HPL2(0,0,x));
    //res = -16.0*pow(1.0+x,2.0)/x*HPL2(0,0,x)+32.0*pow(1.0-x,2.0)/x;
    
    return res;
}

// ----------------------------------------------------------------------------------------

//===============================================================

double Decay_gammagamma::One_to_two_PSP(double* P,const double & Q1_sq,const double & Q2_sq,
                                        double* p1,double* p2,
                                        const double & x1,const double & x2)
{
    //cout<<"\n***** "<<P;
    //:constructs 1->2 phase space factor and the two new final state vectors
    double pi=consts::Pi;
    //: generate the costheta at [-1,1] (induces a jacobian = 2.0)
    double costheta = -1.0+2.0*x1;
    double sintheta = sqrt(1.0-costheta*costheta);
    //: generate the phi angle at [0,2*pi] (induces a jacobian = 2*pi)
    double phi=2.0*pi*x2;
    double p_sq=P[0]*P[0]-P[1]*P[1]-P[2]*P[2]-P[3]*P[3];
    //: calulate the energy of p1 at the rest frame of P
    double E1=(p_sq+Q1_sq-Q2_sq)/2.0/sqrt(p_sq);
    //: the energy of p2
    double E2=sqrt(p_sq)-E1;
    //: the |p1_vector|
    double p1v=sqrt(E1*E1-Q1_sq);
    //: setting up the two vectors at the rest frame of P, so back to back
    p1[0] = E1;
    p1[1] = p1v*sintheta*sin(phi);
    p1[2] = p1v*sintheta*cos(phi);
    p1[3] = p1v*costheta;
    p2[0] = E2;
    p2[1] = -p1v*sintheta*sin(phi);
    p2[2] = -p1v*sintheta*cos(phi);
    p2[3] = -p1v*costheta;
    
    //: returns the jacobian factor 4*pi times the PSP factor sqrt(lambda)/(8*P^2)
    return(4.0*pi/8.0/p_sq*sqrt(PSP_lambda(p_sq,Q1_sq,Q2_sq)));
}

double Decay_gammagamma::PSP_lambda(const double & x,const double & y,const double & z)
{
    
    return (x*x + y*y + z*z - 2.0*x*y - 2.0*y*z - 2.0*z*x);
}
*/

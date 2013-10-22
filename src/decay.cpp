
#include "decay.h"
#ifdef __cplusplus
extern "C" {
#endif
    
    void higgszerfall_(double*
                       ,double*,double*,double*,double*,
                       double*,double*,
                       int*,     // decay mode 0:
                       double*,// passing vegas variables
                       double*,// the weight of the decay will be set here
                       double*,
                       double*,
                       double*,
                       double*
                       );
    
#ifdef __cplusplus
}
#endif
//
inline void higgszerfall(
                         const double& mh,const double& mw,const double& mz,
                         const double& gamw,
                         const double& gamz,
                         const double& GF,
                         double* pH,
                         const int& decaymode,     // decay mode 0:
                         double* decay_xx_vegas,// passing vegas variables
                         double& decay_weight,  // the weight of the decay will be set here
                         double* p1,double* p2,double* p3,double* p4
                         )
    {
    higgszerfall_(
                 (double*) &mh,(double*) &mw,(double*) &mz,(double*) &gamw,
                 (double*) &gamz,
                 (double*) &GF,
                  (double*) pH,
                 (int*) &decaymode,     // decay mode 0:
                 (double*) decay_xx_vegas,// passing vegas variables
                 (double*) &decay_weight,  // the weight of the decay will be set here
                 (double*) p1,
                  (double*) p2,(double*) p3,(double*) p4
                  );
    }



//------------------------------------------------------------------------------


void Decay::set_up_the_hatch(TheHatch* the_hatch)
{

     decay_xx_vegas = the_hatch->RequestPtr();
     for (unsigned i=0;i<dimension_of_integration_for_decay;i++) 
          the_hatch->RequestVar("VEGAS");
}





/*
 double ExclusiveClass::decay_WW()
 {
 #ifdef debug
 cout<<"\n["<<__func__<<"]";
 #endif
 //cout<<"\n["<<__func__<<"]\t  : full WW decay"<<endl;
 double pi=consts::Pi;
 double pH_sq=pH.square();
 double mW=Model.W.m;
 double Gamma_W=Model.W.G;
 double m_w_sq = pow(mW,2.0);
 //: we first generate the invariant mass of VB1, Qa_sq
 //: In order to flaten out the BW, we perform first the mapping
 //: Q^2 = m^2*(1+Gamma/m * tan(pi*lambda))
 //: and we generate lambda
 //: Q_a^2 is limited from 0 to pH^2
 //: which corresponds to the following range for lambda
 double lambda_a_min = 1.0/pi*atan(-mW/Gamma_W);
 double lambda_a_max = 1.0/pi*atan((pH_sq-m_w_sq)/(mW*Gamma_W));
 double lambda_a = lambda_a_min+(lambda_a_max-lambda_a_min)*decay_xx_vegas[0];
 double jac_a = (lambda_a_max-lambda_a_min);
 double Qa_sq = m_w_sq*(1.0+Gamma_W/mW*tan(pi*lambda_a));
 //: we then generate the invariant mass of VB2, Qb_sq
 //: it is limited from 0 to (sqrt(pH^2)-sqrt(Qa^2))^2
 double lambda_b_min = 1.0/pi*atan(-mW/Gamma_W);
 double Kb = pow(sqrt(pH_sq)-sqrt(Qa_sq),2.0);
 double lambda_b_max = 1.0/pi*atan((Kb-m_w_sq)/(mW*Gamma_W));
 double lambda_b = lambda_b_min+(lambda_b_max-lambda_b_min)*decay_xx_vegas[1];
 double jac_b = (lambda_b_max-lambda_b_min);
 double Qb_sq = m_w_sq*(1.0+Gamma_W/mW*tan(pi*lambda_b));
 //: generate the pH   -> pVB1 + pVB2 vectors
 double pspfactor1=One_to_two_PSP(pH,Qa_sq,Qb_sq,pVB1,pVB2,decay_xx_vegas[2],decay_xx_vegas[3]);
 //cout<<"\n VBs = "<<pVB1<<"\t"<<sqrt(pVB1.square())<<" "<<sqrt(Qa_sq)
 ///			<<"\t"<<pVB2<<"\t"<<sqrt(pVB2.square())<<" "<<sqrt(Qb_sq);
 //: generate the pVB1 -> pL1a + pL1b vectors
 double pspfactor2=One_to_two_PSP(pVB1,0.0,0.0,pL1a,pL1b,decay_xx_vegas[4],decay_xx_vegas[5]);
 //: generate the pVB2 -> pL2a + pL2b vectors
 double pspfactor3=One_to_two_PSP(pVB2,0.0,0.0,pL2a,pL2b,decay_xx_vegas[6],decay_xx_vegas[7]);
 //cout<<"\npsfactors : "<<pspfactor1<<" "<<pspfactor2<<" "<<pspfactor3;
 double res = 1.0/pow(2.0*pi,8.0) //:overall pi prefactor 
 *2.0*sqrt(2.0)*pow(mW,8.0)*pow(consts::G_fermi,3.0)//:couplings
 *pow(pi,2.0)/m_w_sq/pow(Gamma_W,2.0) // tan mapping Jacobian
 *jac_a*jac_b //: Jacobian factor from limits on Qa_sq, Qb_sq
 *4.0*(pL1a*pL2b)*(pL2a*pL1b)  //:remaining matrix element, at the rest frame of the Higgs boson
 *pspfactor1
 *pspfactor2
 *pspfactor3
 ;
 return res;
 }
 */

//=================== WW/ZZ ===============================

 Decay_WWZZ::Decay_WWZZ(const UserInterface & UI)
{
    dimension_of_integration_for_decay=5;
    
    my_momenta.init_fvector("lepton1");
    my_momenta.init_fvector("lepton2");
    my_momenta.init_fvector("lepton3");
    my_momenta.init_fvector("lepton4");
}

void Decay_WWZZ::do_decay()
{
    fvector PH_rest(Model.higgs.m(),0.0,0.0,0.0);
    do_decay(PH_rest);
}


void Decay_WWZZ::do_decay(const fvector& PH)
{
    decay_events.clear();
    
    int decaymode = 1;
    // decay mode 1:HZZeemm | 2: HZZllll | 3: HWWlnln | 4: HWWZZlnln
    double pH[4]={PH[0],PH[1],PH[2],PH[3]};
    
    double p1[4];
    double p2[4];
    double p3[4];
    double p4[4];
    double decay_weight = 0.0;
    //cout<<"\n[decay]: hello before"<<endl;
    higgszerfall(
                 Model.higgs.m(),Model.W.m(),Model.Z.m(),
                 Model.W.width(),Model.Z.width(),consts::G_fermi,
                 pH,
                 decaymode,     
                 decay_xx_vegas,// passing vegas variables
                 decay_weight,  // the weight of the decay will be set here
                 p1,p2,p3,p4
                     );  // the four-momenta of the final state particles
                                // as set by HiggsZerfall
    //cout<<"\n[decay] Gamma = "<<decay_weight<<endl;
    my_momenta["lepton1"].set(p1[0],p1[1],p1[2],p1[3]);
    my_momenta["lepton2"].set(p2[0],p2[1],p2[2],p2[3]);
    my_momenta["lepton3"].set(p3[0],p3[1],p3[2],p3[3]);
    my_momenta["lepton4"].set(p4[0],p4[1],p4[2],p4[3]);
    
    decay_weight = decay_weight / Model.higgs.width();
    
    decay_events.push_back(new Event(decay_weight,my_momenta,decay_xx_vegas));
}


//=================== gamma gamma ===============================





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

     my_momenta.init_fvector("gamma1");
     my_momenta.init_fvector("gamma2");

}


void Decay_gammagamma::do_decay()
{
     fvector PH_rest(Model.higgs.m(),0.0,0.0,0.0);
     do_decay(PH_rest);
}

void Decay_gammagamma::do_decay(const fvector& PH)
{
     //my_momenta=prod_mom;
     // higgs --> gamma gamma decay
     // needs two random variables --> decay_xx_vegas[0],decay_xx_vegas[1]
     // the photon momenta are pVB1 and pVB2 (atm)
     // the return value is the decay width
     
     // //check:
     // if(decay_xx_vegas.size() != 2)
     //   {
     //     cerr << "\n\nERROR in " << __func__ << "] : decay_xx_vegas.size() (" << decay_xx_vegas.size() << ") wrong!\nAborting...\n\n";
     //     exit(1);
     //   }
     // get the momenta from the 1->2 PS routine
     double pi=consts::Pi;
     
     double PS = One_to_two_PSP(PH,0.0,0.0,
                                my_momenta[0],
                                my_momenta[1],
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
     // the prefactor, taken from my masterthesis with PS-volume and flux factor divided out
     // and alpha expressed through Gf and mw. all mh are replaced by the higgs virtuality.
     double mh = sqrt(PH.square());
     double alpha = 1.0/137.0; // hardcoded ATM
     double pref = consts::G_fermi*pow(mh,3)*pow(alpha,2)/sqrt(2.0)/16.0/pow(pi,2);
     double Msq = pref*real(temp*conj(temp));
     
     double decay_weight = PS*Msq*mh/pi; //*mh/pi to counter the normalisation of the Breit-Wigner.  
     decay_events.push_back(new Event(decay_weight,my_momenta,decay_xx_vegas));
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

double Decay::One_to_two_PSP(const fvector & P,const double & Q1_sq,const double & Q2_sq,
                             fvector & p1,fvector & p2,
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
     double p_sq=P*P;
     //: calulate the energy of p1 at the rest frame of P
     double E1=(p_sq+Q1_sq-Q2_sq)/2.0/sqrt(p_sq);
     //: the energy of p2
     double E2=sqrt(p_sq)-E1;
     //: the |p1_vector|
     double p1v=sqrt(E1*E1-Q1_sq);
     //: setting up the two vectors at the rest frame of P, so back to back
     p1 = fvector(E1,p1v*sintheta*sin(phi),p1v*sintheta*cos(phi),p1v*costheta);
     p2 = fvector(E2,-p1v*sintheta*sin(phi),-p1v*sintheta*cos(phi),-p1v*costheta);
     
     //: boosting back to the frame where P is defined
     //: i.e. with v = - Pvector / P_0
     p1.boost(-P[1]/P[0],-P[2]/P[0],-P[3]/P[0]);
     p2.boost(-P[1]/P[0],-P[2]/P[0],-P[3]/P[0]);
     
     //: returns the jacobian factor 4*pi times the PSP factor sqrt(lambda)/(8*P^2)
     return(4.0*pi/8.0/p_sq*sqrt(PSP_lambda(p_sq,Q1_sq,Q2_sq)));
}

double Decay::PSP_lambda(const double & x,const double & y,const double & z)
{
#ifdef debug
     cout<<"\n["<<__func__<<"]";
#endif
     return (x*x + y*y + z*z - 2.0*x*y - 2.0*y*z - 2.0*z*x);
}



/*
 void Decay::set_up_vector_bosons(double EnV1,double pzV1,double ptV1,
 double EnV2,double pzV2,double ptV2)
 {
 #ifdef debug
 cout<<"\n["<<__func__<<"]";
 #endif
 double totalphi1,totalphi2;
 double En=pH[0];
 double pZ=pH[3];
 double pH_sq=pH.square();
 double pT=pH.pT();
 double mv12 = EnV1*EnV1-pzV1*pzV1-ptV1*ptV1;
 double mv22 = EnV2*EnV2-pzV2*pzV2-ptV2*ptV2;
 if (pH.pT()<ptbuf) //: Higgs pT practically zero <---- set the ptbuf to some very low value
 {
 //: the two VBs are back to back in an angle phi from reference
 totalphi1 = 2.0*consts::Pi* decay_xx_vegas[9];
 totalphi2 = fmod(consts::Pi+totalphi1,2.0*consts::Pi);
 }
 else
 {
 double cosV1=(EnV1*En-pzV1*pZ+(mv22-mv12-pH_sq)/2.0)/ptV1/pT;
 double cosV2=(EnV2*En-pzV2*pZ+(mv12-mv22-pH_sq)/2.0)/ptV2/pT;
 double cosV1V2=(EnV1*EnV2-pzV1*pzV2+(mv12+mv22-pH_sq)/2.0)/ptV1/ptV2;
 double phi1=acos(cosV1);//: angle of V1 with respect to Higgs
 //: note that acos gives principal value of arc cosine
 //: in the region [0,pi]
 //: so both phi1 and phi2 are in the upper half 
 //:	of the phiplane 
 double phi1tilde=2.0*consts::Pi-phi1;//: same cos complementary angle
 double phi2=acos(cosV2);//:angle of V2 with respect to Higgs
 double phi2tilde=2.0*consts::Pi-phi2;//: same cos complementary angle
 //: there are two equivalent configurations, with phi1 in the upper
 //: or lower quadrant of the phi-plane
 
 if (decay_xx_vegas[9]>0.5)  //:phi1 up
 {
 totalphi1=phi1;
 if (abs(phi1-phi2)==acos(cosV1V2))
 {
 //nothing 
 }
 else
 {
 totalphi2=phi2tilde;
 }
 }
 else
 {
 totalphi1=phi1tilde;
 if (abs(phi1-phi2)==acos(cosV1V2))
 {
 
 totalphi2=phi2tilde;
 }
 else
 {
 //nothing
 }
 }
 //: now we have to rotate by the angle of the Higgs particle:
 totalphi1 = fmod(totalphi1 + pH.phi(), 2.0*consts::Pi);
 totalphi2 = fmod(totalphi2 + pH.phi(), 2.0*consts::Pi);
 }
 
 
 pVB1=fvector(EnV1,ptV1*cos(totalphi1),ptV1*sin(totalphi1),pzV1);
 pVB2=fvector(EnV2,ptV2*cos(totalphi2),ptV2*sin(totalphi2),pzV2);
 //: possible check to implement here: momentum conservation in transverse plain
 
 
 
 }
 
 void ExclusiveClass::set_up_massless_leptons(
 fvector & Mother,
 fvector & D1, fvector & D2,
 double E1,double pz1,
 double E2,double pz2,
 double my_random_number)
 {
 #ifdef debug
 cout<<"\n["<<__func__<<"]";
 #endif
 double pt1 = sqrt(E1*E1-pz1*pz1);//: massless leptons assumed
 double pt2 = sqrt(E2*E2-pz2*pz2);//: massless leptons assumed
 double totalphi1,totalphi2;
 double En=Mother[0];
 double pZ=Mother[3];
 double Mother_sq=Mother*Mother;
 double Mother_pT=Mother.pT();
 double cosV1=(E1*En-pz1*pZ+(-Mother_sq)/2.0)/pt1/Mother_pT;
 double cosV2=(E2*En-pz2*pZ+(-Mother_sq)/2.0)/pt2/Mother_pT;
 double cosV1V2=(E1*E2-pz1*pz2+(-Mother_sq)/2.0)/pt1/pt2;
 double phi1=acos(cosV1);//: angle of V1 with respect to Higgs
 //: note that acos gives principal value of arc cosine
 //: in the region [0,pi]
 //: so both phi1 and phi2 are in the upper half 
 //:	of the phiplane 
 double phi1tilde=2.0*consts::Pi-phi1;//: same cos complementary angle
 double phi2=acos(cosV2);//:angle of V2 with respect to Higgs
 double phi2tilde=2.0*consts::Pi-phi2;//: same cos complementary angle
 //: there are two equivalent configurations, with phi1 in the upper
 //: or lower quadrant of the phi-plane
 
 if (my_random_number>0.5)  //:phi1 up
 {
 totalphi1=phi1;
 if (abs(phi1-phi2)==acos(cosV1V2))
 {
 //nothing 
 }
 else
 {
 totalphi2=phi2tilde;
 }
 }
 else
 {
 totalphi1=phi1tilde;
 if (abs(phi1-phi2)==acos(cosV1V2))
 {
 
 totalphi2=phi2tilde;
 }
 else
 {
 //nothing
 }
 }
 //: now we have to rotate by the angle of the Mother particle:
 totalphi1 = fmod(totalphi1 + pH.phi() , 2.0*consts::Pi);
 totalphi2 = fmod(totalphi2 + pH.phi() , 2.0*consts::Pi);
 
 D1=fvector(E1,pt1*cos(totalphi1),pt1*sin(totalphi1),pz1);
 D2=fvector(E2,pt2*cos(totalphi2),pt2*sin(totalphi2),pz2);
 //: possible check to implement here: momentum conservation in transverse plain
 
 }
 
 double Decay::decay_WW()
 {
 #ifdef debug
 cout<<"\n["<<__func__<<"]";
 #endif
 double scm = pow(Etot,2.0);
 double mW=Model.W.m;
 double mV2=pow(mW,2.0);
 double gamV=Model.W.G;
 double y1 = decay_xx_vegas[0];
 double y2 = decay_xx_vegas[1];  
 double mH2=pow(Model.higgs.m,2.0);  
 double z=mH2/shat; 
 //if(zerowidth.eq.0) then
 double z1min = atan(-mW/gamV);
 double z1max = atan((mH2-mV2)/gamV/mW);
 double z1 = z1min + y1*(z1max-z1min);
 double mv12 = sqrt(mV2)*gamV*tan(z1)+mV2;
 double z2min = z1min;
 double z2max=atan((pow(sqrt(mH2)-sqrt(mv12),2.0)-mV2)/sqrt(mV2)/gamV);
 double z2 = z2min + y2*(z2max-z2min);
 double mv22 = sqrt(mV2)*gamV*tan(z2)+mV2;
 //:
 //:narrow width for W bosons below
 //      else
 //         z1min=-pi/2d0
 //         z1max=pi/2d0
 //         z2min=-pi/2d0
 //         z2max=pi/2d0   
 //         mv12=mV2
 //         mv22=mV2
 //      endif
 
 
 //: higgs width provision below
 //if(higgswidth.gt.0) then
 //         zhmin=datan(-nomm2/gamH/dsqrt(nomm2))
 //         zhmax=datan((scm-nomm2)/gamH/dsqrt(nomm2))
 //      else
 double zhmin= -consts::Pi/2.0;
 double zhmax= consts::Pi/2.0;
 //      endif
 double sz = sqrt(z);
 
 double zv1=mv12/shat;
 double zv2=mv22/shat;
 double szv1=sqrt(zv1);
 double szv2=sqrt(zv2);
 double delmpp = szv1+szv2+sz;
 double delmmm = szv1-szv2-sz;
 double delmpm = szv1+szv2-sz;
 double delmmp = szv1-szv2+sz;
 double prodelm = delmpp*delmmm*delmmp*delmpm;
 double squdelm = sqrt(prodelm);
 
 //C     -------------------------------------------------------
 //C     now calculate the B1w and B2w for the decay H->VV
 //C     -------------------------------------------------------
 double lam2 = decay_xx_vegas[3];
 double lam1 = decay_xx_vegas[4];
 double C1h = z-s1v;
 double C2h = z-s2v;
 double B2w = C2h/2.0/z*(zv2-zv1-z-(1.0-2.0*lam2)*squdelm);
 double B1w=	 (1.0-2.0*lam2)*squdelm/C2h
 +C1h/2.0/z*(zv2-zv1+(-1.0+2.0*lam2)*squdelm)
 -C1h/2.0
 +2.0/pow(C2h,2.0)	*cos(consts::Pi*lam1)
 *sqrt(
 abs(
 pow(C2h,2.0)/z
 *lam2
 *(1.0-lam2)
 *(C1h*C2h-z)
 )
 )
 *squdelm;
 double D2w = -C2h-B2w;
 double D1w = -C1h-B1w;
 
 //C     ------------------------------------------------------
 //C     create lepton invariant masses
 //C     ------------------------------------------------------
 double gam2 = decay_xx_vegas[5];
 double gam1 = decay_xx_vegas[6];
 double a12l=(1.0-gam2)*B2w;
 double a11l=	B1w*(1.0-gam2)
 +(-1.0+2.0*gam2)*zv1/B2w
 +2.0*cos(consts::Pi*gam1)
 *sqrt((-1.0+gam2)*gam2*zv1*(-B1w*B2w+zv1)/pow(B2w,2.0)) ;
 
 
 double a22l = B2w-a12l;
 double a21l = B1w-a11l;
 
 double del2 = decay_xx_vegas[7];
 double del1 = decay_xx_vegas[8];
 double b12l=(1.0-del2)*D2w;
 double b11l=D1w*(1.0-del2)+(-1.0+2.0*del2)*zv2/D2w+2.0*cos(consts::Pi*del1)*
 sqrt((-1.0+del2)*del2*zv2*(-D1w*D2w+zv2)/pow(D2w,2.0)) ;
 double b22l = D2w-b12l;
 double b21l = D1w-b11l;
 
 //C     ------------------------------------------------------
 //C     form VB kinematic variables
 //C     ------------------------------------------------------
 
 double EnV1 = - (p1[0]*B2w+p2[0]*B1w);
 double EnV2 = - (p1[0]*D2w+p2[0]*D1w);
 
 double pzV1 = (p2[0]*B1w-p1[0]*B2w);
 double pzV2 = (p2[0]*D1w-p1[0]*D2w);
 
 double ptV1 = sqrt(shat*(B1w*B2w-zv1));
 double ptV2 = sqrt(shat*(D1w*D2w-zv2));
 
 
 
 
 //C     ----------------------------------------------------------------
 //C     form final state leptons kinematic
 //C     V1 -> l11 l12
 //C     V2 -> l21 l22
 //C     ----------------------------------------------------------------
 double Enl11 =  - (p2[0]*a11l + p1[0]*a12l);
 double Enl12 =  - (p2[0]*a21l + p1[0]*a22l);
 double Enl21 =  - (p2[0]*b11l + p1[0]*b12l);
 double Enl22 =  - (p2[0]*b21l + p1[0]*b22l);
 
 double pzl11 = (p2[0]*a11l-p1[0]*a12l);
 double pzl12 = (p2[0]*a21l-p1[0]*a22l);
 double pzl21 = (p2[0]*b11l-p1[0]*b12l);
 double pzl22 = (p2[0]*b21l-p1[0]*b22l);
 
 //double ptl11 = dsqrt(x1*x2*scm*a11l*a12l);
 //double ptl12 = dsqrt(x1*x2*scm*a21l*a22l);
 //double ptl21 = dsqrt(x1*x2*scm*b11l*b12l);
 //double ptl22 = dsqrt(x1*x2*scm*b21l*b22l);
 
 //double etal11=0.5d0*dlog((Enl11+pzl11)/(Enl11-pzl11));
 //double etal12=0.5d0*dlog((Enl12+pzl12)/(Enl12-pzl12));
 //double etal21=0.5d0*dlog((Enl21+pzl21)/(Enl21-pzl21));
 //double etal22=0.5d0*dlog((Enl22+pzl22)/(Enl22-pzl22));
 
 
 set_up_vector_bosons(EnV1,pzV1,ptV1,EnV2,pzV2,ptV2);
 set_up_massless_leptons(pVB1,pL1a,pL1b,Enl11,pzl11,Enl12,pzl12,decay_xx_vegas[9]);
 set_up_massless_leptons(pVB2,pL2a,pL2b,Enl21,pzl21,Enl22,pzl22,decay_xx_vegas[10]);
 
 double s24 = pL1a*pL2b;
 double s35 = pL1b*pL2a;
 double s23 = pL1a*pL1b;
 double s45 = pL2a*pL2b;
 
 double s45_sq=pow(s45,2.0);
 double s23_sq=pow(s23,2.0);
 double mw_sq_sq=pow(Model.W.m,4.0);
 double mw_sq=pow(Model.W.m,2.0);
 double Gamma_W_sq=pow(Model.W.G,2.0);
 
 double pV1_sq=pVB1.square();
 double pV2_sq=pVB2.square();
 
 double numerator = 4.0*s24*s35 * pow(g_ew,4.0)*pow(consts::vev,2.0);
 double denominator = (s45_sq+mw_sq_sq-2.0*mw_sq*s45+mw_sq*Gamma_W_sq)
 *(s23_sq+mw_sq_sq-2.0*mw_sq*s23+mw_sq*Gamma_W_sq);
 double MWWlnln = numerator/denominator;
 double jac = 1.0/(2048.0*mw_sq*Gamma_W_sq*pow(consts::Pi,3.0))
 *(pV1_sq+mw_sq_sq+mw_sq*(Gamma_W_sq-2.0*pV1_sq))
 *(pV2_sq+mw_sq_sq+mw_sq*(Gamma_W_sq-2.0*pV2_sq))
 *squdelm*(z1max-z1min)*(z2max-z2min)
 *(zhmax-zhmin)/sqrt(mH2)/Model.higgs.m/z;
 return( jac*MWWlnln ); 
 
 }
 
 */



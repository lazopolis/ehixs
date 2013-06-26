/** \file histograms.hpp
  *
  * This file defines defines standard histogram derived from CHistogram.
  */




/** \brief Histograms on fvectors member functions
  *
  * ... See MinCut for more informations ...
  */
template<double (fvector::* pFun)() const>
struct VecHistogram : public CHistogram
{
  /** \brief Constructor
    *
    * See CHistogram::CHistogram(). */
  VecHistogram(string histogramed_quantity,unsigned nbins, unsigned firstbin, const double& lowend, const double& highend, const string& name, bool adapt=false)
    : CHistogram(nbins, firstbin, lowend, highend, name, adapt)
     {hist_quant=histogramed_quantity;}
     string hist_quant;
  /** \brief Call ((X->*pMem).*pFun)() */
  double determine_xval(Event* X)
  { return ((&(X->p[hist_quant]))->*pFun)(); }
};

/** \brief Histogram on pT of the Higgs. */
typedef VecHistogram<&fvector::pT>     Hist_PT;
/** \brief Histogram on zrap of the Higgs. */
typedef VecHistogram<&fvector::zrap>   Hist_rap;


struct Xhistogram:public CHistogram
{
     /** \brief Constructor
      *
      * See CHistogram::CHistogram(). */

     Xhistogram(int histogramed_vegas_var_,unsigned nbins,  const string& name, bool adapt=false)
     : CHistogram(nbins, 0, 0.0, 1.0, name, adapt)
     {histogramed_vegas_var=histogramed_vegas_var_;}
     int histogramed_vegas_var;
     /** \brief Call ((X->*pMem).*pFun)() */
     double determine_xval(Event* X)
     { return (X->xx_vegas[histogramed_vegas_var]); }
};


//#include "mlp.h"
//
///** \brief Neural network based cut
//  *
//  * ... */
//class ANN : public CHistogram
//{
//  /** \brief Perceptron */
//  MultilayerPerceptron my_trained_ann;
//
//public: 
//  ANN(unsigned nbins, unsigned firstbin, const double& lowend, const double& highend, const string& name, bool adapt=false)
//    : CHistogram(nbins, firstbin, lowend, highend, name, adapt) {}
//
//  /** \brief Determine input from NN
//    *
//    * ... some more infos ? */
//  double determine_xval(Momenta* X)
//  {
//    //: wouldn't it be better to have only one function instead of ProcessInput and GetOutput ? 
//    // The network expects an array of 5 unnormalized floats //: actually doubles
//    // as input to MultilayerPerceptron::ProcessInput(float *inputs)
//    // inputs[0] = absolute 3-momentum of photon system (=abs(p1+p2))
//    // inputs[1] = invariant mass of photon system
//    // inputs[2] = rapidity of photon system //: rapidity or pseudorapidity ? 
//    // inputs[3] = energy separation of the photons (=E_photon1 - E_photon2) (positive)  
//    // inputs[4] = rapidity difference of the photons (=eta_photon1 - eta_photon2)  
//    // photon1 is the leading photon
//
//    fvector Phot1, Phot2;
//    //: deciding on which is the leading photon
//    if ( (X->pL1a[0]) > (X->pL1b[0])  )
//    {
//      Phot1 = X->pL1a;
//      Phot2 = X->pL1b;
//    }
//    else
//    {
//      Phot1 = X->pL1b;
//      Phot2 = X->pL1a;
//    }
//
//    fvector Gamma_system=Phot1+Phot2;
//
//    double input[5];
//    input[0] = Gamma_system.abs_of_three_momentum();
//    input[1] = sqrt(Gamma_system.square());
//    input[2] = Gamma_system.zrap();
//    input[3] = Phot1[0]-Phot2[0];
//    input[4] = Phot1.zrap()-Phot2.zrap();
//    my_trained_ann.ProcessInput(input);
//
//    //cout<<"*"<<X->pH.pT();
//    double res =  my_trained_ann.GetOutput(0);
//    //cout<<"\n * ="<<res;
//    //for (int i=0;i<5;i++){cout<<" " <<input[i];}
//    return res;
//  }
//};


/** \brief Neural network based cut
  *
  * ... */

// class XXHistogram : public CHistogram
//{
//  unsigned i;
//
//public: 
//  XXHistogram(unsigned i_, unsigned nbins, unsigned firstbin, const double& lowend, const double& highend, const string& name, bool adapt=false)
//    : CHistogram(nbins, firstbin, lowend, highend, name, adapt), i(i_) {}

  /** \brief Determine input from NN
    *
    * ... some more infos ? */
//  double determine_xval(Momenta* X)
//  {
//    return X->xx_vegas[i];
//  }
//};

///** \brief Histograms on fvectors member functions
//  *
//  * ... See MinCut for more informations ...
//  */
//template<fvector Momenta::* pMem, double (fvector::* pFun)() const>
//struct VecHistogram : public CHistogram
//{
//  /** \brief Constructor
//    *
//    * See CHistogram::CHistogram(). */
//  VecHistogram(unsigned nbins, unsigned firstbin, const double& lowend, const double& highend, const string& name, bool adapt=false)
//    : CHistogram(nbins, firstbin, lowend, highend, name, adapt)
//  {}
//
//  /** \brief Call ((X->*pMem).*pFun)() */
//  double determine_xval(Event* X)
//  { return ((&(X->*pMem))->*pFun)(); }
//};
//
///** \brief Histogram on pT of the Higgs. */
//typedef VecHistogram<&Momenta::pH, &fvector::pT>     H_pt_higgs;
///** \brief Histogram on zrap of the Higgs. */
//typedef VecHistogram<&Momenta::pH, &fvector::zrap>   H_zrap_higgs;
///** \brief Histogram on pT of the Gamma. */
//typedef VecHistogram<&Momenta::pL1a, &fvector::pT>    H_pt_gamma;
///** \brief Histogram on zrap of the Gamma. */
//typedef VecHistogram<&Momenta::pL1a, &fvector::zrap> H_zrap_gamma;
//
//
///** \brief Histogram on Jet_Rates. The jet algorithm implemented is the Cambridge-AAchen, but it makes no difference for two partons if it's CA, or kt or anti-kt */
//class JetRatesHistogram : public CHistogram
//{
//    double ptmax,R,pt_star;
//    
//public: 
//    JetRatesHistogram(const double& ptmax_,const double& R_, const string& name)
//    : CHistogram(3, 0, 0.0,3.0 , name, false), ptmax(ptmax_),R(R_),pt_star(1e-10) {}
//    
//    /** \brief Determine which 
//     *
//     * ... some more infos ? */
//    double determine_xval(Momenta* X)
//    {
//        
//        const double ymax=4.8;
//        //: calculate pt3, pt4
//        const double pt3 = X->p3.pT();
//        const double pt4 = X->p4.pT();
//        //: p3 and p4 are ultra soft
//        if (pt3<=pt_star and pt4<=pt_star) return 0.5; //0-jet bin
//        //: p3 ultra soft
//        if (pt3<=pt_star and pt4>pt_star) 
//        {
//            const double y4=X->p4.zrap();
//            double res=0.5;
//            res += add_one_if_jet(pt4,y4,ymax);
//            //if (pt4>ptmax and abs(y4)<4.8) return 1.5; //p4 hard
//            //else return 0.5; //p4 soft
//            return res;
//        }
//        //: p4 ultra soft
//        if (pt4<=pt_star and pt3>pt_star) 
//        {
//            const double y3=X->p3.zrap();
//            double res=0.5;
//            res += add_one_if_jet(pt3,y3,ymax);
//            //if (pt4>ptmax and abs(y4)<4.8) return 1.5; //p4 hard
//            //else return 0.5; //p4 soft
//            return res;
//        }
//        //: none ultra soft, this is an event coming from honest RR
//        if (pt3>pt_star and pt4>pt_star)
//        {
//            //const double pi = 3.141592653589793;
//            //: now we have to calculate angular seperation
//            const double y3=X->p3.zrap();
//            const double y4=X->p4.zrap();
//            //cout<<"\n phi3 "<<acos(0.7);
//            //cout<<"\n phi4 "<<acos(-0.7);
//            double delta_phi = X->p3.phi() -  X->p4.phi()  ;
//            //: taking care of the dphi>Pi case
//            //if (delta_phi>pi) {delta_phi = 2*pi -delta_phi;cout<<"\n it happened";}
//            const double d12 = pow(y3-y4,2.0) + pow(delta_phi,2.0);
//            //cout<<"\n-----\nd12="<<d12<<"\tRR="<<R<<"\t delta_phi^2="<<delta_phi
//            //<<"\t"<<X->p3.phi()<<"\t"<<X->p4.phi();
//            //cout<<"\np3="<<X->p3<<"\tp4="<<X->p4;
//            if (d12>R*R)//: potential 2-jet case / no parton merging
//            {
//                double res=0.5;
//                res += add_one_if_jet(pt3,y3,ymax);
//                res += add_one_if_jet(pt4,y4,ymax);
//                return res;
//            }
//            else //: we need to merge the two partons in one jet 
//            {
//                fvector pjet = X->p3+X->p4;
//                double res=0.5;
//                res += add_one_if_jet(pjet.pT(),pjet.zrap(),ymax);
//                return res;
//                
//            }
//        }
//    }
//    
//    double add_one_if_jet(const double& pt, const double & y,const double& y_max)
//    {
//        if (pt>ptmax and y<y_max)
//        {
//            return 1.0;
//        }
//        else {
//            return 0.0;
//        }
//    }
//};
//
//
///** \brief Histogram : average pT of two photons in H-> gamma gamma. 
// */
//class AveragePTofPhotons : public CHistogram
//{    
//public: 
//    AveragePTofPhotons(unsigned nbins, unsigned firstbin, const double& lowend, const double& highend, const string& name, bool adapt=false)
//    : CHistogram(nbins, firstbin, lowend,highend , name, false){}
//    
//    /** \brief Determine which 
//     *
//     * ... some more infos ? */
//    double determine_xval(Momenta* X)
//    {
//        return((X->pL1a.pT()+X->pL1b.pT())/2.0 );
//    }
//    
//};
//
///** \brief Histogram :  pT of leading photon  in H-> gamma gamma. 
// */
//class PTofLeadingPhoton : public CHistogram
//{    
//public: 
//    PTofLeadingPhoton(unsigned nbins, unsigned firstbin, const double& lowend, const double& highend, const string& name, bool adapt=false)
//    : CHistogram(nbins, firstbin, lowend,highend , name, false){}
//    
//    /** \brief Determine which 
//     *
//     * ... some more infos ? */
//    double determine_xval(Momenta* X)
//    {
//        return(max(X->pL1a.pT(),X->pL1b.pT()));
//    }
//    
//};
//
///** \brief Histogram :  pT of traling photon  in H-> gamma gamma. 
// */
//class PTofTralingPhoton : public CHistogram
//{    
//public: 
//    PTofTralingPhoton(unsigned nbins, unsigned firstbin, const double& lowend, const double& highend, const string& name, bool adapt=false)
//    : CHistogram(nbins, firstbin, lowend,highend , name, false){}
//    
//    /** \brief Determine which 
//     *
//     * ... some more infos ? */
//    double determine_xval(Momenta* X)
//    {
//        return(min(X->pL1a.pT(),X->pL1b.pT()));
//    }
//    
//};
//
///** \brief Histogram :  Y* = 1/2 |y1-y2|   in H-> gamma gamma. 
// */
//
//class Ystar : public CHistogram
//{    
//public: 
//    Ystar(unsigned nbins, unsigned firstbin, const double& lowend, const double& highend, const string& name, bool adapt=false)
//    : CHistogram(nbins, firstbin, lowend,highend , name, false){}
//    
//    /** \brief Determine which 
//     *
//     * ... some more infos ? */
//    double determine_xval(Momenta* X)
//    {
//        return(0.5*abs(X->pL1a.zrap()-X->pL1b.zrap()));
//    }
//    
//};
//
//
//
///** \brief Histogram : total cross section : ONE BIN HISTOGRAM that keeps track of total sigma
// */
//
//class Total_XS : public AverageObservable
//{    
//public: 
//    Total_XS(const string& name)
//    : AverageObservable(name){}
//    
//    /** \brief Determine which 
//     *
//     * ... some more infos ? */
//    double determine_xval(Momenta* X)
//    {
//        return(1.0);
//    }
//};
//
//
///** \brief Histogram : Average PT : ONE BIN HISTOGRAM 
// */
//
//class PT_H_AVG : public AverageObservable
//{    
//public: 
//    PT_H_AVG(const string& name)
//    : AverageObservable(name){}
//    
//    /** \brief Determine which 
//     *
//     * ... some more infos ? */
//    double determine_xval(Momenta* X)
//    {
//        return(X->pH.pT());
//    }
//};
//
///** \brief Histogram : Average |Y_H| : ONE BIN HISTOGRAM 
// */
//class ABS_Y_H_AVG : public AverageObservable
//{    
//public: 
//    ABS_Y_H_AVG(const string& name)
//    : AverageObservable(name){}
//    
//    /** \brief Determine which 
//     *
//     * ... some more infos ? */
//    double determine_xval(Momenta* X)
//    {
//        return(X->pH.abszrap());
//    }
//};
//
//
///** \brief Histogram on Jet_Rates. The jet algorithm implemented is the Cambridge-AAchen, but it makes no difference for two partons if it's CA, or kt or anti-kt */
//class ZYHIST : public CHistogram2d
//{    
//    public: 
//        ZYHIST(unsigned numbins1_,  
//           const double& lowend1_, const double& highend1_, 
//           unsigned numbins2_,  
//           const double& lowend2_, const double& highend2_, 
//           const std::string& name_)
//        : CHistogram2d(numbins1_, lowend1_, highend1_,numbins2_ , lowend2_, highend2_, name_){}
//    
//        /** \brief Determine which 
//         *
//         * ... some more infos ? */
//        double determine_xval1(Momenta* X){return(X->pH.pT());}
//        double determine_xval2(Momenta* X){return(abs(X->pH.zrap()));}
//};
//



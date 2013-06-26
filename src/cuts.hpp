/** \file cuts.hpp
  *
  * This files defines standard cuts used in most analysis.
  */

/** \brief Generic cut on momentum.
  *
  * This template class uses a pointer on a fvector pMem member of the class ExclusiveClass to call a fvector member function pointed by pFun. See examples after this declaration. */
template<double (fvector::* pFun)() const>
class MinCut : public CCut
{
  
     const string variable;
public:
     MinCut(const string& _var,double _min,string _name) : variable(_var),CCut::CCut(_name,_min) {}

	bool operator()(Event * XX)
    { if (((&(XX->p[variable]))->*pFun)()-min >0.0) return true; else return false; }
};


template<double (fvector::* pFun)() const>
class BinCut : public CCut
{
     
     const string variable;
public:
     BinCut(const string& _var,double _min,double _max,string _name) : variable(_var),CCut::CCut(_name,_min) {max = _max;}
     
	bool operator()(Event * XX)
     { if (((&(XX->p[variable]))->*pFun)()-min >0.0 and ((&(XX->p[variable]))->*pFun)()<max) return true; else return false; }
     double max;
};

///** \brief Cut on the pT  */
typedef MinCut<   &fvector::pT> Pt_cut;
///** \brief Bin on the pT  */
typedef BinCut<   &fvector::pT> Pt_bin;
///** \brief Cut on the Y  */
typedef MinCut< &fvector::abszrap> Abs_y_cut;


//
//
///** \brief Generic cut on momentum.
// *
// * This template class uses a pointer on a fvector pMem member of the class Momenta to call a fvector member function pointed by pFun. See examples after this declaration. */
//template<fvector Momenta::* pMem, double (fvector::* pFun)() const>
//class MaxCut : public CCut
//{
//    const double min;
//    
//public:
//    MaxCut(double _min) : min(_min) {}
//    
//	bool operator()(Momenta * XX)
//    { if (((&(XX->*pMem))->*pFun)()-min <0.0) return true; else return false; }
//};
//
//
///** \brief Cut on the pT of the Higgs. */
//typedef MinCut<&Momenta::pH,   &fvector::pT> pt_higgs;
///** \brief Cut on the pT of the Gamma. */
//typedef MinCut<&Momenta::pL1a, &fvector::pT> pt_gamma1;
///** \brief Cut on the Y of the Gamma1. */
//typedef MaxCut<&Momenta::pL1a, &fvector::abszrap> Y_gamma1;
///** \brief Cut on the Y of the Gamma2. */
//typedef MaxCut<&Momenta::pL1b, &fvector::abszrap> Y_gamma2;
///** \brief Cut on pT3. */
//typedef MinCut<&Momenta::p3,   &fvector::pT> pt_3_cut;
///** \brief Cut on pT4. */
//typedef MinCut<&Momenta::p4,   &fvector::pT> pt_4_cut;
//
//
//class AveragePhotonPTBIN : public CCut
//{
//    const double min,max;
//    
//public:
//    AveragePhotonPTBIN(double _min,double _max) : min(_min),max(_max) {}
//    
//	bool operator()(Momenta * XX)
//    { 
//        double AVPT=(XX->pL1a.pT()+XX->pL1b.pT())/2.0;
//        
//        if (AVPT>min and AVPT<max) return true; 
//        else return false; 
//    }
//};
//
//class YSTARBIN : public CCut
//{
//    const double min,max;
//    
//public:
//    YSTARBIN(double _min,double _max) : min(_min),max(_max) {}
//    
//	bool operator()(Momenta * XX)
//    { 
//        double YSTAR=0.5*abs(XX->pL1a.zrap()-XX->pL1b.zrap());
//        
//        if (YSTAR>min and YSTAR<max) return true; 
//        else return false; 
//    }
//};
//
//
//class LeadingPhotonPTBIN : public CCut
//{
//    const double min,max;
//    
//public:
//    LeadingPhotonPTBIN(double _min,double _max) : min(_min),max(_max) {}
//    
//	bool operator()(Momenta * XX)
//    { 
//        double LeadPT=0.0;
//        if (XX->pL1a.pT()>XX->pL1b.pT())
//        {
//            LeadPT = XX->pL1a.pT();
//        }
//        else 
//        {
//            LeadPT = XX->pL1b.pT();
//        }
//        if (LeadPT>min and LeadPT<max) return true; 
//        else return false; 
//    }
//};
//
//class LeadingPhotonPT : public CCut
//{
//    const double min;
//    
//public:
//    LeadingPhotonPT(double _min) : min(_min) {}
//    
//	bool operator()(Momenta * XX)
//    { 
//        double LeadPT=0.0;
//        if (XX->pL1a.pT()>XX->pL1b.pT())
//        {
//            LeadPT = XX->pL1a.pT();
//        }
//        else 
//        {
//            LeadPT = XX->pL1b.pT();
//        }
//        if (LeadPT-min >0.0) return true; 
//        else return false; 
//    }
//};
//
//class LeadingPhotonPTMAX : public CCut
//{
//    const double maxx;
//    
//public:
//    LeadingPhotonPTMAX(double _maxx) : maxx(_maxx) {}
//    
//	bool operator()(Momenta * XX)
//    { 
//        const double pt1=XX->pL1a.pT();
//        const double pt2=XX->pL1b.pT();
//        double LeadPT ;
//        if (pt1>pt2)
//        {
//            LeadPT = pt1;
//        }
//        else 
//        {
//            LeadPT = pt2;
//        }
//        if (LeadPT<maxx) return true; 
//        else return false; 
//    }
//};
//
//
//class TrailingPhotonPT : public CCut
//{
//    const double min;
//    
//public:
//    TrailingPhotonPT(double _min) : min(_min) {}
//    
//	bool operator()(Momenta * XX)
//    { 
//        double TrailPT=0.0;
//        if (XX->pL1a.pT()<XX->pL1b.pT())
//        {
//            TrailPT = XX->pL1a.pT();
//        }
//        else 
//        {
//            TrailPT = XX->pL1b.pT();
//        }
//        if (TrailPT-min >0.0) return true; 
//        else return false; 
//    }
//};
//
//
//
//class PhotonIsolationCut : public CCut
//{
//    const double maxPT,Rsize;
//    
//public:
//    PhotonIsolationCut(const double & maxPT_,const double & Rsize_) : maxPT(maxPT_),Rsize(Rsize_) {}
//    
//	bool operator()(Momenta * XX)
//    { 
//        if (XX->jetnumber==1)//: if there is one jet
//        {
//            if (too_close(XX->pjet1,XX->pL1a) or too_close(XX->pjet1,XX->pL1b)) 
//                return false;
//            else
//                return true;
//        }
//        else if (XX->jetnumber==2)//: if there are two jets
//        {
//            if (too_close(XX->pjet1,XX->pL1a) or too_close(XX->pjet1,XX->pL1b) or too_close(XX->pjet2,XX->pL1a) or too_close(XX->pjet2,XX->pL1b)) 
//                return false;
//            else
//                return true; 
//        }
//        else
//        {
//            return true;
//        }
//    }
//    
//    bool too_close(const fvector & pjet, const fvector& pgamma)
//    {
//        if (pjet.pT()>maxPT) //: the jet is hard
//        {
//            const double dd = pow(pjet.zrap()-pgamma.zrap(),2.0) + pow(pjet.phi()-pgamma.phi(),2.0);
//            if (dd<Rsize*Rsize)//:too close
//            {
//                return true;
//            }
//            else return false;
//        }
//        else return false;
//    }
//};
//
//















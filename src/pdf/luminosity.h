#ifndef LUMINOSITY_H
#define LUMINOSITY_H

/**
 *
 * \file    luminosity.h
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 * \date    February 2015
 *
 */

#include <string>
using namespace std;

#include "LHAPDF/LHAPDF.h"


/**
 *
 * \class   Luminosity
 * \ingroup tools
 * \brief   Interface with LHAPDFs. It initializes a PDF member (one of the many of a specific PDF grid). Then the user can add luminosity pairs (initial states) with a potential constant factor that depends on the pair. Once configured, the class provides L(x1,x2,muf) = Sum_i {x1*f1_i(x1,muf) * x2*f2_i(x2,muf) * c_i}
 * \todo
 *
 */

class Luminosity{
private:
    /// \name Private data members
    /// @{
    /// \brief pointer to a whole PDF member (includes all flavors)
    LHAPDF::PDF* _pdf;///<
    
    /// \brief each pair is a different initial state
    vector<pair<int,int> > _pairs;

    /// \brief potential coefficient that is different for every initial state pair
    vector<double> _coeff;
    
    /// \brief technical cutoff: we don't allow bjorken xs to go closer to the edges than this cutoff.
    static const double _almost_zero;
    /// @}
public:
    /// \name Constructors and destructors
    /// @{
    Luminosity(const string& gridname);
    
    ~Luminosity(){delete _pdf;}///< we are responsible for deleting the LHAPDF pointer
    
    ///@}
    
    /// \name Input/Output
    /// @{
    
    /// \brief adds a pair of initial states by number id
    void addPair(int left,int right);
    /// \brief adds a pair of initial states by number id, *and* a constant. The constant multiplies the luminosity contribution from this pair of initial states. (Constant coeff defaults to 1.0)
    
    void addPair(int left,int right,const double& c);
    /// \brief clears the class data
    /// \todo where is this used? Is it really useful?

    void clear_pairs(){_pairs.clear();_coeff.clear();}
    /// \brief returns the full luminosity Sum_i {x1*f1_i(x1,muf) * x2*f2_i(x2,muf) * c_i}
    
    double give(const double& x1,const double& x2,const double& muf);
    /// \brief alpha_s_at_mz
    /// \warning mz is at its nominal pdg value here - might not be in sync with model
    
    double alpha_s_at_mz(){return _pdf->alphasQ(91.1876);}
    ///@}
};


//---------------- legacy code below



//#include "pdf.h"
//#include "pdf_hub.h"
//#include "constants.h" // for QCD namespace
//#include <utility>
//#include "one_d_interpolator.h"
//#include "user_interface.h"


/*
class LuminositySinglePair
{
public:
    LuminositySinglePair(CPDF* left,CPDF* right):left_(left),right_(right){};
    double give(const double& x1,const double& x2);
private:
    CPDF* left_;
    CPDF* right_;
};





class Luminosity
{
public://data
        
public://methods

    Luminosity(const UserInterface& UI);
    ~Luminosity(){};
    LuminositySinglePair* give_single_pair_pt(const pdf_desc & ,
                                              const pdf_desc &);
    void add_pair(const pdf_desc & , const pdf_desc &);
    void set_cur_lumi(const double &x1,const double &x2);
    void set_cur_lumiLO(const double &x1,const double &x2);
    
    void set_cur_lumi(const vector<double> &x1,const vector<double> &x2);
    void set_cur_lumiLO(const vector<double> &x1,const vector<double> &x2);
    
    double LL(unsigned i) {return _local_current_luminosity[i];}
    double LL_LO(unsigned i) {return _local_current_luminosity_LO[i];}

    unsigned pdf_size() const {return pdfHub->size();}
    vector<double> alpha_s_at_mz_vector(){return pdfHub->alpha_s_at_mz();}
    vector<double> calculate_pdf_error(const vector<double> xx)
                        {return pdfHub->calculate_pdf_error(xx);}
private://data
    vector< pair<CPDF*,CPDF*> >     pairs;
    vector<double> _local_current_luminosity;
    vector<double> _local_current_luminosity_LO;
    PDFHub* pdfHub;
    vector<LuminositySinglePair*> all_single_pairs_;
private://methods
    void set_specific_lumi(const vector<double>& x1, const vector<double>& x2, vector<double>* which_lumi);
    void set_specific_lumi(const double& x1, const double& x2, vector<double>* which_lumi);

};


class LuminosityStack
{
public:
    LuminosityStack(){};
    void set(Luminosity*, const string& description);
    void add(LuminositySinglePair* new_pair)
    {all_lumis_.push_back(new_pair);}
    double give(const double& x1,const double& x2);    
private:
    vector<LuminositySinglePair*> all_lumis_;
};

#endif

#ifndef PDFPAIRLIST_H
#define PDFPAIRLIST_H

typedef pair<pdf_desc,pdf_desc> pdf_pair;

class pdf_pair_list
{
public:
     pdf_pair_list(){};
     void add_pair(pdf_desc ll, pdf_desc rr){mylist.push_back(pdf_pair(ll,rr));}
     vector<pdf_pair> lumilist(){return mylist;}
     int size(){return int(mylist.size());}
     pdf_pair give_one_pair(int i){return mylist[i];}
private:
     vector<pdf_pair> mylist;
};
 
 */
#endif







#ifndef LUMINOSITY_H
#define LUMINOSITY_H

#include "pdf.h"
#include "pdf_hub.h"
#include "constants.h"
#include <utility>
#include <string>
#include "one_d_interpolator.h"
#include "user_interface.h"
using namespace std;

#include "LHAPDF/LHAPDF.h"

class NewLuminosity{
public:
    NewLuminosity(const UserInterface& UI);
    ~NewLuminosity(){delete pdf_;}
    
    void add_pair(int left,int right)
    {
        pairs.push_back(pair<int,int>(left,right));
        coeff_.push_back(1.);
    }
    void add_pair(int left,int right,const double& c)
    {
        pairs.push_back(pair<int,int>(left,right));
        coeff_.push_back(c);
    }
    
    void clear_pairs(){pairs.clear();coeff_.clear();}
    double give(const double& x1,const double& x2);

    double alpha_s_at_mz(){return pdf_->alphasQ(91.1876);}
private:
    LHAPDF::PDF* pdf_;
    double muf_;
    vector<pair<int,int> > pairs;
    vector<double> coeff_;
    // if any of x1, x2 are closer to 0 or 1 than _almost_zero we will return 0.0
    static const double _almost_zero;
private:
    string determine_gridname(const string& provider, int order,const string&);
};





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
#endif







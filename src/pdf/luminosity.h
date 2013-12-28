#ifndef LUMINOSITY_H
#define LUMINOSITY_H

#include "pdf.h"
#include "constants.h"
#include <utility>
#include <string>
#include "one_d_interpolator.h"
#include "user_interface.h"
using namespace std;

class LuminositySinglePair
{
public:
    LuminositySinglePair(CPDF* left,CPDF* right):left_(left),right_(right){};
    double give(const double& x1,const double& x2);
private:
    CPDF* left_;
    CPDF* right_;
};

class Luminosity;


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







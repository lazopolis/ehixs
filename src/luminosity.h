#ifndef LUMINOSITY_H
#define LUMINOSITY_H

#include "pdf.h"
#include "constants.h"
#include <utility>
#include <string>
#include "one_d_interpolator.h"
#include "user_interface.h"
using namespace std;

class Luminosity;


class Luminosity
{
public://data
        
public://methods

    Luminosity(const UserInterface& UI);
    ~Luminosity(){};
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
private://methods    

};
//
//class InterpolatedLuminosity:public Luminosity{
//public:
//     InterpolatedLuminosity(double NF_, double muf_, double mur_, int pert_order_, const string& provider_, bool pdf_error_,const double & _tau)
//     :Luminosity(NF_,muf_,mur_,pert_order_,provider_,pdf_error_) ,tau(_tau){};
//     ~InterpolatedLuminosity(){};
//     double integrate_out_x1(const double &z);
//     double ff(const double& z){return integrate_out_x1(z);}
//
//     
//     double tau;
//     
//};
//
//class LuminosityInterpolator: public InterpolatorBase
//{
//public:
//     LuminosityInterpolator(InterpolatedLuminosity* _dd):InterpolatorBase(){dd=_dd;}
//private:
//     InterpolatedLuminosity *dd;
//     double f_value(const double & x){return dd->ff(x);}
//};

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







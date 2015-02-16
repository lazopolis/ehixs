#ifndef LUMINOSITY_H
#define LUMINOSITY_H

//#include "pdf.h"
//#include "pdf_hub.h"
//#include "constants.h"
//#include <utility>
//#include <string>
//#include "one_d_interpolator.h"
//#include "user_interface.h"
#include "ihixs_ui.h"
using namespace std;

#include "LHAPDF/LHAPDF.h"

class NewLuminosity{
public:
    NewLuminosity(const IhixsUI& UI);
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




#endif

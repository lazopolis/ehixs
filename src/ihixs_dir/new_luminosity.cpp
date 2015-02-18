#include <string>
#include <iostream>
#include <vector>
#include <math.h>
#include <stdlib.h>

using namespace std;

#include "new_luminosity.h"
#include<algorithm>

const double NewLuminosity::_almost_zero = 1e-12;



NewLuminosity::NewLuminosity(const IhixsUI& UI)
{
    string gridname = determine_gridname(UI.pdf_provider,
                                         UI.perturbative_order_for_pdfs,
                                         UI.pdf_set);
    pdf_ = LHAPDF::mkPDF( gridname, 0);
    muf_ = UI.muf;
    
    
}

string NewLuminosity::determine_gridname(const string& provider,int order, const string& specific_set)
{
    if (specific_set!="none" and provider=="none") return specific_set;
    else if (specific_set=="none" and provider!="none")
    {
        cout<<"\n[NewLuminosity] perturbative order = "<<order<<endl;
        string mstw[3]={"MSTW2008lo68cl","MSTW2008nlo68cl","MSTW2008nnlo68cl"};
        if (provider == "MSTW") return mstw[order];
        else
        {
            cerr   << "Error: PDF-provider '" << provider
            << "' is not supported.";
            exit(1);
        }
    }
    else
    {
        cout<<"\nError in determining pdf sets: you have declared both (or none) of pdf_set and pdf_provider."
        <<"\n pdf_set = "<<specific_set
        <<"\n pdf_provider = "<<provider
        <<"\nPlease choose one of the two options and rerun"<<endl;
        exit(1);
    }
}

double NewLuminosity::give(const double& x1,const double& x2)
{
    if (x1>1.0-_almost_zero or x2>1.0-_almost_zero or x1<_almost_zero or x2<_almost_zero)
    {
        return 0.0;
    }
    double res=0.0;
    for (int i=0;i<pairs.size();i++)
    {
        res +=    pdf_->xfxQ(pairs[i].first,x1,muf_)
        * pdf_->xfxQ(pairs[i].second,x2,muf_)
        * coeff_[i];
    }
    return res;
}













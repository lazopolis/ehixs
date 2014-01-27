#include <string>
#include <iostream>
#include <vector>
#include <math.h>

#include <stdlib.h> //: for exit()

using namespace std;

#include "LHAPDF/LHAPDF.h"
#include <iterator>
#include "interpolator.h"

#include "pdf.h"

//------------------------------------------------------------------------------

CPDF::~CPDF()
{
//    cout<<"\n@@@@destructor called"<<endl;
//    for(unsigned i=0; i<my_cached_interpolators.size(); ++i)
//        delete my_cached_interpolators[i];
}

double CPDF::give_f(const double& x, unsigned i)
{
    return my_cached_interpolators[i]->give_f(x);
    
}

double CPDF::give_f(const vector<double>& xx, unsigned i)
{
    
    return my_convolutions[i]->give_f(xx);
    
}


CPDF::CPDF(const PDFGrid& the_grid,const pdf_desc& my_desc,
           double NF, double muf, double mur)
:_my_desc(my_desc),interpolation_on_(true)
{
    // Print friendly message
    cout << "\n[CPDF::" << __func__
    << "] interpolating pdf ('"<< _my_desc.i<<","<<_my_desc.j<<","
    <<" n_as = "<<_my_desc.n_as<<" n_eps = "<<_my_desc.n_eps<<endl;
    for(unsigned k=0; k<the_grid.size(); k++)
        {
        LHAPDF::usePDFMember(the_grid.grid_id(k),the_grid.member_id(k));
        my_cached_interpolators.push_back(
                                          new CashedInterpolator(
                                                                 NF, muf, mur,
                                                                 my_desc.i, my_desc.j,my_desc.n_as, my_desc.n_eps,
                                                                 true,
                                                                 the_grid.gridname(k),the_grid.member_id(k))
                                          );
        }
}

CPDF::CPDF(const PDFGrid& the_grid,const pdf_desc& my_desc,
           double NF, double muf, double mur,bool interpolation_on)
:_my_desc(my_desc),interpolation_on_(interpolation_on)
{
    if (interpolation_on_ or _my_desc.n_as==0)
        {
        cout << "\n[CPDF] interpolating pdf "<< _my_desc.i<<","<<_my_desc.j
            <<", n_as = "<<_my_desc.n_as<<" n_eps = "<<_my_desc.n_eps<<endl;
        for(unsigned k=0; k<the_grid.size(); k++)
            {
            LHAPDF::usePDFMember(the_grid.grid_id(k),the_grid.member_id(k));
            my_cached_interpolators.push_back(
                    new CashedInterpolator(
                                            NF, muf, mur,
                                            my_desc.i, my_desc.j,
                                            my_desc.n_as, my_desc.n_eps,
                                            true,
                                            the_grid.gridname(k),
                                            the_grid.member_id(k))
                                          );
            }
        }
    else
        {
        cout<<"\n[CPDF] LIVE n*lo pdf "<< _my_desc.i<<","<<_my_desc.j
            <<", n_as = "<<_my_desc.n_as<<" n_eps = "<<_my_desc.n_eps<<endl;
        for(unsigned k=0; k<the_grid.size(); k++)
            {
            LHAPDF::usePDFMember(the_grid.grid_id(k),the_grid.member_id(k));
            my_convolutions.push_back(
                new LivePDFConvolution(
                                    NF, muf, mur,
                                    my_desc.i, my_desc.j,
                                    my_desc.n_as, my_desc.n_eps,
                                    the_grid.gridname(k),
                                    the_grid.member_id(k))
                                              );
            }
        }
}



//------------------------------------------------------------------------------



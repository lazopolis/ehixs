/** \file PDF.h
  *
  */

#ifndef PDF_H
#define PDF_H

#include "interpolator.h"
#include "cached_interpolator.h"
#include "splitting_kernels.h"
#include <sstream>
using namespace std;

struct pdf_desc
{
    pdf_desc(int x):i(x),j(x),n_as(0),n_eps(0){};
    pdf_desc(int i_,int j_,int n_as_, int n_eps_):i(i_),j(j_),n_as(n_as_),n_eps(n_eps_){};
    
    bool operator==(const pdf_desc& o)
    { return i==o.i && j==o.j && n_as==o.n_as && n_eps==o.n_eps; }
    
    int i,j,n_as,n_eps;
    string name(){stringstream s;s<<"F_"
                <<n_as<<"_"<<n_eps<<"_"<<i<<"_"<<j;return s.str();}
    
};

class SinglePDFMember
{
public:
    SinglePDFMember(const string & name,int gridno,int membno):_gridname(name),_grid_id(gridno),_member_id(membno){};
    string gridname(){return _gridname;}
    int grid_id(){return _grid_id;}
    int member_id(){return _member_id;}
private:
    string _gridname;
    int _grid_id;
    int _member_id;
};

class PDFGrid
{
public:
    PDFGrid(){};
    void add(const string & name,int grid_id,int member_id)
    {_grids.push_back(new SinglePDFMember(name,grid_id,member_id));}
    int grid_id(int i) const {return _grids[i]->grid_id();}
    int member_id(int i) const {return _grids[i]->member_id();}
    int size() const {return int(_grids.size());}
    string gridname(int i) const {return _grids[i]->gridname();}
private:
    vector<SinglePDFMember* > _grids;
};

class CPDF
{
public://data
    
public://methods
    ~CPDF();

    CPDF(const PDFGrid&,const pdf_desc& my_desc,double NF, double muf,
         double mur);
    CPDF(const PDFGrid&,const pdf_desc& my_desc,double NF, double muf,
         double mur,bool no_interpolation);
   
    double give_f(const double& x, unsigned i);
    double give_f(const vector<double>& xx, unsigned i);
    /** \brief Computes error
     *
     * The argument result is the final result vector with the length pdf.NumberOfMembers
     * The functions then returns a vector with two entries, containing the "+" and "-" pdf-error
     */
     string name(){return _my_desc.name();}
    vector<double> calculate_pdf_error(const vector<double>& result);
    unsigned size() const{ return NumberOfMembers; }
    vector<double> alpha_s_at_mz;
    bool is(const pdf_desc& ext_desc){return _my_desc==ext_desc;}
private://data
    vector<LivePDFConvolution*> my_convolutions;
    vector<CashedInterpolator*> my_cached_interpolators;
    unsigned NumberOfMembers;
    pdf_desc _my_desc;
    bool interpolation_on_;
private://methods
    //: determine grids returns a vector of one element, the name of the grid,
    //: except in the case that pdf_provider = MSTW, when we have three grids
    vector<string> determine_grids();
};




#endif






#ifndef PDFONTHEFLY_H
#define PDFONTHEFLY_H



#endif



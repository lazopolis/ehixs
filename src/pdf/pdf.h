/** \file CPDF.h
  *
  */

#ifndef PDF_H
#define PDF_H

#include "interpolator.h"
#include "cached_interpolator.h"
#include "splitting_kernels.h"
#include "user_interface.h"

struct pdf_desc
{
    pdf_desc(int i_,int j_,int n_as_, int n_eps_):i(i_),j(j_),n_as(n_as_),n_eps(n_eps_){};
    
    bool operator==(const pdf_desc& o)
    { return i==o.i && j==o.j && n_as==o.n_as && n_eps==o.n_eps; }
    
    int i,j,n_as,n_eps;
    
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



class PDFHub
{
public:
    PDFHub(const UserInterface& UI);
    CPDF* construct_or_locate_pdf(const pdf_desc&);
    vector<double> calculate_pdf_error(const vector<double>& result);
    int size(){return _grids->size();}
    vector<double> alpha_s_at_mz(){return _alpha_s_at_mz;}
private:
    string _provider;
    bool _pdf_error;
    unsigned _pert_order;
    unsigned _number_of_members;
    double _muf;
    double _mur;
    double _Nf;
    vector<double> _alpha_s_at_mz;
    PDFGrid* _grids;
    vector<CPDF*> _all_pdfs;
    
private://methods
    vector<string> determine_grids();
    //    * Error according to eq. 51,52 of http://arxiv.org/abs/0901.0002v3
    vector<double> MSTW_pdferror(const vector<double>&);
    // \brief Error for alpha_s in MSTW.
    // Error according to eq. 51,52 of http://arxiv.org/abs/0901.0002v3
    //
    // NOTE: here, we assume that the MSTW error ALWAYS consists of 3 sets which have the same length each. If this ever changes, the error routine will have to be changed as well.
    vector<double> MSTW_pdf_as_error(const vector<double>&);
    // \brief Error for GJR.
    
    // Note: dS = 1/2 sqrt( sum{ (S_i+ - S_i- )^2 } ). See eq. 2.10 of http://arxiv.org/abs/0902.3947v1. The error is by construction symmetric.
    vector<double> GJR_pdferror(const vector<double>&);
    // \brief Error for ABKM.
    //
    // Note: dS =  sqrt( sum{ (S_i+ - S_0 )^2 } ). The error is by construction symmetric.
    vector<double> ABKM_pdferror(const vector<double>&);
    // \brief Error for NNPDF.
    //
    //Note: dS =  sqrt( 1/(ngrids-1)*sum{ (S_i+ - S_0 )^2 } ) with ngrids not including the central set. The error is by construction symmetric.
    vector<double> NNPDF_pdferror(const vector<double>&);
};


#endif






#ifndef PDFONTHEFLY_H
#define PDFONTHEFLY_H



#endif



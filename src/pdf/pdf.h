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
   
    double give_f(const double& x, unsigned i);
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
    vector<interpolator*> my_interpolators;
    vector<CashedInterpolator*> my_cached_interpolators;
    unsigned NumberOfMembers;
    pdf_desc _my_desc;
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
//
//#include "one_d_interpolator.h"
//
//
//class SinglePDFMemberInterpolator: public InterpolatorBase
//{
//public:
//     SinglePDFMemberInterpolator(int _parton, const double& _muf):InterpolatorBase(){parton=_parton;muf=_muf;}
//private:
//     double f_value(const double & x);
//     int parton;
//     double muf;
//};
//
//
//class PDF_on_the_fly
//{
//public:
//     /** \brief Destructor */
//     ~PDF_on_the_fly();
//     /** \brief Constructor
//      *  the PDF_on_the_fly is fully determined by the integer parton, the name of the provider, muf, the pert_order and whether or not to calculate pdf errors */
//     PDF_on_the_fly(int parton, const string& provider_,
//                    double muf, int pert_order_,  bool pdf_error_);
//     /** \brief Give f from interpolators */
//     double give_f(const double& x, unsigned i){return my_interpolators[i]->give_f(x);}
//     
//     /** \brief Computes error
//      *
//      * The argument result is the final result vector with the length pdf.NumberOfMembers
//      * The functions then returns a vector with two entries, containing the "+" and "-" pdf-error
//      */
//     //vector<double> calculate_pdf_error(const vector<double>& result);
//     
//     /** \brief Return NumberOfMembers */
//     unsigned size() const{ return NumberOfMembers; }
//     /** \brief holds the a_s(Mz) value of each member */
//     vector<double> alpha_s_at_mz;
//     
//private:
//     //* \brief Name of the PDF */
//     string provider;
//     /** \brief Flag for error computation */
//     bool pdf_error;
//     /** \brief Perturbative order */
//     int pert_order;
//     /** \brief Number of members */
//     unsigned NumberOfMembers;
//     /** \brief vector that holds the names of the grids to be loaded */
//     vector<string> gridnames;
//     /** \brief Interpolator list */
//     vector<SinglePDFMemberInterpolator*> my_interpolators;
//     
//     void determine_grids();
//     
//     
//};


//
//class DPDF
//{
//public:
//     /** \brief Constructor needs iparton, from_parton, n_as, n_eps */
//     DPDF( int _iparton, int _n_as, int _n_eps,  int _from_parton);
//     ~DPDF(){};
//     int give_a_power(){return a_power;}
//     int give_e_power(){return e_power;}
//     void init(const string& _provider,const double& _muf,int _pert_order, bool _pdf_error);
//     double give_f(const double & x, int i);
//     double give_f(const double & x, const double & y, int i);
//     double give_f(const double & x, const double & y, const double&z, int i);
//
//private:
//     int iparton,from_parton,a_power,e_power;
//     PDF_on_the_fly * my_pdf;
//     Kernel * my_kernel;
//     void allocate_my_kernel();
//     
//     void print_error_message_and_exit();
//
//     
//};
//
//


#endif



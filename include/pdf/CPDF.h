/** \file CPDF.h
  *
  */

#ifndef PDF_H
#define PDF_H

#include "interpolator.h"
#include "cashed_interpolator.h"
#include "splitting_kernels.h"
class CPDF
{
  //* \brief Name of the PDF */
  string provider;
  /** \brief Flag for error computation */
  bool pdf_error;
  /** \brief Perturbative order */
  int pert_order;
  /** \brief Number of members */
  unsigned NumberOfMembers;
     
  /** \brief Boolean to decide if using cashed interpolator objects*/
     bool use_cashed_interpolator;
     /** \brief Boolean to decide whether to use cashing (we can use the cashed interpolators without cashing as well)*/
     bool cashing;

  /** \brief Init. function
    *
    * Used to determine grids needed in the analysis. Returns a vector of strings with the .LHGRID names of the sets that should be interpolated. In most cases, this will be only one name, the one of the grid corresponding to provider and porder. But e.g. for MSTW, we also want to automate the alpha_s-error, so we need to load 3 sets. In that case, the returned vector will have three entries.
    *
    * To add a new pdf set, one has to add another "else if" here AND (if pdf-error should be supported for that provider) in the function "calculate_pdf_error. */
  vector<string> determine_grids();

  /** \brief Error for MSTW.
    *
    * Error according to eq. 51,52 of http://arxiv.org/abs/0901.0002v3 */
  vector<double> MSTW_pdferror(const vector<double>&);
  /** \brief Error for alpha_s in MSTW. 
    *
    * Error according to eq. 51,52 of http://arxiv.org/abs/0901.0002v3
    *
    * NOTE: here, we assume that the MSTW error ALWAYS consists of 3 sets which have the same length each. If this ever changes, the error routine will have to be changed as well. */
  vector<double> MSTW_pdf_as_error(const vector<double>&);
  /** \brief Error for GJR. 
    *
    * Note: dS = 1/2 sqrt( sum{ (S_i+ - S_i- )^2 } ). See eq. 2.10 of http://arxiv.org/abs/0902.3947v1. The error is by construction symmetric. */
  vector<double> GJR_pdferror(const vector<double>&);
  /** \brief Error for ABKM. 
    *
    * Note: dS =  sqrt( sum{ (S_i+ - S_0 )^2 } ). The error is by construction symmetric. */  
  vector<double> ABKM_pdferror(const vector<double>&);
  /** \brief Error for NNPDF. 
    *
    * Note: dS =  sqrt( 1/(ngrids-1)*sum{ (S_i+ - S_0 )^2 } ) with ngrids not including the central set. The error is by construction symmetric. */
  vector<double> NNPDF_pdferror(const vector<double>&);


  /** \brief Interpolate 
    *
    * More infor please... */
  void interpolate(double, double, double, int, int, int, int,const string&,int);
  /** \brief Test interpolation
    *
    * Tests the interpolation on some sample x-points and prints out the results. TBI !*/
  void test_interpolation(int, double);

  /** \brief Parameter input */
  void read(const string &, const string & );
public:
     
     /** \brief Interpolator list */
     vector<interpolator*> my_interpolators;
     vector<CashedInterpolator*> my_cached_interpolators;
     
     
  /** \brief Empty (should never be used */
  //CPDF();
  /** \brief Destructor */
  ~CPDF();
  /** \brief Constructor
    *
    * ... more info please ! */
  CPDF(double, double, double, int, int, int, int, const string&, bool, int=100);
     CPDF(double, double, double, int, int, int, int, const string&, bool,bool,bool, int=100);
  /** \brief Give f from interpolators
    *
    * Function that actually give a f... See interpolator::give_f(). */
  double give_f(const double& x, unsigned i)
  { if (use_cashed_interpolator )return my_cached_interpolators[i]->give_f(x);
   else return my_interpolators[i]->give_f(x);
  }

  /** \brief Computes error
    *
    * The argument result is the final result vector with the length pdf.NumberOfMembers
    * The functions then returns a vector with two entries, containing the "+" and "-" pdf-error
    */
  vector<double> calculate_pdf_error(const vector<double>& result);

  /** \brief Return NumberOfMembers */
  unsigned size() const
  { return NumberOfMembers; }

  vector<double> alpha_s_at_mz;
     
     string cashing_status(int);
};

#endif






#ifndef PDFONTHEFLY_H
#define PDFONTHEFLY_H

#include "OneDInterpolator.h"


class SinglePDFMemberInterpolator: public InterpolatorBase
{
public:
     SinglePDFMemberInterpolator(int _parton, const double& _muf):InterpolatorBase(){parton=_parton;muf=_muf;}
private:
     double f_value(const double & x);
     int parton;
     double muf;
};


class PDF_on_the_fly
{
public:
     /** \brief Destructor */
     ~PDF_on_the_fly();
     /** \brief Constructor
      *  the PDF_on_the_fly is fully determined by the integer parton, the name of the provider, muf, the pert_order and whether or not to calculate pdf errors */
     PDF_on_the_fly(int parton, const string& provider_,
                    double muf, int pert_order_,  bool pdf_error_);
     /** \brief Give f from interpolators */
     double give_f(const double& x, unsigned i){return my_interpolators[i]->give_f(x);}
     
     /** \brief Computes error
      *
      * The argument result is the final result vector with the length pdf.NumberOfMembers
      * The functions then returns a vector with two entries, containing the "+" and "-" pdf-error
      */
     //vector<double> calculate_pdf_error(const vector<double>& result);
     
     /** \brief Return NumberOfMembers */
     unsigned size() const{ return NumberOfMembers; }
     /** \brief holds the a_s(Mz) value of each member */
     vector<double> alpha_s_at_mz;
     
private:
     //* \brief Name of the PDF */
     string provider;
     /** \brief Flag for error computation */
     bool pdf_error;
     /** \brief Perturbative order */
     int pert_order;
     /** \brief Number of members */
     unsigned NumberOfMembers;
     /** \brief vector that holds the names of the grids to be loaded */
     vector<string> gridnames;
     /** \brief Interpolator list */
     vector<SinglePDFMemberInterpolator*> my_interpolators;
     
     void determine_grids();
     
     
};



class DPDF
{
public:
     /** \brief Constructor needs iparton, from_parton, n_as, n_eps */
     DPDF( int _iparton, int _n_as, int _n_eps,  int _from_parton);
     ~DPDF(){};
     int give_a_power(){return a_power;}
     int give_e_power(){return e_power;}
     void init(const string& _provider,const double& _muf,int _pert_order, bool _pdf_error);
     double give_f(const double & x, int i);
     double give_f(const double & x, const double & y, int i);
     double give_f(const double & x, const double & y, const double&z, int i);

private:
     int iparton,from_parton,a_power,e_power;
     PDF_on_the_fly * my_pdf;
     Kernel * my_kernel;
     void allocate_my_kernel();
     
     void print_error_message_and_exit();

     
};




#endif



/** \file interpolator.h
 *
 * WTF ?
 *
 * \author, Stefu Bulli
 */

#ifndef CASHED_INTERPOLATOR_H
#define CASHED_INTERPOLATOR_H

#define PDF_GRID_PREFIX "../pdf_cache/"


class CashedInterpolator;

typedef void (CashedInterpolator::*pointer_to_fillFGrid_function)();

class CashedInterpolator
{
public:
     CashedInterpolator(const double& , const double& ,const double&, const int& ,const int& ,
                         const int& ,const int& ,bool,const string & ,const int& );
     double give_f(const double&);
     string cashing_status(){return my_cashing_status;}
protected:
     
     double NFl,mur,muf;
     int iprtn,jprtn,n_as,n_eps;
     bool cashing;
     string gridname;
     int member;
     string cashed_file_name;
     bool should_write_fgrid_to_file;
     //: grids needed (only the CoeffGrid is needed post constructor phase)
     vector<double> XGrid;
     vector<double> FGrid;
     vector<vector<double> > CoeffGrid;
     
     void (CashedInterpolator::*cur_fill_FGrid)();
     
     void givecoeff(const double&,const double&,const double&,const double&,const double&,const double&, double&, double&, double&);
     double xmin;
     int NumberOfPoints;
     
     void disambiguateFgrid();
     void set_up_fgrid_and_coefficients();
     

     void fill_XGrid();
     void fill_FGrid();
     
     
     bool cashed_file_found();
     void read_grid_from_file(vector<double> & );
     void write_fgrid_to_file();

     string my_cashing_status;
     
     //:FillGrid functions
     void ffgrid_LO_nosum();
     void ffgrid_LO_sum_light();
     
     void ffgrid_NLO_new();

     
     void ffgrid_NLO_quark();
     void ffgrid_NLO_quark_from_X();
     void ffgrid_NLO_quark_summed();
     
     void ffgrid_NLO_gluon();
     void ffgrid_NLO_gluon_from_X();
     void ffgrid_NLO_gluon_new();

     
     void ffgrid_NNLO_21_quark();
     void ffgrid_NNLO_21_quark_from_X();
     void ffgrid_NNLO_21_quark_summed();
     
     void ffgrid_NNLO_21_gluon();
     void ffgrid_NNLO_21_gluon_from_X();
     
     
     void ffgrid_NNLO_22_quark();
     void ffgrid_NNLO_22_quark_from_X();
     void ffgrid_NNLO_22_quark_summed();
     
     void ffgrid_NNLO_22_gluon();
     void ffgrid_NNLO_22_gluon_from_X();
     

};



class LivePDFConvolution{
public:
    LivePDFConvolution(const double& , const double& ,const double&,
                       const int& ,const int& ,
                       const int& ,const int& ,const string & ,const int& );
    double give_f(const vector<double>& xx){return (this->*integrand_)(xx);}
    double nloIntegrand(const vector<double>& xx);

private:
    CashedInterpolator* parent;
    double nfl_;
    int iprtn_;
    int jprtn_;
    int n_as_;
    int n_eps_;
    double (LivePDFConvolution:: *integrand_)(const vector<double>&);
    
private:
    double (* plus_kernel)(const double& x);
    double (* reg_kernel)(const double& x);
    double (* reg_mixed_kernel)(const double& x);
    double (* delta_kernel)(const double& x);
    double (* boundary_kernel)(const double& x);
};

#endif

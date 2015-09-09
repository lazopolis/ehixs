#ifndef PDFHUB_H
#define PDFHUB_H

#include "option.h"
#include "pdf.h"

#include<vector>
using namespace std;

class IPDFHub : protected OptionSet
{
    string pdf_provider;
    string pdf_set;
    double number_of_flavours;

    IPDFHub()
    {
        _opts().push_back(new Option<string>("pdf_provider",0,"pdf provider",Arg::Required,pdf_provider,"none"));
        /// \todo Nf double? Seriously?
        _opts().push_back(new Option<double>("number_of_flavours",0,"number of active flavors (do not change)",Arg::Required,number_of_flavours,5.0));
//        _opts().push_back(new Option<string>("Fleft",0,"specifies the flavor of the parton on left beam",Arg::Required,Fleft, "none"));
//        _opts().push_back(new Option<string>("Fright",0,"specifies the flavor of the parton on right beam",Arg::Required,Fright, "none"));
        _opts().push_back(new Option<string>(
                                             "pdf_set",0,
                                             "choose a specific pdf set name (one from the LHAPDF6 list found at\
                                             lhapdf.hepforge.org/pdfsets.html). This set will be used irrespectively of order for the entire computation. This option is incompatible with pdf_provider. ",
                                             Arg::Required,
                                             pdf_set,
                                             "none"));
    }

};

class PDFHub
{
public:
    PDFHub();
    CPDF* construct_or_locate_pdf(const pdf_desc&);
    vector<double> calculate_pdf_error(const vector<double>& result);
    int size(){return grids_->size();}
    vector<double> alpha_s_at_mz(){return _alpha_s_at_mz;}
private:
    bool pdf_error_;
    unsigned pert_order_;
    //unsigned number_of_members_;
    double muf_;
    double mur_;
    vector<double> _alpha_s_at_mz;
    PDFGrid* grids_;
    vector<CPDF*> all_pdfs_;
    bool convolutions_by_interpolation;
private://methods
    vector<string> determine_grids();
    void initialize_lha_pdf();
    void initialize_mstw_plain();
    
    // pdf error calculators
    vector<double> MSTW_pdferror(const vector<double>&);// according to eq. 51,52 of http://arxiv.org/abs/0901.0002v3
    // note: here, we assume that the MSTW error ALWAYS consists of 3 sets which have the same length each. If this ever changes, the error routine will have to be changed as well.
    vector<double> MSTW_pdf_as_error(const vector<double>&);
    // Note: dS = 1/2 sqrt( sum{ (S_i+ - S_i- )^2 } ). See eq. 2.10 of http://arxiv.org/abs/0902.3947v1. The error is by construction symmetric.
    vector<double> GJR_pdferror(const vector<double>&);
    // Note: dS =  sqrt( sum{ (S_i+ - S_0 )^2 } ). The error is by construction symmetric.
    vector<double> ABKM_pdferror(const vector<double>&);
    //Note: dS =  sqrt( 1/(ngrids-1)*sum{ (S_i+ - S_0 )^2 } ) with ngrids not including the central set. The error is by construction symmetric.
    vector<double> NNPDF_pdferror(const vector<double>&);
};


#endif


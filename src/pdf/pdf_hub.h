#ifndef PDFHUB_H
#define PDFHUB_H

#include "user_interface.h"
#include "pdf.h"

#include<vector>
using namespace std;

class PDFHub
{
public:
    PDFHub(const UserInterface& UI);
    CPDF* construct_or_locate_pdf(const pdf_desc&);
    vector<double> calculate_pdf_error(const vector<double>& result);
    int size(){return grids_->size();}
    vector<double> alpha_s_at_mz(){return _alpha_s_at_mz;}
private:
    string provider_;
    bool pdf_error_;
    unsigned pert_order_;
    //unsigned number_of_members_;
    double muf_;
    double mur_;
    double Nf_;
    vector<double> _alpha_s_at_mz;
    PDFGrid* grids_;
    vector<CPDF*> all_pdfs_;
    
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


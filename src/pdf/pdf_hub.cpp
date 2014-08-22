#include "pdf_hub.h"
#include "LHAPDF/LHAPDF.h"
#include "math.h"
using namespace std;

//#include "ehixs_config.h"
#define CONFIGURE_LHAPDF_ON

PDFHub::PDFHub(const UserInterface& UI)
{
    Nf_ = UI.number_of_flavours;
    muf_ = UI.muf_over_mhiggs * UI.m_higgs;
    mur_ = UI.mur_over_mhiggs * UI.m_higgs;
    pert_order_ =  UI.perturbative_order;
    provider_ =  UI.pdf_provider;
    pdf_error_ = UI.pdf_error;
    convolutions_by_interpolation = UI.convolutions_by_interpolation;
    
    grids_ = new PDFGrid;
    
    #ifdef CONFIGURE_LHAPDF_ON
    initialize_lha_pdf();
    #else
    initialize_mstw_plain();
    #endif
}

void PDFHub::initialize_lha_pdf()
{
    vector<string> gridnames = determine_grids();
    LHAPDF::setVerbosity(LHAPDF::SILENT);
    
    for(unsigned grid=0; grid<gridnames.size(); ++grid)
        {
        //cout<<"\n[PDFHub] : initializing "<<gridnames[grid]<<endl;
        // initialise the LHAPDF set corresponding to grids[grid]
        int grid_number_for_lhapdf = grid+1;
        //LHAPDF::initPDFSet(grid_number_for_lhapdf,
        //                   gridnames[grid], LHAPDF::LHGRID);
        //lhapdf6 interface
        LHAPDF::PDF* pdf = LHAPDF::mkPDF(gridnames[grid], 0);
            
            
        //cout<<"\n[PDFHub] : pdf_error is "<<pdf_error_<<endl;
        // Using the LHAPDF-routine numberPDF, we can determine the number of members of the currently loaded set
        unsigned membernum;
        if (pdf_error_) membernum = LHAPDF::numberPDF()+1;
        else membernum = 1;
        
        // thank you, GJR, for this additional line of code
        if((provider_ == "GJR") && (pert_order_ == 0)) membernum=1;
        
        
        // loop over members:
        for (int member=0; member<membernum; member++)
            {
            // thank you, GJR, for this additional 2 lines of code
            if((provider_ == "GJR") && (pert_order_ == 0)) ++member;
            
            // initialise the correct member set
            LHAPDF::usePDFMember(grid_number_for_lhapdf,member);
            
            // retrieve the alpha_s of this member !!! hardcoded m_z ok?
            //_alpha_s_at_mz.push_back(LHAPDF::alphasPDF(91.1876));
            //lhapdf6 interface
            _alpha_s_at_mz.push_back(pdf->alphaS().alphasQ(91.1876));

                
                
            grids_->add(gridnames[grid],grid_number_for_lhapdf,member);
            
            }
        }

}

// this was an attempt to decouple from lhapdf. However it presupposes that
// we do the dglap evolution ourselves. This has to be extensiveley
// tested to be implemented if we don't want to run into trouble
// so for the momment we leave it as a future feature
void PDFHub::initialize_mstw_plain()
{
}



CPDF* PDFHub::construct_or_locate_pdf(const pdf_desc& my_pdf_desc)
{
    for(int i=0;i<all_pdfs_.size();i++)
        {
        if (all_pdfs_[i]->is(my_pdf_desc)) return all_pdfs_[i];
        }
    //: if we reach here, the requested pdf was not found, so we create it
    all_pdfs_.push_back(new CPDF(*grids_,my_pdf_desc,Nf_,muf_,mur_,convolutions_by_interpolation));
    return all_pdfs_[all_pdfs_.size()-1];
}



vector<string> PDFHub::determine_grids()
{
    bool ef = false;
    vector<string> gridnames;
    if (provider_ == "MSTW")
        {
        switch(pert_order_)
            {
                case 0:
                gridnames.push_back("MSTW2008lo68cl");
                break;
                case 1:
                gridnames.push_back("MSTW2008nlo68cl");
                if(pdf_error_)
                    {
                    gridnames.push_back("MSTW2008nlo68cl_asmz+68cl");
                    gridnames.push_back("MSTW2008nlo68cl_asmz-68cl");
                    }
                break;
                case 2:
                gridnames.push_back("MSTW2008nnlo68cl");
                if(pdf_error_)
                    {
                    gridnames.push_back("MSTW2008nnlo68cl_asmz+68cl");
                    gridnames.push_back("MSTW2008nnlo68cl_asmz-68cl");
                    }
                break;
                default:
                ef=true;
            }
        }
    else if (provider_ == "ABKM")
        {
        switch(pert_order_)
            {
                case 0:
                gridnames.push_back("a02m_lo");
                break;
                case 1:
                gridnames.push_back("abkm09_5_nlo");
                break;
                case 2:
                gridnames.push_back("abkm09_5_nnlo");
                break;
                default:
                ef = true;
            }
        }
    else if (provider_ == "GJR")
        {
        switch(pert_order_)
            {
                case 0:
                gridnames.push_back("GJR08lo");
                break;
                case 1:
                gridnames.push_back("GJR08VFnloE");
                break;
                case 2:
                gridnames.push_back("JR09VFnnloE");
                break;
                default:
                ef = true;
            }
        }
    else if (provider_ == "NNPDF")
        {
        switch(pert_order_)
            {
                case 0:
                gridnames.push_back("NNPDF21_lo_as_0119_100");
                break;
                case 1:
                gridnames.push_back("NNPDF21_100");
                break;
                case 2:
                gridnames.push_back("NNPDF21_nnlo_100");
                break;
                default:
                ef = 0;
            }
        }
    else {
        std::cerr << "Error: PDF-provider '" << provider_ << "' is not supported.";
        exit(1);
    }
    
    // If error flag ef have set then quit.
    if(ef)
        {
        std::cerr << "Error: PDF-provider '" << provider_ << "' does not support perturbative order " << pert_order_ << ".";
        exit(1);
        }
    
    return gridnames;
}



vector<double> PDFHub::calculate_pdf_error(const vector<double>& result)
{
    // check the length of the result vector.
    if(result.size() != grids_->size())
        {
        cerr << " CPDF : length of result vector ("<<result.size()+") does not fit NumberOfMembers ("<<grids_->size()+")!";
        exit(1);
        }
    
    vector<double> pdferr;
    
    // if we don't want the pdf error, return 0s
    if(!pdf_error_)
        {
        pdferr.push_back(0.0);
        pdferr.push_back(0.0);
        }
    // Otherwise check provider by provider
    else if (provider_ == "MSTW")
        pdferr = (pert_order_ == 0) ? MSTW_pdferror(result) : MSTW_pdf_as_error(result);
    else if (provider_ == "GJR")
        pdferr = GJR_pdferror(result);
    else if (provider_ == "ABKM")
        pdferr = ABKM_pdferror(result);
    else if (provider_ == "NNPDF")
        pdferr = NNPDF_pdferror(result);
    else
        {
        cerr << "PDF error is not supported for provider "<<provider_;
        exit(1);
        }
    
    return pdferr;
}

vector<double> PDFHub::MSTW_pdf_as_error(const vector<double>& result)
{
    unsigned Nmax = grids_->size()/3 - 1;
    
    const double cval = result[0];
    const double cval_asp = result[Nmax+1];
    const double cval_asm = result[2*(Nmax+1)];
    double errp=0.0; double errm=0.0;
    double errp_asp=0.0; double errm_asp=0.0;
    double errp_asm=0.0; double errm_asm=0.0;
    
    for(unsigned i=1; i<=Nmax/2; ++i)
        {
        // indices for central alpha_s set
        int jp = 2*i; int jm = 2*i-1;
        // indices for alpha_s plus set
        int jp_asp = jp + Nmax +1; int jm_asp = jm + Nmax +1;
        // indices for alpha_s minus set
        int jp_asm = jp + 2*(Nmax +1); int jm_asm = jm + 2*(Nmax +1);
        
        const double tmp1 = result[jp] - cval;
        const double tmp2 = result[jm] - cval;
        const double tmp1_asp = result[jp_asp] - cval_asp;
        const double tmp2_asp = result[jm_asp] - cval_asp;
        const double tmp1_asm = result[jp_asm] - cval_asm;
        const double tmp2_asm = result[jm_asm] - cval_asm;
        
        // plus-error
        double temp = (tmp1 > tmp2) ? tmp1 : tmp2; temp = (temp > 0.0) ? temp : 0.0;
        double temp_asp = (tmp1_asp > tmp2_asp) ? tmp1_asp : tmp2_asp; temp_asp = (temp_asp > 0.0) ? temp_asp : 0.0;
        double temp_asm = (tmp1_asm > tmp2_asm) ? tmp1_asm : tmp2_asm; temp_asm = (temp_asm > 0.0) ? temp_asm : 0.0;
        
        errp += pow(temp,2);
        errp_asp += pow(temp_asp,2);
        errp_asm += pow(temp_asm,2);
        
        // minus-error
        temp = (-tmp1 > -tmp2) ? -tmp1 : -tmp2; temp = (temp > 0.0) ? temp : 0.0;
        temp_asp = (-tmp1_asp > -tmp2_asp) ? -tmp1_asp : -tmp2_asp; temp_asp = (temp_asp > 0.0) ? temp_asp : 0.0;
        temp_asm = (-tmp1_asm > -tmp2_asm) ? -tmp1_asm : -tmp2_asm; temp_asm = (temp_asm > 0.0) ? temp_asm : 0.0;
        
        errm += pow(temp,2);
        errm_asp += pow(temp_asp,2);
        errm_asm += pow(temp_asm,2);
        }
    
    errm = sqrt(errm); errp = sqrt(errp);
    errm_asm = sqrt(errm_asm); errp_asm = sqrt(errp_asm);
    errm_asp = sqrt(errm_asp); errp_asp = sqrt(errp_asp);
    
    // envelope
    double lower_edge = ((cval-errm) < (cval_asm-errm_asm)) ? (cval-errm):(cval_asm-errm_asm);
    lower_edge = ((cval_asp-errm_asp) < lower_edge) ? (cval_asp-errm_asp):lower_edge;
    
    double upper_edge = ((cval+errp) > (cval_asm+errp_asm)) ? (cval+errp):(cval_asm+errp_asm);
    upper_edge = ((cval_asp+errp_asp) > upper_edge) ? (cval_asp+errp_asp):upper_edge;
    
    double pluserr = (upper_edge-cval)/cval*100.0;
    double minuserr = (lower_edge-cval)/cval*100.0;
    
    vector<double> ris;
    ris.push_back(pluserr);
    ris.push_back(minuserr);
    
    return ris;
}

vector<double> PDFHub::MSTW_pdferror(const vector<double>& result)
{
    unsigned Nmax = grids_->size() - 1;
    
    const double cval = result[0];
    double errp=0.0; double errm=0.0;
    
    for(unsigned i=1; i<=Nmax/2; ++i)
        {
        unsigned jp = 2*i, jm = 2*i-1;
        
        const double tmp1 = result[jp] - cval;
        const double tmp2 = result[jm] - cval;
        
        // plus-error
        double temp = (tmp1 > tmp2) ? tmp1 : tmp2; temp = (temp > 0.0) ? temp : 0.0;
        errp += temp*temp;
        
        // minus-error
        temp = (-tmp1 > -tmp2) ? -tmp1 : -tmp2; temp = (temp > 0.0) ? temp : 0.0;
        errm += temp*temp;
        }
    
    errm = sqrt(errm); errp = sqrt(errp);
    
    const double pluserr = errp/cval*100.0;
    const double minuserr = -errm/cval*100.0;
    
    vector<double> ris;
    ris.push_back(pluserr);
    ris.push_back(minuserr);
    
    return ris;
}

vector<double> PDFHub::GJR_pdferror(const vector<double>& result)
{
    const double cval = result[0];
    double err = 0.0;
    
    for(unsigned i=1; i<=(grids_->size()-1)/2; ++i)
        {
        int jp = 2*i;
        int jm = 2*i-1;
        
        err += pow(result[jp]-result[jm],2);
        }
    
    err = sqrt(err)/cval*100.0/2.0;
    
    vector<double> ris;
    ris.push_back(err);
    ris.push_back(-err);
    
    return ris;
}

vector<double> PDFHub::ABKM_pdferror(const vector<double>& result)
{
    const double cval = result[0];
    double err = 0.0;
    
    for(unsigned i=1; i<grids_->size(); ++i)
        err += pow(result[i]-cval,2);
    
    err = sqrt(err)/cval*100.0;
    
    vector<double> ris;
    ris.push_back(err);
    ris.push_back(-err);
    
    return ris;
}

vector<double> PDFHub::NNPDF_pdferror(const vector<double>& result)
{
    const double cval = result[0];
    double err = 0.0;
    
    for(unsigned i=1; i<grids_->size(); i++)
        err += pow(result[i]-cval,2);
    
    err = sqrt(err/(grids_->size()-2))/cval*100.0;
    
    vector<double> ris;
    ris.push_back(err);
    ris.push_back(-err);
    
    return ris;
}


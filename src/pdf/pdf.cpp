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


PDFHub::PDFHub(const UserInterface& UI)
{
    _Nf = UI.number_of_flavours;
    _muf = UI.muf_over_mhiggs * UI.m_higgs;
    _mur = UI.mur_over_mhiggs * UI.m_higgs;
    _pert_order =  UI.perturbative_order;
    _provider =  UI.pdf_provider;
    _pdf_error = UI.pdf_error;
    // Print friendly message
    cout << "\n[PDFHub::" << __func__
    << "] Attempting init of '"<< _provider
    << "' pdf at O(as^" << _pert_order
    << ") ,error='" << _pdf_error << "'"<<endl;
    
    vector<string> gridnames = determine_grids();
    
    _grids = new PDFGrid;
    
    LHAPDF::setVerbosity(LHAPDF::SILENT);
    
    for(unsigned grid=0; grid<gridnames.size(); ++grid)
        {
        cout<<"\n[PDFHub] : initializing "<<gridnames[grid]<<endl;
        // initialise the LHAPDF set corresponding to grids[grid]
        int grid_number_for_lhapdf = grid+1;
        LHAPDF::initPDFSet(grid_number_for_lhapdf,
                           gridnames[grid], LHAPDF::LHGRID);
        
        cout<<"\n[PDFHub] : pdf_error is "<<_pdf_error<<endl;
        // Using the LHAPDF-routine numberPDF, we can determine the number of members of the currently loaded set
        unsigned membernum;
        if (_pdf_error) membernum = LHAPDF::numberPDF()+1;
        else membernum = 1;
        
        // thank you, GJR, for this additional line of code
        if((_provider == "GJR") && (_pert_order == 0)) membernum=1;
        
        
        // loop over members:
        for (int member=0; member<membernum; member++)
            {
            // thank you, GJR, for this additional 2 lines of code
            if((_provider == "GJR") && (_pert_order == 0)) ++member;
            
            // initialise the correct member set
            LHAPDF::usePDFMember(grid_number_for_lhapdf,member);
            
            // retrieve the alpha_s of this member !!! hardcoded m_z ok?
            _alpha_s_at_mz.push_back(LHAPDF::alphasPDF(91.1876));
            
            _grids->add(gridnames[grid],grid_number_for_lhapdf,member);
            
            }
        }
    
    cout << ": Success [" << _grids->size() << "] sets!";
    

}

vector<string> PDFHub::determine_grids()
{
    bool ef = false;
    vector<string> gridnames;
    if (_provider == "MSTW")
        {
        switch(_pert_order)
            {
                case 0:
                gridnames.push_back("MSTW2008lo68cl");
                break;
                case 1:
                gridnames.push_back("MSTW2008nlo68cl");
                if(_pdf_error)
                    {
                    gridnames.push_back("MSTW2008nlo68cl_asmz+68cl");
                    gridnames.push_back("MSTW2008nlo68cl_asmz-68cl");
                    }
                break;
                case 2:
                gridnames.push_back("MSTW2008nnlo68cl");
                if(_pdf_error)
                    {
                    gridnames.push_back("MSTW2008nnlo68cl_asmz+68cl");
                    gridnames.push_back("MSTW2008nnlo68cl_asmz-68cl");
                    }
                break;
                default:
                ef=true;
            }
        }
    else if (_provider == "ABKM")
        {
        switch(_pert_order)
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
    else if (_provider == "GJR")
        {
        switch(_pert_order)
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
    else if (_provider == "NNPDF")
        {
        switch(_pert_order)
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
        std::cerr << "Error: PDF-provider '" << _provider << "' is not supported.";
        exit(1);
    }
    
    // If error flag ef have set then quit.
    if(ef)
        {
        std::cerr << "Error: PDF-provider '" << _provider << "' does not support perturbative order " << _pert_order << ".";
        exit(1);
        }
    
    return gridnames;
}

CPDF* PDFHub::construct_or_locate_pdf(const pdf_desc& my_pdf_desc)
{
    for(int i=0;i<_all_pdfs.size();i++)
        {
        if (_all_pdfs[i]->is(my_pdf_desc)) return _all_pdfs[i];
        }
    //: if we reach here, the requested pdf was not found, so we create it
    _all_pdfs.push_back(new CPDF(*_grids,my_pdf_desc,_Nf,_muf,_mur));
    return _all_pdfs[_all_pdfs.size()-1];
}


vector<double> PDFHub::calculate_pdf_error(const vector<double>& result)
{
    // check the length of the result vector.
    if(result.size() != _grids->size())
        {
        cerr << " CPDF : length of result vector ("<<result.size()+") does not fit NumberOfMembers ("<<_grids->size()+")!";
        exit(1);
        }
    
    vector<double> pdferr;
    
    // if we don't want the pdf error, return 0s
    if(!_pdf_error)
        {
        pdferr.push_back(0.0);
        pdferr.push_back(0.0);
        }
    // Otherwise check provider by provider
    else if (_provider == "MSTW")
        pdferr = (_pert_order == 0) ? MSTW_pdferror(result) : MSTW_pdf_as_error(result);
    else if (_provider == "GJR")
        pdferr = GJR_pdferror(result);
    else if (_provider == "ABKM")
        pdferr = ABKM_pdferror(result);
    else if (_provider == "NNPDF")
        pdferr = NNPDF_pdferror(result);
    else
        {
        cerr << "PDF error is not supported for provider "<<_provider;
        exit(1);
        }
    
    return pdferr;
}

vector<double> PDFHub::MSTW_pdf_as_error(const vector<double>& result)
{
    unsigned Nmax = _grids->size()/3 - 1;
    
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
    unsigned Nmax = _grids->size() - 1;
    
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
    
    for(unsigned i=1; i<=(_grids->size()-1)/2; ++i)
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
    
    for(unsigned i=1; i<_grids->size(); ++i)
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
    
    for(unsigned i=1; i<_grids->size(); i++)
        err += pow(result[i]-cval,2);
    
    err = sqrt(err/(_grids->size()-2))/cval*100.0;
    
    vector<double> ris;
    ris.push_back(err);
    ris.push_back(-err);
    
    return ris;
}




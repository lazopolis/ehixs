#include <string>
#include <iostream>
#include <vector>
#include <math.h>
#include <stdlib.h>

using namespace std;

#include "LHAPDF/LHAPDF.h"
#include <iterator>
#include "interpolator.h"

#include "pdf.h"



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
    //cout<<"\n*****\t my interpolators size:"<<my_interpolators.size()<<endl;
    

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

//------------------------------------------------------------------------------

CPDF::~CPDF()
{
     cout<<"\n@@@@destructor called"<<endl;
  for(unsigned i=0; i<my_interpolators.size(); ++i)
    delete my_interpolators[i];
}

double CPDF::give_f(const double& x, unsigned i)
{
    return my_cached_interpolators[i]->give_f(x);
    
}


CPDF::CPDF(const PDFGrid& the_grid,const pdf_desc& my_desc,
           double NF, double muf, double mur)
    :_my_desc(my_desc)
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


//------------------------------------------------------------------------------

//
//
//
////: PDF_on_the_fly
//
//
//double SinglePDFMemberInterpolator::f_value(const double & x)
//     {
//     return LHAPDF::xfx(x,muf,parton)/x;
//     }
//
//
//
//
//
//PDF_on_the_fly::PDF_on_the_fly(int parton, const string& provider_,
//                               double muf, int pert_order_,  bool pdf_error_)
//:  provider(provider_),
//pdf_error(pdf_error_),
//pert_order(pert_order_)
//{
//     // Print friendly message
//     cout << "\n[CPDF::" << __func__ << "] Attempting init of '" << provider << "' pdf at O(as^" << pert_order ;//<< ") ,error='" << pdf_error << "'";
//          
//     // from "provider", "pdferror" and "pert_order", we can determine the exact grid(s) we want to use.
//     // This is done in the function "determine_grids" which has to be modified if
//     // one wants to change / include new pdf sets.
//     determine_grids();
//     
//     NumberOfMembers = 0;
//     LHAPDF::setVerbosity(LHAPDF::SILENT);
//     
//     for(unsigned grid=0; grid<gridnames.size(); ++grid)
//          {
//          // initialise the LHAPDF set corresponding to grids[grid]
//          LHAPDF::initPDFSet(gridnames[grid], LHAPDF::LHGRID, 0);
//          
//          // Using the LHAPDF-routine numberPDF, we can determine the number of members of the currently loaded set
//          unsigned membernum = (pdf_error ? (LHAPDF::numberPDF()+1) : 1);
//          
//          // thank you, GJR, for this additional line of code
//          if((provider == "GJR") && (pert_order == 0)) membernum=1;
//          
//          NumberOfMembers += membernum;
//          
//          // loop over members:
//          for (int member=0; member<membernum; member++)
//               {
//               // thank you, GJR, for this additional 2 lines of code
//               if((provider == "GJR") && (pert_order == 0)) ++member;
//               
//               // initialise the correct member set
//               LHAPDF::usePDFMember(member);
//               
//               // retrieve the alpha_s of this member !!! hardcoded m_z ok?
//               alpha_s_at_mz.push_back(LHAPDF::alphasPDF(91.1876));
//               
//               // perform the interpolation
//               my_interpolators.push_back(new SinglePDFMemberInterpolator(parton,muf));
//               }
//          }
//     
//     cout << ": Success [" << NumberOfMembers << "] sets!";
//     //     cout<<"\n*****\t my interpolators size:"<<my_interpolators.size()<<endl;
//     
//}
//
//
//
//
//void PDF_on_the_fly::determine_grids()
//{
//     bool ef = false;
//     
//     if (provider == "MSTW")
//          {
//          switch(pert_order)
//               {
//                    case 0:
//                    gridnames.push_back("MSTW2008lo68cl");
//                    break;
//                    case 1:
//                    gridnames.push_back("MSTW2008nlo68cl");
//                    if(pdf_error)
//                         {
//                         gridnames.push_back("MSTW2008nlo68cl_asmz+68cl");
//                         gridnames.push_back("MSTW2008nlo68cl_asmz-68cl");
//                         }
//                    break;
//                    case 2:
//                    gridnames.push_back("MSTW2008nnlo68cl");
//                    if(pdf_error)
//                         {
//                         gridnames.push_back("MSTW2008nnlo68cl_asmz+68cl");
//                         gridnames.push_back("MSTW2008nnlo68cl_asmz-68cl");
//                         }
//                    break;
//                    default:
//                    ef=true;
//               }
//          }
//     else if (provider == "ABKM")
//          {
//          switch(pert_order)
//               {
//                    case 0:
//                    gridnames.push_back("a02m_lo");
//                    break;
//                    case 1:
//                    gridnames.push_back("abkm09_5_nlo");
//                    break;
//                    case 2:
//                    gridnames.push_back("abkm09_5_nnlo");
//                    break;
//                    default:
//                    ef = true;
//               }
//          }
//     else if (provider == "GJR")
//          {
//          switch(pert_order)
//               {
//                    case 0:
//                    gridnames.push_back("GJR08lo");
//                    break;
//                    case 1:
//                    gridnames.push_back("GJR08VFnloE");
//                    break;
//                    case 2:
//                    gridnames.push_back("JR09VFnnloE");
//                    break;
//                    default:
//                    ef = true;
//               }
//          }
//     else if (provider == "NNPDF")
//          {
//          switch(pert_order)
//               {
//                    case 0:
//                    gridnames.push_back("NNPDF21_lo_as_0119_100");
//                    break;
//                    case 1:
//                    gridnames.push_back("NNPDF21_100");
//                    break;
//                    case 2:
//                    gridnames.push_back("NNPDF21_nnlo_100");
//                    break;
//                    default:
//                    ef = 0;
//               }
//          }
//     else {
//          std::cerr << "Error: PDF-provider '" << provider << "' is not supported.";
//          exit(1);
//     }
//     
//     // If error flag ef have set then quit.
//     if(ef)
//          {
//          std::cerr << "Error: PDF-provider '" << provider << "' does not support perturbative order " << pert_order << ".";
//          exit(1);
//          }
//     
//}
//
//PDF_on_the_fly::~PDF_on_the_fly()
//{
//     for (unsigned i=0;i<my_interpolators.size();i++) delete my_interpolators[i];
//}
//
//
//DPDF::DPDF( int _iparton, int _n_as, int _n_eps,  int _from_parton)
//{
//     iparton = _iparton;
//     from_parton=_from_parton;
//     a_power = _n_as;
//     e_power= _n_eps;
//}
//
//
//
//
//
//
//void DPDF::init(const string& _provider,const double& _muf,int _pert_order, bool _pdf_error)
//{
//     
//     my_pdf=new PDF_on_the_fly(from_parton,_provider,_muf,_pert_order,_pdf_error);
//     my_kernel = new Kernel(iparton,from_parton,a_power,e_power);
//
//}
//
//double DPDF::give_f(const double & x, int i)
//{
//     if (e_power!=0) print_error_message_and_exit();
//     else
//          {
//          return my_pdf->give_f(x,i);
//          }
//}
//
//double DPDF::give_f(const double & x, const double & y, int i)
//{
//     if (e_power!=-1 or e_power!=-2) print_error_message_and_exit();
//     else
//          {
//          if (y<x) return 0.0; //: implementing theta(y>x)
//          else
//               {
//               double f_of_x_over_y = my_pdf->give_f(x/y,i);
//               double f_of_x = my_pdf->give_f(x,i);
//               double res=1.0/(1.0-x)*f_of_x
//                                   * (my_kernel->delta()+my_kernel->DDB())
//                         + f_of_x_over_y / y * my_kernel->reg(y)
//                         + (f_of_x_over_y/y - f_of_x)*my_kernel->DD(y)
//                    ;
//               return res;
//               }
//          }
//     
//}
//
//void DPDF::print_error_message_and_exit()
//{
//     cout<<"\nError in DPDF: you called give_f with the wrong number of arguments for a PDF of order e^("<<e_power<<"). I exit and you should debug!";
//     exit(1);
//}
//
//




#include <string>
#include <iostream>
#include <vector>
#include <math.h>
#include <stdlib.h>

using namespace std;

#include "luminosity.h"
#include<algorithm>

const double NewLuminosity::_almost_zero = 1e-12;

NewLuminosity::NewLuminosity(const UserInterface& UI)
{
    string gridname = determine_gridname(UI.pdf_provider,UI.perturbative_order);
    pdf_ = LHAPDF::mkPDF( gridname, 0);
    muf_ = UI.muf;


}

string NewLuminosity::determine_gridname(const string& provider,int order)
{
    cout<<"\n[NewLuminosity] perturbative order = "<<order<<endl;
    string mstw[3]={"MSTW2008lo68cl","MSTW2008nlo68cl","MSTW2008nnlo68cl"};
    if (provider == "MSTW") return mstw[order];
    else
    {
        cerr   << "Error: PDF-provider '" << provider
                    << "' is not supported.";
        exit(1);
    }
}

double NewLuminosity::give(const double& x1,const double& x2)
{
    if (x1>1.0-_almost_zero or x2>1.0-_almost_zero or x1<_almost_zero or x2<_almost_zero)
    {
        return 0.0;
    }
    double res=0.0;
    for (int i=0;i<pairs.size();i++)
    {
        res +=    pdf_->xfxQ(pairs[i].first,x1,muf_)
                * pdf_->xfxQ(pairs[i].second,x2,muf_)
                * coeff_[i];
    }
    return res;
}


double LuminositySinglePair::give(const double& x1,const double& x2)
{
    
    const double almost_zero =1e-6;// 1e-23;
    if (x1>1.0-almost_zero or x2>1.0-almost_zero or x1<almost_zero or x2<almost_zero)
    {
 //       cout<<"\n**";
        return 0.0;
    }
    else //: calculate lumi
    {
//        cout<<"\n[lumi] "<<left_->name()<<" x "<<right_->name()
//            <<"x1="<<x1<<" x2="<<x2
//            <<" f(x1)="<<left_->give_f(x1,0)
//            <<" f(x2)="<<right_->give_f(x2,0);
        
        return left_->give_f(x1,0) * right_->give_f(x2,0);
    }

}

void LuminosityStack::set(Luminosity* lumi, const string& description)
{
    if (description=="gg")
        {
            pdf_desc gluon(0);
            all_lumis_.push_back(lumi->give_single_pair_pt(gluon,gluon));
        }
    else if (description=="qg")    
        {
        for (int i=-5;i<6;i++)
            {
            if (i!=0) 
                {
                all_lumis_.push_back(lumi->give_single_pair_pt
                                     (pdf_desc(0),pdf_desc(i))
                                     );
                all_lumis_.push_back(lumi->give_single_pair_pt
                                         (pdf_desc(i),pdf_desc(0))
                                         );

                }
            }
        }
    else if (description=="qqb")
        {
        for (int i=-5;i<6;i++)
            {
            if (i!=0) 
                {
                    all_lumis_.push_back(lumi->give_single_pair_pt
                                         (pdf_desc(-i),pdf_desc(i))
                                         );
                }    
            }
        }
    else if (description=="qq")
        {
            for (int i=-5;i<6;i++)
            {
                if (i!=0) 
                {
                    all_lumis_.push_back(lumi->give_single_pair_pt
                                        (pdf_desc(i),pdf_desc(i))
                                        );
                }    
            }
        }
    else if (description=="q1q2")
        {
            for (int i=-5;i<6;i++)
            {
                if (i!=0) 
                {
                for (int j=-5;j<6;j++)
                    {
                    if (j!=0 and j!=i)
                        {
                        all_lumis_.push_back(lumi->give_single_pair_pt
                                    (pdf_desc(i),pdf_desc(j))
                                    );
                        }
                    }
                }    
            }
        }
    else
    {
        cout<<"\n[LuminosityStack]: error, unknown or non-implemented luminosity descrption "<<description;
    }
}

double LuminosityStack::give(const double& x1,const double& x2)
{
    double res=0.0;
    for (int i=0;i<all_lumis_.size();i++) 
        res += all_lumis_[i]->give(x1,x2);
    return res;
}

Luminosity::Luminosity(const UserInterface& UI)
{
    //cout<<"\n[Luminosity] setting up PDFHub";
    pdfHub = new PDFHub(UI);
    
    _local_current_luminosity = vector<double>(pdfHub->size(),0.0);
    _local_current_luminosity_LO = vector<double>(pdfHub->size(),0.0);
    
}



LuminositySinglePair* Luminosity::give_single_pair_pt(const pdf_desc & left,
                                                      const pdf_desc & right)
{
    CPDF* ptr_to_left = pdfHub->construct_or_locate_pdf(left);
    CPDF* ptr_to_right = pdfHub->construct_or_locate_pdf(right);
    LuminositySinglePair* new_pair = new LuminositySinglePair(
                                    ptr_to_left,ptr_to_right);
    all_single_pairs_.push_back(new_pair);
    return new_pair;

}


void Luminosity::add_pair(const pdf_desc & left, const pdf_desc & right)
{
    CPDF* ptr_to_left = pdfHub->construct_or_locate_pdf(left);
    CPDF* ptr_to_right = pdfHub->construct_or_locate_pdf(right);
    pairs.push_back(pair<CPDF*,CPDF*>(ptr_to_left, ptr_to_right));
}


void  Luminosity::set_cur_lumi(const double &x1,const double &x2)
{
    set_specific_lumi(x1,x2,&_local_current_luminosity);
}

void  Luminosity::set_cur_lumiLO(const double &x1,const double &x2)
{
    set_specific_lumi(x1,x2,&_local_current_luminosity_LO);
}

void  Luminosity::set_specific_lumi(const double &x1,const double &x2, vector<double>* which_lumi)
{
    double locres;
    const double almost_zero =1e-16;// 1e-23;
    
    if (x1>1.0-almost_zero or x2>1.0-almost_zero or x1<almost_zero or x2<almost_zero)
    {
        for (unsigned i=0;i<pdf_size();i++)
        {
            which_lumi->operator[](i)=0.0;
        }
    }
    else //: calculate lumi
    {
        for (unsigned i=0;i<pdf_size();i++)
        {
            locres=0.0;
            //cout<<"[Luminosity] : for member "<<i;
            for (unsigned k=0;k<pairs.size();k++)
            {
                //cout<<"; pair #"<<k;
                locres += pairs[k].first->give_f(x1,i)
                *pairs[k].second->give_f(x2,i)
                *x1*x2;
                //cout<<" locres = "<<locres<<endl;
            }
            which_lumi->operator[](i) = locres;
        }
    }
}

void  Luminosity::set_cur_lumi(const vector<double> &x1,const vector<double> &x2)
{
    set_specific_lumi(x1,x2,&_local_current_luminosity);
}

void  Luminosity::set_cur_lumiLO(const vector<double> &x1,const vector<double> &x2)
{
    set_specific_lumi(x1,x2,&_local_current_luminosity_LO);
}

void  Luminosity::set_specific_lumi(const vector<double> &x1,const vector<double> &x2, vector<double>* which_lumi)
{
    double locres;
    const double almost_zero =1e-16;// 1e-23;
    
    if (x1[0]>1.0-almost_zero or x2[0]>1.0-almost_zero or x1[0]<almost_zero or x2[0]<almost_zero)
    {
        for (unsigned i=0;i<pdf_size();i++)
        {
            which_lumi->operator[](i)=0.0;
        }
    }
    else //: calculate lumi
    {
        for (unsigned i=0;i<pdf_size();i++)
        {
            locres=0.0;
            //cout<<"[Luminosity] : for member "<<i;
            for (unsigned k=0;k<pairs.size();k++)
            {
                //cout<<"; pair #"<<k;
                locres += pairs[k].first->give_f(x1,i)
                *pairs[k].second->give_f(x2,i)
                *x1[0]*x2[0];
                //cout<<" locres = "<<locres<<endl;
            }
            which_lumi->operator[](i) = locres;
        }
    }
}













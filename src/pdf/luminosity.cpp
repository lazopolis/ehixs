#include <string>
#include <iostream>
#include <vector>
#include <math.h>
#include <stdlib.h>

using namespace std;

#include "luminosity.h"
#include<algorithm>


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
        return left_->give_f(x1,0) * right_->give_f(x2,0);
    }

}


Luminosity::Luminosity(const UserInterface& UI)
{
    cout<<"\nLuminosity : setting up PDFHub";
    pdfHub = new PDFHub(UI);
    
    _local_current_luminosity = vector<double>(pdfHub->size(),0.0);
    _local_current_luminosity_LO = vector<double>(pdfHub->size(),0.0);
    
}



LuminositySinglePair* Luminosity::give_single_pair_pt(const pdf_desc & left,
                                                      const pdf_desc & right)
{
    CPDF* ptr_to_left = pdfHub->construct_or_locate_pdf(left);
    CPDF* ptr_to_right = pdfHub->construct_or_locate_pdf(right);
    LuminositySinglePair* new_pair = new LuminositySinglePair(ptr_to_left,
                                                              ptr_to_right);
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
    double locres;
    const double almost_zero =1e-16;// 1e-23;
    
    
    
    if (x1>1.0-almost_zero or x2>1.0-almost_zero or x1<almost_zero or x2<almost_zero)
        {
        for (unsigned i=0;i<pdf_size();i++)
            {
            _local_current_luminosity[i]=0.0;
            }
        }
    else //: calculate lumi
        {
        for (unsigned i=0;i<pdf_size();i++)
            {
            locres=0.0;
            for (unsigned k=0;k<pairs.size();k++)
                {
                locres += pairs[k].first->give_f(x1,i)*pairs[k].second->give_f(x2,i)*x1*x2;
                
                }
            _local_current_luminosity[i]=locres;
            }
        }
}

void  Luminosity::set_cur_lumiLO(const double &x1,const double &x2)
{
    double locres;
    const double almost_zero =1e-16;// 1e-23;
    
    if (x1>1.0-almost_zero or x2>1.0-almost_zero or x1<almost_zero or x2<almost_zero)
        {
        for (unsigned i=0;i<pdf_size();i++)
            {
            _local_current_luminosity_LO[i]=0.0;
            }
        }
    else //: calculate lumi
        {
        for (unsigned i=0;i<pdf_size();i++)
            {
            locres=0.0;
            //cout<<"[Luminosity] : for member 1 ";
            for (unsigned k=0;k<pairs.size();k++)
                {
                //cout<<"; pair #"<<k;
                locres += pairs[k].first->give_f(x1,i)
                        *pairs[k].second->give_f(x2,i)
                        *x1*x2;
                //cout<<" locres = "<<locres<<endl;
                }
            _local_current_luminosity_LO[i]=locres;
            }
        }
}





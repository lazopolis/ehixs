/** \file CHistogram.cpp
  *
  * Implement CHistogram class.
  */

#include <string>
#include <vector>
#include <iostream>
#include <sstream>
#include <stdlib.h>

using namespace std;

//#include "ExclusiveClass.h"
#include "CBin.h"
#include "CHistogram.h"


CHistogram::CHistogram(unsigned numbins_, unsigned firstbin_, 
                       const double& lowend_, const double& highend_, 
                       const std::string& name_, bool adapt_)
  : name(name_), 
    numbins(numbins_+1), 
    firstbin(firstbin_), 
    highend(highend_), 
    lowend(lowend_), 
    adapt(adapt_)
{
  // dewived kwantities
  binsize = (highend-lowend)/(numbins-1);
  // push in all_bins
  for(unsigned i=0; i<numbins; ++i)
    all_bins.push_back(Bin());
}

void CHistogram::set_parameters(int NB,int LE, int HE)
{
     numbins=NB+1;lowend=LE;highend=HE;
     // dewived kwantities
     binsize = (highend-lowend)/(numbins-1);
     // clear all_bins that was created by constructor
     all_bins.clear();
     // push in all_bins
     for(unsigned i=0; i<numbins; ++i)
          all_bins.push_back(Bin());
     
}



void CHistogram::bin_event(Event* X,  const double& vegas_weight)
{
  // Get value to be binned
  const double val = determine_xval(X);
    //if (val>2.0) cout<<"\nw="<<weight*X->vegas_weight;
  // we're inside the histogram
  if((val >= lowend) && (val <= highend))
  {
    // Target bin
    const unsigned bin = firstbin + unsigned((val-lowend)/binsize);
    // If adapt then adapt !
    //if(adapt) X->ff_vegas[bin] += weight;
    // Bin
	  all_bins[bin-firstbin].add(vegas_weight*X->w);
  }
  else 
  {
      if (val==val) all_bins[numbins-1].add(vegas_weight * X->w);
      else 
      {
          cout<<"\n in CHistogram "<<name<<", val out of bounds and a nan :"<<val;
      }
  }
}

void CHistogram::end(unsigned NOP)
{
  for(unsigned i=0; i<numbins; ++i)
	  all_bins[i].update(NOP);
}

void CHistogram::update()
{
  for(unsigned i=0; i<numbins; ++i)
	  all_bins[i].end();
}

ostream& operator<<(ostream& stream, const CHistogram& hist)
{
  double total = 0;

  stream << "\n\n Histogram " << hist.name << endl;
  
  for(unsigned i=0; i<hist.numbins-1; ++i) //: -1 for the overflow bin to be treated separately
  {
    stream << "  [" << hist.lowend+hist.binsize*i << ", " << hist.lowend+hist.binsize*(i+1) << "]" 
           << " : " << hist.all_bins[i] << endl;
      
    total += hist.all_bins[i];
  }
  stream << "  [overflow] : "<<hist.all_bins[hist.numbins-1] << endl;
  stream << " total binned = " << total << endl;

  return stream;
}

string compare_histograms(const CHistogram* H1,const CHistogram* H2,const string & comp_type)
{
     stringstream  stream(stringstream::out);
     double total1 = 0;
     double total2 = 0;
     if (H1->name!=H2->name) stream<<"\n comparison of histograms impossible: the names don't match: "<<H1->name<<" vs "<<H2->name;
     else
          {
          stream << "\n\n Histogram comparison " << H1->name << endl;
     
          for(unsigned i=0; i<H1->numbins-1; ++i) //: -1 for the overflow bin to be treated separately
               {
               double H1_i=H1->all_bins[i].value();
               double H2_i=H2->all_bins[i].value();
               double err_i = max(H1->all_bins[i].error(),H2->all_bins[i].error());
               stream << "  [" << H1->lowend+H1->binsize*i
                              << ", " << H1->lowend+H1->binsize*(i+1)
                         << "]"<< " : "
               << H1_i <<" vs "<<H2_i;
               if (comp_type=="-") stream<<"\t| diff : "<<H1_i-H2_i;
               else if (comp_type=="+") stream<<"\t| sum : "<<H1_i+H2_i;
               stream<<"\t error : "<<err_i<< endl;
               total1 += H1->all_bins[i];
               total2 += H2->all_bins[i];
               }
          stream << "  [overflow] : "<<H1->all_bins[H1->numbins-1] << endl;
          stream << " total binned = " << total1 <<" vs "<<total2<<"\n\n"<< endl;
          }
     return stream.str();
}

string compare_histograms(const vector< CHistogram*> & all_H)
{
     stringstream  stream(stringstream::out);
     bool comparison_possible=true;
     for (int i=0;i<all_H.size();i++)
          {
          if (all_H[i]->name!=all_H[0]->name)
               {
               stream<<"\n comparison of histograms impossible: the names don't match: H[0]="<<all_H[0]->name<<" vs H["<<i<<"]="<<all_H[i]->name;
               comparison_possible=false;
               }
          }
     
     if (comparison_possible)
          {
          stream << "\n\n Histogram comparison " << all_H[0]->name << endl;
          
          vector<double> total(all_H.size(),0.0);
          for(unsigned i=0; i<all_H[0]->numbins-1; ++i) //: -1 for the overflow bin to be treated separately
               {
               double err=0.0;
               
               double sum_for_cur_bin=0.0;
               for (unsigned k=0;k<all_H.size();k++)
                    {
                    double Hcurbin_val=all_H[k]->all_bins[i].value();
                    total[k] += Hcurbin_val;
                    sum_for_cur_bin += Hcurbin_val;
                    //stream << "\t"<<Hcurbin_val;
                    err = max(err,all_H[k]->all_bins[i].error());
                    }
                    
               
               stream << "  [" << all_H[0]->lowend+all_H[0]->binsize*i
               << ", " << all_H[0]->lowend+all_H[0]->binsize*(i+1)
               << "]"<< " : ";
               for (unsigned k=0;k<all_H.size();k++) stream << "\t"<<all_H[k]->all_bins[i].value();
               stream<<"\t| sum : "<<sum_for_cur_bin;
               stream<<"\t error : "<<err;
               stream<< endl;

               }
          stream << "\n  [overflow] : ";
          for (unsigned k=0;k<all_H.size();k++)
               {
               stream<<"\t"<<all_H[k]->all_bins[all_H[k]->numbins-1];
               }
          stream<<"\n\ntotal binned =";
          for (unsigned k=0;k<all_H.size();k++)
               {
               stream<<"\t"<< total[k] ;
               }
          stream<<"\n\n\n";
          }
     return stream.str();
}


string CHistogram::plotinfo()
{
     stringstream  stream(stringstream::out);
     stream << "Histogram " << name << endl;
     stream<<"0.0\t0.0\t0.0"<<endl;
     for(unsigned i=0; i<numbins-1; ++i) //: -1 for the overflow bin to be treated separately
          {
          stream<< lowend+binsize*(i+1) << "\t" << all_bins[i].value()<<"\t"<<all_bins[i].error() << endl;
          }
     return stream.str();

}

string compare_histograms(const vector< CHistogram*> & all_H,bool color)
{
     stringstream  stream(stringstream::out);
     bool comparison_possible=true;
     for (int i=0;i<all_H.size();i++)
          {
          if (all_H[i]->name!=all_H[0]->name)
               {
               stream<<"\n comparison of histograms impossible: the names don't match: H[0]="<<all_H[0]->name<<" vs H["<<i<<"]="<<all_H[i]->name;
               comparison_possible=false;
               }
          }
     
     if (comparison_possible)
          {
          stream << "\n\n Histogram comparison " << all_H[0]->name << endl;
          
          vector<double> total(all_H.size(),0.0);
          for(unsigned i=0; i<all_H[0]->numbins-1; ++i) //: -1 for the overflow bin to be treated separately
               {
               double err=0.0;
               
               double sum_for_cur_bin=0.0;
               for (unsigned k=0;k<all_H.size();k++)
                    {
                    double Hcurbin_val=all_H[k]->all_bins[i].value();
                    total[k] += Hcurbin_val;
                    sum_for_cur_bin += Hcurbin_val;
                    //stream << "\t"<<Hcurbin_val;
                    err = max(err,all_H[k]->all_bins[i].error());
                    }
               
               string color_code;
               if (color)
                    {
                    if (fabs(sum_for_cur_bin)>3.0*err) color_code="\033[1;31m ";
                    else if (fabs(sum_for_cur_bin)>2.0*err) color_code="\033[0;35m ";
                    else if (fabs(sum_for_cur_bin)>1.0*err) color_code="\033[0;34m ";
                    else color_code ="";
                    }
               else color_code ="";
               stream<<color_code;
               stream << "  [" << all_H[0]->lowend+all_H[0]->binsize*i
               << ", " << all_H[0]->lowend+all_H[0]->binsize*(i+1)
               << "]"<< " : ";
               for (unsigned k=0;k<all_H.size();k++) stream << "\t"<<all_H[k]->all_bins[i].value();
               stream<<"\t| sum : "<<sum_for_cur_bin;
               stream<<"\t error : "<<err;
               if (fabs(sum_for_cur_bin)>err) stream<<" \033[0m";
               stream<< endl;
               
               }
          stream << "\n  [overflow] : ";
          for (unsigned k=0;k<all_H.size();k++)
               {
               stream<<"\t"<<all_H[k]->all_bins[all_H[k]->numbins-1];
               }
          stream<<"\n\ntotal binned =";
          for (unsigned k=0;k<all_H.size();k++)
               {
               stream<<"\t"<< total[k] ;
               }
          stream<<"\n\n\n";
          }
     return stream.str();
}

////:=====
//
//
//CHistogram2d::CHistogram2d(unsigned numbins1_,
//                       const double& lowend1_, const double& highend1_, 
//                           unsigned numbins2_,  
//                           const double& lowend2_, const double& highend2_, 
//                       const std::string& name_)
//: name(name_), 
//numbins(numbins1_*numbins2_+1), 
//numbins1(numbins1_), 
//numbins2(numbins2_), 
//highend1(highend1_), 
//lowend1(lowend1_), 
//highend2(highend2_), 
//lowend2(lowend2_)
//{
//    // dewived kwantities
//    binsize1 = (highend1-lowend1)/(numbins1);
//    binsize2 = (highend2-lowend2)/(numbins2);
//    // push in all_bins
//    for(unsigned i=0; i<numbins; ++i) all_bins.push_back(Bin());
//}
//
//void CHistogram2d::bin_event(Momenta* X, const double& weight,const double& vegas_weight)
//{
//    // Get value to be binned
//    const double val1 = determine_xval1(X);
//    const double val2 = determine_xval2(X);
//    //if (val>2.0) cout<<"\nw="<<weight*X->vegas_weight;
//    // we're inside the histogram
//    if((val1 >= lowend1) && (val1 <= highend1) and val2>=lowend2 and val2<=highend2)
//    {
//        // Target bin
//        const unsigned bin1 =  unsigned((val1-lowend1)/binsize1);
//        const unsigned bin2 =  unsigned((val2-lowend2)/binsize2);
//        //cout<<"\nbin1="<<bin1<<" \tbin2="<<bin2<<"\tbin="<<bin2*numbins1+bin1;
//        // If adapt then adapt !
//        //if(adapt) X->ff_vegas[bin] += weight;
//        // Bin
//        all_bins[bin2*numbins1+bin1].add(vegas_weight*weight);
//    }
//    else 
//    {
//        if (val1==val1 and val2==val2) all_bins[numbins-1].add(vegas_weight * weight);
//        else 
//        {
//            cout<<"\n in CHistogram , val out of bounds and a nan :"<<val1<<"\t"<<val2;
//        }
//    }
//}
//
//void CHistogram2d::end(unsigned NOP)
//{
//    for(unsigned i=0; i<numbins; ++i)
//        all_bins[i].update(NOP);
//}
//
//void CHistogram2d::update()
//{
//    for(unsigned i=0; i<numbins; ++i)
//        all_bins[i].end();
//}
//
//ostream& operator<<(ostream& stream, const CHistogram2d& hist)
//{
//    double total = 0;
//    
//    stream << "\n\n Histogram " << hist.name << endl;
//    
//    for(unsigned i=0; i<hist.numbins1; ++i) //: -1 for the overflow bin to be treated separately
//    {
//
//        for (unsigned j=0;j<hist.numbins2;++j)
//        {
//            stream  << "  [" <<hist.lowend1+hist.binsize1*i //+ hist.binsize1/2.0
//                    << ", "
//                    << hist.lowend2+hist.binsize2*j //+ hist.binsize2/2.0 
//                    << "] : "<<hist.all_bins[j*hist.numbins1+i]<<endl;
//            total += hist.all_bins[j*hist.numbins1+i];
//        }
//        
//        
//        
//    }
//    stream << "  [overflow] : "<<hist.all_bins[hist.numbins-1] << endl;
//    stream << " total binned = " << total << endl;
//    
//    return stream;
//}
//
//
////======
//
//
//
//
//AverageObservable::AverageObservable( const std::string& name_)
//: name(name_) {};
//
//void AverageObservable::bin_event(Momenta* X, const double& weight,const double& vegas_weight)
//{
//    // Get value to be binned
//    const double val = determine_xval(X);
//    
//    if (val==val) my_bin.add(vegas_weight*weight*val);
//    else 
//    {
//        cout<<"\n in AverageObservable "<<name<<", val out of bounds and a nan :"<<val;
//    }
//}
//
//void AverageObservable::end(unsigned NOP)
//{
//    my_bin.update(NOP);
//    
//}
//
//void AverageObservable::update()
//{
//    my_bin.end();
//}
//
//ostream& operator<<(ostream& stream, const AverageObservable& hist)
//{
//    stream << "\n\n Histogram " << hist.name << endl;
//    stream << "  [" << hist.name << ", x  ] : " << hist.my_bin << endl;
//    stream << " total binned = " << hist.my_bin << endl;
//    return stream;
//}
//



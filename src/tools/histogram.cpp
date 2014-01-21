/** \file CHistogram.cpp
  *
  * Implement CHistogram class.
  */

#include <string>
#include <vector>

#include <stdlib.h>

#include <iomanip>
using namespace std;

//#include "ExclusiveClass.h"

#include "histogram.h"


SimpleHistogram::SimpleHistogram(unsigned numbins_, unsigned firstbin_, 
                       const double& lowend_, const double& highend_, 
                       const std::string& name_)
  : _name(name_),
    _numbins(numbins_+1),
    _firstbin(firstbin_),
    _highend(highend_),
    _lowend(lowend_)
{
  // dewived kwantities
  _binsize = (_highend-_lowend)/(_numbins-1);
  // push in all_bins
  for(unsigned i=0; i<_numbins; ++i)
    _all_bins.push_back(Bin());
}



void SimpleHistogram::bin_val(const double& val,const double &w)
{
    //if (val>2.0) cout<<"\nw="<<weight*X->vegas_weight;
    // we're inside the histogram
    if((val >= _lowend) && (val <= _highend))
        {
        // Target bin
        const unsigned bin = _firstbin + unsigned((val-_lowend)/_binsize);
        // Bin
        _all_bins[bin-_firstbin].add(w);
        }
    else
        {
        if (val==val) _all_bins[_numbins-1].add(w);
        else
            {
            cout<<"\n in SimpleHistogram "<<_name
            <<", val out of bounds and a nan :"<<val;
            }
        }
}


void SimpleHistogram::set_parameters(int NB,int LE, int HE)
{
     _numbins=NB+1;_lowend=LE;_highend=HE;
     // dewived kwantities
     _binsize = (_highend-_lowend)/(_numbins-1);
     // clear _all_bins that was created by constructor
     _all_bins.clear();
     // push in all_bins
     for(unsigned i=0; i<_numbins; ++i)
          _all_bins.push_back(Bin());
     
}


void SimpleHistogram::end(unsigned NOP)
{
  for(unsigned i=0; i<_numbins; ++i)
	  _all_bins[i].update(NOP);
}

void SimpleHistogram::update()
{
  for(unsigned i=0; i<_numbins; ++i)
	  _all_bins[i].end();
}


string SimpleHistogram::xml()
{
    double total = 0;
    stringstream stream(stringstream::out);
    stream<<"\n\t<histogram name=\""<<_name<<"\">";
    for(unsigned i=0; i<_numbins-1; ++i)
        {
        stream<<"\n\t\t<bin le=\""<<_lowend+_binsize*i<<"\" he=\""
            <<_lowend+_binsize*(i+1)<<"\" >";
        stream<<_all_bins[i].xml();
        stream<<"</bin>";
        total += _all_bins[i];
        }
    stream<<"\n\t\t<overflow> "<<_all_bins[_numbins-1].xml()<<"</overflow>";
    stream<<"\n\t\t<totalbinned>"<< total <<"</totalbinned>";
    stream<<"\n\t</histogram>"<<endl;
    return stream.str();
}

ostream& operator<<(ostream& stream, const SimpleHistogram& hist)
{
    double total = 0;
    stream << "\n\n<histogram> " << hist._name << endl;
    stream  << "#      bin     |"
                  <<"     sigma    |"
                      <<"  error   |"
                    <<"    % error |"
            <<"    chi^2 /   dof   |"
            <<"  # of points "<<endl;
    stream <<"#--------------------------------------------------------"
    <<"----------------------------------"<<endl;
    //: -1 for the overflow bin to be treated separately
    for(unsigned i=0; i<hist._numbins-1; ++i)
        {
        stream  << "[" << setw(5)<<hist._lowend+hist._binsize*i
                << ", " <<setw(5)<< hist._lowend+hist._binsize*(i+1) << "]"
                << " | " << hist._all_bins[i] << endl;
        total += hist._all_bins[i];
        }
    stream << "[  overflow  ] | "<<hist._all_bins[hist._numbins-1] << endl;
    stream << "total binned = " << total << endl;
    stream<<"</histogram>"<<endl;
    return stream;
}

string compare_histograms(const SimpleHistogram* H1,const SimpleHistogram* H2,const string & comp_type)
{
     stringstream  stream(stringstream::out);
     double total1 = 0;
     double total2 = 0;
     if (H1->_name!=H2->_name) stream<<"\n comparison of histograms impossible: the names don't match: "<<H1->_name<<" vs "<<H2->_name;
     else
          {
          stream << "\n\n Histogram comparison " << H1->_name << endl;
     
          for(unsigned i=0; i<H1->_numbins-1; ++i) //: -1 for the overflow bin to be treated separately
               {
               double H1_i=H1->_all_bins[i].value();
               double H2_i=H2->_all_bins[i].value();
               double err_i = max(H1->_all_bins[i].error(),H2->_all_bins[i].error());
               stream << "  [" << H1->_lowend+H1->_binsize*i
                              << ", " << H1->_lowend+H1->_binsize*(i+1)
                         << "]"<< " : "
               << H1_i <<" vs "<<H2_i;
               if (comp_type=="-") stream<<"\t| diff : "<<H1_i-H2_i;
               else if (comp_type=="+") stream<<"\t| sum : "<<H1_i+H2_i;
               stream<<"\t error : "<<err_i<< endl;
               total1 += H1->_all_bins[i];
               total2 += H2->_all_bins[i];
               }
          stream << "  [overflow] : "<<H1->_all_bins[H1->_numbins-1] << endl;
          stream << " total binned = " << total1 <<" vs "<<total2<<"\n\n"<< endl;
          }
     return stream.str();
}

string compare_histograms(const vector< SimpleHistogram*> & all_H)
{
    
    stringstream  stream(stringstream::out);
     bool comparison_possible=true;
     for (int i=0;i<all_H.size();i++)
          {
          if (all_H[i]->_name!=all_H[0]->_name)
               {
               stream<<"\n comparison of histograms impossible: the names don't match: H[0]="<<all_H[0]->_name<<" vs H["<<i<<"]="<<all_H[i]->_name;
               comparison_possible=false;
               }
          }
     
     if (comparison_possible)
          {
          stream << "\n\n Histogram comparison " << all_H[0]->_name << endl;
          
          vector<double> total(all_H.size(),0.0);
          for(unsigned i=0; i<all_H[0]->_numbins-1; ++i) //: -1 for the overflow bin to be treated separately
               {
               double err=0.0;
               
               double sum_for_cur_bin=0.0;
               for (unsigned k=0;k<all_H.size();k++)
                    {
                    double Hcurbin_val=all_H[k]->_all_bins[i].value();
                    total[k] += Hcurbin_val;
                    sum_for_cur_bin += Hcurbin_val;
                    //stream << "\t"<<Hcurbin_val;
                    err = max(err,all_H[k]->_all_bins[i].error());
                    }
                    
               
               stream << "  [" << all_H[0]->_lowend+all_H[0]->_binsize*i
               << ", " << all_H[0]->_lowend+all_H[0]->_binsize*(i+1)
               << "]"<< " : ";
               for (unsigned k=0;k<all_H.size();k++) stream << "\t"<<all_H[k]->_all_bins[i].value();
               stream<<"\t| sum : "<<sum_for_cur_bin;
               stream<<"\t error : "<<err;
               stream<< endl;

               }
          stream << "\n  [overflow] : ";
          for (unsigned k=0;k<all_H.size();k++)
               {
               stream<<"\t"<<all_H[k]->_all_bins[all_H[k]->_numbins-1];
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


string SimpleHistogram::plotinfo()
{
     stringstream  stream(stringstream::out);
     stream << "Histogram " << _name << endl;
     stream<<"0.0\t0.0\t0.0"<<endl;
     for(unsigned i=0; i<_numbins-1; ++i) //: -1 for the overflow bin to be treated separately
          {
          stream<< _lowend+_binsize*(i+1) << "\t" << _all_bins[i].value()<<"\t"<<_all_bins[i].error() << endl;
          }
     return stream.str();

}

string compare_histograms(const vector< SimpleHistogram*> & all_H,bool color)
{
     stringstream  stream(stringstream::out);
     bool comparison_possible=true;
     for (int i=0;i<all_H.size();i++)
          {
          if (all_H[i]->_name!=all_H[0]->_name)
               {
               stream<<"\n comparison of histograms impossible: the names don't match: H[0]="<<all_H[0]->_name<<" vs H["<<i<<"]="<<all_H[i]->_name;
               comparison_possible=false;
               }
          }
     
     if (comparison_possible)
          {
          stream << "\n\n Histogram comparison " << all_H[0]->_name << endl;
          
          vector<double> total(all_H.size(),0.0);
          for(unsigned i=0; i<all_H[0]->_numbins-1; ++i) //: -1 for the overflow bin to be treated separately
               {
               double err=0.0;
               
               double sum_for_cur_bin=0.0;
               for (unsigned k=0;k<all_H.size();k++)
                    {
                    double Hcurbin_val=all_H[k]->_all_bins[i].value();
                    total[k] += Hcurbin_val;
                    sum_for_cur_bin += Hcurbin_val;
                    //stream << "\t"<<Hcurbin_val;
                    err = max(err,all_H[k]->_all_bins[i].error());
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
               stream << "  [" << all_H[0]->_lowend+all_H[0]->_binsize*i
               << ", " << all_H[0]->_lowend+all_H[0]->_binsize*(i+1)
               << "]"<< " : ";
               for (unsigned k=0;k<all_H.size();k++) stream << "\t"<<all_H[k]->_all_bins[i].value();
               stream<<"\t| sum : "<<sum_for_cur_bin;
               stream<<"\t error : "<<err;
               if (fabs(sum_for_cur_bin)>err) stream<<" \033[0m";
               stream<< endl;
               
               }
          stream << "\n  [overflow] : ";
          for (unsigned k=0;k<all_H.size();k++)
               {
               stream<<"\t"<<all_H[k]->_all_bins[all_H[k]->_numbins-1];
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




string compare_histograms(const vector< CHistogram*> & all_H,bool color)
{
    stringstream  stream(stringstream::out);
    bool comparison_possible=true;
    cout<<"\n Hello from compare_histograms. There are "<<all_H.size()<<
    " histos to compare."<<endl;

    for (int i=0;i<all_H.size();i++)
        {
        cout<<"\n i="<<i<<endl;
        if (all_H[i]->_name!=all_H[0]->_name)
            {
            stream<<"\n comparison of histograms impossible: the names don't match: H[0]="<<all_H[0]->_name<<" vs H["<<i<<"]="<<all_H[i]->_name;
            comparison_possible=false;
            }
        else
            {
            cout<<"\n name matched"<<endl;
            }
        }
    
    if (comparison_possible)
        {
        
        stream << "\n\n Histogram comparison " << all_H[0]->_name << endl;
        
        vector<double> total(all_H.size(),0.0);
        for(unsigned i=0; i<all_H[0]->_numbins-1; ++i) //: -1 for the overflow bin to be treated separately
            {
            double err=0.0;
            
            double sum_for_cur_bin=0.0;
            for (unsigned k=0;k<all_H.size();k++)
                {
                double Hcurbin_val=all_H[k]->_all_bins[i].value();
                total[k] += Hcurbin_val;
                sum_for_cur_bin += Hcurbin_val;
                //stream << "\t"<<Hcurbin_val;
                err = max(err,all_H[k]->_all_bins[i].error());
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
            stream << "  [" << all_H[0]->_lowend+all_H[0]->_binsize*i
            << ", " << all_H[0]->_lowend+all_H[0]->_binsize*(i+1)
            << "]"<< " : ";
            for (unsigned k=0;k<all_H.size();k++) stream << "\t"<<all_H[k]->_all_bins[i].value();
            stream<<"\t| sum : "<<sum_for_cur_bin;
            stream<<"\t error : "<<err;
            if (fabs(sum_for_cur_bin)>err) stream<<" \033[0m";
            stream<< endl;
            
            }
        stream << "\n  [overflow] : ";
        for (unsigned k=0;k<all_H.size();k++)
            {
            stream<<"\t"<<all_H[k]->_all_bins[all_H[k]->_numbins-1];
            }
        stream<<"\n\ntotal binned =";
        for (unsigned k=0;k<all_H.size();k++)
            {
            stream<<"\t"<< total[k] ;
            }
        stream<<"\n\n\n";
        }
    else
        {
        cout<<"\n comparison impossible for some reason"<<endl;
        }
    return stream.str();
}


string compare_histograms(const CHistogram* H1,const CHistogram* H2,const string & comp_type)
{
    stringstream  stream(stringstream::out);
    double total1 = 0;
    double total2 = 0;
    if (H1->_name!=H2->_name) stream<<"\n comparison of histograms impossible: the names don't match: "<<H1->_name<<" vs "<<H2->_name;
    else
        {
        stream << "\n\n Histogram comparison " << H1->_name << endl;
        
        for(unsigned i=0; i<H1->_numbins-1; ++i) //: -1 for the overflow bin to be treated separately
            {
            double H1_i=H1->_all_bins[i].value();
            double H2_i=H2->_all_bins[i].value();
            double err_i = max(H1->_all_bins[i].error(),H2->_all_bins[i].error());
            stream << "  [" << H1->_lowend+H1->_binsize*i
            << ", " << H1->_lowend+H1->_binsize*(i+1)
            << "]"<< " : "
            << H1_i <<" vs "<<H2_i;
            if (comp_type=="-") stream<<"\t| diff : "<<H1_i-H2_i;
            else if (comp_type=="+") stream<<"\t| sum : "<<H1_i+H2_i;
            stream<<"\t error : "<<err_i<< endl;
            total1 += H1->_all_bins[i];
            total2 += H2->_all_bins[i];
            }
        stream << "  [overflow] : "<<H1->_all_bins[H1->_numbins-1] << endl;
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
        if (all_H[i]->_name!=all_H[0]->_name)
            {
            stream<<"\n comparison of histograms impossible: the names don't match: H[0]="<<all_H[0]->_name<<" vs H["<<i<<"]="<<all_H[i]->_name;
            comparison_possible=false;
            }
        }
    
    if (comparison_possible)
        {
        stream << "\n\n Histogram comparison " << all_H[0]->_name << endl;
        
        vector<double> total(all_H.size(),0.0);
        for(unsigned i=0; i<all_H[0]->_numbins-1; ++i) //: -1 for the overflow bin to be treated separately
            {
            double err=0.0;
            
            double sum_for_cur_bin=0.0;
            for (unsigned k=0;k<all_H.size();k++)
                {
                double Hcurbin_val=all_H[k]->_all_bins[i].value();
                total[k] += Hcurbin_val;
                sum_for_cur_bin += Hcurbin_val;
                //stream << "\t"<<Hcurbin_val;
                err = max(err,all_H[k]->_all_bins[i].error());
                }
            
            
            stream << "  [" << all_H[0]->_lowend+all_H[0]->_binsize*i
            << ", " << all_H[0]->_lowend+all_H[0]->_binsize*(i+1)
            << "]"<< " : ";
            for (unsigned k=0;k<all_H.size();k++) stream << "\t"<<all_H[k]->_all_bins[i].value();
            stream<<"\t| sum : "<<sum_for_cur_bin;
            stream<<"\t error : "<<err;
            stream<< endl;
            
            }
        stream << "\n  [overflow] : ";
        for (unsigned k=0;k<all_H.size();k++)
            {
            stream<<"\t"<<all_H[k]->_all_bins[all_H[k]->_numbins-1];
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


double SimpleHistogram::tot_running_f()
{
double res=0.0;
for (int i=0;i<_all_bins.size();i++) res += _all_bins[i].give_running_f();
return res;
}


bool CHistogram::the_event_is_in_ith_bin(int bin_id,const CombinedEvent& X)
{
    const double val = determine_xval(X);
    if((val >= _lowend) && (val <= _highend))
    {
        // Target bin
        const unsigned bin = _firstbin + unsigned((val-_lowend)/_binsize);
        if (bin == bin_id) return true;
        else return false;
    }
    else
    {
        if (val==val) 
        {
            if (bin_id == _numbins-1){return true;}
            else return false;
        }
        else
        {
            cout<<"\n in CHistogram "<<_name<<", val out of bounds and a nan :"<<val;
        }
    }
}


void CHistogram::bin_event(const CombinedEvent& X,  const double& vegas_weight)
{
    // Get value to be binned
    const double val = determine_xval(X);
    //if (val>2.0) cout<<"\nw="<<weight*X->vegas_weight;
    // we're inside the histogram
    if((val >= _lowend) && (val <= _highend))
        {
        // Target bin
        const unsigned bin = _firstbin + unsigned((val-_lowend)/_binsize);
        // If adapt then adapt !
        //if(adapt) X->ff_vegas[bin] += weight;
        // Bin
        _all_bins[bin-_firstbin].add(vegas_weight*X.weight());
        }
    else
        {
        if (val==val) _all_bins[_numbins-1].add(vegas_weight * X.weight());
        else
            {
            cout<<"\n in CHistogram "<<_name<<", val out of bounds and a nan :"<<val;
            }
        }
}



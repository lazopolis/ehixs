/** \file CHistogram.h
  *
  * ...
  */

#ifndef CHISTOGRAM_H
#define CHISTOGRAM_H
#include <iostream>
#include <sstream>
using namespace std;
#include "event.h"
#include "bin.h"


class SimpleHistogram{
public:
    SimpleHistogram(unsigned, unsigned, const double&, const double &,
               const std::string &);
    ~SimpleHistogram(){};
    void bin_val(const double& x,const double &w);
    string give_name(){return _name;}
    void set_parameters(int NB,int LE, int HE);
    unsigned size() const { return _numbins; }
    void end(unsigned);
    void update();

    string info(){
        stringstream stream;stream<<give_name()
        <<" : "<<_numbins-1<<" bins\t["<<_lowend
        <<","<<_highend<<"]";return stream.str();
    }
    
    string plotinfo();
    string xml();
    friend ostream& operator<<(ostream&, const SimpleHistogram&);
    friend string compare_histograms( const SimpleHistogram* H1,const SimpleHistogram* H2,const string& comp_type);
    friend string compare_histograms( const vector<SimpleHistogram*> &);
    friend string compare_histograms( const vector<SimpleHistogram*> &,bool color);

    unsigned    _numbins;
    unsigned    _firstbin;
    double      _lowend;
    double      _highend;
    std::string _name;
    double      _binsize;
    std::vector<Bin> _all_bins;

};



/** \brief Histogramming class
  *
  * ... */
class CHistogram: public SimpleHistogram
{
protected:
 

public:
  /** \brief Constructor 
   *
   * Initialize all values ......... */
    CHistogram(unsigned numbins_, unsigned firstbin_,
               const double& lowend_, const double& highend_,
               const std::string& name_, bool adapt_)
    : SimpleHistogram( numbins_, firstbin_,
                      lowend_,highend_,
                      name_){};
  /** \brief Empty destructor */
  ~CHistogram() {}

   /** \brief ... */
  void bin_event(const CombinedEvent&,const double &);
  /** \brief */
  virtual double determine_xval(const CombinedEvent&) = 0;
    
    friend string compare_histograms( const CHistogram* H1,const CHistogram* H2,const string& comp_type);
    friend string compare_histograms( const vector<CHistogram*> &);
    friend string compare_histograms( const vector<CHistogram*> &,bool color);
  
};

#include "user_defined_histograms.h"


#endif

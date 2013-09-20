/** \file CHistogram.h
  *
  * ...
  */

#ifndef CHISTOGRAM_H
#define CHISTOGRAM_H
#include <iostream>
#include <sstream>
using namespace std;
#include "Momenta.h"
#include "bin.h"


class SimpleHistogram{
public:
    /** \brief Constructor
     *
     * Initialize all values ......... */
    SimpleHistogram(unsigned, unsigned, const double&, const double &,
               const std::string &);
    ~SimpleHistogram(){};
    //
    void bin_val(const double& x,const double &w);
    
    //
    /** \brief name getter*/
    string give_name(){return _name;}
    /** \brief parameter setter*/
    void set_parameters(int NB,int LE, int HE);
    unsigned size() const { return _numbins; }
    
    
    /** \brief Call Bin::end() on each bin. */
    void end(unsigned);
    /** \brief Call Bin::update() on each bin. */
    void update();
    /** \brief provide histogram information */
    string info(){
        stringstream stream;stream<<give_name()
        <<" : "<<_numbins-1<<" bins\t["<<_lowend
        <<","<<_highend<<"]";return stream.str();
    }
    
    string plotinfo();
    
    friend ostream& operator<<(ostream&, const SimpleHistogram&);
    friend string compare_histograms( const SimpleHistogram* H1,const SimpleHistogram* H2,const string& comp_type);
    friend string compare_histograms( const vector<SimpleHistogram*> &);
    friend string compare_histograms( const vector<SimpleHistogram*> &,bool color);

    /** \brief Number of bins */
    unsigned    _numbins;
    /** \brief First bin */
    unsigned    _firstbin;
    /** \brief Minimal value */
    double      _lowend;
    /** \brief Maximal value */
    double      _highend;
    /** \brief Name of the histogram */
    std::string _name;
    /** \brief Size of the bin */
    double      _binsize;
    /** \brief Bins */
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
  void bin_event(Event*,const double &);
  /** \brief */
  virtual double determine_xval(Event*) = 0;
    
    friend string compare_histograms( const CHistogram* H1,const CHistogram* H2,const string& comp_type);
    friend string compare_histograms( const vector<CHistogram*> &);
    friend string compare_histograms( const vector<CHistogram*> &,bool color);
  
};




#endif

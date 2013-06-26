/** \file CHistogram.h
  *
  * ...
  */

#ifndef CHISTOGRAM_H
#define CHISTOGRAM_H

#include "Momenta.h"


/** \brief Histogramming class
  *
  * ... */
class CHistogram
{
protected:
  /** \brief Number of bins */
  unsigned    numbins;
  /** \brief First bin */
  unsigned    firstbin;
  /** \brief Minimal value */
  double      lowend;
  /** \brief Maximal value */
  double      highend;
  /** \brief Name of the histogram */
  std::string name;
  /** \brief Size of the bin */
  double      binsize;

  /** \brief Bins */
  std::vector<Bin> all_bins;

public:
  /** \brief Constructor 
   *
   * Initialize all values ......... */
  CHistogram(unsigned, unsigned, const double&, const double &, const std::string &, bool=false);
  /** \brief Empty destructor */
  ~CHistogram() {}

  /** \brief Adaptation mode */
  bool adapt;
  /** \brief name getter*/
  string give_name(){return name;}
  /** \brief parameter setter*/
     void set_parameters(int NB,int LE, int HE);
  /** \brief ... */
  void bin_event(Event*,const double &);
  /** \brief */
  virtual double determine_xval(Event*) = 0;
  /** \brief Return number of bins */
  unsigned size() const
  { return numbins; }

  /** \brief Obscure print function */
//  std::string print(const vector<double>&, const vector<double>&, const vector<double>&);
  /** \brief Call Bin::end() on each bin. */
  void end(unsigned);
  /** \brief Call Bin::update() on each bin. */
  void update();
     /** \brief provide histogram information */
     string info(){stringstream stream;stream<<give_name()<<" : "<<numbins-1<<" bins\t["<<lowend
          <<","<<highend<<"]";return stream.str();}
     
     string plotinfo();
     
     friend ostream& operator<<(ostream&, const CHistogram&);
     friend string compare_histograms( const CHistogram* H1,const CHistogram* H2,const string& comp_type);
     friend string compare_histograms( const vector<CHistogram*> &);
     friend string compare_histograms( const vector<CHistogram*> &,bool color);
};

//
//
//class CHistogram2d
//{
//protected:
//    /** \brief Number of bins */
//    unsigned    numbins,numbins1,numbins2;
//    /** \brief First bin */
//    unsigned    firstbin1,firstbin2;
//    /** \brief Minimal value */
//    double      lowend1,lowend2;
//    /** \brief Maximal value */
//    double      highend1,highend2;
//    /** \brief Name of the histogram */
//    std::string name;
//    /** \brief Size of the bin */
//    double      binsize1,binsize2;
//    
//    /** \brief Bins */
//    std::vector<Bin> all_bins;
//    
//public:
//    /** \brief Constructor 
//     *
//     * Initialize all values ......... */
//    CHistogram2d(unsigned, const double&, const double &, 
//                 unsigned, const double&, const double &,
//                 const std::string &);
//    /** \brief Empty destructor */
//    ~CHistogram2d() {}
//    
//
//    
//    /** \brief ... */
//    void bin_event(Momenta *, const double &, const double &);
//    /** \brief */
//    virtual double determine_xval1(Momenta*) = 0;
//    virtual double determine_xval2(Momenta*) = 0;
//    /** \brief Return number of bins */
//    unsigned size() const
//    { return numbins; }
//    
//    /** \brief Obscure print function */
//    //  std::string print(const vector<double>&, const vector<double>&, const vector<double>&);
//    /** \brief Call Bin::end() on each bin. */
//    void end(unsigned);
//    /** \brief Call Bin::update() on each bin. */
//    void update();
//    
//    friend ostream& operator<<(ostream&, const CHistogram2d&);
//};
//
//
///** \brief Histogramming class
// *
// * ... */
//class AverageObservable
//{
//protected:
//    
//    /** \brief Name of the histogram */
//    std::string name;
//    
//    
//    /** \brief Bins */
//    Bin my_bin;
//    
//public:
//    /** \brief Constructor 
//     *
//     * Initialize all values ......... */
//    AverageObservable(const std::string &);
//    /** \brief Empty destructor */
//    ~AverageObservable() {}
//    
//    /** \brief ... */
//    void bin_event(Momenta *, const double &, const double &);
//    /** \brief */
//    virtual double determine_xval(Momenta*) = 0;
//    /** \brief Return number of bins */
//    unsigned size() const
//    { return 1; }
//    
//    /** \brief Call Bin::end() on each bin. */
//    void end(unsigned);
//    /** \brief Call Bin::update() on each bin. */
//    void update();
//    
//    friend ostream& operator<<(ostream&, const AverageObservable&);
//};
//
//


#endif

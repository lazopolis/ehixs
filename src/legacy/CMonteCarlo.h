/** \brief \brief CMontecarlo.h
  *
  * ...
  */

#ifndef MONTECARLO_H
#define MONTECARLO_H

/** \brief Monte-Carlo parameters
  *
  * This class encloses all parameters relevant for the Mont-Carlo (VEGAS) integration. */
struct CMonteCarlo
{
  /** \brief Constructor
    *
    * Initialize to default values */
  CMonteCarlo();
  /** \brief Empty */
  ~CMonteCarlo() {}

  /** \brief Read runcard parameters */
  void read(const string&, const  string &);

  int imethod;
  double epsrel;
  double epsabs;
  int ntherm;
  int nref;
  int nstart;
  int nincrease;
  int deadtime;
  int AdaptToFirstOnly;
  int consflag;
  int verbose;
  int ndim;
  int production_dim;
  int decay_dim;
  int mineval;
  int maxeval;

  friend ostream& operator<<(ostream&, const CMonteCarlo&);
};

#endif

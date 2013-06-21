/** \file parametrization.hpp
  *
  * Define parametrization objects which basically acts as maps from the abstract phase space to invriants.
  * 
  * \author Romain Mueller, muellrom@ethz.ch
  */

const double Pi = 3.14159265358979323846264338328;

/** \brief Parametrization Identifier */
enum ParametrizationID {
  EnergyAngles,
  Hierarchical
};

/** \brief Parametrization
  *
  * Abstract class defining a general map from the phase space point x[] to invariants. This mother class only defines...
  */
struct Parametrization
{
  /** Invariants */
  /** \brief s12 (pure virtual) */
  virtual double s12() const = 0;
  /** \brief s13 (pure virtual) */
  virtual double s13() const = 0;
  /** \brief s14 (pure virtual) */
  virtual double s14() const = 0;
  /** \brief s23 (pure virtual) */
  virtual double s23() const = 0;
  /** \brief s24 (pure virtual) */
  virtual double s24() const = 0;
  /** \brief s34 (pure virtual) */
  virtual double s34() const = 0;
  /** \brief s134 (virtual) */
  virtual double s134() const
  { return s13()+s34()+s14(); }
  /** \brief s234 (virtual) */
  virtual double s234() const
  { return s23()+s34()+s24(); }
  /** \brief s124 (virtual) */
  virtual double s124() const
  { return s12()+s24()+s14(); }
  /** \brief s123 (virtual) */
  virtual double s123() const 
  { return s12()+s23()+s13(); }

  virtual double operator[](unsigned) const = 0;
};


/** \brief Copy phase space point
  *
  * Copies the values of x1, ..., x4, z, s, bx1, bx2.
  */
class ParametrizationCpy
  : public Parametrization
{
protected:
  // phase space point
  double x[8];

public:
  /** Construct from random number array */
  ParametrizationCpy(double x1, double x2, double x3, double x4, double z, double s, double bx1, double bx2)
  { x[0]=x1; x[1]=x2; x[2]=x3; x[3]=x4; x[4]=z; x[5]=s; x[6]=bx1; x[7]=bx2; }

  double s12() const
  { return x[5]; }

  /** Return phase space point. */
  double operator[](unsigned i) const
  { return x[i]; }
};

/** Flip wrapper
  *
  * Wrapper for 1<->2 flip at the invariant level.
  */
class ParametrizationFlip12
  : public Parametrization
{
  Parametrization& P;

public:
  ParametrizationFlip12(Parametrization& _P)
    : P(_P)
  {}

  double operator[](unsigned i) const
  { return P[i]; }

  double s12() const  { return P.s12(); }
  double s13() const  { return P.s23(); }
  double s14() const  { return P.s24(); }
  double s23() const  { return P.s13(); }
  double s24() const  { return P.s14(); }
  double s34() const  { return P.s34(); }
  double s134() const { return P.s234(); }
  double s234() const { return P.s134(); }
  double s124() const { return P.s124(); }
  double s123() const { return P.s123(); }
};

/** Flip wrapper
  *
  * Wrapper for 3<->4 flip at the invariant level.
  */
class ParametrizationFlip34
  : public Parametrization
{
  Parametrization& P;

public:
  ParametrizationFlip34(Parametrization& _P)
    : P(_P)
  {}

  double operator[](unsigned i) const
  { return P[i]; }

  double s12() const  { return P.s12(); }
  double s13() const  { return P.s14(); }
  double s14() const  { return P.s13(); }
  double s23() const  { return P.s24(); }
  double s24() const  { return P.s23(); }
  double s34() const  { return P.s34(); }
  double s134() const { return P.s134(); }
  double s234() const { return P.s234(); }
  double s124() const { return P.s123(); }
  double s123() const { return P.s124(); }
};

/** Flip wrapper
  *
  * Wrapper for 1<->2 & 3<->4 flips at the invariant level.
  */
class ParametrizationFlip1234
  : public Parametrization
{
  Parametrization& P;

public:
  ParametrizationFlip1234(Parametrization& _P)
    : P(_P)
  {}

  double operator[](unsigned i) const
  { return P[i]; }

  double s12() const  { return P.s12(); }
  double s13() const  { return P.s24(); }
  double s14() const  { return P.s23(); }
  double s23() const  { return P.s14(); }
  double s24() const  { return P.s13(); }
  double s34() const  { return P.s34(); }
  double s134() const { return P.s234(); }
  double s234() const { return P.s134(); }
  double s124() const { return P.s123(); }
  double s123() const { return P.s124(); }
};

/** \brief Template definition */
template<unsigned P>
class ParametrizationDef
  : public ParametrizationCpy
{};

/* ==================================================
   Energy and Angles
   ================================================== */

/** \brief x34 helper function for the EA parametrization */
inline double x34(double x1, double x2, double x3, double x4)
{
  return x3+x2-2*x3*x2-2*cos(Pi*x4)*sqrt(x3*(1-x3)*x2*(1-x2));
}

/** \brief kappa helper function for the EA parametrization */
inline double kappa(double x1, double x2, double x3, double x4, double z)
{
	const double x=x1*(1-x1)*(1-z)*x34(x1, x2, x3, x4);

	if(x<1e-3)
	    return 1+(1+(2+(5+(14+(42+132*x)*x)*x)*x)*x)*x;
	else
	   return (1-sqrt(1-4*x))/(2*x);
}

/** \brief Energy and angles
  *
  * This is the energy and angles parametrization, see paper for more details. Note that we inverted x2 and x4.
  */
template<>
struct ParametrizationDef<EnergyAngles>
  : public ParametrizationCpy
{
  ParametrizationDef(double x1, double x2, double x3, double x4, double z, double s, double bx1, double bx2)
    : ParametrizationCpy(x1, x2, x3, x4, z, s, bx1, bx2)
  {}

  double cos34() const
  { return 1-2*::x34(x[0], x[1], x[2], x[3]); }
  double kappa() const
  { return ::kappa(x[0], x[1], x[2], x[3], x[4]); }
  double s13() const
  { return -x[5]*(1-x[4])*kappa()*x[0]*x[2]; }
  double s23() const
  { return -x[5]*(1-x[4])*kappa()*x[0]*(1-x[2]); }
  double s14() const
  { return -x[5]*(1-x[4])*kappa()*(1-x[0])*x[1]; }
  double s24() const
  { return -x[5]*(1-x[4])*kappa()*(1-x[0])*(1-x[1]); }
  double s34() const
  { return  x[5]*(1-x[4])*(1-x[4])*kappa()*kappa()*x[0]*(1-x[0])*0.5*(1-cos34()); }
  double s134() const
  { return x[5]*(1-x[4])*kappa()*((1-x[4])*kappa()*x[0]*(1-x[0])*0.5*(1-cos34())-x[0]*x[2]-(1-x[0])*x[3]); }
  double s234() const
  { return x[5]*(1-x[4])*kappa()*((1-x[4])*kappa()*x[0]*(1-x[0])*0.5*(1-cos34())-x[0]*(1-x[2])-(1-x[0])*(1-x[3])); }
};

/* ==================================================
   Hierarchical
   ================================================== */

/** \brief Hierarchical
  *
  * This is the hierarchical (rapidity) parametrization, see paper for more details.
  */
template<>
struct ParametrizationDef<Hierarchical>
  : public ParametrizationCpy
{
  ParametrizationDef(double x1, double x2, double x3, double x4, double z, double s, double bx1, double bx2)
    : ParametrizationCpy(x1, x2, x3, x4, z, s, bx1, bx2)
  {}
  
  double h1() const
  { return x[4]+x[0]*(1-x[4]); }
  double h2() const
  { return 2*cos(Pi*x[3])*sqrt(x[1]*(1-x[1])*x[2]*(1-x[2])/h1()); }
  double s13() const
  { return -x[5]*(1-x[4])*x[0]*(x[2]*(1-x[1])+x[1]*(1-x[2])/h1()-h2()); }
  double s14() const
  { return -x[5]*(1-x[4])*x[0]*((1-x[2])*(1-x[1])+x[1]*x[2]/h1()+h2()); }
  double s23() const
  { return -x[5]*(1-x[4])*(1-x[0])*x[2]; }
  double s24() const
  { return -x[5]*(1-x[4])*(1-x[0])*(1-x[2]); }
  double s34() const
  { return  x[5]*(1-x[4])*(1-x[4])*x[0]*(1-x[0])*x[1]/(x[4]+x[0]*(1-x[4])); }
  double s134() const
  { return -x[5]*(1-x[4])*x[0]; }
  double s234() const
  { return -x[5]*(1-x[4])*(1-x[0])*(x[4]+x[0]*(1-x[1])*(1-x[4]))/h1(); }
};

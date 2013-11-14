/** \file fvector.h
  *
  * Created by achilleas on 4/24/09.
  * Revised by romain on 01/12
  * Copyright 2009/2012 ETH Zurich. All rights reserved.
  */

#ifndef FVECTOR_H
#define FVECTOR_H

#include <math.h>
#include <stdlib.h>
#include <iostream>
#include <iomanip>

/** \brief Four-vector (template decl.). */
template<class T>
class fvector_decl
{
  T v[4];

public:
  /** \brief Empty constructor 
    *
    * This constructor does nothing, i.e. internal array is not initialized. */
	fvector_decl() {}
  /** \brief Constructor 
    *
    * Initialize all components to t. */
	fvector_decl(const T& t)
    { for(unsigned i=0; i<4; ++i) v[i]=t; }
  /** \brief Constructor (components). */
  fvector_decl(const T& v1, const T& v2, const T& v3, const T& v4)
    { v[0]=v1; v[1]=v2; v[2]=v3; v[3]=v4; }
  /** \brief Copy constructor */
	fvector_decl(const fvector_decl & p)
     {v[0]=p[0];v[1]=p[1];v[2]=p[2];v[3]=p[3];}

  /** \brief destructor */
	~fvector_decl() {}

  /** \brief Assignment operator */
	fvector_decl& operator=(const fvector_decl& p)
     { 
          if (this!=&p)
          {
               for(unsigned i=0; i<4; ++i) 
                    {
                    v[i]=p.v[i];
                    }
          } 
          return *this;
     }

  /** \brief Rotate along arbitrary axis. */
	void rotate(const T&, const T&);
  /** \brief Rotate along one axis. */
	void rotate(const T&, unsigned);
  /** \brief Boost along arbitrary axis. */
	void boost(const T&, const T&, const T&);
  /** \brief Boost along one axis. */
	void boost(const T&, unsigned);

  /** \brief Lorentz square */
	T square() const;
  /** \brief Transverse component
    * 
    * ... */
	T pT() const
  { return sqrt(pow(v[1],2.0) + pow(v[2],2.0)); }
        /** \brief set
     * Sets all components
     * ... */
    void set(const T & v0,const T & v1,const T & v2,const T & v3)  {v[0]=v0;v[1]=v1;v[2]=v2;v[3]=v3;}
    /** \brief flush
     * Sets all components
     * ... */
    void flush()  {v[0]=0;v[1]=0;v[2]=0;v[3]=0;}
    
    /** \brief Phi
    * Calculates phi
    * ... */
  T phi() const ;
  /** \brief z-rapidity */
	T zrap() const;
    /** \brief abs z-rapidity */
	T abszrap() const;
  /** \brief p-rapidity */
	T prap() const;
  /** \brief JADE distance
    *
    * ... */
	T yJADE(const fvector_decl& p) const;
  /** \brief Absolute value of the 3-momentum */
	T abs_of_three_momentum() const;
  /** \brief Equality test */
	bool operator==(const fvector_decl& p) const
  { for(unsigned i=0; i<4; ++i) if(v[i]!=p[i]) return false; return true; }
  /** \brief Not-equality test */
	bool operator!=(const fvector_decl& p) const
  { return !(this->operator==(p)); }

  /** \brief Addition operator */
	fvector_decl operator+(const fvector_decl&) const;
  /** \brief Subtraction operator */
	fvector_decl operator-(const fvector_decl&) const;
  /** \brief Unary minus */
	fvector_decl operator-() const;
  /** \brief Minkowsky product
    *
    * Return the Lorentz invariant product of two vectors, using the most minus convention. */
	T operator*(const fvector_decl&) const;
  /** \brief Component access */ 
	T& operator[](unsigned i)
  { return v[i]; }
  /** \brief Component access (const) */
  const T& operator[](unsigned i) const
  { return v[i]; }

  template<class A>
	friend fvector_decl<A> operator*(const A&, const fvector_decl<A>&);
  template<class A>
	friend fvector_decl<A> operator*(const fvector_decl<A>&, const A&);

  /** \brief Ostream output 
    *
    * Prints all components in this form: [0, 1, 2, 3]. */
  template<class A>
	friend std::ostream& operator<<(std::ostream&, const fvector_decl<A>&);
};

// ==================================================
// Implementation

template<class T>
fvector_decl<T> fvector_decl<T>::operator+(const fvector_decl<T>& p) const
{
  fvector_decl<T> r;
  for(unsigned i=0; i<4; ++i)
    r[i] = v[i]+p[i];
  return r;
}  

template<class T>
fvector_decl<T> fvector_decl<T>::operator-(const fvector_decl<T>& p) const
{
  fvector_decl<T> r;
  for(unsigned i=0; i<4; ++i)
    r[i] = v[i]-p[i];
  return r;
}

template<class T>
fvector_decl<T> fvector_decl<T>::operator-() const
{
  fvector_decl<T> r;
  for(unsigned i=0; i<4; ++i)
    r[i] = -v[i];
  return r;
}

template<class T>
T fvector_decl<T>::operator*(const fvector_decl<T>& p) const
{
  T r = v[0]*p[0];
  for(unsigned i=1; i<4; ++i)
    r -= v[i]*p[i];
  return r;
}

template<class T>
fvector_decl<T> operator*(const fvector_decl<T>& v, const T& t)
{
  fvector_decl<T> r;
  for(unsigned i=0; i<4; ++i)
    r[i] = t*v[i];
  return r;
}

template<class T>
fvector_decl<T> operator*(const T& t, const fvector_decl<T>& v)
{
  fvector_decl<T> r;
  for(unsigned i=0; i<4; ++i)
    r[i] = t*v[i];
  return r;
}

template<class T>
void fvector_decl<T>::boost(const T& vx, const T& vy, const T& vz)
{
     // square
     const T vsq = vx*vx+vy*vy+vz*vz;
     // only do boost if vsq is different than zero (if vsq=0.0 the code below would set the components to nans)
     if (vsq!=0.0)
     {
          // gamma factor
          const T g   = 1/sqrt(1-vsq);
          // ... and compute the trsf matrix !
          const T vxx = 1+(g-1)*vx*vx/vsq;
          const T vyy = 1+(g-1)*vy*vy/vsq;
          const T vzz = 1+(g-1)*vz*vz/vsq;
          const T vxy = (g-1)*vx*vy/vsq;
          const T vyz = (g-1)*vy*vz/vsq;
          const T vzx = (g-1)*vz*vx/vsq;

          const T x0=    g*(v[0] - vx*v[1] - vy*v[2] - vz*v[3]);
          const T x1= -vx*g*v[0] + vxx*v[1] +vxy*v[2]	+ vzx*v[3];
          const T x2= -vy*g*v[0] + vxy*v[1] +vyy*v[2]	+ vyz*v[3];
          const T x3= -vz*g*v[0] + vzx*v[1] +vyz*v[2]	+ vzz*v[3];

          // set values
          v[0]=x0;
          v[1]=x1;
          v[2]=x2;
          v[3]=x3;
     }
}

template<class T>
void fvector_decl<T>::boost(const T& eta, unsigned axis)
{
  // compute gamma etc.
  const T cheta = cosh(eta);
  const T sheta = sinh(eta);
  // save the energy
  const T E = v[0];
  // and compute
  v[0] = cheta*v[0]+sheta*v[axis];
  v[axis] = sheta*E+cheta*v[axis];
}

template<class T>
void fvector_decl<T>::rotate(const T& theta, unsigned axis)
{
  // compute sin & cos
  const T cth = cos(theta);
  const T sth = sin(theta);
  // rotate
  unsigned i = (axis+1);
  unsigned j = (axis+2);
  if( i>3 ) i-=3;
  if( j>3 ) j-=3;
  const T xi =  cth*v[i] + sth*v[j];
  const T xj = -sth*v[i] + cth*v[j];
  // and set
  v[i] = xi; 
  v[j] = xj;
    }

template<class T>
void fvector_decl<T>::rotate(const T& phi, const T& theta)
{
  // compute sines and coses
  const T c1 = cos(theta);
  const T s1 = sin(theta);
  const T c2 = cos(phi);
  const T s2 = sin(phi);
  // rotate
  const T px = ( c2*v[1] + s2*c1*v[2] + s2*s1*v[3]);
  const T py = (-s2*v[1] + c2*c1*v[2] + c2*s1*v[3]);
  const T pz = (-s1*v[2] + c1*v[3]); 
  // and set
  v[1] = px;
  v[2] = py;
  v[3] = pz;
}

template<class T>
T fvector_decl<T>::square() const
{
  T r = v[0]*v[0];
  for(unsigned i=1; i<4; ++i)
    r -= v[i]*v[i];
  return r;
}

template<class T>
T fvector_decl<T>::phi() const 
{
    //: if my_phi == 100 we need to calculate it
   // if (my_phi==100.0)
    //{
        // this is not good
        T pi=3.141592653589793;
    T res=0.0;
        // handle special values
        if(v[1]==0.0)
        {
            if (v[2]>0) res = pi/2;
                
            else if (v[2]<0) res = 3*pi/2;
            else    
            {
                std::cout << "\nerror in fvector_decl.phi() : both x and y are 0.0, so phi is undefined ";
                exit(0);
            }
        }
        else 
        {
            // that's in the interval [-pi/2,pi/2]
            res = atan(v[2]/v[1]);
            if (v[1]<0) res += pi;
            else //: v[1]>0
            {
                if (v[2]<0) res += 2*pi;
            }
        }
        
   // }
    return res;
  
}

template<class T>
T fvector_decl<T>::zrap() const 
{ return log((v[0]+v[3])/(v[0]-v[3]))/2; }

template<class T>
T fvector_decl<T>::abszrap() const 
{ return abs(log((v[0]+v[3])/(v[0]-v[3]))/2); }

template<class T>
T fvector_decl<T>::prap() const
{
  T norm = sqrt(v[1]*v[1]+v[2]*v[2]+v[3]*v[3]);
  return log((norm+v[3])/(norm-v[3]))/2;
}

template<class T>
T fvector_decl<T>::abs_of_three_momentum() const
{
  T r = 0;
  for(unsigned i=0; i<4; ++i)
    r += v[i]*v[i];
  return sqrt(r);
}

template<class T>
T fvector_decl<T>::yJADE(const fvector_decl<T>& p2) const
{
  // norms
  T norm1 = sqrt(v[1]*v[1]+v[2]*v[2]+v[3]*v[3]);
  T norm2 = sqrt(p2[1]*p2[1]+p2[2]*p2[2]+p2[3]*p2[3]);
  // special value handling
  if(norm1 == 0 || norm2 ==0) return 0;
  // JA4E
  return v[0]*p2[0]*(1-(v[1]*p2[1]+v[2]*p2[2]+v[3]*p2[3])/norm1/norm2)/120/120/2;    
}

template<class T>
std::ostream& operator<<(std::ostream& stream, const fvector_decl<T>& f)
{
  const unsigned prec=6;
  stream << '[';
  for(unsigned i=0; i<3; ++i)
	  stream << std::setprecision(prec) << f[i] <<", ";
     stream<<std::setprecision(prec) << f[3];
  stream << ']';

  return stream;
}

// ==================================================

/** \brief Four-vector. */
typedef fvector_decl<double> fvector;

#endif
